# ================================
# Macro Asset Intelligence Dashboard
# FINAL VERSION - Full Market Structure
# ================================
library(quantmod)
library(TTR)
library(zoo)
library(vars)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
options(getSymbols.warning4.0 = FALSE)

# ================================
# DATA PIPELINE
# ================================
decision_asset_map <- c(
  Gold = "GC=F",
  Silver = "SI=F",
  BTC = "BTC-USD",
  ETH = "ETH-USD",
  Oil = "CL=F",
  USDJPY = "JPY=X",
  EURUSD = "EURUSD=X",
  USDTEC = "^NDX"
)
decision_symbols <- unname(decision_asset_map)
macro_symbols <- c("SPY", "QQQ", "TLT", "DX-Y.NYB", "GLD", "CL=F", "BTC-USD")
data_start_date <- "2020-01-01"

getSymbols(unique(c(decision_symbols, macro_symbols)), src = "yahoo", from = data_start_date)

decision_series <- lapply(decision_asset_map, function(symbol) {
  na.omit(Cl(getSymbols(symbol, src = "yahoo", from = data_start_date, auto.assign = FALSE)))
})

data <- Reduce(function(x, y) merge(x, y, join = "inner"), decision_series)
colnames(data) <- names(decision_asset_map)

data <- na.omit(data)
data <- data[complete.cases(data), ]
data <- data[apply(data, 1, function(r) all(r > 0)), ]

if (is.null(data) || nrow(data) < 50) {
  stop("Data tidak cukup / gagal load dari Yahoo")
}

spy <- na.omit(Cl(SPY))
qqq <- na.omit(Cl(QQQ))
tlt <- na.omit(Cl(TLT))
dxy <- na.omit(Cl(`DX-Y.NYB`))
gld <- na.omit(Cl(GLD))
macro_oil <- na.omit(Cl(`CL=F`))
macro_btc <- na.omit(Cl(`BTC-USD`))

macro_data <- merge(spy, qqq, join = "inner")
macro_data <- merge(macro_data, tlt, join = "inner")
macro_data <- merge(macro_data, dxy, join = "inner")
macro_data <- merge(macro_data, gld, join = "inner")
macro_data <- merge(macro_data, macro_oil, join = "inner")
macro_data <- merge(macro_data, macro_btc, join = "inner")
colnames(macro_data) <- c("SPY", "QQQ", "TLT", "DXY", "GLD", "Oil", "BTC")

macro_data <- na.omit(macro_data)
macro_data <- macro_data[complete.cases(macro_data), ]
macro_data <- macro_data[apply(macro_data, 1, function(r) all(r > 0)), ]

if (is.null(macro_data) || nrow(macro_data) < 50) {
  stop("Macro context basket tidak cukup / gagal load dari Yahoo")
}

# ================================
# FEATURE ENGINEERING
# ================================
ret <- ROC(data, type = "continuous")
ret <- na.omit(ret)
macro_ret <- ROC(macro_data, type = "continuous")
macro_ret <- na.omit(macro_ret)

if (nrow(ret) < 50) {
  stop("Return tidak cukup untuk modeling")
}
if (nrow(macro_ret) < 50) {
  stop("Macro return tidak cukup untuk modeling")
}

vol <- rollapply(ret, 20, sd, fill = NA)

# ================================
# MODEL
# ================================
var_model <- tryCatch({
  VAR(macro_ret, p = 2, type = "const")
}, error = function(e) NULL)

pred <- tryCatch({
  if (!is.null(var_model)) predict(var_model, n.ahead = 5) else NULL
}, error = function(e) NULL)

if (!is.null(pred)) {
  signal <- sapply(pred$fcst, function(x) x[1, 1])
  direction <- ifelse(signal > 0, "UP", "DOWN")
} else {
  signal    <- NULL
  direction <- NULL
}

# ================================
# SIGNAL ENGINE
# ================================
generate_signal <- function(ret, vol) {
  if (is.null(ret) || is.null(vol)) return(NULL)
  
  latest_ret <- as.numeric(tail(ret, 1))
  latest_vol <- as.numeric(tail(vol, 1))
  
  signal <- ifelse(latest_ret > 0 & latest_vol < 0.02, "BUY",
                   ifelse(latest_ret < 0 & latest_vol < 0.02, "SELL", "WAIT"))
  names(signal) <- colnames(ret)
  return(signal)
}

trade_signal <- generate_signal(ret, vol)

# ================================
# ROLLING CORRELATION HELPER
# ================================
compute_roll_cor <- function(ret, window) {
  pairs <- list(
    "SPY vs QQQ" = c("SPY", "QQQ"),
    "SPY vs TLT" = c("SPY", "TLT"),
    "SPY vs DXY" = c("SPY", "DXY"),
    "GLD vs BTC" = c("GLD", "BTC"),
    "Oil vs DXY" = c("Oil", "DXY")
  )
  
  do.call(rbind, lapply(names(pairs), function(pair_name) {
    cols <- pairs[[pair_name]]
    rc <- rollapply(
      ret[, cols], window,
      function(x) cor(x[, 1], x[, 2], method = "spearman"),
      by.column = FALSE, fill = NA
    )
    data.frame(
      date = index(rc),
      corr = as.numeric(rc),
      pair = pair_name
    )
  }))
}

# ================================
# SWING DETECTION
# ================================
find_swings <- function(price_vec, dates, n = 5) {
  highs <- data.frame(date = as.Date(character()), price = numeric(), type = character(),
                      stringsAsFactors = FALSE)
  lows  <- data.frame(date = as.Date(character()), price = numeric(), type = character(),
                      stringsAsFactors = FALSE)
  
  len <- length(price_vec)
  if (len < (2 * n + 1)) return(rbind(highs, lows))
  
  for (i in (n + 1):(len - n)) {
    window <- price_vec[(i - n):(i + n)]
    if (!anyNA(window)) {
      if (price_vec[i] == max(window)) {
        highs <- rbind(highs, data.frame(date = dates[i], price = price_vec[i],
                                         type = "Swing High", stringsAsFactors = FALSE))
      }
      if (price_vec[i] == min(window)) {
        lows <- rbind(lows, data.frame(date = dates[i], price = price_vec[i],
                                       type = "Swing Low", stringsAsFactors = FALSE))
      }
    }
  }
  rbind(highs, lows)
}

classify_swings <- function(swings) {
  if (nrow(swings) < 2) return(swings)
  
  swings       <- swings[order(swings$date), ]
  swings$label <- NA_character_
  
  highs <- swings[swings$type == "Swing High", ]
  lows  <- swings[swings$type == "Swing Low",  ]
  
  if (nrow(highs) >= 2) {
    highs$label[1] <- "HH"
    for (i in 2:nrow(highs)) {
      highs$label[i] <- ifelse(highs$price[i] > highs$price[i - 1], "HH", "LH")
    }
  } else if (nrow(highs) == 1) {
    highs$label[1] <- "HH"
  }
  
  if (nrow(lows) >= 2) {
    lows$label[1] <- "LL"
    for (i in 2:nrow(lows)) {
      lows$label[i] <- ifelse(lows$price[i] > lows$price[i - 1], "HL", "LL")
    }
  } else if (nrow(lows) == 1) {
    lows$label[1] <- "LL"
  }
  
  rbind(highs, lows)
}

# ================================
# BOS & CHoCH DETECTION
# ================================
detect_bos_choch <- function(swings) {
  if (is.null(swings) || nrow(swings) < 4) return(NULL)
  
  labeled <- swings[!is.na(swings$label), ]
  labeled <- labeled[order(labeled$date), ]
  
  events <- data.frame(date = as.Date(character()), price = numeric(),
                       event = character(), stringsAsFactors = FALSE)
  
  highs <- labeled[labeled$type == "Swing High", ]
  lows  <- labeled[labeled$type == "Swing Low",  ]
  
  # BOS Bullish: new HH
  if (nrow(highs) >= 2) {
    for (i in 2:nrow(highs)) {
      if (highs$label[i] == "HH") {
        events <- rbind(events, data.frame(date  = highs$date[i],
                                           price = highs$price[i],
                                           event = "BOS Bull",
                                           stringsAsFactors = FALSE))
      }
    }
  }
  
  # BOS Bearish: new LL
  if (nrow(lows) >= 2) {
    for (i in 2:nrow(lows)) {
      if (lows$label[i] == "LL") {
        events <- rbind(events, data.frame(date  = lows$date[i],
                                           price = lows$price[i],
                                           event = "BOS Bear",
                                           stringsAsFactors = FALSE))
      }
    }
  }
  
  # CHoCH Bullish: first HH after LH
  if (nrow(highs) >= 2) {
    for (i in 2:nrow(highs)) {
      if (highs$label[i - 1] == "LH" && highs$label[i] == "HH") {
        events <- rbind(events, data.frame(date  = highs$date[i],
                                           price = highs$price[i],
                                           event = "CHoCH Bull",
                                           stringsAsFactors = FALSE))
      }
    }
  }
  
  # CHoCH Bearish: first LL after HL
  if (nrow(lows) >= 2) {
    for (i in 2:nrow(lows)) {
      if (lows$label[i - 1] == "HL" && lows$label[i] == "LL") {
        events <- rbind(events, data.frame(date  = lows$date[i],
                                           price = lows$price[i],
                                           event = "CHoCH Bear",
                                           stringsAsFactors = FALSE))
      }
    }
  }
  
  if (nrow(events) == 0) return(NULL)
  events[order(events$date), ]
}

# ================================
# ORDER BLOCK DETECTION
# ================================
detect_order_blocks <- function(df, swings) {
  if (is.null(swings) || nrow(swings) < 2) return(NULL)
  
  labeled <- swings[!is.na(swings$label), ]
  obs     <- data.frame(date_start = as.Date(character()),
                        date_end   = as.Date(character()),
                        price_high = numeric(),
                        price_low  = numeric(),
                        type       = character(),
                        stringsAsFactors = FALSE)
  
  # Bullish OB: zone just before HH
  hh_dates <- labeled$date[labeled$label == "HH"]
  for (d in hh_dates) {
    idx <- which(df$date == d)
    if (length(idx) > 0 && idx > 3) {
      ob_zone <- df[(idx - 3):(idx - 1), ]
      obs <- rbind(obs, data.frame(
        date_start = min(ob_zone$date),
        date_end   = max(ob_zone$date),
        price_high = max(ob_zone$price),
        price_low  = min(ob_zone$price),
        type       = "Bullish OB",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Bearish OB: zone just before LL
  ll_dates <- labeled$date[labeled$label == "LL"]
  for (d in ll_dates) {
    idx <- which(df$date == d)
    if (length(idx) > 0 && idx > 3) {
      ob_zone <- df[(idx - 3):(idx - 1), ]
      obs <- rbind(obs, data.frame(
        date_start = min(ob_zone$date),
        date_end   = max(ob_zone$date),
        price_high = max(ob_zone$price),
        price_low  = min(ob_zone$price),
        type       = "Bearish OB",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(obs) == 0) return(NULL)
  tail(obs[order(obs$date_start), ], 6)
}

get_structure_bias <- function(swings) {
  if (is.null(swings) || nrow(swings) < 3) {
    return(list(bias = "NEUTRAL", color = "gray50", bullish_count = 0, bearish_count = 0))
  }
  
  labeled <- swings[!is.na(swings$label), ]
  if (nrow(labeled) < 3) {
    return(list(bias = "NEUTRAL", color = "gray50", bullish_count = 0, bearish_count = 0))
  }
  
  last3 <- tail(labeled, 3)$label
  bullish_count <- sum(last3 %in% c("HH", "HL"))
  bearish_count <- sum(last3 %in% c("LL", "LH"))
  
  bias <- ifelse(
    bullish_count > bearish_count, "BULLISH",
    ifelse(bearish_count > bullish_count, "BEARISH", "NEUTRAL")
  )
  color <- ifelse(bias == "BULLISH", "green4",
                  ifelse(bias == "BEARISH", "red3", "gray50"))
  
  list(
    bias = bias,
    color = color,
    bullish_count = bullish_count,
    bearish_count = bearish_count
  )
}

detect_liquidity_zones <- function(swings, tolerance_pct = 0.003, lookback = 8) {
  if (is.null(swings) || nrow(swings) < 4) return(NULL)
  
  labeled <- swings[!is.na(swings$label), ]
  if (nrow(labeled) < 4) return(NULL)
  
  build_zones <- function(points, side) {
    if (nrow(points) < 2) return(NULL)
    
    points <- points[order(points$date), ]
    zones <- data.frame(
      side = character(),
      date_start = as.Date(character()),
      date_end = as.Date(character()),
      level = numeric(),
      low = numeric(),
      high = numeric(),
      touches = integer(),
      stringsAsFactors = FALSE
    )
    
    start_idx <- 1
    for (i in 2:nrow(points)) {
      ref_price <- points$price[start_idx]
      within_band <- abs(points$price[i] - ref_price) / ref_price <= tolerance_pct
      
      if (!within_band) {
        if ((i - start_idx) >= 2) {
          cluster <- points[start_idx:(i - 1), ]
          zones <- rbind(zones, data.frame(
            side = side,
            date_start = min(cluster$date),
            date_end = max(cluster$date),
            level = mean(cluster$price),
            low = min(cluster$price),
            high = max(cluster$price),
            touches = nrow(cluster),
            stringsAsFactors = FALSE
          ))
        }
        start_idx <- i
      }
    }
    
    if ((nrow(points) - start_idx + 1) >= 2) {
      cluster <- points[start_idx:nrow(points), ]
      zones <- rbind(zones, data.frame(
        side = side,
        date_start = min(cluster$date),
        date_end = max(cluster$date),
        level = mean(cluster$price),
        low = min(cluster$price),
        high = max(cluster$price),
        touches = nrow(cluster),
        stringsAsFactors = FALSE
      ))
    }
    
    zones
  }
  
  highs <- tail(labeled[labeled$type == "Swing High", c("date", "price")], lookback)
  lows <- tail(labeled[labeled$type == "Swing Low", c("date", "price")], lookback)
  
  zones <- dplyr::bind_rows(
    build_zones(highs, "Buy-side"),
    build_zones(lows, "Sell-side")
  )
  
  if (nrow(zones) == 0) return(NULL)
  zones[order(zones$date_end, decreasing = TRUE), ]
}

detect_liquidity_sweeps <- function(df, liquidity_zones, tolerance_pct = 0.002, confirmation_bars = 3) {
  if (is.null(df) || is.null(liquidity_zones) || nrow(liquidity_zones) == 0) return(NULL)
  
  sweeps <- data.frame(
    side = character(),
    zone_date = as.Date(character()),
    sweep_date = as.Date(character()),
    level = numeric(),
    price = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(liquidity_zones))) {
    zone <- liquidity_zones[i, ]
    start_idx <- which(df$date > zone$date_end)
    if (length(start_idx) == 0) next
    
    for (idx in start_idx) {
      future_idx <- idx:min(idx + confirmation_bars, nrow(df))
      current_price <- df$price[idx]
      
      if (zone$side == "Buy-side" && current_price > zone$high * (1 + tolerance_pct)) {
        confirmed <- any(df$price[future_idx] < zone$level)
        sweeps <- rbind(sweeps, data.frame(
          side = zone$side,
          zone_date = zone$date_end,
          sweep_date = df$date[idx],
          level = zone$level,
          price = current_price,
          status = ifelse(confirmed, "Swept", "Breakout"),
          stringsAsFactors = FALSE
        ))
        break
      }
      
      if (zone$side == "Sell-side" && current_price < zone$low * (1 - tolerance_pct)) {
        confirmed <- any(df$price[future_idx] > zone$level)
        sweeps <- rbind(sweeps, data.frame(
          side = zone$side,
          zone_date = zone$date_end,
          sweep_date = df$date[idx],
          level = zone$level,
          price = current_price,
          status = ifelse(confirmed, "Swept", "Breakout"),
          stringsAsFactors = FALSE
        ))
        break
      }
    }
  }
  
  if (nrow(sweeps) == 0) return(NULL)
  sweeps[order(sweeps$sweep_date, decreasing = TRUE), ]
}

get_cross_asset_context <- function(asset_ret, basket_ret, asset, window = 30) {
  peers_map <- list(
    Gold = c("GLD", "DXY", "TLT", "SPY"),
    Silver = c("GLD", "DXY", "TLT", "SPY"),
    BTC = c("QQQ", "SPY", "TLT", "DXY"),
    ETH = c("BTC", "QQQ", "SPY", "DXY"),
    Oil = c("SPY", "DXY", "TLT", "GLD"),
    USDJPY = c("DXY", "TLT", "SPY", "Oil"),
    EURUSD = c("DXY", "TLT", "SPY", "GLD"),
    USDTEC = c("QQQ", "SPY", "TLT", "DXY")
  )
  
  peers <- peers_map[[asset]]
  if (is.null(peers) || any(!(peers %in% colnames(basket_ret)))) {
    return(list(
      state = "Unavailable",
      mean_corr = NA_real_,
      momentum_alignment = 0,
      score = 0
    ))
  }
  
  asset_window <- tail(asset_ret, window)
  basket_window <- tail(basket_ret[, peers], window)
  mean_corr <- mean(sapply(peers, function(peer) {
    cor(as.numeric(asset_window), basket_window[, peer], method = "spearman")
  }), na.rm = TRUE)
  
  latest_asset_ret <- as.numeric(tail(asset_ret, 1))
  peer_ret <- sapply(peers, function(peer) as.numeric(tail(basket_ret[, peer], 1)))
  momentum_alignment <- sum(sign(latest_asset_ret) == sign(peer_ret), na.rm = TRUE)
  
  state <- ifelse(abs(mean_corr) >= 0.45, "Aligned",
                  ifelse(abs(mean_corr) <= 0.15, "Breakdown", "Mixed"))
  score <- ifelse(state == "Aligned" && momentum_alignment >= 2, 1,
                  ifelse(state == "Breakdown", -1, 0))
  
  list(
    state = state,
    mean_corr = mean_corr,
    momentum_alignment = momentum_alignment,
    score = score
  )
}

classify_regime <- function(df, swings, asset_ret, basket_ret, asset, cor_window = 30) {
  if (is.null(df) || is.null(swings) || is.null(asset_ret) || nrow(df) < 60) {
    return(NULL)
  }
  
  prices <- df$price
  ema_fast <- EMA(prices, 20)
  ema_slow <- EMA(prices, 50)
  slope_fast <- if (length(ema_fast) >= 6) as.numeric(tail(ema_fast, 1) - tail(ema_fast, 6)[1]) else 0
  
  structure <- get_structure_bias(swings)
  trend_state <- ifelse(
    !is.na(tail(ema_fast, 1)) && !is.na(tail(ema_slow, 1)) &&
      tail(ema_fast, 1) > tail(ema_slow, 1) && slope_fast > 0 &&
      structure$bias == "BULLISH",
    "Trend Up",
    ifelse(
      !is.na(tail(ema_fast, 1)) && !is.na(tail(ema_slow, 1)) &&
        tail(ema_fast, 1) < tail(ema_slow, 1) && slope_fast < 0 &&
        structure$bias == "BEARISH",
      "Trend Down",
      "Range"
    )
  )
  
  vol_series <- rollapply(asset_ret, 20, sd, fill = NA, align = "right")
  latest_vol <- as.numeric(tail(vol_series, 1))
  vol_cutoff <- suppressWarnings(as.numeric(quantile(na.omit(vol_series), 0.7, na.rm = TRUE)))
  vol_state <- ifelse(is.na(latest_vol) || is.na(vol_cutoff), "Unknown",
                      ifelse(latest_vol >= vol_cutoff, "High Vol", "Low Vol"))
  
  cross_asset <- get_cross_asset_context(asset_ret, basket_ret, asset, cor_window)
  regime <- paste(trend_state, "/", vol_state, "/", cross_asset$state)
  
  list(
    regime = regime,
    trend_state = trend_state,
    vol_state = vol_state,
    latest_vol = latest_vol,
    cross_asset_state = cross_asset$state,
    mean_corr = cross_asset$mean_corr,
    cross_asset_score = cross_asset$score,
    structure_bias = structure$bias
  )
}

build_decision_score <- function(asset, df, asset_ret, swings, bos, obs, liquidity_zones,
                                 sweeps, regime_info, full_ret) {
  if (is.null(df) || is.null(swings) || is.null(asset_ret) || is.null(regime_info)) {
    return(NULL)
  }
  
  score <- 0
  reasons <- character()
  structure <- get_structure_bias(swings)
  latest_price <- as.numeric(tail(df$price, 1))
  latest_ret <- as.numeric(tail(asset_ret, 1))
  
  if (structure$bias == "BULLISH") {
    score <- score + 2
    reasons <- c(reasons, "Structure bias bullish")
  } else if (structure$bias == "BEARISH") {
    score <- score - 2
    reasons <- c(reasons, "Structure bias bearish")
  }
  
  if (!is.null(bos) && nrow(bos) > 0) {
    last_event <- tail(bos[order(bos$date), ], 1)
    event_score <- switch(last_event$event,
                          "BOS Bull" = 2,
                          "CHoCH Bull" = 1,
                          "BOS Bear" = -2,
                          "CHoCH Bear" = -1,
                          0)
    score <- score + event_score
    reasons <- c(reasons, paste("Latest structure event:", last_event$event))
  }
  
  recent_high <- tail(swings[swings$type == "Swing High", ], 1)
  recent_low <- tail(swings[swings$type == "Swing Low", ], 1)
  if (nrow(recent_high) > 0 && nrow(recent_low) > 0) {
    range_mid <- mean(c(recent_high$price, recent_low$price))
    if (structure$bias == "BULLISH" && latest_price <= range_mid) {
      score <- score + 1
      reasons <- c(reasons, "Price in discount area")
    }
    if (structure$bias == "BEARISH" && latest_price >= range_mid) {
      score <- score - 1
      reasons <- c(reasons, "Price in premium area")
    }
  }
  
  if (!is.null(liquidity_zones) && nrow(liquidity_zones) > 0) {
    liquidity_zones$distance_pct <- abs(latest_price - liquidity_zones$level) / liquidity_zones$level
    nearest_zone <- liquidity_zones[which.min(liquidity_zones$distance_pct), ]
    
    if (nearest_zone$distance_pct <= 0.015) {
      if (structure$bias == "BULLISH" && nearest_zone$side == "Buy-side") {
        score <- score - 1
        reasons <- c(reasons, "Near buy-side liquidity overhead")
      } else if (structure$bias == "BEARISH" && nearest_zone$side == "Sell-side") {
        score <- score + 1
        reasons <- c(reasons, "Near sell-side liquidity below")
      }
    }
  }
  
  if (!is.null(sweeps) && nrow(sweeps) > 0) {
    latest_sweep <- tail(sweeps, 1)
    if (latest_sweep$status == "Swept") {
      if (latest_sweep$side == "Sell-side") {
        score <- score + 1
        reasons <- c(reasons, "Sell-side liquidity sweep suggests reversal support")
      } else if (latest_sweep$side == "Buy-side") {
        score <- score - 1
        reasons <- c(reasons, "Buy-side liquidity sweep suggests reversal risk")
      }
    }
  }
  
  if (regime_info$trend_state == "Range") {
    score <- ifelse(score > 0, score - 1, ifelse(score < 0, score + 1, score))
    reasons <- c(reasons, "Range regime lowers conviction")
  }
  if (regime_info$vol_state == "High Vol") {
    score <- ifelse(score > 0, score - 1, ifelse(score < 0, score + 1, score))
    reasons <- c(reasons, "High volatility reduces entry quality")
  }
  
  score <- score + regime_info$cross_asset_score
  if (regime_info$cross_asset_score > 0) {
    reasons <- c(reasons, "Cross-asset context aligned")
  } else if (regime_info$cross_asset_score < 0) {
    reasons <- c(reasons, "Cross-asset correlation breakdown")
  }
  
  if (!is.na(latest_ret)) {
    if (latest_ret > 0 && structure$bias == "BULLISH") {
      score <- score + 1
      reasons <- c(reasons, "Short-term momentum confirms bullish bias")
    }
    if (latest_ret < 0 && structure$bias == "BEARISH") {
      score <- score - 1
      reasons <- c(reasons, "Short-term momentum confirms bearish bias")
    }
  }
  
  action <- ifelse(score >= 3, "BUY",
                   ifelse(score <= -3, "SELL", "WAIT"))
  
  data.frame(
    Asset = asset,
    Bias = structure$bias,
    Regime = regime_info$regime,
    Score = score,
    Action = action,
    Why = paste(unique(reasons)[1:min(3, length(unique(reasons)))], collapse = "; "),
    stringsAsFactors = FALSE
  )
}

build_supporting_metrics <- function(asset, df, asset_ret, swings, bos, liquidity_zones,
                                     sweeps, regime_info) {
  if (is.null(df) || is.null(swings) || is.null(asset_ret) || is.null(regime_info)) {
    return(NULL)
  }
  
  structure <- get_structure_bias(swings)
  latest_price <- as.numeric(tail(df$price, 1))
  latest_ret <- as.numeric(tail(asset_ret, 1))
  latest_event <- if (!is.null(bos) && nrow(bos) > 0) tail(bos[order(bos$date), ], 1)$event else "None"
  
  recent_high <- tail(swings[swings$type == "Swing High", ], 1)
  recent_low <- tail(swings[swings$type == "Swing Low", ], 1)
  range_position <- "Neutral"
  if (nrow(recent_high) > 0 && nrow(recent_low) > 0) {
    range_mid <- mean(c(recent_high$price, recent_low$price))
    range_position <- ifelse(
      latest_price < range_mid, "Discount",
      ifelse(latest_price > range_mid, "Premium", "Mid")
    )
  }
  
  nearest_side <- "None"
  nearest_distance <- NA_real_
  if (!is.null(liquidity_zones) && nrow(liquidity_zones) > 0) {
    liquidity_zones$distance_pct <- 100 * abs(latest_price - liquidity_zones$level) / liquidity_zones$level
    nearest_zone <- liquidity_zones[which.min(liquidity_zones$distance_pct), ]
    nearest_side <- nearest_zone$side
    nearest_distance <- nearest_zone$distance_pct
  }
  
  latest_sweep <- "None"
  if (!is.null(sweeps) && nrow(sweeps) > 0) {
    latest_sweep <- paste(tail(sweeps, 1)$side, tail(sweeps, 1)$status)
  }
  
  data.frame(
    Metric = c(
      "Bias",
      "Latest Event",
      "Range Position",
      "Nearest Liquidity",
      "Distance to Liquidity",
      "Latest Sweep",
      "Trend",
      "Volatility",
      "Cross-Asset State",
      "Average Correlation",
      "Latest Return"
    ),
    Value = c(
      structure$bias,
      latest_event,
      range_position,
      nearest_side,
      ifelse(is.na(nearest_distance), "N/A", paste0(formatC(nearest_distance, format = "f", digits = 2), "%")),
      latest_sweep,
      regime_info$trend_state,
      regime_info$vol_state,
      regime_info$cross_asset_state,
      formatC(regime_info$mean_corr, format = "f", digits = 4),
      formatC(latest_ret, format = "e", digits = 3)
    ),
    stringsAsFactors = FALSE
  )
}

build_decision_narrative <- function(asset, decision_df, metrics_df, regime_info) {
  if (is.null(decision_df) || is.null(metrics_df) || is.null(regime_info)) {
    return("Decision narrative unavailable.")
  }
  
  metric_value <- function(name) {
    row <- metrics_df[metrics_df$Metric == name, "Value"]
    if (length(row) == 0) "N/A" else row[1]
  }
  
  score <- decision_df$Score[1]
  conviction <- ifelse(abs(score) >= 5, "high",
                       ifelse(abs(score) >= 3, "medium", "low"))
  liquidity_side <- metric_value("Nearest Liquidity")
  liquidity_distance <- metric_value("Distance to Liquidity")
  latest_sweep <- metric_value("Latest Sweep")
  trend_state <- metric_value("Trend")
  vol_state <- metric_value("Volatility")
  avg_corr <- metric_value("Average Correlation")
  latest_return <- metric_value("Latest Return")
  
  paste0(
    asset, " is in a ", tolower(decision_df$Bias[1]), " structure under ",
    trend_state, " / ", vol_state, " conditions, with ",
    metric_value("Latest Event"), " as the latest structural signal and price sitting in ",
    tolower(metric_value("Range Position")), ". ",
    "Nearest liquidity is ", liquidity_side, " at ", liquidity_distance,
    " distance, with latest sweep status: ", latest_sweep, ". ",
    "Macro context is ", metric_value("Cross-Asset State"),
    " with average correlation ", avg_corr,
    " and latest return ", latest_return, ". ",
    "Decision: ", decision_df$Action[1], " with ", conviction,
    " conviction (score ", ifelse(score > 0, paste0("+", score), as.character(score)), ")."
  )
}

# ================================
# SHINY UI
# ================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Macro Asset Intelligence"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "main_tabs",
      selected = "context",
      menuItem("Decision View", tabName = "decision", icon = icon("chart-line")),
      menuItem("Macro Context", tabName = "context", icon = icon("globe"))
    ),
    div(
      class = "dashboard-controls",
      conditionalPanel(
        condition = "input.main_tabs == 'decision'",
        div(class = "controls-title", "Decision Controls"),
        div(class = "controls-subtitle", "Inputs for structure, liquidity, and execution view")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'context'",
        div(class = "controls-title", "Context Controls"),
        div(class = "controls-subtitle", "Correlation settings for macro view")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'decision'",
        selectInput("asset", "Pilih Asset:",
                    choices = names(decision_asset_map)),
        sliderInput("swing_n", "Swing Sensitivity (bars):",
                    min = 3, max = 30, value = 5, step = 1)
      ),
      sliderInput("cor_window", "Correlation Window (days):",
                  min = 10, max = 90, value = 30, step = 5),
      conditionalPanel(
        condition = "input.main_tabs == 'decision'",
        div(class = "controls-note", "Correlation window also affects regime and decision scoring.")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'context'",
        div(class = "controls-note", "This window controls the correlation matrix and rolling Spearman plot.")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-black .main-header .logo {
          background-color: #111827;
          color: #f9fafb;
          font-weight: 700;
        }
        .skin-black .main-header .navbar {
          background-color: #111827;
        }
        .skin-black .main-sidebar {
          background: linear-gradient(180deg, #111827 0%, #1f2937 100%);
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          border-left-color: #f59e0b;
          background-color: rgba(245, 158, 11, 0.12);
        }
        .dashboard-controls {
          margin: 14px 14px 18px 14px;
          padding: 16px 14px 10px 14px;
          border-radius: 14px;
          background: linear-gradient(180deg, rgba(255,255,255,0.08) 0%, rgba(255,255,255,0.04) 100%);
          border: 1px solid rgba(255,255,255,0.08);
          box-shadow: inset 0 1px 0 rgba(255,255,255,0.04);
        }
        .controls-title {
          color: #f9fafb;
          font-size: 15px;
          font-weight: 700;
          margin-bottom: 2px;
        }
        .controls-subtitle {
          color: #9ca3af;
          font-size: 12px;
          margin-bottom: 14px;
        }
        .controls-note {
          color: #d1d5db;
          font-size: 12px;
          line-height: 1.45;
          margin-top: 2px;
        }
        .compact-box .box-body {
          padding: 12px 14px;
        }
        .compact-bias-box .box-header {
          padding: 10px 14px;
        }
        .compact-bias-box .box-body {
          padding: 8px 10px;
        }
        .bias-wrap {
          min-height: 50px;
          display: flex;
          align-items: center;
          justify-content: center;
          background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
          box-shadow: inset 0 0 0 1px rgba(148, 163, 184, 0.15);
          border-radius: 12px;
        }
        .bias-pill {
          display: inline-block;
          padding: 6px 12px;
          border-radius: 999px;
          font-size: 11px;
          font-weight: 800;
          letter-spacing: 0.6px;
          color: #fff;
          text-transform: uppercase;
          box-shadow: 0 8px 18px rgba(15, 23, 42, 0.12);
        }
        .metric-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
          gap: 10px;
        }
        .metric-card {
          padding: 10px 12px;
          border-radius: 12px;
          background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
          border: 1px solid #e5e7eb;
        }
        .metric-label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.4px;
          text-transform: uppercase;
          color: #6b7280;
          margin-bottom: 4px;
        }
        .metric-value {
          font-size: 14px;
          font-weight: 700;
          color: #111827;
          line-height: 1.35;
          word-break: break-word;
        }
        .narrative-text {
          font-size: 14px;
          line-height: 1.65;
          color: #1f2937;
        }
        .content-wrapper, .right-side {
          background: linear-gradient(180deg, #eef2f7 0%, #e5ebf3 100%);
        }
        .content-header {
          padding-bottom: 4px;
        }
        .box {
          border-radius: 12px;
          border-top: 0;
          box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
          overflow: hidden;
        }
        .box.box-solid.box-primary > .box-header {
          background: linear-gradient(135deg, #1d4ed8 0%, #1e3a8a 100%);
        }
        .box.box-solid.box-warning > .box-header {
          background: linear-gradient(135deg, #d97706 0%, #92400e 100%);
          color: #fff;
        }
        .box.box-solid.box-success > .box-header {
          background: linear-gradient(135deg, #059669 0%, #065f46 100%);
        }
        .box.box-solid.box-danger > .box-header {
          background: linear-gradient(135deg, #dc2626 0%, #7f1d1d 100%);
        }
        .box.box-solid.box-info > .box-header {
          background: linear-gradient(135deg, #0891b2 0%, #164e63 100%);
        }
        .box-header .box-title {
          font-weight: 700;
          letter-spacing: 0.2px;
        }
        .box-body {
          padding: 16px 18px;
          background-color: rgba(255, 255, 255, 0.96);
        }
        .table {
          margin-bottom: 0;
          font-size: 13px;
        }
        .table > thead > tr > th {
          border-bottom: 2px solid #e5e7eb;
          color: #374151;
          font-weight: 700;
        }
        .table > tbody > tr > td {
          vertical-align: middle;
        }
        .shiny-input-container {
          margin-bottom: 14px;
        }
        .skin-black .sidebar-form,
        .skin-black .sidebar-menu > li.header {
          background: transparent;
        }
        .sidebar .control-label {
          color: #e5e7eb;
          font-weight: 600;
        }
        .dashboard-controls .form-control {
          border-radius: 10px;
          border: 1px solid rgba(255,255,255,0.08);
          background-color: rgba(255,255,255,0.92);
          box-shadow: none;
        }
        .dashboard-controls .irs {
          margin-top: -4px;
        }
        .irs--shiny .irs-bar,
        .irs--shiny .irs-single,
        .irs--shiny .irs-from,
        .irs--shiny .irs-to {
          background: #f59e0b;
          border-color: #f59e0b;
        }
        .irs--shiny .irs-handle {
          border-color: #f59e0b;
        }
        .nav-tabs-custom,
        .small-box h3 {
          white-space: normal;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "decision",
        fluidRow(
          box(
            title = "Structure Bias", width = 2, status = "primary", solidHeader = TRUE,
            class = "compact-box compact-bias-box",
            uiOutput("biasSummary")
          ),
          box(
            title = "Regime", width = 4, status = "warning", solidHeader = TRUE,
            class = "compact-box",
            tableOutput("regimeTable")
          ),
          box(
            title = "Liquidity", width = 6, status = "success", solidHeader = TRUE,
            class = "compact-box",
            tableOutput("liquidityTable")
          )
        ),
        fluidRow(
          box(
            title = "Decision Summary", width = 12, status = "danger", solidHeader = TRUE,
            tableOutput("decisionTable")
          )
        ),
        fluidRow(
          box(
            title = "Narrative Summary", width = 3, status = "warning", solidHeader = TRUE,
            htmlOutput("narrativeSummary")
          ),
          box(
            title = "Supporting Metrics", width = 9, status = "info", solidHeader = TRUE,
            uiOutput("supportingMetrics")
          )
        ),
        fluidRow(
          box(
            title = "Price Structure + Liquidity Map", width = 12, status = "primary", solidHeader = TRUE,
            plotOutput("pricePlot", height = "560px")
          )
        ),
        fluidRow(
          box(
            title = "Market Structure Levels", width = 6, status = "info", solidHeader = TRUE,
            tableOutput("swingTable")
          ),
          box(
            title = "BOS / CHoCH Events", width = 6, status = "info", solidHeader = TRUE,
            tableOutput("bosTable")
          )
        )
      ),
      tabItem(
        tabName = "context",
        fluidRow(
          box(
            title = "VAR Context", width = 5, status = "primary", solidHeader = TRUE,
            tableOutput("signalTable")
          ),
          box(
            title = "Current Correlation Matrix", width = 7, status = "warning", solidHeader = TRUE,
            tableOutput("corrMatrix")
          )
        ),
        fluidRow(
          box(
            title = "Rolling Spearman Correlation", width = 12, status = "success", solidHeader = TRUE,
            plotOutput("corrPlot", height = "420px")
          )
        )
      )
    )
  )
)

# ================================
# SHINY SERVER
# ================================
server <- function(input, output, session) {
  observeEvent(TRUE, {
    updateTabItems(session, "main_tabs", selected = "context")
  }, once = TRUE)
  
  # --- Shared reactive: base price df ---
  price_df <- reactive({
    req(input$asset)
    if (!(input$asset %in% colnames(data))) return(NULL)
    data.frame(
      date  = as.Date(index(data)),
      price = as.numeric(data[, input$asset])
    )
  })
  
  asset_ret <- reactive({
    req(input$asset)
    ret[, input$asset, drop = FALSE]
  })
  
  # --- Shared reactive: swing data ---
  swing_data <- reactive({
    req(input$swing_n)
    df <- price_df()
    if (is.null(df)) return(NULL)
    swings <- find_swings(df$price, df$date, n = input$swing_n)
    classify_swings(swings)
  })
  
  structure_info <- reactive({
    get_structure_bias(swing_data())
  })
  
  bos_data <- reactive({
    detect_bos_choch(swing_data())
  })
  
  order_block_data <- reactive({
    detect_order_blocks(price_df(), swing_data())
  })
  
  liquidity_data <- reactive({
    detect_liquidity_zones(swing_data())
  })
  
  liquidity_sweep_data <- reactive({
    detect_liquidity_sweeps(price_df(), liquidity_data())
  })
  
  regime_data <- reactive({
    classify_regime(
      df = price_df(),
      swings = swing_data(),
      asset_ret = asset_ret(),
      basket_ret = macro_ret,
      asset = input$asset,
      cor_window = input$cor_window
    )
  })
  
  decision_data <- reactive({
    build_decision_score(
      asset = input$asset,
      df = price_df(),
      asset_ret = asset_ret(),
      swings = swing_data(),
      bos = bos_data(),
      obs = order_block_data(),
      liquidity_zones = liquidity_data(),
      sweeps = liquidity_sweep_data(),
      regime_info = regime_data(),
      full_ret = ret
    )
  })
  
  supporting_metrics_data <- reactive({
    build_supporting_metrics(
      asset = input$asset,
      df = price_df(),
      asset_ret = asset_ret(),
      swings = swing_data(),
      bos = bos_data(),
      liquidity_zones = liquidity_data(),
      sweeps = liquidity_sweep_data(),
      regime_info = regime_data()
    )
  })
  
  narrative_data <- reactive({
    build_decision_narrative(
      asset = input$asset,
      decision_df = decision_data(),
      metrics_df = supporting_metrics_data(),
      regime_info = regime_data()
    )
  })
  
  # --- Structure Bias ---
  output$biasSummary <- renderUI({
    structure <- structure_info()
    swings <- swing_data()
    if (is.null(swings) || nrow(swings) < 2) {
      return(tags$div(style = "color:gray; font-weight:bold;", "Insufficient data"))
    }
    
    bg_color <- ifelse(
      structure$bias == "BULLISH", "#16a34a",
      ifelse(structure$bias == "BEARISH", "#dc2626", "#6b7280")
    )
    
    tags$div(
      class = "bias-wrap",
      tags$span(
        class = "bias-pill",
        style = paste0("background:", bg_color, ";"),
        structure$bias
      )
    )
  })
  
  # --- Price Plot ---
  output$pricePlot <- renderPlot({
    df     <- price_df()
    swings <- swing_data()
    if (is.null(df) || is.null(swings)) return(NULL)
    
    label_colors <- c(
      "HH" = "green4",   "HL" = "limegreen",
      "LH" = "tomato",   "LL" = "red3"
    )
    bos_colors <- c(
      "BOS Bull"    = "green4",
      "BOS Bear"    = "red3",
      "CHoCH Bull"  = "steelblue",
      "CHoCH Bear"  = "orange"
    )
    all_colors <- c(label_colors, bos_colors)
    
    p <- ggplot(df, aes(date, price)) +
      geom_line(color = "steelblue", linewidth = 0.7)
    
    # Order Block shaded zones
    obs <- order_block_data()
    if (!is.null(obs) && nrow(obs) > 0) {
      for (i in seq_len(nrow(obs))) {
        ob       <- obs[i, ]
        fill_col <- ifelse(ob$type == "Bullish OB", "limegreen", "tomato")
        p <- p + annotate("rect",
                          xmin  = ob$date_start, xmax = ob$date_end,
                          ymin  = ob$price_low,  ymax = ob$price_high,
                          fill  = fill_col, alpha = 0.15,
                          color = fill_col, linewidth = 0.3)
      }
    }
    
    liquidity <- liquidity_data()
    if (!is.null(liquidity) && nrow(liquidity) > 0) {
      for (i in seq_len(nrow(liquidity))) {
        zone <- liquidity[i, ]
        zone_color <- ifelse(zone$side == "Buy-side", "firebrick", "darkgreen")
        p <- p +
          annotate("rect",
                   xmin = zone$date_start, xmax = max(df$date),
                   ymin = zone$low, ymax = zone$high,
                   fill = zone_color, alpha = 0.05,
                   color = zone_color, linewidth = 0.2) +
          annotate("text",
                   x = zone$date_end,
                   y = zone$level,
                   label = paste(zone$side, "liq"),
                   color = zone_color,
                   hjust = 0, vjust = -0.4, size = 2.8, fontface = "bold")
      }
    }
    
    # Dashed resistance line (swing highs)
    highs <- swings[swings$type == "Swing High", ]
    highs <- highs[order(highs$date), ]
    if (nrow(highs) >= 2) {
      p <- p + geom_line(data = highs, aes(x = date, y = price),
                         color = "tomato", linetype = "dashed",
                         linewidth = 0.8, inherit.aes = FALSE)
    }
    
    # Dashed support line (swing lows)
    lows <- swings[swings$type == "Swing Low", ]
    lows <- lows[order(lows$date), ]
    if (nrow(lows) >= 2) {
      p <- p + geom_line(data = lows, aes(x = date, y = price),
                         color = "limegreen", linetype = "dashed",
                         linewidth = 0.8, inherit.aes = FALSE)
    }
    
    # Horizontal level lines for latest HH / LH / HL / LL
    labeled <- swings[!is.na(swings$label), ]
    
    add_hline <- function(p, lbl, color) {
      row <- tail(labeled[labeled$label == lbl, ], 1)
      if (nrow(row) == 0) return(p)
      p +
        geom_hline(yintercept = row$price, color = color,
                   linetype = "solid", linewidth = 0.5, alpha = 0.6) +
        annotate("text",
                 x     = as.Date(min(df$date)),
                 y     = row$price,
                 label = paste0(lbl, ": ", formatC(row$price, format = "f", digits = 2)),
                 color = color, hjust = 0, vjust = -0.4,
                 size  = 3.2, fontface = "bold")
    }
    
    p <- add_hline(p, "HH", "green4")
    p <- add_hline(p, "LH", "tomato")
    p <- add_hline(p, "HL", "limegreen")
    p <- add_hline(p, "LL", "red3")
    
    # Swing point dots + labels
    if (nrow(labeled) > 0) {
      labeled$vjust_val <- ifelse(labeled$type == "Swing High", -1.2, 1.8)
      p <- p +
        geom_point(data = labeled, aes(x = date, y = price, color = label),
                   size = 3, inherit.aes = FALSE) +
        geom_text(data = labeled,
                  aes(x = date, y = price, label = label,
                      color = label, vjust = vjust_val),
                  size = 3, fontface = "bold", inherit.aes = FALSE)
    }
    
    # BOS / CHoCH markers
    bos <- bos_data()
    if (!is.null(bos) && nrow(bos) > 0) {
      p <- p +
        geom_point(data = bos, aes(x = date, y = price, color = event),
                   shape = 18, size = 4.5, inherit.aes = FALSE) +
        geom_text(data = bos, aes(x = date, y = price,
                                  label = event, color = event),
                  vjust = -1, size = 2.6, fontface = "bold",
                  inherit.aes = FALSE)
    }
    
    sweeps <- liquidity_sweep_data()
    if (!is.null(sweeps) && nrow(sweeps) > 0) {
      p <- p +
        geom_point(data = sweeps,
                   aes(x = sweep_date, y = price),
                   shape = 8, size = 3.5, color = "black",
                   inherit.aes = FALSE) +
        geom_text(data = sweeps,
                  aes(x = sweep_date, y = price, label = status),
                  vjust = 1.5, size = 2.5, color = "black",
                  inherit.aes = FALSE)
    }
    
    p +
      scale_color_manual(values = all_colors, name = "Structure") +
      labs(
        title = paste("Price:", input$asset, "| Market Structure + BOS/CHoCH + Order Blocks"),
        x = "Date", y = "Price"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # --- Swing Summary Table ---
  output$swingTable <- renderTable({
    swings <- swing_data()
    if (is.null(swings) || nrow(swings) == 0) return(NULL)
    
    labeled <- swings[!is.na(swings$label), ]
    if (nrow(labeled) == 0) return(NULL)
    
    do.call(rbind, lapply(c("HH", "LH", "HL", "LL"), function(lbl) {
      row <- tail(labeled[labeled$label == lbl, ], 1)
      if (nrow(row) == 0) return(NULL)
      data.frame(
        Label = lbl,
        Date  = format(row$date, "%Y-%m-%d"),
        Price = formatC(row$price, format = "f", digits = 2),
        Type  = row$type
      )
    }))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- BOS / CHoCH Table ---
  output$bosTable <- renderTable({
    swings <- swing_data()
    if (is.null(swings)) return(NULL)
    
    bos <- bos_data()
    if (is.null(bos) || nrow(bos) == 0) {
      return(data.frame(Message = "No BOS/CHoCH detected"))
    }
    
    bos_sorted <- bos[order(bos$date, decreasing = TRUE), ]
    head(bos_sorted, 6) |>
      transmute(
        Event = event,
        Date  = format(date, "%Y-%m-%d"),
        Price = formatC(price, format = "f", digits = 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- Rolling Correlation Plot ---
  cor_data <- reactive({
    req(input$cor_window)
    compute_roll_cor(macro_ret, input$cor_window)
  })
  
  output$corrPlot <- renderPlot({
    df <- na.omit(cor_data())
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(date, corr, color = pair, fill = pair)) +
      geom_line(linewidth = 0.7) +
      geom_ribbon(aes(ymin = pmin(corr, 0), ymax = 0), alpha = 0.1, color = NA) +
      geom_ribbon(aes(ymin = 0, ymax = pmax(corr, 0)), alpha = 0.1, color = NA) +
      geom_hline(yintercept =  0,   linetype = "dashed", color = "gray40") +
      geom_hline(yintercept =  0.5, linetype = "dotted", color = "gray60") +
      geom_hline(yintercept = -0.5, linetype = "dotted", color = "gray60") +
      scale_color_manual(values = c("SPY vs QQQ" = "darkorange",
                                    "SPY vs TLT" = "steelblue",
                                    "SPY vs DXY" = "firebrick",
                                    "GLD vs BTC" = "goldenrod",
                                    "Oil vs DXY" = "seagreen")) +
      scale_fill_manual(values  = c("SPY vs QQQ" = "darkorange",
                                    "SPY vs TLT" = "steelblue",
                                    "SPY vs DXY" = "firebrick",
                                    "GLD vs BTC" = "goldenrod",
                                    "Oil vs DXY" = "seagreen")) +
      labs(
        title = paste0("Rolling Spearman Correlation (", input$cor_window, "-day window)"),
        x = "Date", y = "Correlation", color = "Pair", fill = "Pair"
      ) +
      ylim(-1, 1) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # --- Correlation Matrix ---
  output$corrMatrix <- renderTable({
    req(input$cor_window)
    latest <- tail(macro_ret, input$cor_window)
    cm <- round(cor(latest, method = "spearman"), 3)
    formatted_cm <- matrix(
      formatC(cm, format = "f", digits = 4),
      nrow = nrow(cm),
      dimnames = dimnames(cm)
    )
    as.data.frame(formatted_cm)
  }, rownames = TRUE)
  
  # --- VAR Forecast Signal ---
  output$signalTable <- renderTable({
    if (is.null(signal) || is.null(direction)) return(NULL)
    data.frame(
      Asset = names(signal),
      Forecast = formatC(as.numeric(signal), format = "e", digits = 3),
      Direction = as.character(direction)
    )
  })
  
  # --- Regime Table ---
  output$regimeTable <- renderTable({
    regime <- regime_data()
    if (is.null(regime)) return(NULL)
    
    data.frame(
      Trend = regime$trend_state,
      Volatility = regime$vol_state,
      CrossAsset = regime$cross_asset_state,
      AvgCorr = formatC(regime$mean_corr, format = "f", digits = 4)
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- Liquidity Table ---
  output$liquidityTable <- renderTable({
    df <- price_df()
    liquidity <- liquidity_data()
    sweeps <- liquidity_sweep_data()
    if (is.null(df) || is.null(liquidity) || nrow(liquidity) == 0) {
      return(data.frame(Message = "No clear liquidity clusters"))
    }
    
            latest_price <- as.numeric(tail(df$price, 1))
            liquidity$distance_pct <- round(100 * abs(latest_price - liquidity$level) / liquidity$level, 2)
            top_liquidity <- head(liquidity[order(liquidity$distance_pct), ], 2)
            
            latest_sweep <- if (!is.null(sweeps) && nrow(sweeps) > 0) sweeps[1, ] else NULL
            sweep_label <- rep("None", nrow(top_liquidity))
            if (!is.null(latest_sweep)) {
              sweep_label[top_liquidity$side == latest_sweep$side] <- latest_sweep$status
            }
            top_liquidity |>
              transmute(
                Side = side,
                Level = formatC(level, format = "f", digits = 2),
                Touches = touches,
                DistancePct = paste0(distance_pct, "%"),
                Sweep = sweep_label
              )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # --- Decision Table ---
  output$decisionTable <- renderTable({
    decision <- decision_data()
    if (is.null(decision)) return(NULL)
    decision
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$supportingMetrics <- renderUI({
    metrics <- supporting_metrics_data()
    if (is.null(metrics)) return(NULL)
    
    cards <- lapply(seq_len(nrow(metrics)), function(i) {
      tags$div(
        class = "metric-card",
        tags$div(class = "metric-label", metrics$Metric[i]),
        tags$div(class = "metric-value", metrics$Value[i])
      )
    })
    
    tags$div(class = "metric-grid", cards)
  })
  
  output$narrativeSummary <- renderUI({
    narrative <- narrative_data()
    if (is.null(narrative) || identical(narrative, "")) return(NULL)
    
    tags$div(
      class = "narrative-text",
      narrative
    )
  })
}

# ================================
# RUN APP
# ================================
shinyApp(ui, server)
