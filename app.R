# ================================
# Macro Asset Intelligence Dashboard
# FINAL VERSION - Full Market Structure
# ================================
library(quantmod)
library(TTR)
library(zoo)
library(vars)
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
options(getSymbols.warning4.0 = FALSE)

# ================================
# DATA PIPELINE
# ================================
getSymbols(c("GC=F", "BTC-USD", "CL=F"), src = "yahoo", from = "2020-01-01")

gold <- na.omit(Cl(`GC=F`))
btc  <- na.omit(Cl(`BTC-USD`))
oil  <- na.omit(Cl(`CL=F`))

data <- merge(gold, btc, join = "inner")
data <- merge(data, oil, join = "inner")
colnames(data) <- c("Gold", "BTC", "Oil")

data <- na.omit(data)
data <- data[complete.cases(data), ]
data <- data[apply(data, 1, function(r) all(r > 0)), ]

if (is.null(data) || nrow(data) < 50) {
  stop("Data tidak cukup / gagal load dari Yahoo")
}

# ================================
# FEATURE ENGINEERING
# ================================
ret <- ROC(data, type = "continuous")
ret <- na.omit(ret)

if (nrow(ret) < 50) {
  stop("Return tidak cukup untuk modeling")
}

vol <- rollapply(ret, 20, sd, fill = NA)

# ================================
# MODEL
# ================================
var_model <- tryCatch({
  VAR(ret, p = 2, type = "const")
}, error = function(e) NULL)

pred <- tryCatch({
  if (!is.null(var_model)) predict(var_model, n.ahead = 5) else NULL
}, error = function(e) NULL)

if (!is.null(pred)) {
  signal <- data.frame(
    Gold = pred$fcst$Gold[, 1],
    BTC  = pred$fcst$BTC[, 1],
    Oil  = pred$fcst$Oil[, 1]
  )
  direction <- as.data.frame(ifelse(signal > 0, "UP", "DOWN"))
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
    "Gold vs BTC" = c("Gold", "BTC"),
    "Gold vs Oil" = c("Gold", "Oil"),
    "BTC vs Oil"  = c("BTC",  "Oil")
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

# ================================
# SHINY UI
# ================================
ui <- fluidPage(
  titlePanel("Macro Asset Intelligence Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("asset", "Pilih Asset:",
                  choices = c("Gold", "BTC", "Oil")),
      
      sliderInput("swing_n", "Swing Sensitivity (bars):",
                  min = 3, max = 30, value = 5, step = 1),
      
      sliderInput("cor_window", "Correlation Window (days):",
                  min = 10, max = 90, value = 30, step = 5),
      
      hr(),
      h4("Structure Bias"),
      uiOutput("biasSummary"),
      
      hr(),
      h4("VAR Forecast Signal"),
      tableOutput("signalTable"),
      
      hr(),
      h4("Trade Signal"),
      tableOutput("tradeSignal"),
      
      hr(),
      h4("Current Correlation Matrix"),
      tableOutput("corrMatrix")
    ),
    
    mainPanel(
      plotOutput("pricePlot", height = "500px"),
      
      fluidRow(
        column(6,
               h4("Market Structure Levels"),
               tableOutput("swingTable")
        ),
        column(6,
               h4("BOS / CHoCH Events (Latest 6)"),
               tableOutput("bosTable")
        )
      ),
      
      plotOutput("corrPlot", height = "350px")
    )
  )
)

# ================================
# SHINY SERVER
# ================================
server <- function(input, output) {
  
  # --- Shared reactive: base price df ---
  price_df <- reactive({
    req(input$asset)
    if (!(input$asset %in% colnames(data))) return(NULL)
    data.frame(
      date  = as.Date(index(data)),
      price = as.numeric(data[, input$asset])
    )
  })
  
  # --- Shared reactive: swing data ---
  swing_data <- reactive({
    req(input$swing_n)
    df <- price_df()
    if (is.null(df)) return(NULL)
    swings <- find_swings(df$price, df$date, n = input$swing_n)
    classify_swings(swings)
  })
  
  # --- Structure Bias ---
  output$biasSummary <- renderUI({
    swings <- swing_data()
    if (is.null(swings) || nrow(swings) < 2) {
      return(tags$div(style = "color:gray; font-weight:bold;", "Insufficient data"))
    }
    
    labeled <- swings[!is.na(swings$label), ]
    if (nrow(labeled) < 3) return(NULL)
    
    last3         <- tail(labeled, 3)$label
    bullish_count <- sum(last3 %in% c("HH", "HL"))
    bearish_count <- sum(last3 %in% c("LL", "LH"))
    
    bias  <- ifelse(bullish_count > bearish_count, "BULLISH",
                    ifelse(bearish_count > bullish_count, "BEARISH", "NEUTRAL"))
    color <- ifelse(bias == "BULLISH", "green4",
                    ifelse(bias == "BEARISH", "red3", "gray50"))
    
    tags$div(
      style = paste0(
        "font-size:18px; font-weight:bold; color:", color, ";",
        "padding:10px; border:2px solid ", color, ";",
        "border-radius:6px; text-align:center; margin-bottom:8px;"
      ),
      paste("\u25CF", bias)
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
    obs <- detect_order_blocks(df, swings)
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
    bos <- detect_bos_choch(swings)
    if (!is.null(bos) && nrow(bos) > 0) {
      p <- p +
        geom_point(data = bos, aes(x = date, y = price, color = event),
                   shape = 18, size = 4.5, inherit.aes = FALSE) +
        geom_text(data = bos, aes(x = date, y = price,
                                  label = event, color = event),
                  vjust = -1, size = 2.6, fontface = "bold",
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
    
    bos <- detect_bos_choch(swings)
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
    compute_roll_cor(ret, input$cor_window)
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
      scale_color_manual(values = c("Gold vs BTC" = "darkorange",
                                    "Gold vs Oil"  = "steelblue",
                                    "BTC vs Oil"   = "seagreen")) +
      scale_fill_manual(values  = c("Gold vs BTC" = "darkorange",
                                    "Gold vs Oil"  = "steelblue",
                                    "BTC vs Oil"   = "seagreen")) +
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
    latest <- tail(ret, input$cor_window)
    cm <- round(cor(latest, method = "spearman"), 3)
    as.data.frame(cm)
  }, rownames = TRUE)
  
  # --- VAR Forecast Signal ---
  output$signalTable <- renderTable({
    if (is.null(signal) || is.null(direction)) return(NULL)
    data.frame(
      Asset     = c("Gold", "BTC", "Oil"),
      Forecast  = round(c(signal$Gold[1], signal$BTC[1], signal$Oil[1]), 5),
      Direction = c(direction[1, "Gold"], direction[1, "BTC"], direction[1, "Oil"])
    )
  })
  
  # --- Trade Signal ---
  output$tradeSignal <- renderTable({
    if (is.null(trade_signal)) return(NULL)
    data.frame(
      Asset  = names(trade_signal),
      Signal = as.character(trade_signal)
    )
  })
}

# ================================
# RUN APP
# ================================
shinyApp(ui, server)
