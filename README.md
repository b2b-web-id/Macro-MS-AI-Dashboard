# Macro Asset Intelligence Dashboard

A `shinydashboard` app for reading macro and cross-asset structure before making a directional call.

The dashboard combines:

- market structure
- liquidity clustering and sweep detection
- rule-based regime awareness
- macro reference basket context
- score-based decision output

The goal is not raw prediction. The app is designed to help frame context, identify nearby liquidity, and summarize whether conditions are supportive for a `BUY`, `SELL`, or `WAIT` decision.

## App Structure

The app is split into two tabs:

1. `Decision View`
   - structure bias
   - regime summary
   - liquidity summary
   - decision table
   - narrative summary
   - supporting metrics grid
   - price structure and liquidity chart

2. `Macro Context`
   - VAR context
   - current correlation matrix
   - rolling Spearman correlation

The default startup tab is `Macro Context`.

## Decision Asset Universe

Decision-layer assets currently available in the sidebar:

- `Gold` via Yahoo ticker `GC=F`
- `Silver` via Yahoo ticker `SI=F`
- `BTC` via Yahoo ticker `BTC-USD`
- `ETH` via Yahoo ticker `ETH-USD`
- `Oil` via Yahoo ticker `CL=F`
- `USDJPY` via Yahoo ticker `JPY=X`
- `EURUSD` via Yahoo ticker `EURUSD=X`
- `USDTEC` via Yahoo ticker `^NDX`

Notes:

- `Silver` uses `SI=F`, not spot `XAGUSD`
- `USDTEC` is currently proxied with `^NDX`

## Macro Reference Basket

The `Macro Context` tab uses the following reference basket:

- `SPY`
- `QQQ`
- `TLT`
- `DXY` via Yahoo ticker `DX-Y.NYB`
- `GLD`
- `CL=F`
- `BTC-USD`

This basket is used for:

- `VAR` forecast context
- current correlation matrix
- rolling Spearman correlation
- cross-asset regime confirmation inside the decision layer

## Core Features

### 1. Market Structure

The app detects:

- `HH`
- `HL`
- `LH`
- `LL`
- `BOS`
- `CHoCH`
- simple order blocks

### 2. Liquidity Model

The app includes:

- buy-side liquidity zones
- sell-side liquidity zones
- equal highs / equal lows clustering
- liquidity sweep vs breakout detection
- nearest-liquidity distance in the decision summary

### 3. Regime Awareness

Regime classification is rule-based and uses:

- fast vs slow EMA
- structure bias
- rolling volatility
- cross-asset correlation state

Regime output includes:

- `Trend Up`, `Trend Down`, or `Range`
- `High Vol` or `Low Vol`
- `Aligned`, `Mixed`, or `Breakdown`

### 4. Decision Layer

The final `BUY / SELL / WAIT` action is built from:

- structure bias
- latest BOS / CHoCH event
- premium / discount location inside the active swing range
- distance to nearest liquidity
- latest liquidity sweep state
- volatility regime
- cross-asset context
- short-term momentum

The decision output includes:

- action
- score
- bias
- reasons
- narrative summary
- supporting metrics cards

## UI Notes

Sidebar controls are contextual:

- in `Decision View`: asset, swing sensitivity, correlation window
- in `Macro Context`: correlation window only

The `Decision View` layout currently uses:

- compact `Structure Bias` pill
- `Narrative Summary` in a narrow left column
- `Supporting Metrics` in a wider adaptive grid

## How To Run

### 1. Install packages

```r
install.packages(c(
  "quantmod",
  "TTR",
  "zoo",
  "vars",
  "shiny",
  "shinydashboard",
  "ggplot2",
  "tidyr",
  "dplyr"
))
```

### 2. Run the app

```r
runApp("your_project_folder")
```

## Data Source

All data is currently loaded from Yahoo Finance through `quantmod::getSymbols()`.

Important notes:

- Yahoo ticker availability can change without notice
- `DXY` uses `DX-Y.NYB`
- `Silver` uses `SI=F`
- `USDTEC` uses `^NDX`
- if Yahoo fails at startup, the app may fail to load
- the current pipeline is designed around daily data

## Current Limitations

- the app still lives in a single `app.R` file
- liquidity detection is still close-based rather than full OHLC-based
- VAR is used as context, not as a full probabilistic model
- there is no backtest layer yet
- intraday timeframes such as `4H`, `1H`, and `15M` are not implemented yet
- the app depends heavily on Yahoo symbol stability

## Suggested Next Steps

- split helpers into separate modules
- move liquidity detection to full OHLC logic
- add backtesting and evaluation
- add caching or a fallback data source
- add intraday-capable provider support for `4H`, `1H`, and `15M`
