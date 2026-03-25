# 📊 Macro Market Structure Dashboard

A lightweight Shiny dashboard to analyze **Gold, Bitcoin, and Oil** using market structure and cross-asset insights.

`“Don’t predict price. Understand structure.”`

---

## 🚀 What This Does

This dashboard helps you quickly understand:

- Market direction (trend vs reversal)
- Key structure levels (HH, HL, LH, LL)
- Breakouts and shifts (BOS / CHoCH)
- Institutional zones (Order Blocks)
- Cross-asset relationships (Correlation)

> 🎯 Focus: **market context, not price prediction**

---

## 🧠 Key Features

### 📈 Market Structure
- Automatic detection of:
  - Higher High (HH)
  - Higher Low (HL)
  - Lower High (LH)
  - Lower Low (LL)
- Real-time structure bias:
  - Bullish / Bearish / Neutral

---

### 🔄 BOS & CHoCH
- Identifies:
  - Break of Structure (trend continuation)
  - Change of Character (potential reversal)

---

### 🟩 Order Blocks
- Highlights potential institutional zones
- Visualized directly on chart

---

### 🔗 Correlation Analysis
- Rolling correlation between:
  - Gold vs BTC
  - Gold vs Oil
  - BTC vs Oil
- Adjustable time window

---

### ⚡ Signal Summary
- Simple trade bias:
  - BUY / SELL / WAIT
- Based on momentum and volatility

---

## 🛠️ How to Run

### 1. Install packages

```r
install.packages(c(
  "quantmod", "TTR", "zoo", "vars",
  "shiny", "ggplot2", "tidyr", "dplyr"
))
```

### 2. Run app

```r
runApp("your_project_folder")
```

## 🎮 How to Use

* Select asset: Gold / BTC / Oil
* Adjust:
  - Swing sensitivity (structure detail)
  - Correlation window (market view)
* Read:
  - Structure bias
  - Key levels
  - Correlation behavior

## ⚠️ Notes

- Data sourced from Yahoo Finance
- Market hours differ across assets
- Signals are contextual, not trading advice

## 🔮 Future Ideas

* Liquidity zones (equal highs/lows)
* Premium vs discount areas
* Backtesting engine
* Alert system
