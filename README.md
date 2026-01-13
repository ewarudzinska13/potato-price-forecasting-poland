# potato-price-forecasting-poland
Time series forecasting of potato prices in Poland (2009-2018) comparing naive, moving average, exponential smoothing, Holt-Winters, ETS, and SARIMA models

# Potato Price Forecasting in Poland (2009-2018)

Time series analysis and forecasting of retail potato prices in Poland using various forecasting methods. Course project for Forecasting and Simulations at University of Warsaw.

## Project Overview

This study compares the effectiveness of simple and advanced forecasting methods for potato prices in a highly volatile market. Inspired by Şahinli (2020) paper on Turkish potato market, adapted and extended for Polish conditions.

## Research Question

**Which forecasting method performs best for volatile potato prices in Poland?**

Potato prices exhibit strong seasonal patterns and high volatility due to:
- Weather conditions affecting harvest yields
- Seasonal consumption patterns (prices rise in winter/spring, fall during autumn harvest)
- Storage period dynamics

## Data

**Source**: Statistics Poland (GUS) - Local Data Bank  
**Variable**: Average retail potato prices (PLN/kg)  
**Period**: January 2009 - December 2018 (120 monthly observations)  
**Split**: 
- In-sample: Jan 2009 - Dec 2017 (108 observations)
- Out-of-sample: Jan 2018 - Dec 2018 (12 observations)

### Data Characteristics

- Strong **annual seasonality** (s = 12)
- **Multiplicative** seasonal pattern (amplitude varies with level)
- Outliers identified by Tramo-Seats decomposition
- High short-term volatility

## Methodology

### Decomposition: Tramo-Seats

Used JDemetra+ Tramo-Seats method to:
- Identify seasonal pattern (multiplicative)
- Extract trend and irregular components
- Detect outliers
- Produce seasonally adjusted series for forecasting

### Forecasting Methods

1. **Naive method** (benchmark)
2. **Moving Average (MA)**
   - Tested windows: k = 2, 3, 6, 12 months
   - Optimized window selection (k = 1 to 60)
3. **Simple Exponential Smoothing (SES)**
4. **Holt-Winters**
   - Additive version
   - Multiplicative version
5. **ETS (automatic selection)**
6. **SARIMA**
   - General-to-specific procedure
   - ACF/PACF analysis
   - Stationarity tests (ADF, KPSS)
   - Model selection via AIC/BIC

### Model Evaluation

**Error metrics**: RMSE, MAE, MAPE  
**Statistical test**: Diebold-Mariano test (forecast accuracy comparison)

## Key Results

### Forecast Accuracy (Out-of-Sample)

| Rank | Method | RMSE | MAE | MAPE (%) |
|------|--------|------|-----|----------|
| 1 | **MA(3)** | **0.0731** | **0.0448** | **3.03** |
| 2 | SES | 0.1502 | 0.0966 | 6.44 |
| 3 | Naive | 0.1569 | 0.1054 | 7.51 |
| 4 | ETS | 0.2154 | 0.1752 | 12.26 |
| 5 | Holt-Winters (add) | 0.2291 | 0.2153 | 15.78 |
| 6 | Holt-Winters (mult) | 0.2617 | 0.2215 | 16.28 |
| 7 | SARIMA(1,1,0)(1,1,0)₁₂ | 0.2568 | 0.1977 | 13.56 |

### Main Findings

**Simple methods outperformed complex models:**

1. **Moving Average (k=3)** achieved best results
   - RMSE nearly **4x lower** than SARIMA
   - Only uses last 3 months of data
   - Optimal window from systematic search (k=1 to 60)

2. **Simple Exponential Smoothing** ranked second
   - Alpha ≈ 0.99 (almost naive)
   - Fast adaptation to recent changes

3. **SARIMA performed worst** despite theoretical advantages
   - Failed to capture volatile price movements
   - Long-term patterns less relevant in high-volatility market

### Diebold-Mariano Test Results

Statistically significant differences (α = 0.05):
- MA(3) vs. all complex methods: **MA(3) significantly better**
- SES vs. Holt-Winters: **SES significantly better**
- No significant differences among complex methods (HW, ETS, SARIMA)

### Why Simple Methods Won?

**Market characteristics favor recent data:**
- Prices respond quickly to supply/demand shocks
- Historical patterns beyond 3-6 months have limited predictive power
- High volatility makes long-term patterns unreliable
- Short forecast horizon (12 months) suits simple methods

## SARIMA Specification

### Identification Process

**Original series**: Non-stationary (visual inspection + tests)

**First regular differences**: Still shows seasonal patterns  
- ACF: Significant spikes at lags 12, 24, 36  
- PACF: Decay after lag 1

**First seasonal differences** (after regular differencing):  
- ADF test: p = 0.05 (borderline stationary)
- KPSS test: p > 0.10 (fail to reject stationarity)
- Ljung-Box: Reject white noise → proceed with modeling

**Integration orders**: d = 1, D = 1

### Model Selection: General-to-Specific

Starting model: SARIMA(1,1,1)(1,1,1)₁₂

| Model | AIC | BIC | Selected |
|-------|-----|-----|----------|
| (1,1,1)(1,1,0)₁₂ | -78.65 | -68.43 | |
| (0,1,1)(1,1,0)₁₂ | -79.67 | -72.01 | |
| **(1,1,0)(1,1,0)₁₂** | **-80.15** | **-74.49** | **✓** |
| (0,1,0)(1,1,0)₁₂ | -78.84 | -73.73 | |

**Final model**: SARIMA(1,1,0)(1,1,0)₁₂

**Diagnostics**:
- Ljung-Box test: p = 0.24 (residuals are white noise ✓)
- ACF/PACF of residuals: No significant spikes ✓

## Comparison with Baseline Study

**Şahinli (2020) - Turkey**:
- Best method: ARIMA(1,1,2)
- Holt-Winters ranked second
- Turkish market likely less volatile

**This study - Poland**:
- Best method: MA(3)
- SARIMA performed worst
- Polish market exhibits higher volatility

**Conclusion**: Market volatility determines optimal forecasting approach

## Implications

### For Producers
- Short-term forecasts (1-3 months) most reliable
- Plan planting based on recent price trends
- Simple methods provide adequate decision support

### For Processors & Retailers
- Recent price movements best predictor of near-term prices
- Complex inventory models may not add value
- Focus on responsive supply chain management

### For Policy
- Price stabilization requires addressing short-term shocks
- Early warning systems can use simple indicators
- Complex econometric models may overcomplicate monitoring

## Tools & Software

- **R** (primary analysis environment)
  - Packages: `forecast`, `RJDemetra`, `tseries`, `Metrics`, `zoo`
- **JDemetra+** (seasonal decomposition via Tramo-Seats)


## Reproducibility
```r
# Install required packages
install.packages(c("forecast", "RJDemetra", "tseries", "Metrics", "zoo"))

# Load data (GUS format)
# data <- read.csv("potato_prices.csv")

# Run analysis
source("potato_price_forecasting_clean.R")
```

## Key Visualizations

1. **Time series decomposition** (Tramo-Seats)
2. **RMSE by MA window length** (optimization)
3. **ACF/PACF plots** (SARIMA identification)
4. **Forecast comparison** (all methods vs. actual)
5. **Forecast accuracy barplot** (RMSE comparison)

## Limitations

- Short forecast horizon (12 months only)
- No exogenous variables (weather, fuel prices, EU policies)
- Single market (Poland) - results may not generalize
- Structural changes not modeled (e.g., CAP reforms)

## Future Research

- Include weather data as exogenous variables
- Test machine learning methods (LSTM, Random Forest)
- Multi-step ahead forecasting (h > 12)
- Regional price analysis (voivodeship level)
- Forecast combination methods

---

**Course**: Forecasting and Simulations (Prognozowanie i Symulacje)  
**Institution**: University of Warsaw, Faculty of Economic Sciences  
**Supervisor**: Dr. Łukasz Postek

## References

Şahinli, M. A. (2020). Potato price forecasting with Holt-Winters and ARIMA methods: a case study. *American Journal of Potato Research*, 97(4), 336-346.
