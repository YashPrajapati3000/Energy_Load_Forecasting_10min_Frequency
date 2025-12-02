# ⚡ Energy Load Forecasting (10-Minute Frequency)  
High-Resolution Power Consumption Modeling Across Three Zones

**Author:** Yashkumar Prajapati  
**Project Type:** High-frequency forecasting (10-minute data)

---

## 📘 1. Project Overview

This project focuses on **short-term energy load forecasting** using **high-frequency (10-minute interval)** power consumption data across three operational zones (Zone 1, Zone 2, Zone 3).  
The objective is to build reliable, diagnostic-driven forecasting models using:

- Fourier-augmented regression  
- ARIMA models with correlated errors  
- MSTL decomposition  
- Temperature pre-whitening & CCF analysis  
- 3-day ahead out-of-sample forecasting  
- Backtesting on seasonally-adjusted series  

The project demonstrates complete end-to-end time-series modeling: data cleaning, visualization, decomposition, model selection, diagnostics, forecasting, and backtesting.

---

## 🎯 2. Project Objectives

1. Conduct full exploratory analysis of raw, log-transformed, and seasonally adjusted series.  
2. Use MSTL to extract trend, seasonal, and remainder components for all three zones.  
3. Identify lag structure using ACF, PACF, and Extended ACF (EACF).  
4. Perform pre-whitening to analyze CCF between temperature and each zone.  
5. Build regression models with Fourier terms (K = 12) and ARIMA(0,1,5) errors.  
6. Diagnose model adequacy using:  
   - Residual plots  
   - ACF of residuals  
   - Histograms + density  
7. Generate 3-day (432-step) forecasts for all zones.  
8. Perform rolling backtests on seasonally adjusted data.  
9. Present findings via a recorded presentation and written final report.

---

## 📁 3. Folder Structure
Energy_Load_Forecasting_10min/
│
├── Code/
│ └── EDA.R
│ └── Power_Demand_Forecasting_Final.R
│
├── Dataset/
│ └── powerconsumption.csv
│
├── Plots/
│ └── All Plots
│
├── Presentation/
│ ├── Power_Presentation.pptx
│ └── Power_Presentation.pdf
│
├── Report/
│ └── Final_Report.pdf
│
└── readme.md

---

## 🧪 4. Key Components

### **A. Data Preparation**
- Cleaned Datetime values with multiple formats  
- Ensured strict 10-minute spacing & removed duplicates  
- Constructed ts objects with `frequency = 144`  
- Log transformation for variance stabilization  

---

### **B. Exploratory Time-Series Analysis**
Included for all three zones:
- Raw time-series  
- Log-transformed series  
- ACF/PACF plots  
- EACF tables for AR/MA structure identification  
- Temperature cross-correlation (CCF)  
- MSTL decomposition (Trend, Seasonal(144), Remainder)

---

### **C. Regression + ARIMA Error Modeling**
Main forecasting model used:
y_t = β0 + β1 * Temperature_t + Fourier_K(t) + ARIMA(0,1,5) errors


Where:
- Fourier Terms: K = 12  
- Errors follow: ARIMA(0,1,5)  
- Selected based on EACF + ACF/PACF analysis  

---

### **D. Model Diagnostics**
For each zone:
- Residual time-series  
- Residual ACF  
- Histogram + kernel density  
- Ljung-Box tests  
- Breusch-Godfrey tests  
- ACF/PACF of differenced residuals  

The residuals show:
- No remaining autocorrelation  
- Symmetric residual distribution  
- Well-behaved ARIMA(0,1,5) errors  

---

### **E. Forecasting**
Generated:
- **3-day (432-step) ahead forecasts**  
- Forecasts plotted against the **last 7 days actuals**  
- Separate forecasts for Zone 1, Zone 2, Zone 3

---

### **F. Backtesting**
Conducted on:
- **Seasonally adjusted (MSTL) log series**  
- Rolling-origin forecast windows  
- Highlighted performance stability across trends  

---

## 📈 5. Visual Outputs

Stored inside `/Plots/`:
- Raw vs Log plots  
- ACF/PACF (raw + log)  
- MSTL decomposition  
- EACF tables  
- Temperature CCF comparisons  
- Residual diagnostics  
- 7-day actual vs 3-day forecast  
- Backtesting curves  

---

## 🎥 6. Presentation Video

The project presentation is available on YouTube:

🔗 **YouTube Link:** *[https://www.youtube.com/watch?v=v5ArMqDpfcw]*

---

## 📝 7. Final Report  
A full 15–20 page report containing:

- Methodology  
- Mathematical walkthrough  
- All diagnostics  
- Interpretations  
- Forecasting outcomes  
- Backtest results  

📄 Available in:  
`/Report/Final_Report.pdf`

---

## 🛠 8. Tools & Technologies

| Category     | Tools |
|--------------|-------|
| Language     | **R** |
| Libraries    | forecast, TSA, tseries, ggplot2, dplyr, lubridate |
| Models       | Regression + ARIMA errors, MSTL, Fourier terms |
| Diagnostics  | ACF, PACF, EACF, CCF, Ljung-Box, BG test |
| Forecasting  | 10-minute interval, short-horizon |
| Visualization| ggplot2, autoplot |

---

## ▶ 9. How to Run the Project

1. Clone the repository:
```bash
git clone https://github.com/YashPrajapati3000/Energy_Load_Forecasting_10min_Frequency.git

2. Open the R script:
Energy_Load_Forecasting_10min/Code/final_model.R

3. Make sure working directory is set:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

4. Load the dataset:
df <- read.csv("../Datasets/powerconsumption.csv")

5. Run all sections in order:
1. **Data loading**  
2. **Time-series creation**  
3. **MSTL**  
4. **EACF + ACF/PACF**  
5. **CCF**  
6. **Regression with ARIMA errors**  
7. **Forecasts + Plots**  
8. **Backtesting**

---

### 📬 Contact

**Author:** Yashkumar Prajapati  
**Email:** yashpraj62@gmail.com  
**Project Type:** Academic — Time Series Forecasting