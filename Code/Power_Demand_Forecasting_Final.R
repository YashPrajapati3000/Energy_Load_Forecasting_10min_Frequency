library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(vars)
library(gridExtra)

# ============================================================
# 1. Load & Prepare Data
# ============================================================

df <- read.csv("../Dataset/powerconsumption.csv", header = TRUE, na.strings = c("", "NA"))

df <- df %>%
  mutate(
    Datetime = parse_date_time(
      Datetime,
      orders = c("dmy HM","dmy HMS","mdy HM","mdy HMS")
    )
  ) %>%
  arrange(Datetime)

# Common series
temp <- ts(df$Temperature, frequency = 144)

### ==========================
### TEMPERATURE SERIES
### ==========================
autoplot(temp) +
  labs(
    title = "Temperature – 10-min Observations",
    x = "Time",
    y = "Temperature (°C)"
  ) 

cat("\nADF Test — Temperature\n")
print(adf.test(temp))

cat("\nKPSS Test — Temperature\n")
print(kpss.test(temp))

p_acf_temp <- ggAcf(temp) +
  labs(title = "Temperature – ACF", x = "Lag", y = "ACF")

p_pacf_temp <- ggPacf(temp) +
  labs(title = "Temperature – PACF", x = "Lag", y = "PACF")

grid.arrange(p_acf_temp, p_pacf_temp, ncol = 2)

cat("\nExtended ACF (EACF) – Temperature\n")
print(eacf(temp))

# ============================================================
# ===================== ZONE 1 ================================
# ============================================================

z1 <- ts(df$PowerConsumption_Zone1, frequency = 144)
### ZONE 1 — RAW SERIES
autoplot(z1) +
  labs(
    title = "Zone 1 – Power Consumption (Raw)",
    x = "Time",
    y = "Power Consumption (kW)"
  )

cat("\nADF Test — Zone 1 (Raw)\n")
print(adf.test(z1))

cat("\nKPSS Test — Zone 1 (Raw)\n")
print(kpss.test(z1))

### ZONE 1 — LOG SERIES
lz1 <- log(z1)

autoplot(lz1) +
  labs(
    title = "Zone 1 – Power Consumption (Log Transformed)",
    x = "Time",
    y = "log(Power Consumption)"
  ) 

cat("\nADF Test — Zone 1 (Log)\n")
print(adf.test(lz1))

cat("\nKPSS Test — Zone 1 (Log)\n")
print(kpss.test(lz1))

# RAW
p_acf_z1_raw <- ggAcf(z1) +
  labs(title = "Zone 1 – ACF (Raw)", x = "Lag", y = "ACF")

p_pacf_z1_raw <- ggPacf(z1) +
  labs(title = "Zone 1 – PACF (Raw)", x = "Lag", y = "PACF")

# LOG
p_acf_z1_log <- ggAcf(lz1) +
  labs(title = "Zone 1 – ACF (Log)", x = "Lag", y = "ACF")

p_pacf_z1_log <- ggPacf(lz1) +
  labs(title = "Zone 1 – PACF (Log)", x = "Lag", y = "PACF")

grid.arrange(
  p_acf_z1_raw, p_pacf_z1_raw,
  p_acf_z1_log, p_pacf_z1_log,
  ncol = 2
)

cat("\nExtended ACF (EACF) – Log Zone 1\n")
print(eacf(lz1))

p_ccf1 <- ggCcf(z1, temp, lag.max = 300) +
  ggtitle("CCF: Zone 1 vs Temperature") +
  xlab("Lag") + ylab("Correlation")

p_ccf1

d1 <- mstl(lz1)
autoplot(d1) + ggtitle("Zone 1 – MSTL Decomposition") + 
  xlab("Time") + ylab("Decomposition Components")

sa_z1 <- lz1 - d1[, "Seasonal144"]

K <- 8
F1 <- fourier(ts(lz1, frequency=144), K)

fit_z1 <- Arima(
  sa_z1,
  order = c(3,1,2),
  xreg = cbind(F1, temp),
  include.drift = TRUE
)
summary(fit_z1)
checkresiduals(fit_z1)

# ---- Forecasting ----
h <- 144 * 3
F1_future <- fourier(ts(lz1, frequency=144), K = K, h = h)
colnames(F1_future) <- colnames(F1)
temp_future <- rep(tail(temp, 1), h)

fc_z1_sa <- forecast(
  fit_z1,
  xreg = cbind(F1_future, temp_future),
  h = h
)

S1_future <- rep(tail(d1[, "Seasonal144"], 144), length.out = h)
fc_z1_log <- fc_z1_sa$mean + S1_future
fc_z1 <- exp(fc_z1_log)

# Build a clean dataframe for plotting
df_plot_z1 <- data.frame(
  Time = 1:(1008 + h),
  Actual = c(tail(z1, 1008), rep(NA, h)),
  Forecast = c(rep(NA, 1008), as.numeric(fc_z1))
)

# Plot
ggplot(df_plot_z1, aes(x = Time)) +
  geom_line(aes(y = Actual, colour = "Actual (Last 7 Days)"), size = 0.8) +
  geom_line(aes(y = Forecast, colour = "Forecast (Next 3 Days)"), size = 0.8) +
  scale_colour_manual(values = c("Actual (Last 7 Days)" = "blue",
                                 "Forecast (Next 3 Days)" = "red")) +
  labs(
    title = "Zone 1 – Last 7 Days vs 3-Day Forecast",
    x = "Time Index",
    y = "Power Consumption (Z1)",
    colour = ""
  ) +
  theme(
    legend.position = c(0.90, 0.90),    # inside top-right corner
    legend.background = element_rect(fill = "white",colour = "black"),
    legend.key = element_blank()
  )

# ---- Backtest Z1 ----
h_test <- 144 * 3
n1 <- length(sa_z1)

sa_z1_train <- sa_z1[1:(n1 - h_test)]
sa_z1_test  <- sa_z1[(n1 - h_test + 1):n1]

F1_train <- F1[1:(n1 - h_test), ]
F1_test  <- F1[(n1 - h_test + 1):n1, ]

temp_train1 <- temp[1:(n1 - h_test)]
temp_test1  <- temp[(n1 - h_test + 1):n1]

fit_z1_bt <- Arima(
  sa_z1_train,
  order = c(3,1,2),
  xreg = cbind(F1_train, temp_train1),
  include.drift = TRUE
)

fc_z1_bt <- forecast(
  fit_z1_bt,
  xreg = cbind(F1_test, temp_test1),
  h = h_test
)

err_z1 <- sa_z1_test - fc_z1_bt$mean
rmse_z1 <- sqrt(mean(err_z1^2))
mae_z1  <- mean(abs(err_z1))
mape_z1 <- mean(abs(err_z1 / sa_z1_test)) * 100

rmse_z1; mae_z1; mape_z1

autoplot(cbind(actual = sa_z1_test, forecast = fc_z1_bt$mean)) +
  ggtitle("Backtest — Zone 1 (Seasonally Adjusted, Log Scale)")+
  xlab("Time") + ylab("SA Log(Power)")


# ============================================================
# ===================== ZONE 2 ================================
# ============================================================

z2 <- ts(df$PowerConsumption_Zone2, frequency = 144)

### ZONE 2 — RAW SERIES
autoplot(z2) +
  labs(
    title = "Zone 2 – Power Consumption (Raw)",
    x = "Time",
    y = "Power Consumption (kW)"
  )

cat("\nADF Test — Zone 2 (Raw)\n")
print(adf.test(z2))

cat("\nKPSS Test — Zone 2 (Raw)\n")
print(kpss.test(z2))

### ZONE 2 — LOG SERIES
lz2 <- log(z2)

autoplot(lz2) +
  labs(
    title = "Zone 2 – Power Consumption (Log Transformed)",
    x = "Time",
    y = "log(Power Consumption)"
  ) 

cat("\nADF Test — Zone 2 (Log)\n")
print(adf.test(lz2))

cat("\nKPSS Test — Zone 2 (Log)\n")
print(kpss.test(lz2))

# RAW
p_acf_z2_raw <- ggAcf(z2) +
  labs(title = "Zone 2 – ACF (Raw)", x = "Lag", y = "ACF")

p_pacf_z2_raw <- ggPacf(z2) +
  labs(title = "Zone 2 – PACF (Raw)", x = "Lag", y = "PACF")

# LOG
p_acf_z2_log <- ggAcf(lz2) +
  labs(title = "Zone 2 – ACF (Log)", x = "Lag", y = "ACF") 

p_pacf_z2_log <- ggPacf(lz2) +
  labs(title = "Zone 2 – PACF (Log)", x = "Lag", y = "PACF")

grid.arrange(
  p_acf_z2_raw, p_pacf_z2_raw,
  p_acf_z2_log, p_pacf_z2_log,
  ncol = 2
)

cat("\nExtended ACF (EACF) – Log Zone 2\n")
print(eacf(lz2))

# ccf
p_ccf2 <- ggCcf(z2, temp, lag.max = 300) +
  ggtitle("CCF: Zone 2 vs Temperature") +
  xlab("Lag") + ylab("Correlation")

p_ccf2

d2 <- mstl(lz2)
autoplot(d2) + ggtitle("Zone 2 – MSTL Decomposition") + 
  xlab("Time") + ylab("Decomposition Components")

sa_z2 <- lz2 - d2[, "Seasonal144"]

F2 <- fourier(ts(lz2, frequency=144), K)

fit_z2 <- Arima(
  sa_z2,
  order = c(0,1,5),
  xreg = cbind(F2, temp),
  include.drift = FALSE
)
summary(fit_z2)
checkresiduals(fit_z2)

# ---- Forecast Z2 ----
F2_future <- fourier(ts(lz2, frequency=144), K = K, h = h)
colnames(F2_future) <- colnames(F2)

fc_z2_sa <- forecast(
  fit_z2,
  xreg = cbind(F2_future, temp_future),
  h = h
)

S2_future <- rep(tail(d2[, "Seasonal144"], 144), length.out = h)
fc_z2_log <- fc_z2_sa$mean + S2_future
fc_z2 <- exp(fc_z2_log)

# Build a clean dataframe for plotting
df_plot_z2 <- data.frame(
  Time = 1:(1008 + h),
  Actual = c(tail(z2, 1008), rep(NA, h)),
  Forecast = c(rep(NA, 1008), as.numeric(fc_z2))
)

# Plot
ggplot(df_plot_z2, aes(x = Time)) +
  geom_line(aes(y = Actual, colour = "Actual (Last 7 Days)"), size = 0.8) +
  geom_line(aes(y = Forecast, colour = "Forecast (Next 3 Days)"), size = 0.8) +
  scale_colour_manual(values = c("Actual (Last 7 Days)" = "blue",
                                 "Forecast (Next 3 Days)" = "red")) +
  labs(
    title = "Zone 2 – Last 7 Days vs 3-Day Forecast",
    x = "Time Index",
    y = "Power Consumption (Z1)",
    colour = ""
  )

# ---- Backtest Z2 ----
n2 <- length(sa_z2)
sa_z2_train <- sa_z2[1:(n2 - h_test)]
sa_z2_test  <- sa_z2[(n2 - h_test + 1):n2]

F2_train <- F2[1:(n2 - h_test), ]
F2_test  <- F2[(n2 - h_test + 1):n2, ]

temp_train2 <- temp[1:(n2 - h_test)]
temp_test2  <- temp[(n2 - h_test + 1):n2]

fit_z2_bt <- Arima(
  sa_z2_train,
  order = c(0,1,5),
  xreg = cbind(F2_train, temp_train2),
  include.drift = FALSE
)

fc_z2_bt <- forecast(
  fit_z2_bt,
  xreg = cbind(F2_test, temp_test2),
  h = h_test
)

err_z2 <- sa_z2_test - fc_z2_bt$mean
rmse_z2 <- sqrt(mean(err_z2^2))
mae_z2  <- mean(abs(err_z2))
mape_z2 <- mean(abs(err_z2 / sa_z2_test)) * 100

rmse_z2; mae_z2; mape_z2

autoplot(cbind(actual = sa_z2_test, forecast = fc_z2_bt$mean)) +
  ggtitle("Backtest — Zone 2 (Seasonally Adjusted, Log Scale)")+
  xlab("Time") + ylab("SA Log(Power)")

# ============================================================
# ===================== ZONE 3 ================================
# ============================================================
z3  <- ts(df$PowerConsumption_Zone3, frequency = 144)

autoplot(z3) +
  labs(
    title = "Zone 3 – Power Consumption (Raw)",
    x = "Time",
    y = "Power Consumption (kW)"
  )

cat("\nADF Test — Zone 3 (Raw)\n")
print(adf.test(z3))

cat("\nKPSS Test — Zone 3 (Raw)\n")
print(kpss.test(z3))

### ZONE 3 — LOG SERIES
lz3 <- log(z3)

autoplot(lz3) +
  labs(
    title = "Zone 3 – Power Consumption (Log Transformed)",
    x = "Time",
    y = "log(Power Consumption)"
  ) 

cat("\nADF Test — Zone 3 (Log)\n")
print(adf.test(lz3))

cat("\nKPSS Test — Zone 3 (Log)\n")
print(kpss.test(lz3))

# RAW
p_acf_z3_raw <- ggAcf(z3) +
  labs(title = "Zone 3 – ACF (Raw)", x = "Lag", y = "ACF")

p_pacf_z3_raw <- ggPacf(z3) +
  labs(title = "Zone 3 – PACF (Raw)", x = "Lag", y = "PACF")

# LOG
p_acf_z3_log <- ggAcf(lz3) +
  labs(title = "Zone 3 – ACF (Log)", x = "Lag", y = "ACF")

p_pacf_z3_log <- ggPacf(lz3) +
  labs(title = "Zone 3 – PACF (Log)", x = "Lag", y = "PACF")

grid.arrange(
  p_acf_z3_raw, p_pacf_z3_raw,
  p_acf_z3_log, p_pacf_z3_log,
  ncol = 2
)

cat("\nExtended ACF (EACF) – Log Zone 3\n")
print(eacf(lz3))

# ccf
p_ccf3 <- ggCcf(z3, temp, lag.max = 300) +
  ggtitle("CCF: Zone 3 vs Temperature") +
  xlab("Lag") + ylab("Correlation")

p_ccf3

d3 <- mstl(lz3)
autoplot(d3) + ggtitle("Zone 3 – MSTL Decomposition") + 
  xlab("Time") + ylab("Decomposition Components")

sa_z3 <- lz3 - d3[, "Seasonal144"]

F3 <- fourier(ts(lz3, frequency = 144), K)

fit_z3 <- Arima(
  sa_z3,
  order = c(0,1,5),
  xreg = cbind(F3, temp),
  include.drift = TRUE
)

summary(fit_z3)
checkresiduals(fit_z3)

# ---- Forecast Z3 ----
F3_future <- fourier(ts(lz3, frequency = 144), K = K, h = h)
colnames(F3_future) <- colnames(F3)
temp_future3 <- rep(tail(temp, 1), h)

fc_z3_sa <- forecast(
  fit_z3,
  xreg = cbind(F3_future, temp_future3),
  h = h
)

S3_future <- rep(tail(d3[, "Seasonal144"], 144), length.out = h)
fc_z3_log <- fc_z3_sa$mean + S3_future
fc_z3 <- exp(fc_z3_log)


# Build a clean dataframe for plotting
df_plot_z3 <- data.frame(
  Time = 1:(1008 + h),
  Actual = c(tail(z3, 1008), rep(NA, h)),
  Forecast = c(rep(NA, 1008), as.numeric(fc_z3))
)

# Plot
ggplot(df_plot_z3, aes(x = Time)) +
  geom_line(aes(y = Actual, colour = "Actual (Last 7 Days)"), size = 0.8) +
  geom_line(aes(y = Forecast, colour = "Forecast (Next 3 Days)"), size = 0.8) +
  scale_colour_manual(values = c("Actual (Last 7 Days)" = "blue",
                                 "Forecast (Next 3 Days)" = "red")) +
  labs(
    title = "Zone 3 – Last 7 Days vs 3-Day Forecast",
    x = "Time Index",
    y = "Power Consumption (Z1)",
    colour = ""
  ) 

# ---- Backtest Z3 ----
h_test <- 144 * 3
n3 <- length(sa_z3)

sa_z3_train <- sa_z3[1:(n3 - h_test)]
sa_z3_test  <- sa_z3[(n3 - h_test + 1):n3]

F3_train <- F3[1:(n3 - h_test), ]
F3_test  <- F3[(n3 - h_test + 1):n3, ]

temp_train3 <- temp[1:(n3 - h_test)]
temp_test3  <- temp[(n3 - h_test + 1):n3]

fit_z3_bt <- Arima(
  sa_z3_train,
  order = c(0,1,5),
  xreg = cbind(F3_train, temp_train3),
  include.drift = TRUE
)

fc_z3_bt <- forecast(
  fit_z3_bt,
  xreg = cbind(F3_test, temp_test3),
  h = h_test
)

err_z3 <- sa_z3_test - fc_z3_bt$mean
rmse_z3 <- sqrt(mean(err_z3^2))
mae_z3  <- mean(abs(err_z3))
mape_z3 <- mean(abs(err_z3 / sa_z3_test)) * 100

rmse_z3; mae_z3; mape_z3

autoplot(cbind(actual = sa_z3_test, forecast = fc_z3_bt$mean)) +
  ggtitle("Backtest — Zone 3 (Seasonally Adjusted, Log Scale)") +
  xlab("Time") + ylab("SA Log(Power)")

