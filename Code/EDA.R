library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(TSA)
library(gridExtra)

# ---- 1) Load and Parse ----
df <- read.csv("../Dataset/powerconsumption.csv", header = TRUE, na.strings = c("", "NA"))

# parse datetime
df <- df %>%
  mutate(Datetime = parse_date_time(Datetime,
                                    orders = c("dmy HM","dmy HMS","mdy HM","mdy HMS"),
                                    tz = "UTC")) %>%
  arrange(Datetime)

# convert to ts objects (10-min frequency: 144 per day)
z1 <- ts(df$PowerConsumption_Zone1, frequency = 144)
z2 <- ts(df$PowerConsumption_Zone2, frequency = 144)
z3 <- ts(df$PowerConsumption_Zone3, frequency = 144)

temp <- ts(df$Temperature, frequency = 144)
hum  <- ts(df$Humidity, frequency = 144)
wind <- ts(df$WindSpeed, frequency = 144)

# 1. raw time-series plots
p1 <- autoplot(z1) + ggtitle("Zone1 raw")
p2 <- autoplot(z2) + ggtitle("Zone2 raw")
p3 <- autoplot(z3) + ggtitle("Zone3 raw")
grid.arrange(p1, p2, p3, ncol=1)

# 2. STL decomposition for seasonality patterns
stl_z1 <- stl(z1, s.window="periodic")
stl_z2 <- stl(z2, s.window="periodic")
stl_z3 <- stl(z3, s.window="periodic")

autoplot(stl_z1) + ggtitle("Zone1 STL")
autoplot(stl_z2) + ggtitle("Zone2 STL")
autoplot(stl_z3) + ggtitle("Zone3 STL")

# 3. ACF/PACF to detect AR/MA components and seasonality strength
acf(z1, lag.max = 1000, main="ACF Z1")
pacf(z1, lag.max = 1000, main="PACF Z1")

acf(z2, lag.max = 1000, main="ACF Z2")
pacf(z2, lag.max = 1000, main="PACF Z2")

acf(z3, lag.max = 1000, main="ACF Z3")
pacf(z3, lag.max = 1000, main="PACF Z3")

# 4. seasonal subseries plots to reveal daily/weekly patterns
ggseasonplot(z1, year.labels=FALSE) + ggtitle("Seasonplot Z1")
ggsubseriesplot(z1) + ggtitle("Subseries Z1")

ggseasonplot(z2, year.labels=FALSE) + ggtitle("Seasonplot Z2")
ggsubseriesplot(z2) + ggtitle("Subseries Z2")

ggseasonplot(z3, year.labels=FALSE) + ggtitle("Seasonplot Z3")
ggsubseriesplot(z3) + ggtitle("Subseries Z3")

# 5. cross-correlation with weather (lead/lag detection)
ccf(z1, temp, lag.max = 500, main="Z1 vs Temp CCF")
ccf(z1, hum,  lag.max = 500, main="Z1 vs Hum CCF")
ccf(z1, wind, lag.max = 500, main="Z1 vs Wind CCF")

ccf(z2, temp, lag.max = 500, main="Z2 vs Temp CCF")
ccf(z3, temp, lag.max = 500, main="Z3 vs Temp CCF")

# 6. variance diagnostics for GARCH relevance
plot(z1, main="Z1 raw")
acf(z1^2, lag.max=500, main="ACF of Z1 squared")

plot(z2, main="Z2 raw")
acf(z2^2, lag.max=500, main="ACF of Z2 squared")

plot(z3, main="Z3 raw")
acf(z3^2, lag.max=500, main="ACF Z3 squared")
