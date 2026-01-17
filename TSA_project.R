library(dplyr)
library(lubridate)
library(ggplot2)

electric_raw <- read.csv("C:/Users/johns/OneDrive/R/electric_demand_combined.csv")
weather_raw  <- read.csv("C:/Users/johns/OneDrive/R/weather_combined.csv")

electric <- electric_raw %>%
  dplyr::select(
    period,
    day_ahead_demand_forecast,
    demand,
    net_generation
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(
    datetime = ymd_h(period, tz = "America/New_York")
  ) %>%
  group_by(datetime) %>%
  summarise(
    period = first(period),
    day_ahead_demand_forecast = mean(day_ahead_demand_forecast, na.rm = TRUE),
    demand                    = mean(demand,                    na.rm = TRUE),
    net_generation            = mean(net_generation,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(datetime)

weather_clean <- weather_raw %>%
  dplyr::select(
    timestamp,
    temperature_c,
    relative_humidity,
    wind_speed_kmh
  ) %>%
  filter(complete.cases(temperature_c,
                        relative_humidity,
                        wind_speed_kmh)) %>%
  mutate(
    datetime = ymd_hms(timestamp, tz = "America/New_York")
  ) %>%
  group_by(datetime) %>%
  summarise(
    temperature_c     = mean(temperature_c),
    relative_humidity = mean(relative_humidity),
    wind_speed_kmh    = mean(wind_speed_kmh),
    .groups = "drop"
  ) %>%
  arrange(datetime)

overlap_start <- max(min(electric$datetime), min(weather_clean$datetime))
overlap_end   <- min(max(electric$datetime), max(weather_clean$datetime))

electric_use <- electric %>%
  filter(datetime >= overlap_start,
         datetime <= overlap_end) %>%
  arrange(datetime)

t_weather <- as.numeric(weather_clean$datetime)
t_elec    <- as.numeric(electric_use$datetime)

interp_to_elec <- function(y_vec) {
  approx(
    x    = t_weather,
    y    = y_vec,
    xout = t_elec
  )$y
}

electric_use$temperature_c     <- interp_to_elec(weather_clean$temperature_c)
electric_use$relative_humidity <- interp_to_elec(weather_clean$relative_humidity)
electric_use$wind_speed_kmh    <- interp_to_elec(weather_clean$wind_speed_kmh)

combined <- electric_use %>%
  mutate(timestamp = format(datetime, "%Y-%m-%d %H:%M:%S")) %>%
  dplyr::select(
    datetime,
    period,
    day_ahead_demand_forecast,
    demand,
    net_generation,
    timestamp,
    temperature_c,
    relative_humidity,
    wind_speed_kmh
  ) %>%
  arrange(datetime)

ggplot(combined, aes(x = datetime, y = demand)) +
  geom_line(color = "steelblue") +
  labs(title = "Electricity Demand Over Time",
       x = "Time",
       y = "Demand (MW)") +
  theme_minimal()

combined$hour <- hour(combined$datetime)
combined$date <- as.Date(combined$datetime)

daily_profile <- combined %>%
  group_by(date, hour) %>%
  summarise(demand = mean(demand), .groups="drop")
combined$weekday <- wday(combined$datetime, label = TRUE, abbr = FALSE)

weekly_pattern <- combined %>%
  group_by(weekday, hour) %>%
  summarise(demand = mean(demand), .groups = "drop")

ggplot(weekly_pattern, aes(x = hour, y = demand, color = weekday)) +
  geom_line(size = 1) +
  labs(title = "Average Hourly Demand by Day of Week",
       x = "Hour of Day",
       y = "Demand (MW)",
       color = "Day") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ts_demand <- ts(combined$demand, frequency = 24)

acf(ts_demand, lag.max = 7*24, main="ACF of Demand")

ggplot(combined, aes(x = temperature_c, y = demand)) +
  geom_point(alpha = 0.25, color = "darkblue") +
  geom_smooth(method="loess", color="red", se=FALSE) +
  labs(title = "Demand vs Temperature",
       x = "Temperature (°C)",
       y = "Demand (MW)") +
  theme_minimal()

ggplot(combined, aes(x = temperature_c, y = demand, color = relative_humidity)) +
  geom_point(alpha=0.6) +
  scale_color_viridis_c(option="plasma") +
  labs(title="Demand vs Temperature colored by Humidity",
       x = "Temperature (°C)",
       y = "Demand (MW)",
       color = "Humidity (%)") +
  theme_minimal()

combined <- combined %>% 
  mutate(hour = hour(datetime))

demand_by_hour <- combined %>%
  group_by(hour) %>%
  summarise(mean_demand = mean(demand, na.rm = TRUE))

temp_by_hour <- combined %>%
  group_by(hour) %>%
  summarise(mean_temp = mean(temperature_c, na.rm = TRUE))

demand_by_hour$z_demand <- scale(demand_by_hour$mean_demand)
temp_by_hour$z_temp     <- scale(temp_by_hour$mean_temp)

df_merge <- demand_by_hour %>%
  left_join(temp_by_hour, by = "hour")

ggplot(df_merge, aes(x = hour)) +
  geom_line(aes(y = z_demand, color="Demand"), size=1.2) +
  geom_line(aes(y = z_temp, color="Temperature"), size=1.2) +
  scale_color_manual(values=c("Demand"="steelblue",
                              "Temperature"="firebrick")) +
  labs(title="Daily Rhythms: Demand vs Temperature (Standardized)",
       x="Hour of Day",
       y="Standardized Value",
       color="Variable") +
  theme_minimal()

library(forecast)
demand <- combined$demand

ts_demand <- ts(demand, frequency = 24)

par(mfrow=c(1,1), mar=c(5,5,4,4))
acf(ts_demand, lag.max = 7 * 24, main="ACF of Demand (Original)")
pacf(ts_demand, lag.max = 7 * 24, main="PACF of Demand (Original)")

ts_season_diff <- diff(ts_demand, lag = 24)

acf(ts_season_diff, lag.max = 7 * 24,
    main="ACF after Seasonal Diff (lag=24)")
pacf(ts_season_diff, lag.max = 7 * 24,
     main="PACF after Seasonal Diff (lag=24)")

ts_double_diff <- diff(ts_season_diff, differences = 1)

par(mfrow=c(1,1), mar=c(5,5,4,4))
acf(ts_double_diff, lag.max = 7 * 24,
    main="ACF after Seasonal + Non-seasonal Diff")
pacf(ts_double_diff, lag.max = 7 * 24,
     main="PACF after Seasonal + Non-seasonal Diff")

X_all <- model.matrix(
  ~ day_ahead_demand_forecast +
    temperature_c +
    I(temperature_c^2) +
    relative_humidity +
    wind_speed_kmh - 1,
  data = combined
)

y_all <- combined$demand

test_start <- tail(combined$datetime, 7 * 24)[1]

idx_train <- which(combined$datetime <  test_start)
idx_test  <- which(combined$datetime >= test_start)

y_train <- y_all[idx_train]
y_test  <- y_all[idx_test]

X_train <- X_all[idx_train, ]
X_test  <- X_all[idx_test, ]

fit_reg_forc <- Arima(
  y_train,
  order    = c(1,1,1),
  seasonal = list(order = c(0,1,1), period = 24),
  xreg     = X_train
)

summary(fit_reg_forc)
checkresiduals(fit_reg_forc)

fc_forc <- forecast(fit_reg_forc, xreg = X_test, h = length(y_test))
pred_forc <- as.numeric(fc_forc$mean)

rmse <- function(e) sqrt(mean(e^2))
mae  <- function(e) mean(abs(e))

forecast_official <- combined$day_ahead_demand_forecast[idx_test]
actual            <- y_test

RMSE_model    <- rmse(pred_forc - actual)
RMSE_official <- rmse(forecast_official - actual)
MAE_model     <- mae(pred_forc - actual)
MAE_official  <- mae(forecast_official - actual)

results <- data.frame(
  Method = c("Our Model: Reg+ARIMA+Forecast+Weather",
             "Official Day-Ahead Forecast"),
  RMSE   = c(RMSE_model, RMSE_official),
  MAE    = c(MAE_model, MAE_official)
)

results$Rel_RMSE_vs_Official <- results$RMSE / RMSE_official
results

ml_df <- combined %>%
  mutate(
    temperature_sq = temperature_c^2,
    hour = hour(datetime),
    weekday = wday(datetime),
  ) %>%
  select(
    demand,
    day_ahead_demand_forecast,
    temperature_c, temperature_sq,
    relative_humidity, wind_speed_kmh,
    hour, weekday
  )
train_ml <- ml_df[combined$datetime < test_start, ]
test_ml  <- ml_df[combined$datetime >= test_start, ]

X_train_ml <- train_ml[, -1]
y_train_ml <- train_ml$demand

X_test_ml <- test_ml[, -1]
y_test_ml <- test_ml$demand

library(xgboost)

dtrain <- xgb.DMatrix(as.matrix(X_train_ml), label = y_train_ml)
dtest  <- xgb.DMatrix(as.matrix(X_test_ml),  label = y_test_ml)

params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

fit_xgb <- xgb.train(params, dtrain, nrounds = 500)

pred_xgb <- predict(fit_xgb, dtest)
rmse_xgb <- sqrt(mean((pred_xgb - y_test_ml)^2))

library(randomForest)

fit_rf <- randomForest(
  x = X_train_ml,
  y = y_train_ml,
  ntree = 500,
  mtry = 4
)

pred_rf <- predict(fit_rf, X_test_ml)
rmse_rf <- sqrt(mean((pred_rf - y_test_ml)^2))

data.frame(
  Model = c("ARIMA", "XGBoost", "Random Forest", "Official"),
  RMSE = c(
    RMSE_model,
    rmse_xgb,
    rmse_rf,
    RMSE_official
  )
)