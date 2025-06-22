#### R Code Part ####

# load library
library(tidyverse)
library(lubridate)
library(gridExtra)
library(forecast)
library(ggplot2)
library(knitr)
library(readr)
library(dplyr)
library(astsa)
library(here)

# load data
data <- read.csv("../Data/cleaned_data.csv")


data <- data %>%
  mutate(
    Date = as.Date(paste(Year, Month, "1", sep = "-"), format = "%Y-%m-%d")
  )

ggplot(data, aes(x = Date, y = Total_CO2_Emissions)) +
  geom_line(size = 1) +
  labs(
    x     = "Date",
    y     = "Total CO2 Emissions (million metric tons)"
  ) +
  theme_minimal()


data_zoom <- data %>%
  filter(Date >= as.Date("2015-01-01") & Date <= as.Date("2022-12-31"))


ggplot(data_zoom, aes(x = Date, y = Total_CO2_Emissions)) +
  geom_line(size = 1) +
  labs(
    title = "Total CO2 Emissions (2015–2022)",
    x     = "Date",
    y     = "Total CO2 Emissions (million metric tons)"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  

# Convert to time series first
co2_ts <- ts(data$Total_CO2_Emissions)

# Generate ACF object without plotting
acf_obj <- acf(co2_ts, plot = FALSE)

# Manually plot it without a title
plot(acf_obj, main = "")

data_train <- data[data$Year <= 2020, ]
data_test  <- data[data$Year > 2020, ]

model1 <- lm(
  Total_CO2_Emissions ~ . 
    - Date - Year - Month,
  data = data_train
)

model_summary <- summary(model1)

# Convert coefficients to a data frame
coef_table <- as.data.frame(coef(model_summary))

# Add row names as a column
coef_table$Variable <- rownames(coef_table)
rownames(coef_table) <- NULL

# Reorder
coef_table <- coef_table[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Shorten variable names using a named vector
var_names <- c(
  "(Intercept)" = "(Intercept)",
  "Transportarion.Petroleum.Consumption" = "Transportation",
  "Total.Fossil.Fuels.Production" = "Fossil Fuel Prod.",
  "Total.Renewable.Energy.Production" = "Renewable Energy Prod.",
  "Commercial_Consumption" = "Commercial",
  "Industrial_Consumption" = "Industrial",
  "Residential_Consumption" = "Residential"
)

# Apply shortened names
coef_table$Variable <- var_names[coef_table$Variable]

# Add significance codes (optional, mimicking base R behavior)
coef_table$Significance <- cut(coef_table$`Pr(>|t|)`,
                               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                               labels = c("***", "**", "*", ".", ""))

# Display the table
knitr::kable(coef_table, digits = 4)

plot(model1$fitted.values, resid(model1),
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

plot(data_train$Date, resid(model1),
     type = "l",
     xlab = "Date",
     ylab = "Residuals")
abline(h = 0, col = "red")

hist(resid(model1),
     breaks = 30,
     main = NULL,
     xlab = "Residuals")

# Residuals satisfied identical normal distribution
qqnorm(resid(model1), main = NULL)
qqline(resid(model1), col = "red")

acf(resid(model1), main = "")

pacf(resid(model1), main = "")

# ljung-box test shows that the residuals are auto-correlated
ljung_result <- Box.test(resid(model1), lag = 20, type = "Ljung-Box")

# Create table from the output
box_test_table <- data.frame(
  `Test`       = "Box-Ljung",
  `Statistic`  = round(ljung_result$statistic, 2),
  `df`         = ljung_result$parameter,
  `p-value`    = format.pval(ljung_result$p.value, digits = 3, eps = .001)
)

# Display the table
knitr::kable(box_test_table)

# auto fitted arima
resid_ts <- ts(resid(model1))
resid_arima <- forecast::auto.arima(resid_ts)

# Extract summary metrics
arima_summary <- summary(resid_arima)

# Get training accuracy metrics separately
train_metrics <- accuracy(resid_arima)

# Build a combined table
arima_metrics <- data.frame(
  Metric = c("Sigma²", "Log Likelihood", "AIC", "AICc", "BIC",
             "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1"),
  Value = c(
    arima_summary$sigma2,
    arima_summary$loglik,
    arima_summary$aic,
    arima_summary$aicc,
    arima_summary$bic,
    train_metrics[1, "ME"],
    train_metrics[1, "RMSE"],
    train_metrics[1, "MAE"],
    train_metrics[1, "MPE"],
    train_metrics[1, "MAPE"],
    train_metrics[1, "MASE"],
    train_metrics[1, "ACF1"]
  )
)

# Display the table
knitr::kable(arima_metrics, digits = 4)

# Fit ARIMA model
resid_ts <- ts(resid(model1))
resid_arima <- forecast::auto.arima(resid_ts)

# Extract coefficients and standard errors
coef_vals <- coef(resid_arima)
se_vals <- sqrt(diag(resid_arima$var.coef))

# Create table
arima_coef_table <- data.frame(
  Term = names(coef_vals),
  Estimate = round(coef_vals, 4),
  Std_Error = round(se_vals, 4)
)

# Display
knitr::kable(arima_coef_table)

y_hat_lm <- fitted(model1) 

resid_hat_arima <- fitted(resid_arima)
y_hat_combined <- y_hat_lm + resid_hat_arima

plot(data_train$Date, data_train$Total_CO2_Emissions, type = "l", col = "black", lwd = 2,
     ylab = "Total CO2 Emissions (million metric tons)",
     xlab = "Date",
     cex.lab = 1.2, cex.main = 1.4)

lines(data_train$Date, y_hat_combined, col = "red", lwd = 2)

legend("topright",
       legend = c("Observed", "LM + ARIMA Fit"),
       col = c("black", "red"),
       lty = 1,
       lwd = 2,
       cex = 1.1,
       bty = "n") 



y_true <- data_train$Total_CO2_Emissions
y_lm   <- fitted(model1)

plot(data_train$Date, y_true, type = "l", col = "black", lwd = 2,
     ylab = "Total CO2 Emissions (million metric tons)",
     xlab = "Date",
     cex.lab = 1.2, cex.main = 1.4)

lines(data_train$Date, y_lm, col = "red", lwd = 2)

legend("topright",
       legend = c("Observed", "Linear Model Fit"),
       col = c("black", "red"),
       lty = 1,
       lwd = 2,
       cex = 1.1,
       bty = "n")

# Compute RSS and RMSE if not already done
rss_lm <- sum((y_true - y_lm)^2)
rss_combined <- sum((y_true - y_hat_combined)^2)

rmse_lm <- sqrt(mean((y_true - y_lm)^2))
rmse_combined <- sqrt(mean((y_true - y_hat_combined)^2))

# Create a comparison table
model_comparison <- data.frame(
  Model = c("Linear Model", "Linear + ARIMA Model"),
  RSS   = c(rss_lm, rss_combined),
  RMSE  = c(rmse_lm, rmse_combined)
)

# Display the table
knitr::kable(model_comparison, digits = 4)

# Predict data adter 2020
pred_result <- predict(model1, newdata = data_test, interval = "prediction", level = 0.95)

y_test_true <- data_test$Total_CO2_Emissions
y_test_pred <- pred_result[, "fit"]
y_test_lower <- pred_result[, "lwr"]
y_test_upper <- pred_result[, "upr"]

plot(data_test$Date, y_test_true, type = "l", col = "black", lwd = 2,
     ylab = "Total CO2 Emissions (million metric tons)",
     xlab = "Date",
     cex.lab = 1.2, cex.main = 1.4)

lines(data_test$Date, y_test_pred, col = "blue", lwd = 2)
lines(data_test$Date, y_test_lower, col = "blue", lty = 2, lwd = 1.5)
lines(data_test$Date, y_test_upper, col = "blue", lty = 2, lwd = 1.5)

legend("topright",
       legend = c("Observed", "Predicted", "95% Prediction Interval"),
       col = c("black", "blue", "blue"),
       lty = c(1, 1, 2),
       lwd = 2,
       cex = 1.1,
       bty = "n")






