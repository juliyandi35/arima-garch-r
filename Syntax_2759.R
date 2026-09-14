# Step 1: Load required libraries
library(tseries)
library(fGarch)
library(forecast)
library(rugarch)
library(readxl)
set.seed(123)  # Set seed for reproducibility

# Step 2: Input return data for MAPA and ICBP
MAP=read_excel("MAPA.xlsx",sheet="MAPA",col_names=T)
ICBP=read_excel("ICBP.xlsx",sheet="ICBP",col_names=T)
mapa_returns <- MAP$Return
icbp_returns <- ICBP$Return
returns <- cbind(mapa_returns,icbp_returns)

# Step 4: Calculate variance and mean for each window
calculate_variance_mean <- function(data_windows) {
  results <- list()
  for (i in 1:length(data_windows)) {
    window <- data_windows[[i]]
    variance <- var(window)
    mean_return <- mean(window)
    results[[i]] <- list(variance = variance, mean_return = mean_return)
  }
  return(results)
}

# Step 5: Monte Carlo simulation for each window
monte_carlo_simulation <- function(data_windows, num_simulations) {
  simulated_returns <- list()
  for (i in 1:length(data_windows)) {
    # Generate random residuals based on quantile regression model
    residuals <- residuals(model_quantile) + rnorm(nrow(train_data))

    # Generate simulated returns
    simulated_returns[j] <- predict(model_quantile, newdata = data.frame(train_data = residuals))
  }
  return(simulated_returns)
}

# Step 6: Maximum loss estimation in confidence intervals
calculate_max_loss <- function(simulated_returns, confidence_level, t_period) {
  max_losses <- list()
  for (i in 1:length(simulated_returns)) {
    window_simulations <- simulated_returns[[i]]
    quantile <- quantile(window_simulations, 1 - confidence_level)
    max_losses[[i]] <- apply(window_simulations, 2, function(x) max(x[t_period] - quantile))
  }

  return(max_losses)
}

# Step 7: Calculate average Value at Risk
calculate_average_var <- function(max_losses) {
  var_average <- sapply(max_losses, mean)
  return(var_average)
}

# Step 9: Stationarity test using ADF and differencing
stationarity_test <- function(data) {
  adf_result <- adf.test(data)
  if (adf_result$p.value > 0.05) {
    data_diff <- diff(data)
    return(data_diff)
  } else {
    return(data)
  }
}

# Step 10: Identify ARIMA model using ACF and PACF plots
identify_arima_model <- function(data) {
  acf_plot <- acf(data)
  pacf_plot <- pacf(data)
  # Analyze ACF and PACF plots to identify the appropriate ARIMA model
  arima_order <- auto.arima(data,stepwise = F,approximation = F)

  return(arima_order) # Replace with the identified ARIMA order
}

# Step 11: Estimate ARIMA model parameters using MLE
estimate_arima_parameters <- function(data, arima_order) {
  arima_model <- arima(data, order = arima_order)
  return(arima_model$coef)
}

# Step 12: Test parameter significance in the ARIMA model
test_parameter_significance <- function(data_windows,arima_order) {
  # Perform significance tests for each parameter in the ARIMA model
  for (i in 1:length(data_windows)) {
    window <- data_windows[[i]]
    arima_model <- arima(window, order = arima_order)
    parameter_significance <- summary(arima_model)$coefficients[, 'Pr(>|t|)']
    parameter_significance[[i]] <- parameter_significance
  }
  return(parameter_significance) # Replace with the test results
}

# Step 13: Check white noise assumption using Ljung-Box and Kolmogorov-Smirnov tests
check_white_noise <- function(arima_residuals) {
  ljung_box_test <- Box.test(arima_residuals, lag = 20, type = "Ljung-Box")
  ks_test <- ks.test(arima_residuals, "pnorm")
  return(list(ljung_box_test = ljung_box_test, ks_test = ks_test))
}

# Step 14: Choose the best ARIMA model based on AIC and SBC
choose_best_arima_model <- function(data_windows, arima_order) {
  best_model <- NULL
  best_aic <- Inf
  best_bic <- Inf

  for (i in 1:length(data_windows)) {
    window <- data_windows[[i]]
    arima_models <- arima(window, order = arima_order)
    model_aic <- AIC(arima_models)
    model_bic <- BIC(arima_models)

    if (model_aic < best_aic && model_bic < best_bic) {
      best_model <- arima_models
      best_aic <- model_aic
      best_bic <- model_bic
    }
  }

  return(best_model)
}


# Step 15: Determine GARCH model for each window
determine_garch_model <- function(data_windows) {
  garch_models <- list()
  for (i in 1:length(data_windows)) {
    window <- data_windows[[i]]
    garch_model <- garchFit(formula = ~garch(1, 1), data = window, trace = FALSE)
    garch_models[[i]] <- garch_model
  }
  return(garch_models)
}

# Step 16: Estimate ARIMA-GARCH model parameters for each window
estimate_arima_garch_parameters <- function(data_windows, arima_order, garch_models) {
  arima_garch_models <- list()
  for (i in 1:length(data_windows)) {
    window <- as.numeric(na.omit(data_windows[[i]]))
    spec <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = arima_order)
    )
    fit <- ugarchfit(spec, data = window)
    arima_garch_models[[i]] <- fit
  }

  return(arima_garch_models)
}

# Step 17: Calculate Value at Risk for each window based on ARIMA-GARCH model
calculate_var_arima_garch <- function(arima_garch_models, confidence_level, t_period) {
  var_values <- list()
  for (i in 1:length(arima_garch_models)) {
    arima_garch_model <- arima_garch_models[[i]]
    var_value <- quantile(arima_garch_model@fit$residuals / arima_garch_model@fit$sigma, 1 - confidence_level) * arima_garch_model@fit$sigma[t_period]
    var_values[[i]] <- var_value* qnorm(0.05)
  }
  return(var_values)
}

# Step 19: Apply the defined functions to perform the analysis
window_size <- 250
num_simulations <- 7000
confidence_level <- 0.95
t_period <- 10

# Step 2: Time series plots
plot(mapa_returns, type = "l", main = "MAPA Stock Returns")
plot(icbp_returns, type = "l", main = "ICBP Stock Returns")

# Step 3: Sliding window
for (i in window_size:(nrow(returns))) {
  window_data <- returns[(i - window_size + 1):i, ]
  train_data <- window_data[1:(window_size - 1), ]
  test_data <- window_data[window_size, ]
}

# Step 4: Calculate variance and mean
mapa_params <- calculate_variance_mean(returns)
icbp_params <- calculate_variance_mean(icbp_windows)

# Fit quantile regression model on training data
model_quantile <- rq(formula = train_data ~ 1, tau = quantile)

# Step 5: Monte Carlo simulation
mapa_simulations <- monte_carlo_simulation(mapa_windows, num_simulations)
icbp_simulations <- monte_carlo_simulation(icbp_windows, num_simulations)

# Step 6: Maximum loss estimation
mapa_max_losses <- calculate_max_loss(mapa_simulations, confidence_level, t_period)
icbp_max_losses <- calculate_max_loss(icbp_simulations, confidence_level, t_period)

# Step 7: Calculate average Value at Risk
mapa_average_var <- calculate_average_var(mapa_max_losses)
icbp_average_var <- calculate_average_var(icbp_max_losses)

# Step 8: Backtesting
Monte_Carlo_mapa_backtesting <- for (i in 1:length(mapa_windows)){
  actual <- mapa_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p <- mean(mapa_average_var)

  mapa_kupiec_test_statistic <- -2 * (num_failures * log(p) + num_successes * log(1 - p))
  critical_value <- qchisq(confidence_level, df = 1)

  mapa_kupiec_p_value <- 1 - pchisq(mapa_kupiec_test_statistic, df = 1)

  print(paste("MAPA Kupiec test statistic:", mapa_kupiec_test_statistic))
  print(paste("MAPA p-value:", mapa_kupiec_p_value))
}

Monte_Carlo_icbp_backtesting <- for (i in 1:length(icbp_windows)){
  actual <- icbp_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p <- mean(icbp_average_var)

  icbp_kupiec_test_statistic <- -2 * (num_failures * log(p) + num_successes * log(1 - p))
  critical_value <- qchisq(confidence_level, df = 1)

  icbp_kupiec_p_value <- 1 - pchisq(icbp_kupiec_test_statistic, df = 1)

  print(paste("ICBP Kupiec test statistic:", icbp_kupiec_test_statistic))
  print(paste("ICBP p-value:", icbp_kupiec_p_value))
}

# Step 9: Stationarity test
mapa_returns_diff <- stationarity_test(mapa_returns)
icbp_returns_diff <- stationarity_test(icbp_returns)

# Step 10: Identify ARIMA model
mapa_arima_order <- identify_arima_model(mapa_returns_diff)
icbp_arima_order <- identify_arima_model(icbp_returns_diff)

# Step 11: Estimate ARIMA parameters
mapa_arima_parameters <- estimate_arima_parameters(mapa_returns_diff, mapa_arima_order$arma[1:3])
icbp_arima_parameters <- estimate_arima_parameters(icbp_returns_diff, icbp_arima_order$arma[1:3])

# Step 12: Test parameter significance
mapa_parameter_significance <- test_parameter_significance(mapa_windows,mapa_arima_order$arma[1:3])
icbp_parameter_significance <- test_parameter_significance(icbp_windows,icbp_arima_order$arma[1:3])

# Step 13: Check white noise assumption
mapa_residuals <- residuals(arima(mapa_returns_diff, order = mapa_arima_order$arma[1:3]))
icbp_residuals <- residuals(arima(icbp_returns_diff, order = icbp_arima_order$arma[1:3]))
mapa_white_noise_tests <- check_white_noise(mapa_residuals)
icbp_white_noise_tests <- check_white_noise(icbp_residuals)

# Step 14: Choose best ARIMA model
mapa_best_model <- choose_best_arima_model(mapa_windows,mapa_arima_order$arma[1:3])
icbp_best_model <- choose_best_arima_model(icbp_windows,icbp_arima_order$arma[1:3])

# Step 15: Determine GARCH model
mapa_garch_models <- determine_garch_model(mapa_windows)
icbp_garch_models <- determine_garch_model(icbp_windows)

# Step 16: Estimate ARIMA-GARCH parameters
mapa_arima_garch_models <- estimate_arima_garch_parameters(mapa_windows, mapa_arima_order$arma, mapa_garch_models)
icbp_arima_garch_models <- estimate_arima_garch_parameters(icbp_windows, icbp_arima_order$arma, icbp_garch_models)

# Step 17: Calculate Value at Risk for ARIMA-GARCH
mapa_var_values <- calculate_var_arima_garch(mapa_arima_garch_models, confidence_level, t_period)
icbp_var_values <- calculate_var_arima_garch(icbp_arima_garch_models, confidence_level, t_period)

# Step 18: Backtesting for ARIMA-GARCH
mapa_backtest_results_arima_garch <- for (i in 1:length(mapa_windows)){
  actual <- mapa_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p_arima_garch <- as.matrix(mapa_var_values[[i]])[1]

  mapa_kupiec_test_statistic_arima_garch <- -2 * (num_failures * log(p_arima_garch) + num_successes * log(1 - p_arima_garch))
  critical_value <- qchisq(confidence_level, df = 1)

  mapa_kupiec_p_value_arima_garch <- 1 - pchisq(mapa_kupiec_test_statistic_arima_garch, df = 1)

  print(paste("MAPA Kupiec test statistic of ARIMA-GARCH:", mapa_kupiec_test_statistic_arima_garch))
  print(paste("MAPA p-value of ARIMA-GARCH:", mapa_kupiec_p_value_arima_garch))
}

icbp_backtest_results_arima_garch <- for (i in 1:length(icbp_windows)){
  actual <- icbp_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p_arima_garch <- as.matrix(icbp_var_values[[i]])[1]

  icbp_kupiec_test_statistic_arima_garch <- -2 * (num_failures * log(p_arima_garch) + num_successes * log(1 - p_arima_garch))
  critical_value <- qchisq(confidence_level, df = 1)

  icbp_kupiec_p_value_arima_garch <- 1 - pchisq(icbp_kupiec_test_statistic_arima_garch, df = 1)

  print(paste("ICBP Kupiec test statistic of ARIMA-GARCH:", icbp_kupiec_test_statistic_arima_garch))
  print(paste("ICBP p-value of ARIMA-GARCH:", icbp_kupiec_p_value_arima_garch))
}

# Untuk confidence_level <- 0.99
confidence_level_2 <- 0.99

# Step 6: Maximum loss estimation
mapa_max_losses99 <- calculate_max_loss(mapa_simulations, confidence_level_2, t_period)
icbp_max_losses99 <- calculate_max_loss(icbp_simulations, confidence_level_2, t_period)

# Step 7: Calculate average Value at Risk
mapa_average_var99 <- calculate_average_var(mapa_max_losses99)
icbp_average_var99 <- calculate_average_var(icbp_max_losses99)

# Step 8: Backtesting
Monte_Carlo_mapa_backtesting99 <- for (i in 1:length(mapa_windows)){
  actual <- mapa_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p <- mean(mapa_average_var99)

  mapa_kupiec_test_statistic <- -2 * (num_failures * log(p) + num_successes * log(1 - p))
  critical_value <- qchisq(confidence_level_2, df = 1)

  mapa_kupiec_p_value <- 1 - pchisq(mapa_kupiec_test_statistic, df = 1)

  print(paste("MAPA Kupiec test statistic at CI 99%:", mapa_kupiec_test_statistic))
  print(paste("MAPA p-value at CI 99%:", mapa_kupiec_p_value))
}

Monte_Carlo_icbp_backtesting99 <- for (i in 1:length(icbp_windows)){
  actual <- icbp_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p <- mean(icbp_average_var99)

  icbp_kupiec_test_statistic <- -2 * (num_failures * log(p) + num_successes * log(1 - p))
  critical_value <- qchisq(confidence_level_2, df = 1)

  icbp_kupiec_p_value <- 1 - pchisq(icbp_kupiec_test_statistic, df = 1)

  print(paste("ICBP Kupiec test statistic at CI 99%:", icbp_kupiec_test_statistic))
  print(paste("ICBP p-value at CI 99%:", icbp_kupiec_p_value))
}

# Step 17: Calculate Value at Risk for ARIMA-GARCH
mapa_var_values99 <- calculate_var_arima_garch(mapa_arima_garch_models, confidence_level_2, t_period)
icbp_var_values99 <- calculate_var_arima_garch(icbp_arima_garch_models, confidence_level_2, t_period)

# Step 18: Backtesting for ARIMA-GARCH
mapa_backtest_results_arima_garch99 <- for (i in 1:length(mapa_windows)){
  actual <- mapa_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p_arima_garch <- as.matrix(mapa_var_values99[[i]])[1]

  mapa_kupiec_test_statistic_arima_garch <- -2 * (num_failures * log(p_arima_garch) + num_successes * log(1 - p_arima_garch))
  critical_value <- qchisq(confidence_level_2, df = 1)

  mapa_kupiec_p_value_arima_garch <- 1 - pchisq(mapa_kupiec_test_statistic_arima_garch, df = 1)

  print(paste("MAPA Kupiec test statistic of ARIMA-GARCH at CI 99%:", mapa_kupiec_test_statistic_arima_garch))
  print(paste("MAPA p-value of ARIMA-GARCH at CI 99%:", mapa_kupiec_p_value_arima_garch))
}

icbp_backtest_results_arima_garch99 <- for (i in 1:length(icbp_windows)){
  actual <- icbp_windows[[i]]
  num_obs <- length(actual)
  num_failures <- sum(actual)
  num_successes <- num_obs - num_failures
  p_arima_garch <- as.matrix(icbp_var_values99[[i]])[1]

  icbp_kupiec_test_statistic_arima_garch <- -2 * (num_failures * log(p_arima_garch) + num_successes * log(1 - p_arima_garch))
  critical_value <- qchisq(confidence_level_2, df = 1)

  icbp_kupiec_p_value_arima_garch <- 1 - pchisq(icbp_kupiec_test_statistic_arima_garch, df = 1)

  print(paste("ICBP Kupiec test statistic of ARIMA-GARCH at CI 99%:", icbp_kupiec_test_statistic_arima_garch))
  print(paste("ICBP p-value of ARIMA-GARCH at CI 99%:", icbp_kupiec_p_value_arima_garch))
}

# Display all results
# Stationarity test results
mapa_returns_diff
icbp_returns_diff

# ARIMA model
mapa_arima_order
icbp_arima_order

# ARIMA parameters
mapa_arima_parameters
icbp_arima_parameters

# White Noise Tests
mapa_white_noise_tests
icbp_white_noise_tests

# Best ARIMA model
mapa_best_model
icbp_best_model

# For CL 95%
mapa_max_losses
icbp_max_losses

mapa_average_var
icbp_average_var

mapa_var_values
icbp_var_values

# For CL 99%
mapa_max_losses99
icbp_max_losses99

mapa_average_var99
icbp_average_var99

mapa_backtest_results99
icbp_backtest_results99

mapa_var_values99
icbp_var_values99

mapa_backtest_results_arima_garch99
icbp_backtest_results_arima_garch99


