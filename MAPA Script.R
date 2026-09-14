# Load data return saham dan lakukan pendugaan distribusi
library(quantreg)
library(tseries)
library(fGarch)
library(forecast)
library(rugarch)
library(readxl)
library(fitdistrplus)
library(MASS)
library(GLDEX)
#Import data return hasil prediksi BPNN
data.ret.prediksi <- read_excel("MAPA.xlsx",sheet="MAPA",col_names=T)
ret.prediksi <- data.ret.prediksi$Return
#ret.prediksi <- var_estimates2
#ret.prediksi <- as.numeric(as.vector(ret.prediksi))
#Pendugaan sesuai distribusi umum
par(mfrow=c(1,1))
descdist(ret.prediksi)
#Pendugaan Distribusi Menggunakan Generalize Lambda
#Estimasi parameter
dist.pre <- fun.data.fit.mm(ret.prediksi)
dist.pre
#Menggunakan 1000000 sampel acak untuk menentukan metode distribusi
# generalized lambda
# GLD RS
set.seed(99)
rs_sample.pre <- rgl(n = 1000000, lambda1=dist.pre[1,1], lambda2 =
                       dist.pre[2,1],lambda3 = dist.pre[3,1],lambda4 = dist.pre[4,1],param
                     = "rs")
# GLD FMKL
set.seed(99)
fmkl_sample.pre <- rgl(n = 1000000, lambda1=dist.pre[1,2],
                       lambda2=dist.pre[2,2], lambda3 = dist.pre[3,2],lambda4 =
                         dist.pre[4,2],param = "fmkl")
fun.moments.r(rs_sample.pre, normalise="Y")
fun.moments.r(fmkl_sample.pre, normalise="Y")
fun.moments.r(ret.prediksi, normalise="Y")
#Mendapatkan tingkat akurasi
accuracy(rs_sample.pre,ret.prediksi)
accuracy(fmkl_sample.pre,ret.prediksi)
#Visualisasi pendugaan distribusi Generalized Lambda
fun.plot.fit(fit.obj = dist.pre, data = ret.prediksi, nclass =
               100,param = c("rs", "fmkl"), xlab ="Returns")
#Pemilihan parameter distribusi yang akan digunakan adalah dengan
#meninjau nilai akurasi dengan tingkat error yang paling kecil
#GLD RS yang dipilih

mapa_returns <- ret.prediksi
returns <- mapa_returns
# Parameters
window_size <- 250  # Ukuran sliding window
quantile <- 0.05  # Nilai kuantil yang digunakan
num_simulations <- 7000  # Jumlah simulasi Monte Carlo
confidence_level <- 0.95

plot.ts(returns, type = "l", main = "MAPA Stock Returns")

adf.test(returns)

acf(returns, lag.max=12)
axis(1, at=1:12, labels=1:12)

pacf(returns, lag.max=12)
axis(1, at=1:12, labels=1:12)

library(TSA)
eacf(returns)
# Calon-calon model yang didapat (0,0,0),(2,0,2) dan (3,0,3),
# namun setelah diuji coba, hanya model (0,0,0) yang berhasil
# korvergen saat proses running. Sementara model lainnya mengalami
# divergen di window tertentu.
Arima_model <- c(0,0,0)

kupiec_test=function(VaR,alpha){
  n1=sum(ifelse(VaR<=alpha,1,0));n0=length(VaR)-n1
  p1=n1/(n0+n1);p0=1-p1
  Kupiec=2*log(((p0^n0)*(p1^n1))/((((1-alpha)^n0))*(alpha^n1)))
  return(1-pchisq(Kupiec,1))
}

# Initialize vectors to store VaR estimates
var_estimates <- list()
var_actuals <- list()

set.seed(1234)

# Perform backtesting with sliding window
for (i in window_size:length(returns)) {
  # Get data for current window
  window_data <- returns[(i - window_size + 1):i]
  train_data <- window_data[1:(window_size - 1)]
  test_data <- window_data[window_size]

  # Fit quantile regression model on training data
  model_quantile <- rq(formula = train_data ~ 1, tau = quantile)

  # Perform Monte Carlo simulation
  simulated_returns <- numeric(num_simulations)
  for (j in 1:num_simulations) {
    # Generate random residuals based on quantile regression model
    residuals <- residuals(model_quantile) + rnorm(length(train_data))

    # Generate simulated returns
    simulated_returns[j] <- predict(model_quantile, newdata = data.frame(train_data = residuals))
  }

  # Estimate VaR as quantile of simulated returns
  var_estimate <- quantile(simulated_returns, quantile)

  # Store VaR estimate and actual return
  var_estimates[i - window_size + 1] <- var_estimate
  var_actuals[i - window_size + 1] <- test_data
}

# Perform backtesting evaluation (e.g., Kupiec test, Christoffersen test, etc.)
Monte_carlo_Kupiec_test_p_value95 <- kupiec_test(var_estimates,0.95)
Monte_carlo_Kupiec_test_p_value95
Monte_carlo_Kupiec_test_p_value99 <- kupiec_test(var_estimates,0.99)
Monte_carlo_Kupiec_test_p_value99

# Print results
for (i in 1:length(var_estimates)) {
  print(paste("Window : ", i, "Estimated VaR =", var_estimates[i], ", Actual Return =", var_actuals[i]))
}

var_estimates2 <- list()
var_actuals2 <- list()

for (i in window_size:length(returns)) {
  # Get data for current window
  window_data <- returns[(i - window_size + 1):i]
  train_data <- window_data[1:(window_size - 1)]
  test_data <- window_data[window_size]
  variance <- var(train_data)
  mean_return <- mean(train_data)

  # Fit ARIMA model on training data
  model_arima <- arima(train_data,order = Arima_model)

  # Get residuals from ARIMA model
  residuals <- residuals(model_arima)

  # Check seasonal
  checkresiduals(model_arima)

  ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
  ks_test <- ks.test(residuals, "pnorm")

  # Fit GARCH model on residuals
  model_garch <- ugarchspec(mean.model = list(armaOrder = Arima_model),
                            variance.model = list(garchOrder = c(1,0)))
  model_garch_fit <- ugarchfit(spec = model_garch, data = residuals, solver = "hybrid")

  # Calculate GARCH volatility on testing data
  volatility <- as.numeric(sigma(model_garch_fit))

  # Fit quantile regression model on training data
  model_quantile <- rq(formula = train_data ~ volatility, tau = quantile)

  # Predict quantile value on testing data
  var_estimate <- predict(model_quantile, newdata = data.frame(volatility = volatility))

  # Store VaR estimates and actual returns
  var_estimates2[i - window_size + 1] <- var_estimate
  var_actuals2[i - window_size + 1] <- test_data

}

# Perform backtesting evaluation
ARIMA_Garch_Kupiec_test_p_value95 <- kupiec_test(var_estimates2,0.95)
ARIMA_Garch_Kupiec_test_p_value95
ARIMA_Garch_Kupiec_test_p_value99 <- kupiec_test(var_estimates2,0.99)
ARIMA_Garch_Kupiec_test_p_value99

# Print results
for (i in 1:length(var_estimates2)) {
  print(paste("Window : ", i, "Estimated VaR =", var_estimates2[i], ", Actual Return =", var_actuals2[i]))
}
