# 1. Library yang digunakan
#library
library(readxl)
library(tseries)
library(forecast)
library(lmtest)
library(pastecs)
library(moments)
library(tidyverse)
library(TSPred)
library(aTSA)
library(ggplot2)
library(rugarch)

# 2. Input Data Return 
#Input data
MAP=read_excel("MAPA.xlsx",sheet="MAPA",col_names=T)
ICBP=read_excel("ICBP.xlsx",sheet="ICBP",col_names=T)

#data frame
datee=as.Date(MAP$Date)
datee=as.data.frame.factor(datee)
RTMAPA=MAP$Return
RTICBP=ICBP$Return

gabungan=data.frame(cbind(datee,RTMAPA,RTICBP))
view(gabungan)


# 3. Analisis Data Statistika Deskriptif
#(Statistika Deskriptif)
summary(gabungan)
stat.desc(gabungan)
skewness(gabungan)
kurtosis(gabungan)

# 4. Plot Return
#Visualisasi data return
#Plot Return MAPA
plot(RTMAPA, main = "Plot Return Saham MAPA", ylab = "Return", col = "blue")
ggplot(data=gabungan, aes(x = datee, y = RTMAPA)) +
  geom_line(color = "blue") +
  labs(title = "Plot Return Saham MAPA", x = "Date", y = "Return")
#Plot Return ICBP
plot(RTICBP, main = "Plot Return Saham ICBP", ylab = "Return", col = "purple")
ggplot(data=gabungan, aes(x = datee, y = RTMAPA)) +
  geom_line(color = "purple") +
  labs(title = "Plot Return Saham ICBP", x = "Date", y = "Return")
##plot QQ normal
par(mfrow=c(1,2))
qqnorm(RTMAPA,main = "MAPA Return Norm Plot")
qqline(RTMAPA)
qqnorm(RTICBP,main = "ICBP Return Norm Plot")
qqline(RTICBP)

# 5. Uji Kolmogorov Smirnov
#uji kolmogorov smirnov
ks.test(RTMAPA,"pnorm")
ks.test(RTICBP,"pnorm")

# 6. Uji Statsioneritas
#Uji Stasioneritas Mean
#GAgal Tolak H0 apabila p-value<0,05 (data stasioner)
adf.test(RTMAPA)
adf.test(RTICBP)

# 7. Plot ACF PACF Return
#Plot ACF PACF
par(mfrow=c(2,1))
acf((RTMAPA),main="MAPA")
pacf((RTMAPA),main="MAPA")
acf((RTICBP),main="ICBP")
pacf((RTICBP),main="ICBP")

# Cek Kemiungkinan Model Menggunakan auto.arima
#Model ARIMA MAPA
modelMAPA <- auto.arima(RTMAPA,stepwise = F,approximation = F)
modelMAPA
#Model ARIMA ICBP
modelICBP <- auto.arima(RTICBP,stepwise = F,approximation = F)
modelICBP


# 9. Diagnostic Checking
#White Noise MAPA
WNMAPA<-arima(RTMAPA,order = c(0,0,0),method = "ML" )
resiRTMAPA<-WNMAPA$residuals
ks.test(resiRTMAPA,"pnorm")
Box.test(resiRTMAPA,type = "Ljung-Box")
#White Noise ICBP
WNICBP<-arima(RTICBP,order = c(0,0,0),method = "ML" )
resiRTICBP<-WNICBP$residuals
ks.test(resiRTICBP,"pnorm")
Box.test(resiRTICBP,type = "Ljung-Box")

# 10. Uji Efek ARCH/GARCH
#Uji Efek ARCH ICBP dan MAP
arch.test(WNMAPA) #mod
arch.test(WNICBP)

# 11. Plot ACF dan PACF GARCH
#PLOT GARCH
resmapa=(resiRTMAPA)^2
resmapa
resicbp=(resiRTICBP)^2
resicbp
win.graph()
par(mfrow=c(2,1))
acf(resmapa, main="MAPA.JK")
pacf(resmapa, main="MAPA.JK")
win.graph()
par(mfrow=c(2,1))
acf(resicbp, main="ICBP.JK")
pacf(resicbp, main="ICBP.JK")

# Define window size for sliding window
window_intervals <- c(250, 375, 500)

# Initialize variables for the best model
best_garch_model_fit_mapa1 <- NULL
best_garch_model_fit_mapa2 <- NULL
best_garch_model_fit_mapa3 <- NULL
best_garch_model_fit_icbp1 <- NULL
best_garch_model_fit_icbp2 <- NULL
best_garch_model_fit_icbp3 <- NULL
best_VaR_mapa1 <- NULL
best_VaR_mapa2 <- NULL
best_VaR_mapa3 <- NULL
best_VaR_icbp1 <- NULL
best_VaR_icbp2 <- NULL
best_VaR_icbp3 <- NULL
best_backtest_mapa1 <- Inf
best_backtest_mapa2 <- Inf
best_backtest_mapa3 <- Inf
best_backtest_icbp1 <- Inf
best_backtest_icbp2 <- Inf
best_backtest_icbp3 <- Inf

# 12. Sliding window and backtesting
# Steps 3-7: ARMAX model estimation and selection
for (window in window_intervals) {
  # Select a subset of data for the current window
  subset_data <- gabungan[(nrow(gabungan) - window + 1):nrow(gabungan), ]
  
  # Extract return series from the window data
  window_return_mapa <- subset_data$RTMAPA
  window_return_icbp <- subset_data$RTICBP
  
  # Model GARCH MAPA.JK
  spec.mapa1=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(1,1)),distribution.model="std")
  garch.fit.mapa1=ugarchfit(spec=spec.mapa1,data=window_return_mapa,solver="solnp")
  
  spec.mapa2=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(1,2)),distribution.model="std")
  garch.fit.mapa2=ugarchfit(spec=spec.mapa2,data=window_return_mapa,solver="solnp")
  
  spec.mapa3=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(2,1)),distribution.model="std")
  garch.fit.mapa3=ugarchfit(spec=spec.mapa3,data=window_return_mapa,solver="solnp")
  
  # 13. Model GARCH ICBP
  spec.icbp1=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(1,1)),distribution.model="std")
  garch.fit.icbp1=ugarchfit(spec=spec.icbp1,data=window_return_icbp,solver="solnp")
  
  spec.icbp2=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(1,2)),distribution.model="std")
  garch.fit.icbp2=ugarchfit(spec=spec.icbp2,data=window_return_icbp,solver="solnp")
  
  spec.icbp3=ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(garchOrder=c(2,1)),distribution.model="std")
  garch.fit.icbp3=ugarchfit(spec=spec.icbp3,window_return_icbp,solver="solnp")
  
  # Step 12: VaR calculation
  VaR_forecast_mapa1 <- ugarchforecast(garch.fit.mapa1, n.ahead = 1, n.roll = 0, data = resiRTMAPA)
  VaR_forecast_mapa2 <- ugarchforecast(garch.fit.mapa2, n.ahead = 1, n.roll = 0, data = resiRTMAPA)
  VaR_forecast_mapa3 <- ugarchforecast(garch.fit.mapa3, n.ahead = 1, n.roll = 0, data = resiRTMAPA)
  VaR_forecast_icbp1 <- ugarchforecast(garch.fit.icbp1, n.ahead = 1, n.roll = 0, data = resiRTICBP)
  VaR_forecast_icbp2 <- ugarchforecast(garch.fit.icbp2, n.ahead = 1, n.roll = 0, data = resiRTICBP)
  VaR_forecast_icbp3 <- ugarchforecast(garch.fit.icbp3, n.ahead = 1, n.roll = 0, data = resiRTICBP)
  VaR_mapa1 <- as.numeric(sigma(VaR_forecast_mapa1)) * qnorm(0.05)
  VaR_mapa2 <- as.numeric(sigma(VaR_forecast_mapa2)) * qnorm(0.05)
  VaR_mapa3 <- as.numeric(sigma(VaR_forecast_mapa3)) * qnorm(0.05)
  VaR_icbp1 <- as.numeric(sigma(VaR_forecast_icbp1)) * qnorm(0.05)
  VaR_icbp2 <- as.numeric(sigma(VaR_forecast_icbp2)) * qnorm(0.05)
  VaR_icbp3 <- as.numeric(sigma(VaR_forecast_icbp3)) * qnorm(0.05)
  
  # Step 14: VaR model accuracy with backtesting
  backtest_mapa1 <- sum(resiRTMAPA < -VaR_mapa1) / length(resiRTMAPA)
  backtest_mapa2 <- sum(resiRTMAPA < -VaR_mapa2) / length(resiRTMAPA)
  backtest_mapa3 <- sum(resiRTMAPA < -VaR_mapa3) / length(resiRTMAPA)
  backtest_icbp1 <- sum(resiRTICBP < -VaR_icbp1) / length(resiRTICBP)
  backtest_icbp2 <- sum(resiRTICBP < -VaR_icbp2) / length(resiRTICBP)
  backtest_icbp3 <- sum(resiRTICBP < -VaR_icbp3) / length(resiRTICBP)
  
  # Print the results for the best model only
  if (backtest_mapa1 < best_backtest_mapa1) {
    best_garch_model_fit_mapa1 <- garch.fit.mapa1
    best_VaR_mapa1 <- VaR_mapa1
    best_backtest_mapa1 <- backtest_mapa1
    
    # Print the test results for the best model
    print("Best Model For MAPA (1,1):")
    print(best_garch_model_fit_mapa1)
    print("Value at Risk (VaR) For MAPA (1,1):")
    print(best_VaR_mapa1)
    print("Backtest Result For MAPA (1,1):")
    print(best_backtest_mapa1)
  }
  # Print the results for the best model only
  if (backtest_mapa2 < best_backtest_mapa2) {
    best_garch_model_fit_mapa2 <- garch.fit.mapa2
    best_VaR_mapa2 <- VaR_mapa2
    best_backtest_mapa2 <- backtest_mapa2
    
    # Print the test results for the best model
    print("Best Model For MAPA (1,2):")
    print(best_garch_model_fit_mapa2)
    print("Value at Risk (VaR) For MAPA (1,2):")
    print(best_VaR_mapa2)
    print("Backtest Result For MAPA (1,2):")
    print(best_backtest_mapa2)
  }
  # Print the results for the best model only
  if (backtest_mapa3 < best_backtest_mapa3) {
    best_garch_model_fit_mapa3 <- garch.fit.mapa3
    best_VaR_mapa3 <- VaR_mapa3
    best_backtest_mapa3 <- backtest_mapa3
    
    # Print the test results for the best model
    print("Best Model For MAPA (2,1):")
    print(best_garch_model_fit_mapa3)
    print("Value at Risk (VaR) For MAPA (2,1):")
    print(best_VaR_mapa3)
    print("Backtest Result For MAPA (2,1):")
    print(best_backtest_mapa3)
  }
  # Print the results for the best model only
  if (backtest_icbp1 < best_backtest_icbp1) {
    best_garch_model_fit_icbp1 <- garch.fit.icbp1
    best_VaR_icbp1 <- VaR_icbp1
    best_backtest_icbp1 <- backtest_icbp1
    
    # Print the test results for the best model
    print("Best Model For ICBP (1,1):")
    print(best_garch_model_fit_icbp1)
    print("Value at Risk (VaR) For ICBP (1,1):")
    print(best_VaR_icbp1)
    print("Backtest Result For ICBP (1,1):")
    print(best_backtest_icbp1)
  }
  # Print the results for the best model only
  if (backtest_icbp2 < best_backtest_icbp2) {
    best_garch_model_fit_icbp2 <- garch.fit.icbp2
    best_VaR_icbp2 <- VaR_icbp2
    best_backtest_icbp2 <- backtest_icbp2
    
    # Print the test results for the best model
    print("Best Model For ICBP (1,2):")
    print(best_garch_model_fit_icbp2)
    print("Value at Risk (VaR) For ICBP (1,2):")
    print(best_VaR_icbp2)
    print("Backtest Result For ICBP (1,2):")
    print(best_backtest_icbp2)
  }
  # Print the results for the best model only
  if (backtest_icbp3 < best_backtest_icbp3) {
    best_garch_model_fit_icbp3 <- garch.fit.icbp3
    best_VaR_icbp3 <- VaR_icbp3
    best_backtest_icbp3 <- backtest_icbp3
    
    # Print the test results for the best model
    print("Best Model For ICBP (2,1):")
    print(best_garch_model_fit_icbp3)
    print("Value at Risk (VaR) For ICBP (2,1):")
    print(best_VaR_icbp3)
    print("Backtest Result For ICBP (2,1):")
    print(best_backtest_icbp3)
  }
}

# See The Results
best_garch_model_fit_mapa1  
best_garch_model_fit_mapa2  
best_garch_model_fit_mapa3  
best_garch_model_fit_icbp1  
best_garch_model_fit_icbp2  
best_garch_model_fit_icbp3  
best_VaR_mapa1  
best_VaR_mapa2  
best_VaR_mapa3  
best_VaR_icbp1  
best_VaR_icbp2  
best_VaR_icbp3  
best_backtest_mapa1  
best_backtest_mapa2  
best_backtest_mapa3  
best_backtest_icbp1  
best_backtest_icbp2  
best_backtest_icbp3  

