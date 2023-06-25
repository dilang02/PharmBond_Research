rm(list=ls()) # Decomposition, MA, and ADF Forecasts of Returns
graphics.off()
library(zoo)
library(forecast)
data_full <- read.csv("C:/Users/dilan/OneDrive/Documents/FIR Thesis/Corporate Bond Data.csv")
products <- as.vector(unique(data_full$bond_sym_id))
data <- split(data_full,data_full$bond_sym_id)
for (i in 1:length(products)){
  product <- products[i]
  print(product)
  ret <- data[[i]]$RET_EOM
  ret <- ts(c(ret,ret),frequency = 12)
  plot(decompose(ret))
  abline(v=8/3,col="red")
  ret <- data[[i]]$RET_EOM
  
  MA <- rollmean(ret,k=5,fill=NA,align="right") # Create moving average and ARIMA forecasts
  plot.ts(ret,main="Moving Average Forecast")
  lines(MA,col="red")
  plot(ur.df(ret,type='drift',selectlags='AIC'))

}