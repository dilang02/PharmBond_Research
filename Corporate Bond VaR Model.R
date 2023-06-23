rm(list=ls())
graphics.off()

data_full <- read.csv("C:/Users/dilan/OneDrive/Documents/FIR Thesis/Corporate Bond Data.csv")
products <- as.vector(unique(data_full$bond_sym_id))
show(paste("There are",length(unique(data_full$bond_sym_id)),"corporations in this study."))
data <- split(data_full,data_full$bond_sym_id)
var_list <- c()
conf <- as.numeric(readline("Input confidence level as a decimal: "))
if (is.na(conf) == TRUE){
  conf <- 0.95
}
horizon <- as.numeric(readline("Input time horizon in days: "))
if (is.na(horizon) == TRUE){
  horizon <- 1
  
}
duration_list <- c()
convexity_list <- c()
for (i in 1:length(products)){
  product <- products[i]
  show(paste("BOND:",product))
  prices <- data[[i]]$PRICE_EOM
  returns <- data[[i]]$RET_EOM
  vol <- sd(returns)
  z <- qnorm(1 - conf)
  VaR <- -prices[length(prices)]*vol*z*sqrt(horizon)
  var_list <- append(var_list,VaR)
  show(paste0("VaR at ", conf * 100, "% confidence level for a ", horizon, " day time horizon: $", round(VaR,2)))
  
  cf <- data[[i]]$PRINCIPAL_AMT * data[[i]]$COUPON / 100
  time <- seq(1,length(cf))
  ytm <- data[[i]]$YIELD
  for (j in 1:length(ytm)){
    yield <- ytm[j]
    price <- data[[i]]$PRICE_EOM[j]
    PV <- cf / (1 + yield) ^ time
    w_PV <- PV * time
    wA_PV <- sum(w_PV) / price
    duration <- wA_PV / (1 + yield)
    convexity <- sum((time - duration)^2 * PV) / (price * (1 + yield)^2)
    
    duration_list[j] <- duration
    convexity_list[j] <- convexity
  }
  assign(paste0(product,".dur"),duration_list)
  assign(paste0(product,".cvx"),convexity_list)
  #plot.ts(duration_list,main=paste("Duration:",product))
  #plot.ts(convexity_list,main=paste("Convexity:",product))
  duration_list <- c()
  convexity_list <- c()
}
