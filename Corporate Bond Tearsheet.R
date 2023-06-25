rm(list=ls())
graphics.off()
library(knitr)
library(rmarkdown)
library(zoo)
library(urca)
library(pagedown) # Export markdown file to HTML & PDF
library(pdftools)
product_list <- sort(c("LLY", "JNJ", "ABBV", "VTRS", "BIIB", "TAK", "AZN", "PFE",
                  "MRK", "REGN", "GSK", "TEVA"))
data_full <- read.csv("C:/Users/dilan/OneDrive/Documents/FIR Thesis/Corporate Bond Data.csv")
products <- sort(as.vector(unique(data_full$bond_sym_id)))
data <- split(data_full,data_full$bond_sym_id)

conf <- 0.95
horizon <- 1
var_list <- c()
duration_list <- c()
convexity_list <- c()

for (i in 1:length(products)){
  product <- products[i]
  bond <- data[[i]]
  #show(paste("BOND:",product))
  ytm <- data[[i]]$YIELD
  ttm <- data[[i]]$TMT
  price <- data[[i]]$PRICE_EOM
  coupon <- round(data[[i]]$COUPON[1],2)
  return <- data[[i]]$RET_EOM
  rating <- data[[i]]$R_SP[1]
  if (rating == "AAA"){
    credit <- "Prime"
  } else if (substr(rating,1,2) == "AA"){
    credit <- "High grade"
  } else if (substr(rating,1,1) == "A"){
    credit <- "Upper Medium Grade"
  } else if (substr(rating,1,3) == "BBB"){
    credit <- "Lower Medium Grade"
  } else if (substr(rating,1,2) == "BB"){
    credit <- "Non-investment Grade (Speculative)"
  } else {
    credit <- "Junk Bond"
  }
  #show(paste("Credit Rating:",rating," || Grade:",credit))
  #show(paste0("Coupon Rate: ",coupon,"%"))
  #plot.ts(ytm,main=paste("YTM:",product))
  #plot.ts(price,main=paste("Bond Price:",product))
  #plot.ts(return,main=paste("Returns",product))
  
  yield_curve <- lm(data[[i]]$YIELD~data[[i]]$TMT+I(data[[i]]$TMT^2))
  #plot(data[[i]]$TMT,fitted(yield_curve),main=paste("Yield Curve:",product),type="l")
  #points(data[[i]]$TMT,data[[i]]$YIELD,col="blue")
  
  prices <- data[[i]]$PRICE_EOM
  returns <- data[[i]]$RET_EOM
  vol <- sd(returns)
  z <- qnorm(1 - conf)
  VaR <- -prices[length(prices)]*vol*z*sqrt(horizon)
  var_list <- append(var_list,VaR)
  #show(paste0("VaR at ", conf * 100, "% confidence level for a ", horizon, " day time horizon: $", round(VaR,2)))
  
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
  #plot.ts(duration_list,main=paste("Duration:",product))
  #plot.ts(convexity_list,main=paste("Convexity:",product))
  
  ret <- data[[i]]$RET_EOM
  ret <- ts(c(ret,ret),frequency = 12)
  #plot(decompose(ret))
  #abline(v=8/3,col="red")
  ret_o <- data[[i]]$RET_EOM
  
  MA <- rollmean(ret_o,k=5,fill=NA,align="right") # Create moving average and ARIMA forecasts
  #plot.ts(ret_o,main="Moving Average Forecast")
  #lines(MA,col="red")
  #plot(ur.df(ret_o,type='drift',selectlags='AIC'))



  tearsheet <- paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/RMD & HTML Files/",product,".Tearsheet.rmd")
  content <- "
# Bond Tearsheet: `r product`
  
### Product Information:
  
  **Credit Rating:** `r rating` || **Grade:** `r credit`
  
  **Coupon Rate:** `r paste0(coupon,'%')`
  
  **VaR at 95% confidence level for a 1-day time horizon:** `r paste0('$',round(VaR,2))`
  
  ```{r,fig.width=6,fig.height=4,echo=FALSE}
  price <- data[[i]]$PRICE_EOM
  plot.ts(ytm,main=paste('YTM:',product))
  plot.ts(price,main=paste('Bond Price:',product))
  plot.ts(return,main=paste('Returns',product))
  plot(data[[i]]$TMT,fitted(yield_curve),main=paste('Yield Curve:',product),type='l')
  points(data[[i]]$TMT,data[[i]]$YIELD,col='blue')
  plot.ts(duration_list,main=paste('Duration:',product))
  plot.ts(convexity_list,main=paste('Convexity:',product))
  ```  
  
### Forecasts of Returns:
```{r,fig.width=6,fig.height=4,echo=FALSE}
plot(decompose(ret))
abline(v=8/3,col='red')
plot.ts(ret_o,main='Moving Average Forecast')
lines(MA,col='red')
plot(ur.df(ret,type='drift',selectlags='AIC'))
```

Please view the corresponding tearsheet to learn more about this company.

  "

  writeLines(content,tearsheet)
  chrome_print(input = tearsheet, output = paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/PDF Files/",product,".Tearsheet.pdf"))
  equity <- paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/PDF Files/",product_list[i],".Tearsheet.pdf")
  bond <- paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/PDF Files/",product,".Tearsheet.pdf")
  output_pdf <- paste0("C:/Users/dilan/OneDrive/Documents/FIR Thesis/PDF Files/",product_list[i],".COMPLETE.Tearsheet.pdf")
  pdf_combine(c(equity,bond),output_pdf)

  duration_list <- c()
  convexity_list <- c()
}