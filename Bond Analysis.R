rm(list=ls())
graphics.off()

data_full <- read.csv("C:/Users/dilan/OneDrive/Documents/FIR Thesis/Corporate Bond Data.csv")
products <- as.vector(unique(data_full$bond_sym_id))
show(paste("There are",length(unique(data_full$bond_sym_id)),"corporations in this study."))
data <- split(data_full,data_full$bond_sym_id)
p_vals_yield <- c()
for (i in 1:length(products)){
product <- products[i]
bond <- data[[i]]
show(paste("BOND:",product))
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
show(paste("Credit Rating:",rating," || Grade:",credit))
show(paste0("Coupon Rate: ",coupon,"%"))
plot.ts(ytm,main=paste("YTM:",product))
plot.ts(price,main=paste("Bond Price:",product))
plot.ts(return,main=paste("Returns",product))

yield_curve <- lm(data[[i]]$YIELD~data[[i]]$TMT+I(data[[i]]$TMT^2))
p_vals_yield <- append(p_vals_yield,summary(yield_curve)$coef[2,4])
#show(summary(yield_curve))
plot(data[[i]]$TMT,fitted(yield_curve),main=paste("Yield Curve:",product),type="l")
points(data[[i]]$TMT,data[[i]]$YIELD,col="blue")
plot.ts(p_vals_yield,main="P-values by Product",xlab="Dates")
points(p_vals_yield,col="blue",pch=16)
text(p_vals_yield,products,cex=0.5,pos=3)

abline(h=0.5,col="red")
abline(h=0.75,col="orange")
}

data_dt <- split(data_full,data_full$DATE)
dates <- as.vector(unique(data_full$DATE))
p_vals <- c()
for (i in 1:length(dates)){
  yield <- data_dt[[i]]$YIELD
  nsp <- data_dt[[i]]$N_SP
  
  #show(dates[i])
  lm <- lm(yield~nsp)
  p_vals <- append(p_vals,summary(lm)$coef[2,4])
  #show(summary(lm))
}
plot.ts(p_vals,main="P-values over Time",xlab="Dates")
points(p_vals,col="blue",pch=16)
text(p_vals,dates,cex=0.5,pos=3)

abline(h=0.05,col="red")
abline(h=0.1,col="orange")


