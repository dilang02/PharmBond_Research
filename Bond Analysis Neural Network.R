library(tidyverse)
library(neuralnet)
options(mySeed=7410)
nn_data <- read.csv("C:/Users/dilan/OneDrive/Documents/FIR Thesis/Corporate Bond Data.csv")

model = neuralnet( # Create broad categorization network
  formula = R_SP~YIELD+COUPON+PRICE_EOM+RET_EOM,
  data=nn_data,
  hidden=c(4,4),
  linear.output = FALSE,
  stepmax = 1e+06,
  threshold = 0.01,
  rep = 1
)
plot(model,rep = "best")

pred <- predict(model, nn_data) # Check accuracy of model
confusion <- table(as.numeric(factor(nn_data$R_SP)),max.col(pred))
row.names(confusion) <- levels(factor(nn_data$R_SP))
colnames(confusion) <- levels(factor(nn_data$R_SP))
names(dimnames(confusion)) <- c("observed","predicted")
check = as.numeric(factor(nn_data$R_SP)) == max.col(pred)
accuracy = (sum(check)/nrow(nn_data))*100
show(paste0(round(accuracy,2),"% accurate"))
show(confusion)