
rm(list=ls())
library(data.table)
library(dplyr)
library(forecast)
library(ggplot2)
library(mgcv)

setwd('/Users/wenrurumon/Documents/gsk/gsk2022/forecasting/hybrid/mar2')
write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  print(x)
  write.table(x, file=clip, sep = ',',row.names=F)   
  close(clip)
}

raw <- openxlsx::read.xlsx('Full working file_0701 V1.xlsx',sheet=4)
X <- raw[-1:-5,-1]
colnames(X) <- raw[5,-1]
X <- apply(X,2,as.numeric)
month <- c('2018-12-1',paste(rep(2019:2024,each=12),rep(1:12,5),1,sep='-'))

model <- function(y){
  x <- 1:73
  b0 <- coef(lm(y~x))
  fit <- predict(gam(y~s(x)),data.frame(x=1:73))
  plot(y); lines(fit,col=2)
  fit
}
X2 <- apply(X,2,model)
X[is.na(X)] <- X2[is.na(X)]
write.clip(X)
