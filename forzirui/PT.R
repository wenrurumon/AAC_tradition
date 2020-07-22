
rm(list=ls())
setwd('/Users/wenrurumon/Documents/zirui')
library(openxlsx)
library(data.table)
library(dplyr)

#Retention
ret <- function(x,r=0.1){
  out <- x
  for(i in 2:length(x)){out[i] <- out[i-1]*r+out[i]}
  out
}
#Seasonality
ss <- function(x,freq=7){
  x <- rep(1:freq,length=length(x))
  outer(x,unique(x),'==')
}
#ployfit 
ployfit <- function(x,p=10){
  y <- lm(x~do.call(cbind,lapply(1:p,function(p){
    (1:length(x))^p
  }))) %>% predict
  plot.ts(x); lines(y,col=2)
  return(y)
}
#gettrend
gettrend <- function(Y,freq=7){
  Y.decomp <- stats::decompose(ts(Y,freq=freq))
  Y.trend <- Y.decomp$trend; 
  Y.trend[1:(floor(freq/2))] <- Y.trend[!is.na(Y.trend)][1]; 
  Y.trend[length(Y.trend)+0:-(floor(freq/2)-1)] <- Y.trend[length(Y.trend)-floor(freq/2)]
  as.numeric(Y.trend)
}

#import data
raw <- read.xlsx('pt.xlsx')
map <- t(sapply(strsplit(colnames(raw)[-1:-2],'_'),function(x){
  c(x[1],x[2],paste(x[-1:-2],collapse='_'))
}))
colnames(map) <- c('Format','Platform','KOL')

###############

set.seed(123)

#full model
Y <- raw[,2]
Y.trend <- gettrend(Y,7)
Y.trend <- as.numeric(ifelse(Y<Y.trend*0.9,Y,Y.trend*0.9))
plot.ts(raw[,2]); lines(Y.trend,col=2)
sum(Y.trend)/sum(Y)
Y <- Y-Y.trend

X <- raw[,-1:-2]
Xi <- t(apply(X,1,function(x){
  tapply(x,paste(map[,1],map[,2]),sum)
}))
Xi <- apply(Xi,2,ret,r=0.4)
S <- ss(Y,freq=7)+0
coef1 <- t(sapply(1:2000,function(i){
  sel <- sample(1:length(Y),0.8*length(Y))
  Y <- Y[sel]
  Xi <- Xi[sel,]
  S <- S[sel,]
  out <- coef(lm(Y~cbind(Xi,S)-1))[1:ncol(Xi)]
  names(out) <- colnames(Xi)
  out
}))
coef1 <- apply(coef1,2,function(x){
  x[is.na(x)] <- mean(x,na.rm=T)
  x
})

#Rolling Model
K <- sapply(1:ncol(X),function(k){
  mapk <- paste(map[,1],map[,2])[k]
  Xk <- Xi
  xk <- ret(X[,k],0.4)
  Xk[,which(colnames(Xi)==mapk)] <- (Xi[,which(colnames(Xi)==mapk)]+xk)/
    (sum(Xi[,which(colnames(Xi)==mapk)]+xk))*
    (sum(Xi[,which(colnames(Xi)==mapk)]))
  Xk[is.na(Xk)] <- 0
  coef(lm(Y~cbind(Xk,S)-1))
})

#Summary
coef2 <- sapply(1:ncol(X),function(k){
  mapk <- paste(map[,1],map[,2])[k]
  coefk <- K[which(colnames(Xi)==mapk),k]
  coefk <- sort(coef1[,which(colnames(Xi)==mapk)])[sum(coef1[,which(colnames(Xi)==mapk)]>coefk)]
  if(coefk<0){coefk <- min(abs(coef1[,which(colnames(Xi)==mapk)]))}
  coefk
})

#Decomp
decomp <- sapply(1:length(coef2),function(i){X[,i]*coef2[i]})
colnames(decomp) <- colnames(X)
res <- raw[,2]-rowSums(decomp)
decomp <- data.frame(raw[,1:2],base=gettrend(res),decomp)
plot.ts(decomp[,2]); lines(rowSums(decomp[,-1:-2]),col=2)
rlt <- data.table(rbind('Base',map),coef=c(NA,coef2),contribution=colSums(decomp)[-1:-2]/sum(raw[,2]))
rlt %>% group_by(Format,Platform) %>% summarise(contribution=sum(contribution))

write.csv(rlt,'PT.csv')

