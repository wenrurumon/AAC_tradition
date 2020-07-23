
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
# raw <- read.xlsx('rj.xlsx')
map <- t(sapply(strsplit(colnames(raw)[-1:-2],'_'),function(x){
  c(x[1],x[2],paste(x[-1:-2],collapse='_'))
}))
colnames(map) <- c('Format','Platform','KOL')

###############

set.seed(123)

#setup
Y <- raw[,2]
Y.trend <- gettrend(Y,7) 
trend.importance <- 0.9  
Y.trend <- as.numeric(ifelse(Y<Y.trend*trend.importance,Y,Y.trend*trend.importance))
plot.ts(raw[,2]); lines(Y.trend,col=2)
sum(Y.trend)/sum(Y)
Y <- Y-Y.trend 

X <- raw[,-1:-2] 
Xi <- t(apply(X,1,function(x){
  tapply(x,paste(map[,1],map[,2]),sum)
}))
Xi <- apply(Xi,2,ret,r=0.4)
S <- ss(Y,freq=7)+0 

#Rolling Model
w <- 2
system.time(
  K <- sapply(1:ncol(X),function(k){
    mapk <- paste(map[,1],map[,2])[k]
    Xk <- Xi
    xk <- ret(X[,k],0.4)
    Xk[,which(colnames(Xi)==mapk)] <- (Xi[,which(colnames(Xi)==mapk)]+xk)/
      (sum(Xi[,which(colnames(Xi)==mapk)]+xk))*
      (sum(Xi[,which(colnames(Xi)==mapk)]))
    Xk[is.na(Xk)] <- 0
    sapply(1:1000,function(i){
      sel <- sample(1:length(Y),0.7*length(Y))
      coef(lm(Y[sel]~cbind(Xk[sel,],S[sel,])-1))[which(colnames(Xi)==mapk)]  
    })
  })
)


#Summary
K[K<=0] <- NA
rlt <- data.frame(coef=colMeans(K,na.rm=T),error_rate=colSums(is.na(K))/1000)

#Decomp
decomp <- sapply(1:nrow(rlt),function(i){X[,i]*rlt$coef[i]})
colnames(decomp) <- colnames(X)
res <- raw[,2]-rowSums(decomp)
decomp <- data.frame(base=gettrend(res),decomp)
decomp <- decomp/rowSums(decomp)*raw[,2]
plot.ts(raw[,2]); lines(rowSums(decomp),col=2)
decomp[,1] <- gettrend(decomp[,1])
coef2 <- coef(lm(raw[,2] ~ cbind(decomp[,1],rowSums(decomp[,-1]))-1))
decomp[,1] <- decomp[,1] * coef2[1]
decomp[,-1] <- decomp[,-1] * coef2[2]
plot.ts(raw[,2]); lines(rowSums(decomp),col=2)

rlt <- data.table(
  variable=colnames(decomp),
  rbind(NA,map),
  contribution=colSums(decomp),
  coef=colSums(decomp)/c(NA,colSums(X)),
  support=c(NA,colSums(X)))
  
write.csv(rlt,'PT.out')

