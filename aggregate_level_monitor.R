
rm(list=ls())
library(openxlsx)
library(dplyr)
library(forecast)

setwd('/Users/wenrurumon/Documents/GSK/GSK2019/forecasting/model')
ssddata <- read.xlsx('Consumption Forecasting.xlsx',sheet='Sensodyne Model Data')
fbddata <- read.xlsx('Consumption Forecasting.xlsx',sheet='Fenbid Model Data')

##########################
# Module
##########################

qarima <- function(x,freq=12,h=18,fforce=function(x){x*(x>0)},ifplot=F){
  x.ts <- ts(x,frequency=freq)
  x.f <- forecast(x.arima <- auto.arima(x.ts),h=h)
  # plot(x.f)
  x.f <- fforce(x.f$mean)
  if(ifplot){plot.ts(c(x,x.f))}
  list(
    mean = x.f,
    fitted = c(x,x.f)
  )
}
ret <- function(x,r=0.3){
  out <- x
  for(i in 2:length(x)){
    out[i] <- out[i-1]*r+x[i]
  }
  out * sum(x)/sum(out)
}
minmax <- function(x,mi=NULL,ma=NULL){
  if(is.null(mi)){mi <- min(x)}
  if(is.null(ma)){ma <- max(x)}
  (x-mi)/(ma-mi)
}
scalein <- function(x,mi=0.98,ma=1.02){
  x2 <- x-1
  x2i <- x2[x2<0]/min(x2)*(max(mi-1,min(x2)))+1
  x2a <- x2[x2>0]/max(x2)*(min(ma-1,max(x2)))+1
  out <- rep(NA,length(x2))
  out[x2<0] <- x2i
  out[x2>0] <- x2a
  out
}

##########################
# Fenbid
##########################

#Data Processing
colnames(fbddata) <- tolower(colnames(fbddata))
fbddata <- mutate(fbddata,
                  category.avp=category_value.mrmb/category_volume.00du
                  )

fbddata$total.fenbid_brand.reminder.spending <- 
  qarima(fbddata$total.fenbid_brand.reminder.spending[1:24],h=10,ifplot=F)$fitted
fbddata$total.fenbid_posm.spending <- 
  qarima(fbddata$total.fenbid_posm.spending[1:24],h=10,ifplot=F)$fitted

y <- fbddata$fenbid_value.mrmb * 2.2961061031 * 1000
x.base <- fbddata[,c(5:9),drop=F][,2:4]
x.tmedia <- fbddata[,c(10),drop=F]
x.dmedia <- fbddata[,c(11:14),drop=F]
x.tmk <- fbddata[,c(15:16),drop=F]
x.comp <- fbddata[,c(17:51),drop=F]
x.comp <- x.comp[,grep('mrmb',colnames(x.comp)),drop=F]
x <- data.frame(y=y
                ,x.base
                ,tmedia=ret(rowSums(x.tmedia),0.35)
                ,dmedia=ret(rowSums(x.dmedia),0.25)
                ,tmk=ret(rowSums(x.tmk),0.1)
                #,x.comp
                )

#First Model
hold <- x$tmedia + x$dmedia + x$tmk
y2 <- y-hold
model <- lm(y2~x$fenbid_distribution.wd+x$fenbid_price.avp+x$category_value.mrmb)

#Second Model
X <- x; X[,1] <- 1; colnames(X)[1] <- 'intercept'
B <- as.numeric(c(coef(model),rep(1,3)))
Y.raw <- as.matrix(X) %*% (cbind(B))
obj <- function(b){
  sum((as.matrix(X) %*% (cbind(b))-y)^2)
}
obj.raw <- obj(B)
B.optim1 <- optim(B,obj,control=list(maxit=5000))

#Third Model
obj <- function(b,l1,l2){
  loss1 <- ((sum((as.matrix(X) %*% (cbind(b))-y)^2)/B.optim1$value-1))^2
  loss2 <- sum(((abs(b/B-1))[2:4])^2) * l1
  loss3 <- sum(((abs(b/B-1))[-1:-4])^2) * l2
  loss1+loss2+loss3
}
B.optim2 <- optim(B,obj,l1=0.1,l2=1,control=list(maxit=5000))
cbind(B,B.optim1$par,B.optim2$par)

#Model Summary
plot.ts(y)
lines(as.matrix(X) %*% cbind(B),col=2)
lines(as.matrix(X) %*% cbind(B.optim1$par),col=3)
lines(as.matrix(X) %*% cbind(B.optim2$par),col=4)
decomp <- as.matrix(X)
for(i in 1:ncol(decomp)){
  decomp[,i] <- decomp[,i]*B.optim2$par[i]
}
decomp <- cbind(value=y,decomp)
plot.ts(y); lines(rowSums(decomp[,-1]),col=2)

#Monthly Adjustment
B <- matrix(1,nrow=nrow(decomp),ncol=ncol(decomp)-1)
obj <- function(b,l1,l2){
  loss1 <- sum((decomp[,1] - rowSums(b * decomp[,-1]))^2)/
    sum((decomp[,1] - rowSums(decomp[,-1]))^2)
  loss2 <- var(as.vector(b))
  loss1*l1+loss2*l2
}
B.optim3 <- optim(B,obj,l1=0.1,l2=1,control=list(maxit=20000))
B.optim3$par <- apply(B.optim3$par,2,scalein,mi=0.98,ma=1.02)
B.optim3$par[,2:4] <- apply(B.optim3$par[,2:4],2,scalein,mi=0.99,ma=1.01)
decomp2 <- decomp
decomp2[,-1] <- B.optim3$par * decomp[,-1]
decomp2[,2] <- mean(decomp2[,2])

#Output
plot.ts(y); 
lines(rowSums(decomp[,-1]),col=2); 
decomp2[,2] <- decomp2[,2] + 
  rep(tapply(decomp2[,1]-rowSums(decomp2[,-1]),rep(1:12,length=34),mean),length=34)
lines(rowSums(decomp2[,-1]),col=4)
decomp2 <- cbind(decomp2[,-2:-5],
                 base=rowSums(decomp2[,2:5]),
                 fit=rowSums(decomp2[,-1]))
rownames(decomp2) <- fbddata$code
write.csv(decomp2,pipe('pbcopy'))

##########################
# Sensodyne
##########################

colnames(ssddata) <- tolower(colnames(ssddata))
ssddata <- mutate(ssddata,
                  category.avp=category_sales.value.mrmb/category_sales.volume.mkg
)

y <- ssddata$舒适达_sales.value.mrmb * 1.6477053673 * 1000
x.base <- ssddata[,4:7,drop=F]
x.tmedia <- ssddata[,8,drop=F]
x.dmedia <- ssddata[,9:12,drop=F]
x.tmk <- ssddata[,13:15,drop=F]
x.comp <- ssddata[,16:45,drop=F]
x.comp <- x.comp[,grep('mrmb',colnames(x.comp)),drop=F]
x <- data.frame(y=y
                ,x.base[,-4]
                ,tmedia=ret(rowSums(x.tmedia),0.35)
                ,dmedia=ret(rowSums(x.dmedia),0.25)
                ,tmk=ret(rowSums(x.tmk),0.1)
                #,x.comp
)

#First Model
hold <- x$tmedia + x$dmedia + x$tmk
y2 <- y-hold
model <- lm(y2~x$舒适达_distribution.wd+x$舒适达_price.avp+x$category_sales.value.mrmb)
plot.ts(y); lines(predict(model)+hold,col=2)

#Second Model
X <- x; X[,1] <- 1; colnames(X)[1] <- 'intercept'
B <- as.numeric(c(coef(model),rep(1,3)))
Y.raw <- as.matrix(X) %*% (cbind(B))
obj <- function(b){
  sum((as.matrix(X) %*% (cbind(b))-y)^2)
}
obj.raw <- obj(B)
B.optim1 <- optim(B,obj,control=list(maxit=5000))

#Third Model
obj <- function(b,l1,l2){
  loss1 <- ((sum((as.matrix(X) %*% (cbind(b))-y)^2)/B.optim1$value-1))^2
  loss2 <- sum(((abs(b/B-1))[2:4])^2) * l1
  loss3 <- sum(((abs(b/B-1))[-1:-4])^2) * l2
  loss1+loss2+loss3
}
B.optim2 <- optim(B,obj,l1=0.1,l2=1,control=list(maxit=5000))
cbind(B,B.optim1$par,B.optim2$par)

#Model Summary
plot.ts(y)
lines(as.matrix(X) %*% cbind(B),col=2)
lines(as.matrix(X) %*% cbind(B.optim1$par),col=3)
lines(as.matrix(X) %*% cbind(B.optim2$par),col=4)
decomp <- as.matrix(X)
for(i in 1:ncol(decomp)){
  decomp[,i] <- decomp[,i]*B.optim2$par[i]
}
decomp <- cbind(value=y,decomp)
plot.ts(y); lines(rowSums(decomp[,-1]),col=2)

#Monthly Adjustment
B <- matrix(1,nrow=nrow(decomp),ncol=ncol(decomp)-1)
obj <- function(b,l1,l2){
  loss1 <- sum((decomp[,1] - rowSums(b * decomp[,-1]))^2)/
    sum((decomp[,1] - rowSums(decomp[,-1]))^2)
  loss2 <- var(as.vector(b))
  loss1*l1+loss2*l2
}
B.optim3 <- optim(B,obj,l1=0.1,l2=1,control=list(maxit=20000))
B.optim3$par <- apply(B.optim3$par,2,scalein,mi=0.9,ma=1.1)
decomp2 <- decomp
B.optim3$par[,2:4] <- apply(B.optim3$par[,2:4],2,scalein,mi=0.98,ma=1.02)
decomp2[,-1] <- B.optim3$par * decomp[,-1]
decomp2[,2] <- mean(decomp2[,2])

#Output
plot.ts(y); 
lines(rowSums(decomp[,-1]),col=2); 
decomp2[,2] <- decomp2[,2] + 
  rep(tapply(decomp2[,1]-rowSums(decomp2[,-1]),rep(1:12,length=34),mean),length=34)
lines(rowSums(decomp2[,-1]),col=4)
decomp2 <- cbind(decomp2[,-2:-5],
                 base=rowSums(decomp2[,2:5]),
                 fit=rowSums(decomp2[,-1]))
rownames(decomp2) <- fbddata$code
write.csv(decomp2,pipe('pbcopy'))
