rm(list=ls())
library(data.table)
library(dplyr)

s <- function(b){
  # b <- 1.67
  x <- (1:3000)/1000
  cv <- 1-exp(-1*(x)^b)
  mar <- diff(cv); roi <- cv/(1:length(cv))
  # plot.ts(mar,col=2); lines(roi)
  lift <- roi[which(roi==max(roi))]/roi[which(mar==max(mar))]
  range <- which(roi==max(roi))/which(mar==max(mar))
  list(cv=cv,coef=c(b=b,lift=lift,range=range))
}
curvemap <- as.data.frame(
  t(sapply((500:5000)/1000,function(i){
    s(i)$coef
  }))
)
rawcurve <- function(blift,brange){
  # blift <- 3.4/3.1; brange <- 98/59 #1.096774, #1.661017
  sel <- head(arrange(mutate(curvemap,l1=(1-lift/blift)^2,l2=(1-range/brange)^2),l1+l2),20)
  sel <- (mutate(sel,l3=ifelse(l1<l2,l2,l1)) %>% arrange(l3))[1,1:3]
  f <- function(x){1-exp(-1*(x)^sel$b)}
  list(fomula=f,sel=sel)
}
getcurve <- function(f,C,Cs,ret){
  # f <- rawcurve(34/31,98/59)
  # C <- 98; Cs <- 0.34; ret <- 0.75
  
  qx <- (1:3000)/1000
  b <- f$sel$b
  cv <- (f$fomula(qx))
  margin <- diff(cv)
  roi <- cv/(1:length(qx))
  plot.ts(margin,col=2); lines(roi)
  
  prf2 <- qx[which(roi==max(roi))]/(C/(1-ret))
  f <- function(x){(1-exp(-1*(x*prf2)^b))}
  cv <- f(1:1000)
  roi <- cv/((1:1000)) #x = (1:1000)*(1-ret)
  
  prf <- Cs/roi[which(roi==max(roi))]
  f <- function(x){prf * (1-exp(-1*(x*prf2)^b))}
  margin <- diff(f(1:500))
  roi <- f(1:500)/(1:500)
  plot.ts(margin,col=2); lines(roi)
  prf <- prf * (1-ret)
  fomula <- parse(text=paste0(prf,"*(1-exp(-1*(x*",prf2,")^",b,'))'))
  list(coef=c(prf=prf,prf2=prf2,b=b,ret=ret),fomula=fomula,fun=f)
}
scurve <- function(orange,rrange,ret){
  o1 <- orange[1]
  o2 <- orange[2]
  r1 <- rrange[1]
  r2 <- rrange[2]
  f <- rawcurve(r2/r1,o2/o1)
  getcurve(f,o2,r2,ret)
}

#Voltaren TV
scurve(c(59,98),c(0.31,0.34),0.75)
#Voltaren OTV
f <- scurve(c(1,8),c(0.68,1.1),0.75)
#Voltaren Search
f <- scurve(c(25,40),c(0.22,0.25),0.75)

#Voltaren Wechat
f <- scurve(c())
which(f$fun(1:1000)/(1:1000)==max(f$fun(1:1000)/(1:1000)))
which(diff(f$fun(1:1000))==max(diff(f$fun(1:1000))))
