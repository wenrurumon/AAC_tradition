
rm(list=ls())
library(data.table)
library(dplyr)

s1 <- function(x,C,D,plot=F){
  s <- 1 - exp(-C * x^D)
  xmar <- diff(s)
  xavg <- s/x
  if(plot){plot.ts(xmar); lines(xavg,col=2)}
  list(curve=s,b=which(xmar==max(xmar)),c=which(xavg==max(xavg)))
}

s <- function(x,C,D){
  # C <- 0.0001; D <- 1.3; x <- 1:1000
  s <- s1(x,C,D)
  lift <- (s$curve[s$c]/s$c)/(s$curve[s$b]/s$b)
  list(curve=s$curve,coef=c(C=C,D=D,lift=lift,range=s$c/s$b))
}

Cs <- 0.00001 * (1:40)
Ds <- 1.3+0.01*(1:40)
curvemap <- do.call(rbind,lapply(Cs,function(C){
  t(sapply(Ds,function(D){
    s(1:1000,C,D)[[2]]
  }))
}))
curvemap <- as.data.frame(curvemap)

#blift <- 3.4/3.1; brange <- 98/59; h <- 1
fit <- function(blift,brange,h=1){
  f <- head(mutate(curvemap,dlift=(1-lift/blift)^2,drange=(1-range/brange)^2,d=dlift+drange) %>% arrange(d),20)
  f <- head(mutate(f,m=ifelse(dlift>drange,dlift,drange)) %>% arrange(m),h)[1:4]
  list(sel=f,fomula=parse(text=paste0('1-exp(',-f$C,'*(1:500)^',f$D,')')))
}

sumfit <- function(f,B,C,ret=0.75,maxroi=NA){
  # f <- fit(3.4/3.1,98/59); B <- 59; C <- 98; ret <- 0.75
  B <- B/(1-ret)
  C <- C/(1-ret)
  cv <- eval(f[[2]])
  cmar <- diff(cv)
  croi <- cv/(1:length(cv))
  prf <- (which(cmar==max(cmar))+which(croi==max(croi)))/sum(B+C)
  f <- parse(text=paste0('1-exp(',-f[[1]]$C,'*(x*',prf,')^',f[[1]]$D,')'))
  x <- 1:500
  cv <- eval(f)
  plot.ts(diff(cv),col=2); lines(cv/1:length(cv))
  f <- list(fomula=f,margin=diff(cv),roi=cv/(1:length(cv)))
  f$maxmargin <- which(f$margin==max(f$margin))/(1/(1-ret))
  f$maxroi <- which(f$roi==max(f$roi))/(1/(1-ret))
  if(!is.na(maxroi)){
    pf2 <- maxroi / f$roi[f$maxroi * (1/(1-ret))]
    f$fomula <- parse(text=paste(pf2,'*(',f$fomula,')'))
    cv <- eval(f$fomula)
    plot.ts(diff(cv),col=2); lines(cv/1:length(cv))
    f <- list(fomula=f$fomula,margin=diff(cv),roi=cv/(1:length(cv)))
    f$maxmargin <- which(f$margin==max(f$margin))/(1/(1-ret))
    f$maxroi <- which(f$roi==max(f$roi))/(1/(1-ret))
  }
  f
}

###################################
#
###################################

f <- fit(3.4/3.1,98/59)
f <- sumfit(f,59,98,0.75,3.4)

