
rm(list=ls())
library(data.table)
library(dplyr)

s_full <- function(b1,b2,b3,b4,b5,b6){
  x <- (1:1000)/400
  (b1 + b2 * exp(-1*(b3*x+b4)^b5)) * b6
}

s <- function(b){
  x <- (1:1000)/400
  cv <- 1-exp(-1*x^b)
  mar <- diff(cv); roi <- cv/(1:length(cv))
  plot.ts(mar,col=2); lines(roi)
  lift <- roi[which(roi==max(roi))]/roi[which(mar==max(mar))]
  range <- which(roi==max(roi))/which(mar==max(mar))
  list(cv=cv,coef=c(b=b,lift=lift,range=range))
}

curvemap <- as.data.frame(
  t(sapply((500:2500)/1000,function(i){
    s(i)$coef
  }))
)

blift <- 3.4/3.1; brange <- 98/59
sel <- head(arrange(mutate(curvemap,l1=(1-lift/blift)^2,l2=(1-range/brange)^2),l1+l2),20)
mutate(sel,l3=ifelse(l1<l2,l2,l1)) %>% arrange(l3)
