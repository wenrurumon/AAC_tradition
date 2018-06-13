
rm(list=ls())
s1 <- function(x,C,D){
  s <- 1 - exp(-C * x^D)
  xmar <- diff(s)
  xavg <- s/x
  plot.ts(xmar); lines(xavg,col=2)
  list(curve=s,b=which(xmar==max(xmar)),c=which(xavg==max(xavg)))
}

s <- function(x,C,D){
  # C <- 0.0001; D <- 1.3; x <- 1:1000
  s <- s1(x,C,D)
  lift <- (s$curve[s$c]/s$c)/(s$curve[s$b]/s$b)
  list(curve=s$curve,coef=c(C=C,D=D,lift=lift,range=s$c/s$b))
}

x <- s(1:1000,0.0001,1.6)

Cs <- 0.00001 * (1:40)
Ds <- 1.3+0.01*(1:40)
curvemap <- do.call(rbind,lapply(Cs,function(C){
  t(sapply(Ds,function(D){
    s(1:1000,C,D)[[2]]
  }))
}))

