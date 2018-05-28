rm(list=ls())
s <- function(egrp,C,D){1 - exp(-C * egrp^D)}
s2 <- function(n,C,D){s(1:n,C,D)}
plot.ts(diff(s2(500,0.0001,1.7)))

cs <- 1:1000/100000
ds <- 700:2200 / 1000
test <- lapply(cs,function(C){
  x <- sapply(ds,function(D){
    s2(600,C,D)
  })
  colnames(x) <- paste(C,ds)
  x
})
test <- do.call(cbind,test)

####
