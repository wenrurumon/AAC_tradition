
rm(list=ls())
setwd("/Users/wenrurumon/Downloads")
library(data.table)
library(dplyr)
sales <- fread('storetracking.csv') %>% filter(val>0)
w <- sort(unique(sales$week))
w2 <- rep(1:13,each=8)[-1]
w3 <- c(rep(1,3),rep(2:14,each=8))[1:103]
sales$m1 <- w2[match(sales$week,w)]
sales$m2 <- w3[match(sales$week,w)]

ssd.sales <- sales %>% group_by(brand,store,week) %>% summarise(
  ival=sum(val),ivol=sum(vol)
)
tt.sales <- ssd.sales %>% group_by(store,week) %>% summarise(
  cval=sum(ival),cvol=sum(ivol)
)
ssd.sales <- filter(ssd.sales,brand=='SENSODYNE')[,-1]
sales <- filter(sales,brand=='SENSODYNE')
sales <- merge(sales,ssd.sales,by=c('store','week'))
sales <- merge(sales,tt.sales,by=c('store','week'))
sales <- select(sales,-brand) %>% mutate(
  avp = val/vol,
  cval = cval-ival, cvol = cvol-ivol, cvp = cval/cvol,
  ival = ival-val, ivol = ivol-vol, ivp = ival/ivol,
) %>% arrange(ppg,store,week)
ssales <- sales %>% group_by(store,ppg) %>% summarise(svol=mean(vol))
sales <- merge(sales,ssales,by=c('store','ppg')) %>% mutate(vol=vol/svol)
ppg <- (sales %>% group_by(ppg) %>% 
          summarise(val=sum(val),vol=sum(vol),avp=val/vol*100) %>% 
          arrange(desc(val)))
rp <- sales %>% group_by(ppg,store,m1,m2) %>% summarise(rp=max(avp))
rp1 <- rp %>% group_by(ppg,store,m=m1) %>% summarise(rp=max(rp))
rp2 <- rp %>% group_by(ppg,store,m=m2) %>% summarise(rp=max(rp))
rp <- rbind(rp1,rp2) %>% group_by(ppg,store,m) %>% summarise(rp=max(rp))

k1 <- paste(sales$store,sales$ppg,sales$m1)
k2 <- paste(sales$store,sales$ppg,sales$m2)
kr <- paste(rp$store,rp$ppg,rp$m)
sales$rp <- apply(cbind(rp$rp[match(k1,kr)],rp$rp[match(k2,kr)]),1,max)
sales <- sales %>% mutate(pp = ifelse(avp/rp>=0.95,1,avp/rp))

test <- t(sapply(ppg$ppg,function(p){
  x <- filter(sales,ppg==p) %>% select(
    vol,rp,pp,avp,ivp,cvp,pack
  ) %>% mutate(
    vol=log(vol),pp=log(pp),rp=log(rp),avp=log(avp),ivp=log(ivp),cvp=log(cvp),pack=log(pack)
  )
  x[x==-Inf] <- NaN
  x <- x[rowSums(is.na(x))==0,]
  coef(lm(vol~rp+pp+ivp+cvp+pack+1,data=x))
}))
head(test)
