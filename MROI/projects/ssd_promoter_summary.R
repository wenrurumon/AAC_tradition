rm(list=ls())
setwd('e:\\huzixin')
library(data.table)
library(dplyr)
f <- dir(pattern='.csv')[1:17]
x <- fread(f[1])
cn <- colnames(x)

x <- lapply(f,function(x){
  print(x)
  x <- fread(x) 
  colnames(x) <- cn
  # x
  lp <- unique(filter(x,!is.na(PROMO_LONP)&PROMO_LONP>0) %>% select(MONTH,shop))
  mp <- unique(filter(x,!is.na(PROMO_MEDP)&PROMO_MEDP>0) %>% select(MONTH,shop))
  sp <- unique(filter(x,PROMO_TEMP+PROMO_WEKP+PROMO_SHAP>0&!is.na(PROMO_TEMP)|!is.na(PROMO_WEKP)|!is.na(PROMO_SHAP)) %>% select(MONTH,shop))
  list(lp,mp,sp)
})

lp <- unique(do.call(rbind,lapply(x,function(x){x[[1]]})))
mp <- unique(do.call(rbind,lapply(x,function(x){x[[2]]})))
sp <- unique(do.call(rbind,lapply(x,function(x){x[[3]]})))

x <- lapply(f,function(x){
  print(x)
  x <- fread(x)
  colnames(x) <- cn
  x %>% group_by(shop,year=floor(MONTH)) %>% summarise(val=sum(act_sales_value))
})
x <- do.call(rbind,x) 

k1 <- paste(x$shop,x$year)
klp <- paste(lp$shop,lp$MONTH)
kmp <- paste(mp$shop,mp$MONTH)
ksp <- paste(sp$shop,sp$MONTH)

x <- cbind(x,lp=k1%in%klp+0,mp=k1%in%kmp+0,sp=k1%in%ksp+0)

x %>% group_by(lp,year) %>% summarise(max(val),min(val),mean(val),n=n())
x %>% group_by(sp,year) %>% summarise(max(val),min(val),mean(val),n=n())
x %>% group_by(mp,year) %>% summarise(max(val),min(val),mean(val),n=n())

shop <- x %>% group_by(shop) %>% summarise(sval=mean(val))

summary(filter(shop,shop%in%filter(x,year==2017&lp==1)$shop)$sval)
summary(filter(shop,shop%in%filter(x,year==2018&lp==1)$shop)$sval)

summary(filter(shop,shop%in%filter(x,year==2017&mp==1)$shop)$sval)
summary(filter(shop,shop%in%filter(x,year==2018&mp==1)$shop)$sval)

summary(filter(shop,shop%in%filter(x,year==2017&sp==1)$shop)$sval)
summary(filter(shop,shop%in%filter(x,year==2018&sp==1)$shop)$sval)
