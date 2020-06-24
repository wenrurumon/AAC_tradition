
rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
setwd("/Users/wenrurumon/Desktop")

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}
RAW <- raw <- read.xlsx('Book1 (1).xlsx',sheet='Subbrand')
colnames(raw) <- tolower(colnames(raw))

##############

#Subbrand Sales
x <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)") %>% select(brand,subbrand,val=total)
x2 <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Volume ('000Kg)") %>% select(brand,subbrand,vol=total)
x3 <- data.table(x,vol=x2$vol) %>% mutate(avp=val/vol/10)
  # Upper Premium 8-14.99 RMB/100g
  # Mass Premium 5-7.99 RMB/100g
  # Super Premium >= 15 RMB/100g
  # Value <5 RMB/100g
x3 <- mutate(x3,premium=(avp>=5)+(avp>=8)+(avp>=15)) %>% select(brand,subbrand,premium,avp)
x <- merge(filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)"),x3,by=c('brand','subbrand'))
x2 <- merge(filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Volume ('000Kg)"),x3,by=c('brand','subbrand'))
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$premium,sum,na.rm=T)}))[-1:-10,]
x2 <- t(apply(x2[,-1:-5],2,function(y){tapply(y,x2$premium,sum,na.rm=T)}))[-1:-10,]
write.clip(x)
sel <- paste(filter(x3,premium==3)$brand,filter(x3,premium==3)$subbrand)

#Category Sales
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total')
t(apply(x[,-1:-5],2,function(y){tapply(y,x$fact,sum,na.rm=T)}))[-1:-10,] %>% write.clip

#Category Price
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total')
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$fact,sum,na.rm=T)}))[-1:-10,]
x <- x[,1,drop=F]/x[,2,drop=F]/10
colnames(x) <- 'RMB_00DU'
x %>% write.clip

#Category Top Brand
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)")
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$brand,sum,na.rm=T)}))[-1:-10,]
colnames(x) <- ifelse(colnames(x)%in%names(sort(colSums(-x)))[1:10],colnames(x),'O.Brand')
x <- t(apply(x,1,function(y){tapply(y,colnames(x),sum)}))
brandname <- names(sort(-colSums(x)))
x[,match(brandname,colnames(x))] %>% write.clip

#Category Top Brand - MT
x <- filter(raw,subbrand=='TOTAL'&channel=="Nat-H/S/Mini/CVS"&fact=="Sales Value(000RMB)")
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$brand,sum,na.rm=T)}))[-1:-10,]
colnames(x) <- ifelse(colnames(x)%in%names(sort(colSums(-x)))[1:10],colnames(x),'O.Brand')
x <- t(apply(x,1,function(y){tapply(y,colnames(x),sum)}))
brandname <- names(sort(-colSums(x)))
x[,match(brandname,colnames(x))] %>% write.clip

#Sensodyne Sales
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total'&brand=='Sensodyne')
t(apply(x[,-1:-5],2,function(y){tapply(y,x$fact,sum,na.rm=T)}))[-1:-10,] %>% write.clip

#Sensodyne Price
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total'&brand=='Sensodyne')
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$fact,sum,na.rm=T)}))[-1:-10,]
x <- x[,1,drop=F]/x[,2,drop=F]/10
colnames(x) <- 'RMB_00DU'
x %>% write.clip

#Sensodyne by Subbrand
x <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&brand=='Sensodyne'&fact=="Sales Value(000RMB)")
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$subbrand,sum,na.rm=T)}))[-1:-10,]
subname <- names(sort(-colSums(x)))
x <- x[,match(subname,colnames(x))]
temp <- x
x %>% write.clip

#Sensodyne by Subbrand Price
x <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&brand=='Sensodyne'&fact=="Sales Volume ('000Kg)")
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$subbrand,sum,na.rm=T)}))[-1:-10,]
x <- x[,match(subname,colnames(x))]
(temp/x/10) %>% write.clip

#Category by Channel
x <- filter(raw,subbrand=='TOTAL'&channel%in%unique(raw$channel)[3:5]&fact=='Sales Value(000RMB)')
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$channel,sum,na.rm=T)}))[-1:-10,]
x %>% write.clip

#Sensodyne by Channel
x <- filter(raw,subbrand=='TOTAL'&brand=='Sensodyne'&channel%in%unique(raw$channel)[3:5]&fact=='Sales Value(000RMB)')
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$channel,sum,na.rm=T)}))[-1:-10,]
x %>% write.clip

##########################

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
setwd("/Users/wenrurumon/Desktop")

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}
raw <- read.xlsx('Book1 (1).xlsx',sheet='Subbrand')
colnames(raw) <- tolower(colnames(raw))

##############

#Subbrand Sales
x <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)") %>% select(brand,subbrand,val=total)
x2 <- filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Volume ('000Kg)") %>% select(brand,subbrand,vol=total)
x3 <- data.table(x,vol=x2$vol) %>% mutate(avp=val/vol/10)
x3 <- mutate(x3,premium=(avp>=5)+(avp>=8)+(avp>=15)) %>% select(brand,subbrand,premium)
x <- merge(filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)"),x3,by=c('brand','subbrand'))
x2 <- merge(filter(raw,subbrand!='TOTAL'&channel=='Nat Total'&fact=="Sales Volume ('000Kg)"),x3,by=c('brand','subbrand'))
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$premium,sum,na.rm=T)}))[-1:-10,]
x2 <- t(apply(x2[,-1:-5],2,function(y){tapply(y,x2$premium,sum,na.rm=T)}))[-1:-10,]
sel <- paste(filter(x3,premium==3)$brand,filter(x3,premium==3)$subbrand)

#category sales
x <- filter(raw,channel=='Nat Total')
t(apply(x[,-1:-5],2,function(y){tapply(y,x$fact,sum,na.rm=T)}))[-1:-10,] %>% write.clip

#top brands
x <- filter(raw,channel=='Nat Total'&fact=="Sales Value(000RMB)")
x <- x %>% mutate(brand=ifelse(brand%in%brandname,brand,'O.Brand'))
x <- t(apply(x[,-1:-5],2,function(y){tapply(y,x$brand,sum,na.rm=T)}))[-1:-10,]
x[,match(brandname,colnames(x))] %>% write.clip


#Price Tier
x <- filter(raw,subbrand=='TOTAL'&channel=='Nat Total'&fact=="Sales Value(000RMB)")


##########################


rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
setwd("/Users/wenrurumon/Desktop")

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}
raw1 <- read.xlsx('SEM raw.xlsx',sheet='1819')
raw2 <- read.xlsx('SEM raw.xlsx',sheet='20')
raw <- rbind(raw1,raw2)

todate <- function(x){
  as.Date('2018-01-08')-43108+x
}
raw <- mutate(raw,day=todate(DATE))
map <- data.table(day=as.Date('2018-01-01')+0:(140*6-1),week=rep(1:120,each=7))
raw <- merge(raw,map,id='day')

x <- raw %>% group_by(week) %>% summarise(click=sum(CLICK),spending=sum(SPENDING))
write.clip(x)

x <- raw %>% group_by(week,CATEGORY) %>% summarise(click=sum(CLICK),spending=sum(SPENDING))
x <- reshape2::acast(x,week~CATEGORY,value.var='spending')
write.clip(x[,match(names(sort(-colSums(x,na.rm=T))),colnames(x))])

x <- raw %>% group_by(CATEGORY) %>% summarise(click=sum(CLICK)/1000000,spending=sum(SPENDING)/1000000)

##############

