
rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)
max2 <- function(x){sort(x)[length(x)-1]}

raw <- fread('item_709_20170152_20200112.csv') %>% 
select(week=V1,store=V4,ITEMCODE=V5,value=V6,unit=V7,cell=V8)
raw <- raw[value>0&unit>0]

item <- read.xlsx('9.xlsx') %>% select(
	ITEMCODE,brand=BRAND_5,multi=MULTIPACK,quantity=QUANTITY,tier=COLGPRICE_339_M_PRICETIER,subbrand=SUBBRAND_300_SUBBRAND)
item2 <- read.xlsx("reported item4grouping_withItemName.xlsx") %>% select(ITEMCODE,ppg=Item_name)
item <- sqldf('select a.*, b.ppg from item a left join item2 b on a.ITEMCODE=b.ITEMCODE')

#Brand Recode
item$map <- NA
item$map[!is.na(item$ppg)] <- sapply(strsplit(item$ppg[!is.na(item$ppg)],'_'),function(x){x[1]})
map <- filter(item,!is.na(ppg)) %>% select(brand,map) %>% unique
item$map <- map$map[match(item$brand,map$brand)]
item <- item %>% mutate(brand=ifelse(is.na(map),brand,map))

#Subbrand Recode
item$map <- NA
item$map[!is.na(item$ppg)] <- sapply(strsplit(item$ppg[!is.na(item$ppg)],'_'),function(x){paste(x[1],x[2],sep='_')})
item <- mutate(item,subbrand=paste(brand,subbrand,sep='_'))
map <- filter(item,!is.na(ppg)) %>% select(subbrand,map) %>% unique
item$map <- map$map[match(item$subbrand,map$subbrand)]
item <- item %>% mutate(subbrand=ifelse(is.na(map),subbrand,map))

item <- mutate(item,map=ifelse(is.na(map),subbrand,map),
	sel=ITEMCODE%in%item2$ITEMCODE)
item <- mutate(item,ppg2=paste(map,quantity,sep='_'),packsize=multi*quantity)

#filter(raw,ppg=='SENSODYNE_COMPLETE_100'&store=='7600622805')

#Merging item and sales

item2 <- select(item,ITEMCODE,brand,multi,packsize,tier,subbrand,ppg=ppg2,sel)
raw <- merge(raw,item2,by='ITEMCODE')
raw <- raw[tier%in%c('PREM','UPPER')]
raw <- raw[brand%in%raw[sel==T]$brand]

sales <- mutate(raw,vol=unit*packsize) %>% group_by(ppg,sel,week,store) %>% summarise(value=sum(value),vol=sum(vol)) %>% mutate(avp=value/vol*100) %>% arrange(ppg,store,week)
sales$map <- paste(sales$ppg,sales$store)
map <- unique(sales$map)
write.csv(sales,'sales.csv',row.names=F)

#Calculate Regular Price for sensodyne

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)
max2 <- function(x){
  if(length(x)>=7){
    sort(x)[length(x)-1]
  } else {
    max(x)
  }
}
rp <- function(x){
  if(length(x)<7){return(max(x))}
  apply(sapply(0:-6,function(j){
    sapply(1:length(x),function(i){max2(x[max(i+j,1):min(i+j+7,length(x))])})  
  }),1,max2)
}
raw <- fread('sales.csv')
raw[grepl('SENSODYNE',ppg)] %>% group_by(ppg) %>% summarise(val=sum(value)) %>% arrange(desc(val))
sales <- raw[grepl('SENSODYNE',ppg)]
map <- unique(sales$map)

j <- 0
system.time(test <- lapply(map,function(p){
	j<<-j+1
	if(j%%1000==1){
		print(paste(j/length(map),Sys.time()))
	}
	salesi <- sales[map==p]
	salesi <- arrange(salesi,desc(week))
	salesi$rp <- rp(salesi$avp)
	salesi
}))

for(i in 1:length(test)){
	write.table(select(test[[i]],-map),'sensodyne_sales3.csv',row.names=F,col.names=F,append=T,sep=',')
}



#Calculate Regular Price for Other SEL

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)
max2 <- function(x){
  if(length(x)>=7){
    sort(x)[length(x)-1]
  } else {
    max(x)
  }
}
rp <- function(x){
  if(length(x)<7){return(max(x))}
  apply(sapply(0:-6,function(j){
    sapply(1:length(x),function(i){max2(x[max(i+j,1):min(i+j+7,length(x))])})  
  }),1,max2)
}
raw <- fread('sales.csv')
raw[grepl('SENSODYNE',ppg)] %>% group_by(ppg) %>% summarise(val=sum(value)) %>% arrange(desc(val))
sales <- raw[!grepl('SENSODYNE',ppg)&sel==T]
map <- unique(sales$map)

j <- 0
system.time(test <- lapply(map,function(p){
	j<<-j+1
	if(j%%1000==1){
		print(paste(j/length(map),Sys.time()))
	}
	salesi <- sales[map==p]
	salesi <- arrange(salesi,desc(week))
	salesi$rp <- rp(salesi$avp)
	salesi
}))

for(i in 1:length(test)){
	write.table(select(test[[i]],-map),'other_sales3.csv',row.names=F,col.names=F,append=T,sep=',')
}

#Calculate Regular Price for non SEL

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)
max2 <- function(x){
  if(length(x)>=7){
    sort(x)[length(x)-1]
  } else {
    max(x)
  }
}
rp <- function(x){
  if(length(x)<7){return(max(x))}
  apply(sapply(0:-6,function(j){
    sapply(1:length(x),function(i){max2(x[max(i+j,1):min(i+j+7,length(x))])})  
  }),1,min)
}
raw <- fread('sales.csv')
raw[grepl('SENSODYNE',ppg)] %>% group_by(ppg) %>% summarise(val=sum(value)) %>% arrange(desc(val))
sales <- raw[!grepl('SENSODYNE',ppg)&sel==F]
map <- unique(sales$map)

j <- 0
system.time(test <- lapply(map,function(p){
	j<<-j+1
	if(j%%1000==1){
		print(paste(j/length(map),Sys.time()))
	}
	salesi <- sales[map==p]
	salesi <- arrange(salesi,desc(week))
	salesi$rp <- rp(salesi$avp)
	salesi
}))

for(i in 1:length(test)){
	write.table(test[[i]],'othernon_sales2.csv',row.names=F,col.names=F,append=T,sep=',')
}




