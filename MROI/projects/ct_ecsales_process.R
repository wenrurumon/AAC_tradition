
rm(list=ls())
setwd('/Users/wenrurumon/Documents/GSK/CT2020/data review')
library(dplyr)
library(openxlsx)
library(ggplot2)
library(sqldf)
raw <- read.xlsx("Centrum EC Raw Data_Mar.20.xlsx",sheet='Sheet1')
raw <- filter(raw,Month>=201800)
tomonth <- function(x){as.Date(paste(substr(x,1,4),substr(x,5,6),1,sep='-'))}
item <- read.xlsx("Centrum EC Raw Data_Mar.20.xlsx",sheet='Sheet2')

storesales <- raw %>% group_by(Channel,SubChannel,Shopname) %>% 
  summarise(value=sum(Centrum.Sales)) %>% arrange(desc(value))
schannelsales <- storesales %>% group_by(SubChannel) %>% summarise(cvalue=sum(value))
storesales <- merge(storesales,schannelsales,by='SubChannel') %>% mutate(prop=round(value/cvalue,2))
storesales <- filter(storesales,prop>0.1&Shopname!='others') %>% arrange(SubChannel) %>% mutate(
  key=paste(Channel,SubChannel,Shopname,sep='_')
)

x <- raw %>% mutate(key=paste(Channel,SubChannel,Shopname,sep='_')) 
x <- x %>% 
  filter(key%in%storesales$key&SubCategory=="复合维生素"&Brandname=='善存/Centrum') %>% 
  group_by(Month,Skuname,Channel,SubChannel,Shopname,key) %>%
  summarise(value=sum(`Value(RMB)`),pack=sum(Volume_Pack))
# x <- sqldf('select a.*, b.packsize from x a left join item b on a.Skuname=b.Skuname')
# x %>% group_by(key,is.na(packsize)) %>% summarise(sum(value)/1000)
x <- merge(x,item,by='Skuname')
x <- x %>% group_by(Month,Channel,SubChannel,Shopname,key) %>% summarise(value=sum(value),volume=sum(pack*packsize)) %>% mutate(price=value/volume)

ggplot() + 
  geom_line(data=x,aes(x=tomonth(Month),y=value,colour=key),size=0.5)
ggplot() + 
  geom_line(data=x,aes(x=tomonth(Month),y=volume,colour=key),size=0.5)
ggplot() + 
  geom_line(data=x,aes(x=tomonth(Month),y=price,colour=key),size=0.5)

write.csv(x,'centrum_ecsales.csv')
write.csv(raw %>% group_by(Channel,Month) %>% summarise(value=sum(`Value(RMB)`)),'cat_ecsales.csv')
