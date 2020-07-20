
rm(list=ls())
library(openxlsx)
library(data.table)
library(dplyr)

x <- read.xlsx('data4coding.xlsx',sheet=3)
x2 <- x %>% 
filter(Channel!='C2C'&Month>=201806&Skuname!='Others') %>% 
select(
	Brand=Brandname,Subbrand=ProductLine,
	ProductLine=Channel,SKU=Skuname,
	TA=Target.Audience,value=`Value(RMB)`
	) %>%
mutate(ProductLine=gsub('\\(.+?\\)','',SKU)) %>%
group_by(Brand,Subbrand,ProductLine,SKU,TA) %>%
summarise(value=sum(value))
x2 <- x2 %>% group_by(Brand,Subbrand,ProductLine,TA) %>% 
summarise(SKU=paste(SKU,collapse=', '),value=sum(value),n=n())

x <- read.xlsx('data4coding.xlsx',sheet=4)
colnames(x)[19] <- 'value'
x3 <- x %>% 
filter(Year*100+Month>=201806) %>% 
group_by(Brand=Brand.CN,Subbrand=gene.CN,ProductLine=gene.CN
	,TA=Consumer.Type,SKU=SKU.Name) %>% summarise(value=sum(as.numeric(value)))
x3 <- x3 %>% group_by(Brand,Subbrand,ProductLine,TA) %>%
summarise(SKU=paste(SKU,collapse=', '),value=sum(value),n=n())

x2 <- data.table(Channel='EC',x2)
x3 <- data.table(Channel='OTC',x3
x <- rbind(x2,x3) %>% select(Channel,Brand,Subbrand,ProductLine,TA)
