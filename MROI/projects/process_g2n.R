
rm(list=ls())
setwd('/Users/wenrurumon/Downloads')
library(data.table)
library(dplyr)

#################################

x <- fread('Promoter_weeksplit.csv')
colnames(x) <- c('period','channel','detail','spending')
x$period <- gsub("12019/12/15","2019/12/15",x$period)
x$period <- gsub("2019/11/31","2019/11/30",x$period)
x <- cbind(x,do.call(rbind,strsplit(x$period,'-')))
x$spending[is.na(x$spending)] <- 0
x[5530,]$V1 <- "2018/12/25"
map <- as.Date("2018/12/24")+1:490
map <- data.table(date=(map),week=rep(1:70,each=7))
test <- lapply(1:nrow(x),function(i){
  print(i)
  i <- x[i,]
  data.table(week=map$date[(unique(filter(map,date>=i$V1&date<=i$V2)$week)-1)*7+1],
             channel=i$channel,detail=i$detail,
             spending=i$spending/length(unique(filter(map,date>=i$V1&date<=i$V2)$week)))
})
sum(sapply(test,function(x){sum(x$spending)}))
test <- do.call(rbind,test)
test <- test %>% group_by(week,channel,detail) %>% summarise(
   n=n(),spending=sum(spending)
)
write.csv(test,'processed_promoter.csv')

#################################

rm(list=ls())
setwd('/Users/wenrurumon/Desktop/g2n')
library(data.table)
library(dplyr)
library(openxlsx)
library(ggplot2)
x <- read.xlsx('Book2.xlsx')
colnames(x) <- c('spending','start','end','channel','detail')
map <- as.Date("2018/12/03")+1:490
map <- data.table(date=(map),week=rep(1:70,each=7))

x$start <- as.Date(x$start)
x$end <- as.Date(x$end)
x <- x[rowSums(is.na(x))==0,]
x$spending <- as.numeric(x$spending)
x <- x[rowSums(is.na(x))==0,]
x$week <- map$week[match(x$start,map$date)]
test <- x %>% group_by(detail,week) %>% summarise(n=n(),spending=sum(spending))

# test <- x %>% group_by(channel,detail,week) %>% summarise(n=n(),spending=sum(as.numeric(spending),na.rm=T))
# x$week <- map$date[(x$week-1)*7+1]
# ggplot() + geom_line(data=test,aes(x=week,y=n,colour=paste(detail)))

test <- lapply(1:nrow(x),function(i){
  print(i)
  i <- x[i,]
  i.week <- filter(map,date>=i$start&date<=i$end)$week %>% unique()
  i.week <- map$date[(i.week-1)*7+1]
  data.table(week=i.week,detail=i$detail,start=i$start,end=i$end,spending=as.numeric(i$spending)/length(i.week))
})
test <- do.call(rbind,test)
test <- filter(test,!is.na(week))
test <- filter(test,detail!=0)
test <- test %>% group_by(week,detail) %>% summarise(n=n(),spending=sum(spending))
ggplot() + geom_line(data=test,aes(x=week,y=n,colour=detail))

write.csv(test,'g2n.csv')


