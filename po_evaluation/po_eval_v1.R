
rm(list=ls())
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
load("/Users/wenrurumon/Documents/temp/baolelijia/anya/output.RData")
setwd('/Users/wenrurumon/Documents/temp/baolelijia/anya/')

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')
  close(clip)
}

#####

max(process.actual$STD_DATE)
process.actual <- process.actual %>%
  filter(STD_DATE<202310)

enddate <- 202406
monthlist <- rep(2015:2030,each=12)
monthlist <- monthlist*100+rep(1:12,length=length(monthlist))

################################################################################
#Process data
#把所有的日期都算到YYYYMM
################################################################################

Xpo <- process.contract %>%
  select(po=1,parent=2,brand=3,povalue=4,start,end,interval) %>%
  mutate(start=as.numeric(substr(start,1,6)),end=as.numeric(substr(end,1,6))) %>%
  filter(povalue>0)

Xvol <- process.actual %>% 
  select(month=1,brand=2,store=3,value=4)

Xmap <- level.mapping %>%
  select(store=1,parent=2) %>%
  unique()

KEYpo <- Xpo %>%
  group_by(po) %>%
  summarise(start=min(start),end=max(end)) %>%
  mutate(closed=(end<=max(Xvol$month)),far=(end>enddate)) %>%
  mutate(newpo=paste0(po,':',start,'-',end))
  
KEYpo %>%
  group_by(closed,far) %>%
  summarise(n=n()) %>%
  ggplot() + 
  geom_tile(aes(x=closed,y=far,fill=n),colour='black') +
  geom_text(aes(x=closed,y=far,label=n),size=8) +
  theme_bw() +
  scale_fill_gradientn(colours=c('lightblue','blue')) +
  labs(x='PO already closed',y=paste('PO end after',enddate),fill='Count') +
  theme(text=element_text(size=15))

#
Xvol %>% group_by(month) %>% summarise(value=sum(value)) %>% write.clip
(test <- do.call(rbind,lapply(1:nrow(Xpo),function(i){
  data.table(po=Xpo$po[i],brand=Xpo$brand[i],
             month=monthlist[match(Xpo$start[i],monthlist):match(Xpo$end[i],monthlist)],
             value=Xpo$povalue[i]/
               length(match(Xpo$start[i],monthlist):match(Xpo$end[i],monthlist)))
}))) %>%
  group_by(month) %>%
  summarise(value=sum(value)) %>%
  write.clip

################################################################################
#Sample
################################################################################

test <- Xvol %>%
  filter(value>0) %>%
  group_by(store) %>%
  summarise(min=min(month),max=max(month)) %>%
  merge(Xmap) %>%
  group_by(parent) %>%
  summarise(store=n_distinct(store),min=min(min),max=max(max)) %>%
  arrange(desc(store)) %>%
  merge(
    Xpo %>%
      mutate(newpo=paste0(po,':',start,'-',end)) %>%
      filter(newpo%in%(KEYpo %>% filter(!closed,!far))$newpo) %>%
      select(parent,newpo)
  ) %>%
  group_by(newpo) %>%
  summarise(min=min(min),max=max(max),store=max(store)) %>%
  arrange(desc(store))

test <- Xpo %>%
  filter(po%in%(KEYpo %>% filter(newpo=='24221003:202304-202403'))$po) %>%
  merge(Xmap)

test %>%
  group_by(brand) %>%
  summarise(povalue=mean(povalue)) %>%
  write.clip

Xvol %>%
  filter(store%in%test$store,
         brand%in%test$brand) %>%
  group_by(month,brand) %>%
  summarise(value=sum(value)) %>%
  acast(month~brand,value.var='value',fill=0) %>%
  write.clip

#KeyPO

key.po <- '24221003'
  
#该po对应的信息，start1是这个po的截止日往前推12个月，xstone记录时间节点，xpo记录其他信息
xpo <- Xpo %>% filter(po==key.po) %>%
  select(po,parent,brand,povalue,start0=start,end0=end) %>%
  mutate(start1=monthlist[match(end0,monthlist)-11])
xstone <- xpo %>% 
  select(start0,start1,end0) %>% 
  unique()
xpo <- xpo[,1:4]
#po对应的门店和品牌的历史销售
xmap <- Xmap %>% filter(parent%in%xpo$parent)
xvol <- Xvol %>% 
  filter(store%in%xmap$store,brand%in%xpo$brand) %>%
  group_by(month,brand) %>%
  summarise(value=sum(value)) %>%
  mutate(value=ifelse(value<0,0,value))
xvol <- data.table(month=sort(unique(Xvol$month))) %>%
  merge(xvol,all.x=T) %>%
  acast(month~brand,fill=0) 
#如果这些店这些品牌什么都没卖出去过
xvol2 <- apply(xvol[,colnames(xvol)!='NA',drop=F],2,function(x){
  rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
})
xvol <- xvol[,colnames(xvol)!='NA',drop=F]
xvol <- xvol %>%
  melt() %>%
  select(date=1,brand=2,value=3) %>%
  mutate(year=as.numeric(substr(date,1,4)),month=date-year*100) %>%
  mutate(value=ifelse(is.na(value),0,value))
xvol2 <- xvol2 %>%
  melt() %>%
  select(date=1,brand=2,value=3) %>%
  mutate(year=as.numeric(substr(date,1,4)),month=date-year*100) %>%
  mutate(value=ifelse(is.na(value),0,value))
#计算每个月占全年的比例
xmean <- xvol2 %>%
  group_by(brand,year) %>%
  summarise(prop=sum(value)) %>%
  merge(xvol2) %>%
  mutate(prop=value/prop) %>%
  group_by(brand,month) %>%
  summarise(mean=mean(prop,na.rm=T),sd=sd(prop,na.rm=T),obs=sum(!is.na(prop)))
#计算这个po还差多少量没完成
xpo <- xpo %>%
  merge(
    xvol %>%
      filter(date>=xstone$start0,date<xstone$start1) %>%
      group_by(brand) %>%
      summarise(freeze=sum(value)) %>% 
      merge(
        xvol %>%
          filter(date>=xstone$start1) %>%
          group_by(brand) %>%
          summarise(finish=sum(value)),all=T
      ),all.x=T
  ) %>%
  mutate(freeze=ifelse(is.na(freeze),0,freeze),
         finish=ifelse(is.na(finish),0,finish),
         vol2go=povalue-freeze-finish) %>%
  mutate(prop2go=ifelse(vol2go<0,0,vol2go)/(povalue-freeze)) 
#计算prop2go发生的概率
xpo %>%
  merge(
    xmean %>%
      filter(month%in%
               (monthlist[(match(max(xvol$date),monthlist):
                             match(xstone$end0,monthlist))[-1]]%%100)) %>%
      group_by(brand) %>%
      summarise(mean=sum(mean,na.rm=T),sd=sum(sd,na.rm=T),obs=mean(obs)),
    all.x=T
  ) %>%
  mutate(z=(prop2go-mean)/sd,pval=(1-pnorm(z)))

################################################################################
#Modeli
################################################################################

modeli <- function(key.po){
  print(key.po)
  #该po对应的信息，start1是这个po的截止日往前推12个月，xstone记录时间节点，xpo记录其他信息
  xpo <- Xpo %>% filter(po==key.po) %>%
    select(po,parent,brand,povalue,start0=start,end0=end) %>%
    mutate(start1=monthlist[match(end0,monthlist)-11])
  xstone <- xpo %>% 
    select(start0,start1,end0) %>% 
    unique()
  xpo <- xpo[,1:4]
  #po对应的门店和品牌的历史销售
  xmap <- Xmap %>% filter(parent%in%xpo$parent)
  xvol <- Xvol %>% 
    filter(store%in%xmap$store,brand%in%xpo$brand) %>%
    group_by(month,brand) %>%
    summarise(value=sum(value)) %>%
    mutate(value=ifelse(value<0,0,value))
  xvol <- data.table(month=sort(unique(Xvol$month))) %>%
    merge(xvol,all.x=T) %>%
    acast(month~brand,fill=0) 
  #如果这些店这些品牌什么都没卖出去过
  if(mean(colnames(xvol)=='NA')==1){
    return(
      xpo %>%
        select(brand,po,parent,povalue) %>%
        mutate(freeze=0,finish=0) %>%
        mutate(vol2go=povalue-freeze-finish) %>%
        mutate(prop2go=ifelse(vol2go<0,0,vol2go)/(povalue-freeze)) %>%
        mutate(mean=NA,sd=NA,obs=0,z=NA,pval=NA)
    )
  } else {
    xvol2 <- apply(xvol[,colnames(xvol)!='NA',drop=F],2,function(x){
      rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
    })
    xvol <- xvol[,colnames(xvol)!='NA',drop=F]
    xvol2 <- xvol2 %>%
      melt() %>%
      select(date=1,brand=2,value=3) %>%
      mutate(year=as.numeric(substr(date,1,4)),month=date-year*100) %>%
      mutate(value=ifelse(is.na(value),0,value))
    xvol <- xvol %>%
      melt() %>%
      select(date=1,brand=2,value=3) %>%
      mutate(year=as.numeric(substr(date,1,4)),month=date-year*100) %>%
      mutate(value=ifelse(is.na(value),0,value))
    #计算每个月占全年的比例
    xmean <- xvol2 %>%
      group_by(brand,year) %>%
      summarise(prop=sum(value)) %>%
      merge(xvol2) %>%
      mutate(prop=value/prop) %>%
      group_by(brand,month) %>%
      summarise(mean=mean(prop,na.rm=T),sd=sd(prop,na.rm=T),obs=sum(!is.na(prop)))
    #计算这个po还差多少量没完成
    xpo <- xpo %>%
      merge(
        xvol %>%
          filter(date>=xstone$start0,date<xstone$start1) %>%
          group_by(brand) %>%
          summarise(freeze=sum(value)) %>% 
          merge(
            xvol %>%
              filter(date>=xstone$start1) %>%
              group_by(brand) %>%
              summarise(finish=sum(value)),all=T
          ),all.x=T
      ) %>%
      mutate(freeze=ifelse(is.na(freeze),0,freeze),
             finish=ifelse(is.na(finish),0,finish),
             vol2go=povalue-freeze-finish) %>%
      mutate(prop2go=ifelse(vol2go<0,0,vol2go)/(povalue-freeze)) 
    #计算prop2go发生的概率
    return(
      xpo %>%
        merge(
          xmean %>%
            filter(month%in%
                     (monthlist[(match(max(xvol$date),monthlist):
                                   match(xstone$end0,monthlist))[-1]]%%100)) %>%
            group_by(brand) %>%
            summarise(mean=sum(mean,na.rm=T),sd=sum(sd,na.rm=T),obs=mean(obs)),
          all.x=T
        ) %>%
        mutate(z=(prop2go-mean)/sd,pval=pnorm(z,lower.tail=F))
    )
  }
}

system.time(
  rawrlts <- rlts <- lapply((KEYpo %>% filter(!closed,!far))$po,modeli)
)

#梳理结果

rlts <- do.call(rbind,rawrlts)

rlts <- rlts %>%
  merge(
    rlts %>%
      group_by(brand) %>%
      summarise(sd2=mean(sd,na.rm=T)*2)
  ) %>%
  mutate(sd=ifelse(sd==0|is.na(sd),sd2,sd)) %>%
  mutate(mean=ifelse(is.na(mean),0,mean)) %>%
  select(-sd2) %>%
  mutate(z=(prop2go-mean)/sd,pval=pnorm(z,lower.tail=F))

do.call(rbind,lapply(1:20,function(i){
  thres <- i/20
  rlts %>%
    group_by(thres=thres,i=pval<=thres) %>%
    summarise(n=n()/nrow(rlts))
})) %>%
  acast(thres~i,value.var='n') %>%
  write.clip

rlts %>%
  mutate(avgfinish=mean,avgfail=1-avgfinish,tar=mean+sd*qnorm(0.75)) %>%
  mutate(vol1=finish/avgfinish*tar,vol2=vol2go/avgfail*tar) %>%
  mutate(vol1=ifelse(vol1==Inf,0,vol1),vol2=ifelse(vol2==Inf,0,vol2)) %>%
  mutate(vol1=ifelse(is.na(vol1),0,vol1),vol2=ifelse(is.na(vol2),0,vol2)) %>%
  mutate(vol1=ifelse(vol1>vol2,vol1,vol2),vol2=vol1+finish) %>%
  group_by(brand) %>%
  summarise(povalue=sum(povalue),value=sum(vol2),finish=sum(finish)) %>%
  mutate(finish2=value>povalue) %>%
  arrange(desc(povalue)) %>%
  mutate(vol1=log(ifelse(finish2,povalue,0),10),
         vol2=log(ifelse(finish2,0,povalue),10),
         finishr=finish/povalue) %>%
  write.clip

rlts %>%
  mutate(avgfinish=mean,avgfail=1-avgfinish,tar=mean+sd*qnorm(0.75)) %>%
  mutate(vol1=finish/avgfinish*tar,vol2=vol2go/avgfail*tar) %>%
  mutate(vol1=ifelse(vol1==Inf,0,vol1),vol2=ifelse(vol2==Inf,0,vol2)) %>%
  mutate(vol1=ifelse(is.na(vol1),0,vol1),vol2=ifelse(is.na(vol2),0,vol2)) %>%
  mutate(vol1=ifelse(vol1>vol2,vol1,vol2),vol2=vol1+finish)  %>%
  select(po,brand,povalue,freeze,finish,vol2go,pval,expect=vol2)
  
  
