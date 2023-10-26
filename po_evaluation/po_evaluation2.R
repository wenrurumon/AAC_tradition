
rm(list=ls())
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
load("/Users/wenrurumon/Documents/temp/baolelijia/anya/output.RData")
setwd('/Users/wenrurumon/Documents/temp/baolelijia/anya/')
load("/Users/wenrurumon/Library/Containers/com.tencent.xinWeChat/Data/Library/Application Support/com.tencent.xinWeChat/2.0b4.0.9/da60239cab44a6cfe3abf90382713324/Message/MessageTemp/9f9fa1021888bee4014be11a520a2916/File/store_map.RData")

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
  mutate(class=paste(Region,Division,Channel)) %>%
  select(store=1,parent=2,region=Region,division=Division,channel=Channel) %>%
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

POhis <- Xpo %>%
  filter(po%in%((KEYpo %>% filter(!closed,!far))$po)) %>%
  select(po,parent) %>%
  unique() %>%
  merge(
    Xmap %>%
      merge(
        Xvol %>%
          group_by(store) %>%
          summarise(storehis = match(max(month),monthlist)-match(min(month),monthlist)) %>%
          filter(store!='')
      ) %>%
      group_by(parent) %>%
      summarise(storehis=max(storehis))    
  ) %>%
  group_by(po) %>%
  summarise(storehis=max(storehis))

################################################################################
#Variation at Store Type level
################################################################################

#By Channel Summary

Xvol2 <- Xvol
Xvol2$class <- Xmap$channel[match(Xvol2$store,Xmap$store)]
Xvol2 <- Xvol2 %>%
  group_by(class,month,brand) %>%
  summarise(value=sum(value))

Summary.channel <- do.call(rbind,lapply(unique(Xvol2$class)[!is.na(unique(Xvol2$class))],function(classi){
  print(classi)
  xvol <- Xvol2 %>% filter(class==classi)
  x <- data.table(
    month=monthlist[match(min(xvol$month),monthlist):match(max(Xvol$month),monthlist)]
  ) %>%
    merge(xvol,all.x=T) %>%
    acast(month~brand,value.var='value',fill=0)
  x <- apply(x <- x[,colnames(x)!='NA',drop=F],2,function(x){
    rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  })
  x[x<0] <- 0
  x <- data.table(month=as.numeric(rownames(x)),x) %>%
    mutate(month=month%%100)
  if(nrow(x)<12){
    return(NULL)
  }
  out <- lapply(1:nrow(x),function(i){
    data.table(roll=i,x[1:12+i-1,])
  })
  out <- do.call(rbind,out[sapply(out,function(x){mean(is.na(x))})==0]) %>%
    melt(id=c('roll','month'))
  out <- out %>%
    merge(
      out %>%
        group_by(roll,variable) %>%
        summarise(prop=sum(value))
    ) %>%
    mutate(prop=value/prop) %>%
    filter(!is.na(prop)) %>%
    group_by(month,brand=variable) %>%
    summarise(mean=mean(prop),sd=sd(prop),min=min(prop),max=max(prop),obs=n())
  data.table(class=classi,out)
}))

#By Region Summary

Xvol2 <- Xvol
Xvol2$class <- Xmap$region[match(Xvol2$store,Xmap$store)]
Xvol2 <- Xvol2 %>%
  group_by(class,month,brand) %>%
  summarise(value=sum(value))

Summary.region <- do.call(rbind,lapply(unique(Xvol2$class)[!is.na(unique(Xvol2$class))],function(classi){
  print(classi)
  xvol <- Xvol2 %>% filter(class==classi)
  x <- data.table(
    month=monthlist[match(min(xvol$month),monthlist):match(max(Xvol$month),monthlist)]
  ) %>%
    merge(xvol,all.x=T) %>%
    acast(month~brand,value.var='value',fill=0)
  x <- apply(x <- x[,colnames(x)!='NA',drop=F],2,function(x){
    rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  })
  x[x<0] <- 0
  x <- data.table(month=as.numeric(rownames(x)),x) %>%
    mutate(month=month%%100)
  if(nrow(x)<12){
    return(NULL)
  }
  out <- lapply(1:nrow(x),function(i){
    data.table(roll=i,x[1:12+i-1,])
  })
  out <- do.call(rbind,out[sapply(out,function(x){mean(is.na(x))})==0]) %>%
    melt(id=c('roll','month'))
  out <- out %>%
    merge(
      out %>%
        group_by(roll,variable) %>%
        summarise(prop=sum(value))
    ) %>%
    mutate(prop=value/prop) %>%
    filter(!is.na(prop)) %>%
    group_by(month,brand=variable) %>%
    summarise(mean=mean(prop),sd=sd(prop),min=min(prop),max=max(prop),obs=n())
  data.table(class=classi,out)
}))

#By Division Summary

Xvol2 <- Xvol
Xvol2$class <- Xmap$division[match(Xvol2$store,Xmap$store)]
Xvol2 <- Xvol2 %>%
  group_by(class,month,brand) %>%
  summarise(value=sum(value))

Summary.division <- do.call(rbind,lapply(unique(Xvol2$class)[!is.na(unique(Xvol2$class))],function(classi){
  print(classi)
  xvol <- Xvol2 %>% filter(class==classi)
  x <- data.table(
    month=monthlist[match(min(xvol$month),monthlist):match(max(Xvol$month),monthlist)]
  ) %>%
    merge(xvol,all.x=T) %>%
    acast(month~brand,value.var='value',fill=0)
  x <- apply(x <- x[,colnames(x)!='NA',drop=F],2,function(x){
    rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  })
  x[x<0] <- 0
  x <- data.table(month=as.numeric(rownames(x)),x) %>%
    mutate(month=month%%100)
  if(nrow(x)<12){
    return(NULL)
  }
  out <- lapply(1:nrow(x),function(i){
    data.table(roll=i,x[1:12+i-1,])
  })
  out <- do.call(rbind,out[sapply(out,function(x){mean(is.na(x))})==0]) %>%
    melt(id=c('roll','month'))
  out <- out %>%
    merge(
      out %>%
        group_by(roll,variable) %>%
        summarise(prop=sum(value))
    ) %>%
    mutate(prop=value/prop) %>%
    filter(!is.na(prop)) %>%
    group_by(month,brand=variable) %>%
    summarise(mean=mean(prop),sd=sd(prop),min=min(prop),max=max(prop),obs=n())
  data.table(class=classi,out)
}))


###########################################################################
#Those PO with stores obs greater than 12
###########################################################################

run12 <- function(poi){
  # poi <- unique((KEYpo %>% filter(!closed,!far))$po)[120]
  print(poi)
  xpo <- Xpo %>% filter(po==poi)
  xmap <- Xmap %>% filter(parent%in%unique(xpo$parent))
  xvol <- Xvol %>% filter(brand%in%xpo$brand,store%in%xmap$store) %>%
    group_by(month,brand) %>%
    summarise(value=sum(value))
  #Parent/Brand Summary
  x <- data.table(
    month=monthlist[match(min(xvol$month),monthlist):match(max(Xvol$month),monthlist)]
  ) %>%
    merge(xvol,all.x=T) %>%
    acast(month~brand,value.var='value',fill=0)
  x <- apply(x <- x[,colnames(x)!='NA',drop=F],2,function(x){
    rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  })
  x[x<0] <- 0
  x <- data.table(month=as.numeric(rownames(x)),x) %>%
    mutate(month=month%%100)
  out <- lapply(1:nrow(x),function(i){
    data.table(roll=i,x[1:12+i-1,])
  })
  out <- do.call(rbind,out[sapply(out,function(x){mean(is.na(x))})==0]) %>%
    melt(id=c('roll','month'))
  out <- out %>%
    merge(
      out %>%
        group_by(roll,variable) %>%
        summarise(prop=sum(value))
    ) %>%
    mutate(prop=value/prop) %>%
    filter(!is.na(prop)) %>%
    group_by(month,brand=variable) %>%
    summarise(mean=mean(prop),sd=sd(prop),min=min(prop),max=max(prop),obs=n())
  data.table(po=poi,out)
}
Summary.po12 <- lapply((POhis %>% filter(storehis>12))$po,run12)
Summary.po12 <- do.call(rbind,Summary.po12)

###########################################################################
#PO Summary
###########################################################################

sumpoi <- function(poi){
  print(poi)
  xpo <- Xpo %>% filter(po==poi)
  xmap <- Xmap %>% filter(parent%in%unique(xpo$parent))
  xvol <- Xvol %>%
    filter(store%in%xmap$store,brand%in%xpo$brand) %>%
    group_by(month,brand) %>%
    summarise(value=sum(value)) %>%
    mutate(value=ifelse(value<0,0,value))
  #finished
  xsum <- xvol %>%
    filter(month>=min(xpo$start)) %>%
    group_by(po=poi,brand) %>%
    summarise(finish=sum(value)) %>%
    merge(
      xvol %>%
        filter(month>=monthlist[match(max(xpo$end),monthlist)-11]) %>%
        group_by(brand) %>%
        summarise(close_12month=sum(value))
    )
  monthleft <- (monthlist[match(max(Xvol$month),monthlist):
                            match(max(xpo$end),monthlist)]%%100)[-1]
  mtd <- (1:12)[!(1:12)%in%monthleft]
  #Division
  xmapi <- xmap %>%
    group_by(class=division) %>%
    summarise(prop=n()/nrow(xmap))
  out.division <- Summary.division %>%
    filter(month%in%mtd) %>%
    group_by(class,brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    merge(xmapi) %>%
    mutate(mean=mean*prop,sd=sd*prop) %>%
    group_by(brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    melt(id='brand') %>%
    mutate(class='division') %>%
    mutate(po=poi)
  #Region
  xmapi <- xmap %>%
    group_by(class=region) %>%
    summarise(prop=n()/nrow(xmap))
  out.region <- Summary.region %>%
    filter(month%in%mtd) %>%
    group_by(class,brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    merge(xmapi) %>%
    mutate(mean=mean*prop,sd=sd*prop) %>%
    group_by(brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    melt(id='brand') %>%
    mutate(class='region') %>%
    mutate(po=poi)
  #Channel
  xmapi <- xmap %>%
    group_by(class=channel) %>%
    summarise(prop=n()/nrow(xmap))
  out.channel <- Summary.channel %>%
    filter(month%in%mtd) %>%
    group_by(class,brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    merge(xmapi) %>%
    mutate(mean=mean*prop,sd=sd*prop) %>%
    group_by(brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    melt(id='brand') %>%
    mutate(class='channel') %>%
    mutate(po=poi)
  #Parent
  xeval <- Summary.po12 %>%
    filter(po==poi) %>%
    filter(month%in%mtd) %>%
    group_by(brand) %>%
    summarise(mean=sum(mean),sd=sum(sd),obs=mean(obs)) %>%
    melt(id='brand') %>%
    mutate(class='PO',po=poi) %>%
    rbind(out.region) %>%
    rbind(out.division) %>%
    rbind(out.channel)
  list(status=xsum,model=xeval)
}
rlt <- lapply(unique((KEYpo %>% filter(!closed,!far))$po),sumpoi)
save(rlt,Summary.po12,Summary.channel,Summary.division,Summary.region,file='rlt1026.rda')

load('rlt1026.rda')

################################################################################
#Weight Model
################################################################################

wdata <- do.call(rbind,lapply(rlt,function(x){x[[2]]}))
wdata <- wdata %>%
  filter(grepl('mean',variable)) %>%
  acast(po+brand~class,value.var='value') 
wdata <- wdata[!is.na(wdata[,3]),]
wdata <- data.frame(po=wdata[,3],wdata[,-3])
wcoef <- coef(lm(wdata[,1]~as.matrix(wdata[,2:4]) %*% 
          cbind(apply(wdata[,2:4],2,function(x){coef(lm(wdata[,1]~x-1))}))-1)) *
  apply(wdata[,2:4],2,function(x){coef(lm(wdata[,1]~x-1))})
wcoef <- c(1,wcoef)
names(wcoef)[1] <- 'PO'
wdata <- do.call(rbind,lapply(rlt,function(x){x[[2]]})) %>%
  merge(data.table(class=names(wcoef),wcoef),all.x=T)

wdata <- wdata %>%
  merge(
    wdata %>%
      group_by(po,class) %>%
      summarise(wcoef=mean(wcoef)) %>%
      group_by(po) %>%
      summarise(coef=sum(wcoef)) 
  ) %>%
  mutate(coef=wcoef/coef) %>%
  mutate(value=value*coef) %>%
  group_by(po,brand,variable) %>%
  summarise(value=sum(value))

wdata <- wdata %>% tidyr::spread(key='variable',value='value')
colnames(wdata)[3:5] <- paste0('w',colnames(wdata)[3:5])
wdata2 <- do.call(rbind,lapply(rlt,function(x){x[[2]]})) %>%
  filter(class=='PO') %>%
  select(-class) %>%
  tidyr::spread(key='variable',value='value')
wdata <- wdata %>% merge(wdata2,all=T)
cor(wdata[,-1:-2],use='pairwise')[1:3,-1:-3]

rlt <- do.call(rbind,lapply(rlt,function(x){x[[1]]})) %>%
  merge(wdata) %>%
  mutate(itv=qt(0.75,wobs-1)*wsd) %>%
  mutate(vol2g=finish/wmean-finish,
         vol2g.low = finish/(wmean+itv)-finish,
         vol2g.high=vol2g*2-vol2g.low) %>%
  select(-mean,-sd,-obs,-itv)



