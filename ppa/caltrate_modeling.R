
##########################################################################################
# Modeling
##########################################################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)

write.clip <- function(x){
  print(head(x))
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}

setwd('/Users/wenrurumon/Documents/GSK/PPA/model')
raw.ec <- read.xlsx("EC & IMS VMS data.xlsx",sheet=1)
raw.ims <- read.xlsx("EC & IMS VMS data.xlsx",sheet=2)
raw.gc <- read.xlsx("Gluco.xlsx",sheet=1)
raw.gcims <- read.xlsx("Gluco.xlsx",sheet=2)
sum2 <- function(x){sum(as.numeric(x),na.rm=T)}

raw.ec <- raw.ec %>% mutate(CompetitorType=ifelse(grepl('金斯利安',Brandname),'KeyCompetitor',CompetitorType))

brands.ec <- raw.ec %>% group_by(Category,CompetitorType,Brandname) %>% summarise(value=sum2(`Value(RMB)`)) %>% arrange(Category,desc(value))
brands.ims <- select(raw.ims,Brand,Brand.CN) %>% mutate(
  Brand2 = gsub(' |-','',tolower(Brand)),
  Brand.CN2 = gsub(' |-','',tolower(Brand.CN))
) %>% unique()

########################################################
# Caltrate offline
########################################################

#Process

raw <- raw.ims %>% mutate(date=Year*100+Month) %>% 
  group_by(date,Category=Defined.Category,Subcategory=Sub.Category,Skuname=SKU.Name,
           OTCHF=`OTC/HF`,Drugform=Drug.Form,Package=Package,
           BrandE=Brand,Brand=Brand.CN,ProductLine=gene.CN,TA=Consumer.Type) %>%
  summarise(value=sum2(`CurPer.Sales.Val('000.RMB)`),
            du=sum2(`CurPer.Sales.Vol('00.DU)`)/10,
            pack=sum2(`CurPerSales.Vol.by.Pack.('00)`)/10)
out.ims <- raw %>% 
  group_by(date,Category,Subcategory,Skuname,Brand,BrandE,ProductLine,TA,OTCHF,Drugform,Package) %>% 
  summarise(n=n(),val=sum(value),vol=sum(du),pack=sum(pack)) %>% 
  filter(Category=='Calcium')
dim(out.ims)

x <- filter(out.ims,Category=='Calcium')
map <- read.xlsx('coding/offline_caltrate.xlsx',sheet=1)
map2 <- read.xlsx('coding/offline_caltrate.xlsx',sheet=3)
benefits.label <- c(map2$`Ingredients/benefit`[-1],map2$Claim[-1])
benefits.label <- benefits.label[!is.na(benefits.label)]
colnames(map) <- strsplit('id,category,brand,brande,productline,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit5,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,claim1,claim2,claim3,claim4,claim5,claim6,claim7,country,flavor,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit|claim',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
map$flavor <- ifelse(is.na(map$flavor),'原味',map$flavor)
benefits <- as.matrix(benefits)+0
key <- paste(map$brand,sapply(strsplit(map$productline,'\\('),function(x){
  paste(x[-length(x)],collapse='')
}))
for(keyi in unique(key)){
  i <- which(key==keyi)
  for(j in i){benefits[j,] <- unlist((apply(benefits[i,,drop=F],2,max)))}
  map$usage2[i] <- as.numeric(names(sort(table(round(map$usage2[i])),T)))[1]
}
benefits <- as.data.table(benefits>0)
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}

map <- cbind(map %>% select(Brand=brand,BrandE=brande,ProductLine=productline,usage=usage2,ta,jixing,country,flavor,leixing),benefits)

raw <- merge(out.ims,map,by=c('Brand','BrandE','ProductLine'))
paed.label <- unique((select(raw,Brand,ProductLine,TA,ta) %>% filter(TA=='PAEDIATRIC'))$ta)
raw$ta[is.na(raw$ta)] <- 'NA'
paed.label[is.na(paed.label)] <- 'NA'
paed.label
raw$TA <- ifelse(raw$TA=='PAEDIATRIC',c('BABY','TEENAGER')[2-c(0,0,0,1,0)][match(raw$ta,paed.label)],raw$TA)
unique(raw %>% filter(grepl('caltrate',Skuname,ignore.case=T)) %>% select(ProductLine,usage))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% 
  mutate(Drugform=ifelse(grepl('CHEW',Package),'Chewable',Drugform))

colnames(data)[colnames(data)=='ta'] <- 'taref'
colnames(data) <- tolower(colnames(data))
sum(data$val)
sum(filter(data,!is.na(usage))$val)
data <- mutate(data,days=vol/usage,dayprice=val/days) %>% filter(!is.na(usage))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

data <- data %>% select(date,brand,brande,productline,skuname,category,subcategory,ta,otchf,drugform,usage,country,flavor,leixing,22:44,val,vol,pack,days) %>% mutate(avp=val/vol,adp=val/days,ta=ifelse(ta=='OTHERS','GENERAL',ta))
write.csv(data,'rlt3/raw_offline_caltrate.csv')

#Segmentation

rlts <- list()
for(i in (6:37)){
  data$cluster <- data[[i]]
  temp <- data %>% group_by(date,brand,cluster) %>% 
    summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),val=sum(val),days=sum(days),adp=sum(val)/sum(days)
    )
  segment2 <- data %>% filter(grepl('caltrate',brande,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(tval=sum(val),tdays=sum(days))
  segment3 <- data %>% group_by(segment=colnames(data)[i],cluster=cluster,skuname,brand) %>% summarise(val=sum(val),avp=sum(val)/sum(vol))
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    b <- abs(b)
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  b <- sum(segment$val)*b_adp*segment$days/sum(b_adp*segment$days)/segment$days
  segment <- data.table(segment,b=b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment2 b on a.cluster = b.cluster')) %>% 
    select(-cluster..8)
  rlts[[i-5]] <- list(rlt=rlti,decomp=segment3)
}
segmentation <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[1]])}))
segmentation <- segmentation %>% filter(segment %in% (segmentation %>% filter(brand>=10) %>% group_by(segment) %>% summarise(n=n()) %>% filter(n>1))$segment) %>% filter(!is.na(cluster))
decomp <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[2]])})) %>% filter(segment%in%segmentation$segment) %>% filter(!is.na(cluster))

write.csv(segmentation,'rlt3/segmentation_offline_caltrate.csv')
write.csv(decomp,'rlt3/decomp_offline_caltrate.csv')

########################################################
# Centrum Online
########################################################

#Process

raw <- raw.ec %>% mutate(date=Month) %>% 
  group_by(date,Channel,SubChannel,Category,SubCategory,Brand=Brandname,ProductLine,Skuname,
           OTCHF=`otc/.healthy.food`,TA=Target.Audience) %>% 
  summarise(value=sum2(`Value(RMB)`),du=sum2(Volume_Unit),pack=sum2(Volume_Pack)) %>% 
  filter(Skuname!='Others')
out.ims <- raw %>% 
  group_by(date,Channel,SubChannel,Category,SubCategory,Skuname,Brand,ProductLine,TA,OTCHF) %>% 
  summarise(n=n(),val=sum(value),vol=sum(du),pack=sum(pack)) %>% filter(Category=='Calcium')
dim(out.ims)

map <- read.xlsx('coding/online_caltrate.xlsx',sheet=1)
map2 <- read.xlsx('coding/online_caltrate.xlsx',sheet=3)

benefits.label <- c(map2$`Ingredients/benefit`[-1],map2$Claim[-1])
benefits.label <- benefits.label[!is.na(benefits.label)]
colnames(map) <- strsplit('id,category,brand,productline,skuname,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit5,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,claim1,claim2,claim3,claim4,claim5,claim6,claim7,country,flavor,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit|claim',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
map$flavor <- ifelse(is.na(map$flavor),'原味',map$flavor)

benefits <- as.matrix(benefits)+0
key <- paste(map$brand,sapply(strsplit(map$skuname,'\\('),function(x){
  paste(x[-length(x)],collapse='')
}))
for(keyi in unique(key)){
  i <- which(key==keyi)
  for(j in i){benefits[j,] <- unlist((apply(benefits[i,,drop=F],2,max)))}
  map$usage2[i] <- as.numeric(names(sort(table(round(map$usage2[i])),T)))[1]
}
benefits <- as.data.table(benefits>0)
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}

map <- cbind(map %>% select(category=category,Brand=brand,ProductLine=productline,Skuname=skuname,usage=usage2,ta,jixing,country,flavor,leixing),benefits)
raw <- merge(out.ims,map,by=c('Brand','ProductLine','Skuname'))
raw$ta[is.na(raw$ta)] <- 'General'
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% select(1:6,7:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))



sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize)
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))
write.csv(data,'rlt3/raw_online_caltrate.csv')

#Segmentation

rlts <- list()
for(i in (5:39)){
  data$cluster <- data[[i]]
  if(colnames(data)[i]=='channel'){
    temp <- data %>% 
      group_by(date,
               brand=paste(c(1,1,1,2,3)[match(data$jixing,unique(data$jixing))],brand),
               cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else if (colnames(data)[i]=='jixing'){
    temp <- data %>% 
      group_by(date,
               brand=paste(channel,brand),
               cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else {
    temp <- data %>% 
      group_by(date,
               brand=paste(channel,c(1,1,1,2,3)[match(data$jixing,unique(data$jixing))],brand),
               cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  }
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),val=sum(val),days=sum(days),adp=sum(val)/sum(days)
    )
  segment2 <- data %>% filter(grepl('钙尔奇',brand,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(tval=sum(val),tdays=sum(days))
  segment3 <- data %>% group_by(segment=colnames(data)[i],cluster=cluster,skuname,brand) %>% summarise(val=sum(val),avp=sum(val)/sum(vol))
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    b <- abs(b)
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- optim(b0,loss,control=list(maxit=1000))$par}
  b <- sum(segment$val)*b_adp*segment$days/sum(b_adp*segment$days)/segment$days
  segment <- data.table(segment,b=b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment2 b on a.cluster = b.cluster')) %>% 
    select(-cluster..8)
  rlts[[i-4]] <- list(rlt=rlti,decomp=segment3)
}
segmentation <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[1]])}))
segmentation <- segmentation %>% filter(segment %in% (segmentation %>% filter(brand>=30) %>% group_by(segment) %>% summarise(n=n()) %>% filter(n>1))$segment) %>% filter(!is.na(cluster))
decomp <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[2]])})) %>% filter(segment%in%segmentation$segment) %>% filter(!is.na(cluster))

write.csv(segmentation,'rlt3/segmentation_online_caltrate.csv')
write.csv(decomp,'rlt3/decomp_online_caltrate.csv')

########################################################
# Caltrate Resulting
########################################################

#Resulting offline

rm(list=ls())
setwd('/Users/wenrurumon/Documents/GSK/PPA/model')
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)
library(reshape2)

write.clip <- function(x){
  print(head(x))
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}

#Offline

#Data Summary
data <- fread('rlt3/raw_offline_caltrate.csv')[,-1] %>% mutate(vol=days)
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000) %>% write.clip
data %>% group_by(date>=(201906)) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000) %>% mutate(avp=val/vol,app=val/pack)

data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000) %>% write.clip

#Price Distribution
temp <- data %>% group_by(skuname,brand) %>% mutate(brand=ifelse(brand%in%c('钙尔奇'),brand,'Others')) %>%
  summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% arrange(avp)
temp$val <- cumsum(temp$val/sum(temp$val))
temp$vol <- cumsum(temp$vol/sum(temp$vol))
temp <- filter(temp,avp<=quantile(temp$avp,0.95))
temp <- data.table(
  avp=temp$avp,
  t(sapply(1:nrow(temp),function(i){
    x <- rep(-1,2)
    x[which(c('Others','钙尔奇')==temp$brand[i])] <- temp$vol[i]
    x
  })),
  val=temp$val,vol=temp$vol)
colnames(temp)[2:3] <- c('Others','Caltrate')
temp %>% write.clip #Price Distribution
temp$avp[which(temp$val>0.2)[1]]
temp$avp[which(temp$val>0.6)[1]]
temp$avp[which(temp$val>0.9)[1]]

#Tier Summary

temp <- data %>% group_by(brand) %>% summarise(val=sum(val)) %>% arrange(desc(val))
temp$prop <- cumsum(temp$val)/sum(temp$val)
brand2 <- temp$brand[1:10]

temp <- data %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% mutate(tier=(avp>=1.01)+(avp>=1.99)+(avp>=3.66))
temp <- merge(data %>% group_by(brand=ifelse(brand%in%brand2,brand,'Others'),skuname,p=(date>=201906)+1) %>% summarise(val=sum(val)),
              temp %>% select(skuname,tier)) %>% group_by(brand,p,tier) %>% summarise(val=sum(val))
temp <- temp %>% acast(brand~paste(tier,p),value.var='val')
temp[match(c(brand2,'Others'),rownames(temp)),] %>% write.clip

#Segmentation Results

seg <- fread('rlt3/segmentation_offline_caltrate.csv')[,-1]
decomp <- fread('rlt3/decomp_offline_caltrate.csv')[,-1]

seg %>% mutate(idx=val,val=val-ifelse(is.na(tval),0,tval),adp=adp,tadp=tval/tdays) %>% select(segment,cluster,val,tval,adp,tadp,idx,b) %>% 
  filter(!segment %in% c('leixing')) %>%
  select(segment,cluster,adp,val,tval,tadp,idx,b) %>% arrange(match(segment,segment),desc(idx)) %>%  write.clip #Price Summary

temp <- data %>% mutate(packsize=round(days/pack,0)) %>% 
  select(date,productline,packsize,val,days) %>% 
  group_by(date,productline,packsize) %>% 
  summarise(val=sum(val),days=sum(days),adp=val/days)
temp %>% filter(packsize<=quantile(temp$packsize,0.95)) %>% group_by(packsize=ceiling(packsize/10)*10) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% select(packsize,adp,val) %>% write.clip #Packsize Trend

temp %>% group_by(packsize=ceiling(packsize/10)*10) %>% 
  group_by(packsize=(packsize>=30)+(packsize>=60)) %>% 
  summarise(val=sum(val),days=sum(days),adp=val/days) %>% select(packsize,adp,val) %>% write.clip #P


#Price Map

temp <- data %>% filter(date>=201906) %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),pack=sum(pack),avp=val/vol)
temp <- unique(select(data,2:37)) %>% merge(temp,by='skuname')
write.clip(filter(temp,grepl('caltrate',skuname,ignore.case=T)))

#Online

#Data Summary

data <- fread('rlt3/raw_online_caltrate.csv')[,-1] %>% mutate(vol=days/1000,val=val/1000,pack=pack/1000,days=days/1000)

data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000) %>% write.clip
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000) %>% write.clip

data %>% group_by(date>=(201906)) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000,avp=val/vol,app=val/pack)

#Price Distribution
temp <- data %>% group_by(skuname,brand) %>% mutate(brand=ifelse(grepl('钙尔奇',brand,ignore.case=T),brand,'Others')) %>%
  summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% arrange(avp)
temp$val <- cumsum(temp$val/sum(temp$val))
temp$vol <- cumsum(temp$vol/sum(temp$vol))
temp <- filter(temp,avp<=quantile(temp$avp,0.95))
temp <- data.table(
  avp=temp$avp,
  t(sapply(1:nrow(temp),function(i){
    x <- rep(-1,2)
    x[which(c('Others','钙尔奇/Caltrate')==temp$brand[i])] <- temp$vol[i]
    x
  })),
  val=temp$val,vol=temp$vol)
colnames(temp)[2:3] <- c('Others','Caltrate')
temp %>% write.clip #Price Distribution

temp$avp[which(temp$val>0.2)[1]] #1.42
temp$avp[which(temp$val>0.6)[1]] #2.52
temp$avp[which(temp$val>0.9)[1]] #5.98

#Tier Summary

temp <- data %>% group_by(brand) %>% summarise(val=sum(val)) %>% arrange(desc(val))
temp$prop <- cumsum(temp$val)/sum(temp$val)
brand2 <- temp$brand[1:10]

temp <- data %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% mutate(tier=(avp>=1.42)+(avp>=2.52)+(avp>=5.98))
temp <- merge(data %>% group_by(brand=ifelse(brand%in%brand2,brand,'Others'),skuname,p=(date>=201906)+1) %>% summarise(val=sum(val)),
              temp %>% select(skuname,tier)) %>% group_by(brand,p,tier) %>% summarise(val=sum(val))
temp <- temp %>% acast(brand~paste(tier,p),value.var='val')
temp[match(c(brand2,'Others'),rownames(temp)),] %>% write.clip

#Segmentation Results

seg <- fread('rlt3/segmentation_online_caltrate.csv')[,-1]
decomp <- fread('rlt3/decomp_online_caltrate.csv')[,-1]

seg %>% mutate(idx=val,val=val-ifelse(is.na(tval),0,tval),adp=adp,tadp=tval/tdays) %>% select(segment,cluster,val,tval,adp,tadp,idx,b) %>% 
  # filter(!segment %in% c('leixing')) %>%
  select(segment,cluster,adp,val,tval,tadp,idx,b) %>% arrange(match(segment,segment),desc(idx)) %>%  write.clip #Price Summary

#Packsize

temp <- data %>% mutate(packsize=round(vol/pack,0)) %>% 
  select(date,productline,packsize,val,vol) %>% 
  group_by(date,productline,packsize) %>% 
  summarise(val=sum(val),days=sum(vol),adp=val/days)

temp %>% filter(packsize<=quantile(temp$packsize,0.95)) %>% group_by(packsize=ceiling(packsize/10)*10) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% select(packsize,adp,val) %>% write.clip #Packsize Trend

temp %>% group_by(packsize=ceiling(packsize/10)*10) %>% 
  group_by(packsize=(packsize>=30)+(packsize>=60)) %>% 
  summarise(val=sum(val),days=sum(days),adp=val/days) %>% select(packsize,val,days,adp) %>% write.clip #P

sum(data$days)/sum(data$pack)

#Price map

temp <- data %>% filter(date>=201906) %>% group_by(channel,skuname) %>% summarise(val=sum(val),vol=sum(vol),pack=sum(pack),avp=val/vol)
temp <- unique(select(data,c(-4,-6,-40:-44))) %>% merge(temp,by=c('skuname','channel'))
write.clip(filter(temp,grepl('钙尔奇',skuname,ignore.case=T)))
