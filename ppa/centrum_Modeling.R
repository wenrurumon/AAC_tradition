
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
# Centrum Offline
########################################################

#Processa

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
  filter(Category!='Calcium')
dim(out.ims)

x <- filter(out.ims,Category!='Calcium')
map <- read.xlsx('coding/offline_centrum.xlsx',sheet=1)
map2 <- read.xlsx('coding/offline_centrum.xlsx',sheet=3)
map <- map %>% mutate(Usage=ifelse(ProductLine%in%c('善存多种维生素多种矿物质片(女士)'),1,Usage))

benefits.label <- map2$`23.benefit`[!is.na(map2$`23.benefit`)][-1]
colnames(map) <- strsplit('id,category,brand,brande,productline,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit4,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,benefit17,benefit18,benefit19,benefit20,benefit21,benefit22,benefit23,country,chengfen,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}
map <- cbind(map %>% select(Brand=brand,BrandE=brande,ProductLine=productline,usage=usage2,ta,jixing,country,chengfen,leixing),benefits)

raw <- merge(out.ims,map,by=c('Brand','BrandE','ProductLine'))
paed.label <- unique((select(raw,Brand,ProductLine,TA,ta) %>% filter(TA=='PAEDIATRIC'))$ta)
raw$ta[is.na(raw$ta)] <- 'NA'
paed.label[is.na(paed.label)] <- 'NA'
raw$TA <- ifelse(raw$TA=='PAEDIATRIC',c('BABY','TEENAGER')[2-c(0,0,0,0,1,1,1,0,1,1,0,0,0,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,1,0)][match(raw$ta,paed.label)],raw$TA)

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% 
  mutate(Drugform=ifelse(grepl('CHEW',Package),'Chewable',Drugform))
colnames(data)[colnames(data)=='ta'] <- 'taref'
colnames(data) <- tolower(colnames(data))
data <- mutate(data,days=vol/usage,dayprice=val/days) %>% filter(!is.na(usage))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))
data <- data %>% select(date,brand,brande,productline,skuname,category,subcategory,ta,otchf,drugform,usage,country,chengfen,leixing,22:44,val,vol,pack,days) %>% mutate(avp=val/vol,adp=val/days,ta=ifelse(ta=='OTHERS','GENERAL',ta))
write.csv(data,'rlt3/raw_offline_centrum.csv')

# select(data,2:37) %>% unique() %>% filter(grepl('centrum',skuname,ignore.case=T)) %>% filter(usage==2)

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
  segment2 <- data %>% filter(grepl('centrum',brande,ignore.case=T)) %>% 
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
  rlts[[i-5]] <- list(rlt=rlti,decomp=segment3)
}
segmentation <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[1]])}))
segmentation <- segmentation %>% filter(segment %in% (segmentation %>% filter(brand>=20) %>% group_by(segment) %>% summarise(n=n()) %>% filter(n>1))$segment) %>% filter(!is.na(cluster))
decomp <- do.call(rbind,lapply(rlts,function(x){as.data.table(x[[2]])})) %>% filter(segment%in%segmentation$segment) %>% filter(!is.na(cluster))

write.csv(segmentation,'rlt3/segmentation_offline_centrum.csv')
write.csv(decomp,'rlt3/decomp_offline_centrum.csv')

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
  summarise(n=n(),val=sum(value),vol=sum(du),pack=sum(pack)) %>% filter(Category=='Vitamin')
dim(out.ims)

map <- read.xlsx('coding/online_centrum.xlsx',sheet=1)
map2 <- read.xlsx('coding/online_centrum.xlsx',sheet=3)
map$Usage[grep('善存复合维生素\\(女士型\\)',map$Skuname)] <- 1
map$Usage[grep('善存银片女性',map$Skuname)] <- 1

benefits.label <- map2$`23.benefit`[!is.na(map2$`23.benefit`)][-1]
colnames(map) <- strsplit('id,category,brand,productline,skuname,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit4,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,benefit17,benefit18,benefit19,benefit20,benefit21,benefit22,benefit23,country,chengfen,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}
map <- cbind(map %>% select(category=category,Brand=brand,ProductLine=productline,Skuname=skuname,usage=usage2,ta,jixing,country,chengfen,leixing),benefits)
raw <- merge(out.ims,map,by=c('Brand','ProductLine','Skuname'))
#filter(raw,ta=='婴(含3岁以下人群)'&TA!='Infant') %>% select(Brand,ProductLine,Skuname,TA,ta) %>% unique
raw$ta[is.na(raw$ta)] <- 'General'
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% select(1:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize) %>% filter(!is.na(jixing))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))
write.csv(data,'rlt3/raw_online_centrum.csv')

select(data,c(-4:-6,-40:-44)) %>% unique() %>% filter(grepl('善存',skuname,ignore.case=T)) %>% filter(usage==2)


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
  segment2 <- data %>% filter(grepl('善存',brand,ignore.case=T)) %>% 
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

write.csv(segmentation,'rlt3/segmentation_online_centrum.csv')
write.csv(decomp,'rlt3/decomp_online_centrum.csv')

########################################################
# Centrum Resulting
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
data <- fread('rlt3/raw_offline_centrum.csv')[,-1] %>% mutate(vol=days)
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000) %>% write.clip
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000) %>% write.clip

#Price Distribution
temp <- data %>% group_by(skuname,brand) %>% mutate(brand=ifelse(brand%in%c('善存'),brand,'Others')) %>%
  summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% arrange(avp)
temp$val <- cumsum(temp$val/sum(temp$val))
temp$vol <- cumsum(temp$vol/sum(temp$vol))
temp <- filter(temp,avp<=quantile(temp$avp,0.95))
temp <- data.table(
  avp=temp$avp,
  t(sapply(1:nrow(temp),function(i){
    x <- rep(-1,2)
    x[which(c('Others','善存')==temp$brand[i])] <- temp$vol[i]
    x
  })),
  val=temp$val,vol=temp$vol)
colnames(temp)[2:3] <- c('Others','Centrum')
temp %>% write.clip #Price Distribution

#Tier Summary

temp <- data %>% group_by(brand) %>% summarise(val=sum(val)) %>% arrange(desc(val))
temp$prop <- cumsum(temp$val)/sum(temp$val)
brand2 <- filter(temp,(prop<=0.72))$brand

temp <- data %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% mutate(tier=(avp>=1.08)+(avp>=2.28)+(avp>=3.72))
temp <- merge(data %>% group_by(brand=ifelse(brand%in%brand2,brand,'Others'),skuname,p=(date>=201906)+1) %>% summarise(val=sum(val)),
      temp %>% select(skuname,tier)) %>% group_by(brand,p,tier) %>% summarise(val=sum(val))
temp <- temp %>% acast(brand~paste(tier,p),value.var='val')
temp[match(c(brand2,'Others'),rownames(temp)),] %>% write.clip

#Segmentation Results

seg <- fread('rlt3/segmentation_offline_centrum.csv')[,-1]
decomp <- fread('rlt3/decomp_offline_centrum.csv')[,-1]

seg %>% mutate(idx=val,val=val-ifelse(is.na(tval),0,tval),adp=adp,tadp=tval/tdays) %>% select(segment,cluster,val,tval,adp,tadp,idx) %>% filter(segment %in% c('subcategory','ta','otchf','drugform','usage','country','leixing','benefits5','benefits22')) %>% select(segment,cluster,adp,val,tval,tadp,idx) %>% arrange(match(segment,segment),desc(idx)) %>%  write.clip #Price Summary

temp <- data %>% mutate(packsize=round(days/pack,0)) %>% 
  select(date,productline,packsize,val,days) %>% 
  group_by(date,productline,packsize) %>% 
  summarise(val=sum(val),days=sum(days),adp=val/days)
temp %>% filter(packsize<=quantile(temp$packsize,0.95)) %>% group_by(packsize=ceiling(packsize/10)*10) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% select(packsize,adp,val) %>% write.clip #Packsize Trend

#Price Map

temp <- data %>% filter(date>=201906) %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),pack=sum(pack),avp=val/vol)
temp <- unique(select(data,2:37)) %>% merge(temp,by='skuname')
write.clip(filter(temp,grepl('centrum',skuname,ignore.case=T)))

#Online

#Data Summary
data <- fread('rlt3/raw_online_centrum.csv')[,-1] %>% mutate(vol=days/1000,val=val/1000,pack=pack/1000)
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000) %>% write.clip
data %>% group_by(date) %>% summarise(val=sum(val)/1000,vol=sum(days)/1000,pack=sum(pack)/1000) %>% write.clip

#Price Distribution
temp <- data %>% group_by(skuname,brand) %>% mutate(brand=ifelse(grepl('善存',brand,ignore.case=T),brand,'Others')) %>%
  summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% arrange(avp)
temp$val <- cumsum(temp$val/sum(temp$val))
temp$vol <- cumsum(temp$vol/sum(temp$vol))
temp <- filter(temp,avp<=quantile(temp$avp,0.95))
temp <- data.table(
  avp=temp$avp,
  t(sapply(1:nrow(temp),function(i){
    x <- rep(-1,2)
    x[which(c('Others','善存/Centrum')==temp$brand[i])] <- temp$vol[i]
    x
  })),
  val=temp$val,vol=temp$vol)
colnames(temp)[2:3] <- c('Others','Centrum')
temp %>% write.clip #Price Distribution

#Tier Summary

temp <- data %>% group_by(brand) %>% summarise(val=sum(val)) %>% arrange(desc(val))
temp$prop <- cumsum(temp$val)/sum(temp$val)
brand2 <- filter(temp,(prop<=0.8041910))$brand

temp <- data %>% group_by(skuname) %>% summarise(val=sum(val),vol=sum(vol),avp=val/vol) %>% mutate(tier=(avp>=0.7)+(avp>=2.16)+(avp>=6.03))
temp <- merge(data %>% group_by(brand=ifelse(brand%in%brand2,brand,'Others'),skuname,p=(date>=201906)+1) %>% summarise(val=sum(val)),
              temp %>% select(skuname,tier)) %>% group_by(brand,p,tier) %>% summarise(val=sum(val))
temp <- temp %>% acast(brand~paste(tier,p),value.var='val')
temp[match(c(brand2,'Others'),rownames(temp)),] %>% write.clip

#Price map


temp <- data %>% filter(grepl('善存',brand)) %>% mutate(packsize=round(days/pack,0)) %>% group_by(brand,skuname,productline,packsize,channel) %>% summarise(val=sum(val),vol=sum(days),avp=val/vol) %>% arrange(brand,productline,packsize,channel)
temp %>% write.clip

temp <- data %>% filter(skuname%in%('善存佳维片(30)
善存佳维片(60)
善存佳维片(120)
善存佳维片(150)
善存千林多种维生素矿物质片(60)
善存银片女性(100)
善存银片(100)
善存银片女性(160)
善存银片男性(160)
善存银片女性(200)
善存银片男性(200)
善存银片女性(250)
善存银片男性(250)
善存银片女性(275)
善存复合维生素(女士型)(80)
善存复合维生素(男士型)(80)
善存复合维生素(女士型)(120)
善存复合维生素(男士型)(120)
善存复合维生素(女士型)(200)
善存复合维生素(男士型)(200)
善存复合维生素(女士型)(200)
善存复合维生素(家庭装)(425)
善存多维元素片(60)
善存多维元素片(100)
善存小佳维咀嚼片(香甜柠檬味)(40)
善存小佳维咀嚼片(香甜柠檬味)(80)
善存褪黑素维生素B6软胶囊(90)' %>% strsplit('\n') %>% unlist())) %>% mutate(packsize=round(days/pack,0))

temp <- eval(parse(text=paste0(
  'temp %>% group_by(',
  paste(c('brand','productline','skuname','packsize',unique((seg %>% filter(cluster%in%('DEC
CBEC
复合维生素
维生素C
维生素E
维生素A,D
维生素B
其他维生素
General
孕
婴(含3岁以下人群)
童(4~17岁人群)
男性
女性
中老年
片剂
软胶囊
液体剂
咀嚼片
粉剂
国产
进口
保健食品  
一般食品
OTC
Claim 缓解体力疲劳
NoClaim 缓解体力疲劳
Claim 改善生长发育
NoClaim 改善生长发育
Claim 增加骨密度
NoClaim 增加骨密度
Claim 改善皮肤
NoClaim 改善皮肤
Claim 补充维生素与矿物质
NoClaim 补充维生素与矿物质
Claim 补充能量 / 保持精力
NoClaim 补充能量 / 保持精力
Claim 增强免疫力
NoClaim 增强免疫力'%>%strsplit('\n')%>%unlist)))$segment %>% unlist())),
        collapse=','),
  ') %>% summarise(val=sum(val),vol=sum(vol),avp=val/vol)')))

write.clip(temp)

