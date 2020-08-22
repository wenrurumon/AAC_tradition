
##########################################################################################
# Modeling
##########################################################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(sqldf)

write.clip <- function(x){
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

raw.ims %>% filter(grepl('caltrate',SKU.Name,ignore.case=T)) %>% group_by(
  Defined.Category,`OTC/HF`,Sub.Category,Brand,date=Year*100+Month
) %>% summarise(value=sum2(`CurPer.Sales.Val('000.RMB)`)) %>% arrange(desc(value))

########################################################
# Centrum Offline
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
  filter(Category!='Calcium')
dim(out.ims)

x <- filter(out.ims,Category!='Calcium')
map <- read.xlsx('coding/offline_centrum.xlsx',sheet=1)
map2 <- read.xlsx('coding/offline_centrum.xlsx',sheet=3)
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
unique(raw %>% filter(grepl('centrum',Skuname,ignore.case=T)) %>% select(ProductLine,usage))
raw %>% group_by(Drugform) %>% summarise(val=sum(val)/sum(raw$val))
data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(Drugform %in% c('Capsules','Coated Tablets','Tablets')) %>% select(1:4,7,5:6,8:10,16:43,13:15)
# data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% select(1:4,7,5:6,8:10,16:43,13:15) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum(filter(data,!is.na(usage))$val)
data <- mutate(data,days=vol/usage,dayprice=val/days) %>% filter(!is.na(usage))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in (6:38)){
  data$cluster <- data[[i]]
  temp <- data %>% group_by(date,brand,cluster) %>% 
    summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- optim(b0,loss,control=list(maxit=1000))$par}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- optim(b0,loss,control=list(maxit=1000))$par}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('centrum',brande,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-5]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_offline_centrum.csv')
write.csv(scurve,'rlt/scurve_offline_centrum.csv')

########################################################
# Caltrate Offline - Tablets
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
raw %>% group_by(Drugform) %>% summarise(value=sum(val)/sum(raw$val))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(Drugform %in% c('Capsules','Coated Tablets','Tablets')) %>% select(1:4,7,5:6,8:10,16:44,13:15) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum(filter(data,!is.na(usage))$val)
data <- mutate(data,days=vol/usage,dayprice=val/days) %>% filter(!is.na(usage))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in 6:39){
  data$cluster <- data[[i]]
  temp <- data %>% group_by(date,brand,cluster) %>% 
    summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- optim(b0,loss,control=list(maxit=1000))$par}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- optim(b0,loss,control=list(maxit=1000))$par}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('caltrate',brande,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-5]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  # x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
  x[[2]]}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_offline_caltrate_tablet.csv')
write.csv(scurve,'rlt/scurve_offline_caltrate_tablet.csv')

########################################################
# Caltrate Offline - Liquid
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
raw %>% group_by(Drugform) %>% summarise(value=sum(val)/sum(raw$val))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(Drugform %in% c('Liquids')) %>% select(1:4,7,5:6,8:10,16:44,13:15) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum(filter(data,!is.na(usage))$val)
data <- mutate(data,days=vol/usage,dayprice=val/days) %>% filter(!is.na(usage))
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in 6:39){
  data$cluster <- data[[i]]
  temp <- data %>% group_by(date,brand,cluster) %>% 
    summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_avp)=='try-error'){b_avp <- b0}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('caltrate',brande,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-5]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  # x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
  x[[2]]}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_offline_caltrate_liquid.csv')
write.csv(scurve,'rlt/scurve_offline_caltrate_liquid.csv')

########################################################
# Centrum Online - Tablets
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
raw %>% group_by(jixing) %>% summarise(val=sum(val)/sum(raw$val))
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(jixing %in% c('咀嚼片','片剂','软胶囊')) %>% select(1:6,7:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize)
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in (5:39)){
  data$cluster <- data[[i]]
  if(colnames(data)[i]=='channel'){
    temp <- data %>% group_by(date,brand,cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else {
    temp <- data %>% group_by(date,brand=paste(channel,brand),cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  }
  #Segment
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_avp)=='try-error'){b_avp <- b0}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('善存',brand,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-4]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_online_centrum_tablet.csv')
write.csv(scurve,'rlt/scurve_online_centrum_tablet.csv')

########################################################
# Centrum Online - Liquid
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
raw %>% group_by(jixing) %>% summarise(val=sum(val)/sum(raw$val))
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(jixing %in% c('液体剂')) %>% select(1:6,7:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize)
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in (5:39)){
  data$cluster <- data[[i]]
  if(colnames(data)[i]=='channel'){
    temp <- data %>% group_by(date,brand,cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else {
    temp <- data %>% group_by(date,brand=paste(channel,brand),cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  }
  #Segment
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_avp)=='try-error'){b_avp <- b0}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('善存',brand,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-4]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_online_centrum_liquid.csv')
write.csv(scurve,'rlt/scurve_online_centrum_liquid.csv')

########################################################
# Caltrate Online - Tablets
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

######

benefits.label <- c(map2$`Ingredients/benefit`[-1],map2$Claim[-1])
benefits.label <- benefits.label[!is.na(benefits.label)]
colnames(map) <- strsplit('id,category,brand,productline,skuname,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit5,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,claim1,claim2,claim3,claim4,claim5,claim6,claim7,country,flavor,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit|claim',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}
map <- cbind(map %>% select(category=category,Brand=brand,ProductLine=productline,Skuname=skuname,usage=usage2,ta,jixing,country,flavor,leixing),benefits)
raw <- merge(out.ims,map,by=c('Brand','ProductLine','Skuname'))
# filter(raw,ta=='婴(含3岁以下人群)'&TA!='Infant') %>% select(Brand,ProductLine,Skuname,TA,ta) %>% unique
raw$ta[is.na(raw$ta)] <- 'General'
raw %>% group_by(jixing) %>% summarise(val=sum(val)/sum(raw$val))
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(jixing %in% c('咀嚼片','片剂','软胶囊')) %>% select(1:6,7:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize)
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in (5:39)){
  data$cluster <- data[[i]]
  if(colnames(data)[i]=='channel'){
    temp <- data %>% group_by(date,brand,cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else {
    temp <- data %>% group_by(date,brand=paste(channel,brand),cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  }
  #Segment
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_avp)=='try-error'){b_avp <- b0}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('钙尔奇',brand,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-4]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_online_caltrate_tablet.csv')
write.csv(scurve,'rlt/scurve_online_caltrate_tablet.csv')

########################################################
# Caltrate Online - Tablets
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

######

benefits.label <- c(map2$`Ingredients/benefit`[-1],map2$Claim[-1])
benefits.label <- benefits.label[!is.na(benefits.label)]
colnames(map) <- strsplit('id,category,brand,productline,skuname,value,usage,usage2,ta,jixing,benefit1,benefit2,benefit3,benefit5,benefit5,benefit6,benefit7,benefit8,benefit9,benefit10,benefit11,benefit12,benefit13,benefit14,benefit15,benefit16,claim1,claim2,claim3,claim4,claim5,claim6,claim7,country,flavor,leixing',',')[[1]]
benefits <- apply(map[,grepl('benefit|claim',colnames(map))],1,function(x){x[!is.na(x)]})
benefits <- lapply(benefits,function(x){benefits.label%in%x})
benefits <- as.data.table(do.call(rbind,benefits))
colnames(benefits) <- paste0('benefits',1:ncol(benefits))
for(i in 1:ncol(benefits)){
  benefits[[i]] <- ifelse(benefits[[i]],paste('Claim',benefits.label[i]),paste('NoClaim',benefits.label[i]))
}
map <- cbind(map %>% select(category=category,Brand=brand,ProductLine=productline,Skuname=skuname,usage=usage2,ta,jixing,country,flavor,leixing),benefits)
raw <- merge(out.ims,map,by=c('Brand','ProductLine','Skuname'))
# filter(raw,ta=='婴(含3岁以下人群)'&TA!='Infant') %>% select(Brand,ProductLine,Skuname,TA,ta) %>% unique
raw$ta[is.na(raw$ta)] <- 'General'
raw %>% group_by(jixing) %>% summarise(val=sum(val)/sum(raw$val))
raw$packsize <- as.numeric(sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){x[length(x)]}))

data <- raw %>% filter((vol>0)&(!is.na(Brand))) %>% filter(jixing %in% c('液体剂')) %>% select(1:6,7:10,15:44,12:14,45) %>% mutate(vol=pack*packsize) 
colnames(data)[colnames(data)=='ta'] <- 'taref'
data <- data %>% select(-category)
colnames(data) <- tolower(colnames(data))

sum(data$val)
sum((data %>% filter(!is.na(usage)))$val)
data <- data %>% filter(!is.na(usage)) %>% mutate(days=vol/usage,dayprice=val/days) %>% select(-packsize)
data %>% group_by(brand) %>% summarise(val=sum(val),days=sum(days),vol=sum(vol),
                                       dayprice=val/days,duprice=val/vol) %>% arrange(desc(val))

#Brand Importance

rlts <- list()
for(i in (5:39)){
  data$cluster <- data[[i]]
  if(colnames(data)[i]=='channel'){
    temp <- data %>% group_by(date,brand,cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  } else {
    temp <- data %>% group_by(date,brand=paste(channel,brand),cluster) %>% 
      summarise(sku=n_distinct(skuname),val=sum(val),vol=sum(vol),days=sum(days))
  }
  #Segment
  #Segment
  segment <- temp %>% 
    group_by(cluster) %>% 
    summarise(
      sku=sum(sku),brand=n_distinct(brand),
      val=sum(val),vol=sum(vol),days=sum(days),avp=sum(val)/sum(vol),adp=sum(val)/sum(days)
    )
  segment$avpidx <- segment$avp/min(segment$avp)
  segment$adpidx <- segment$adp/min(segment$adp)
  scurve <- data %>% group_by(skuname,cluster) %>% summarise(val=sum(val),days=sum(days),adp=val/days) %>% arrange(adp)
  scurve <- filter(scurve,prod(adp-quantile(scurve$adp,c(0.05,0.95)))<=0)
  scurve <- scurve %>% mutate(valpct=val/sum(scurve$val),dayspct=days/sum(scurve$days))
  scurve$valpct <- cumsum(scurve$valpct); scurve$dayspct <- cumsum(scurve$dayspct)
  scurve <- data.table(segment=colnames(data)[i],scurve)
  #GAP Modeling
  temp <- merge(temp,temp,by=c('date','brand')) %>% 
    group_by(brand,cluster.x,cluster.y) %>%
    summarise(val.x=sum(val.x),val.y=sum(val.y),vol.x=sum(vol.x),vol.y=sum(vol.y),
              days.x=sum(days.x),days.y=sum(days.y))
  gaps <- temp %>% 
    merge(temp %>% group_by(cluster.x) %>% summarise(w.x=sum(val.x)),by='cluster.x') %>% 
    mutate(w.x=val.x/w.x) %>% 
    merge(temp %>% group_by(cluster.y) %>% summarise(w.y=sum(val.y)),by='cluster.y') %>%
    mutate(w.y=val.y/w.y) %>% 
    group_by(cluster.y,cluster.x) %>%
    summarise(avpgap=sum((val.y/vol.y)/(val.x/vol.x)*(w.x*w.y))/sum(w.x*w.y),
              adpgap=sum((val.y/days.y)/(val.x/days.x)*(w.x*w.y))/sum(w.x*w.y),
              w=sum(w.x*w.y))
  #b_adp
  b0 <- segment$adp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$adpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$adpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_adp <- 1} else {b_adp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_adp)=='try-error'){b_adp <- b0}
  #b_avp
  b0 <- segment$avp; names(b0) <- segment$cluster
  b.y <- match(gaps$cluster.y,names(b0))
  b.x <- match(gaps$cluster.x,names(b0))
  loss <- function(b){
    l1 <- sum((b[b.y]/b[b.x] - gaps$avpgap)^2 * gaps$w)/sum((b0[b.y]/b0[b.x] - gaps$avpgap)^2 * gaps$w)
    l2 <- sum((b/b0-1)^2*segment$val/sum(segment$val))
    l1+l2
  }
  if(length(b0)==1){b_avp <- 1} else {b_avp <- try(optim(b0,loss,control=list(maxit=1000))$par)}
  if(class(b_avp)=='try-error'){b_avp <- b0}
  b <- cbind(avprlt=b_avp/min(b_avp),adprlt=b_adp/min(b_adp))
  #
  segment_ct <- data %>% filter(grepl('钙尔奇',brand,ignore.case=T)) %>% 
    group_by(cluster) %>% summarise(ctval=sum(val),ctvol=sum(vol),ctdays=sum(days),
                                    ctavp=sum(val)/sum(vol),ctadp=sum(val)/sum(days))
  segment_ct$ctavpidx <- segment_ct$ctavp/min(segment_ct$ctavp)
  segment_ct$ctadpidx <- segment_ct$ctadp/min(segment_ct$ctadp)
  segment <- cbind(segment,b)
  #Resulting
  rlti <- 
    data.table(segment=colnames(data)[i],
               sqldf('select a.*, b.* from segment a 
                     left join segment_ct b on a.cluster = b.cluster')) %>% select(-cluster..13)
  rlts[[i-4]] <- list(scurve=scurve,segmentation=rlti)
}

segmentation <- do.call(rbind,lapply(rlts,function(x){
  x[[2]] %>% select(segment,cluster,sku,brand,val,days,adp,adpidx,adprlt,ctval,ctdays,ctadp,ctadpidx)}
))
scurve <- do.call(rbind,lapply(rlts,function(x){x[[1]]}))

write.csv(segmentation,'rlt/segmentation_online_caltrate_liquid.csv')
write.csv(scurve,'rlt/scurve_online_caltrate_liquid.csv')
