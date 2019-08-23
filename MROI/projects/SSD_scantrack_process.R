# rm(list=ls())
# setwd('E:\\gsk\\SSD2019\\sales')
# library(data.table)
# library(dplyr)
# 
# # #Factor
# # ft <- fread('factor.csv\\factor.csv')
# # pc <- fread('periodmap.csv')
# # colnames(ft) <- c('store','month','z','y','x')
# # colnames(pc) <- c('week','month')
# # ft <- sqldf::sqldf('select a.store,b.week,a.z,a.y,a.x from pc b inner join ft a on a.month = b.month')
# # 
# # # SALES
# # raw <- fread('raw.csv\\raw.csv')
# # colnames(raw) <- strsplit('store,item,week,price,unit',',')[[1]]
# # raw2 <- filter(raw,week>=20170000)
# # ft2 <- filter(ft,week>=20170000)  %>% select(-y,-z) %>% mutate(x=ifelse(is.na(x),1,x))
# # key.raw <- paste(raw2$store,raw2$week)
# # key.ft <- paste(ft2$store,ft2$week)
# # key <- match(key.raw,key.ft)
# # rm(list=ls()[!ls()%in%c('raw2','ft2','key')])
# # key.ft2 <- ft2$x[key]
# # key.ft2[is.na(key.ft2)] <- 1
# # raw2$unit <- raw2$unit * key.ft2
# # write.csv(raw2,'psales2.csv',row.names=F)
# 
# #ITEM
# bundle <- readLines('bundle item list.csv')
# item <- fread('itemmaster.csv')
# subbrand <- fread('subbrand.csv',header=F)[,-2]
# colnames(subbrand) <- c('item','subbrand')
# item <- merge(item,subbrand,by='item')
# # target = strsplit('高露洁,黑人,冷酸灵,狮王,欧乐-B,舒客,舒适达,云南白药',',')[[1]]
# item$brand <- toupper(item$brand)
# item <- item %>% mutate(bundle=ifelse(item%in%bundle,'BUNDLE','NA'))
# cbrand <- unique(item$cbrand)
# l <- sort(table(unlist(select(item,seg1,seg2))))
# l[names(l)=='NA09'] <- Inf
# seg <- cbind(match(item$seg1,names(l)), match(item$seg2,names(l)))
# seg <- cbind(apply(seg,1,min),apply(seg,1,max))
# seg <- cbind(names(l)[seg[,1]],names(l)[seg[,2]])
# lcseg <- unique(rbind(as.matrix(unique(select(item,seg1,cseg1))),as.matrix(unique(select(item,seg2,cseg2)))))
# cseg <- cbind(lcseg[match(seg[,1],lcseg[,1]),2],lcseg[match(seg[,2],lcseg[,1]),2])
# item$seg1 <- seg[,1]
# item$seg2 <- seg[,2]
# item$cseg1 <- cseg[,1]
# item$cseg2 <- cseg[,2]
# item <- mutate(item,ppg=ifelse(is.na(subbrand),paste(paste(seg1),paste(seg2),sep='_'),paste(subbrand)))
# item <- mutate(item,ppg=paste(brand,ppg,size,sep="_"))
# item2 <- fread('ppgmaster.csv')
# item2 <- item2 %>% mutate(selection = (selection+(brand=='SENSODYNE'))>0 )
# item.target <- item$item
# item2.target <- filter(item2,selection==TRUE)$item
# target <- unique(item$cbrand)
# 
# #Sales  
# raw <- fread('psales2.csv')
# raw <- mutate(raw,val=unit*price) 
# sum(raw$val);dim(raw)
# sales <- filter(raw,item%in%item.target)
# sum(sales$val);dim(sales)
# sales <- filter(sales,item%in%item2.target)
# sum(sales$val);dim(sales)
# sales$ppg <- item$ppg[match(sales$item,item$item)]
# sales$pack <- item$pack[match(sales$item,item$item)]
# sales$size <- item$size[match(sales$item,item$item)]
# sales$brand <- item$brand[match(sales$item,item$item)]
# sales <- sales %>% mutate(vol=unit*pack*size)
# cat.sales <- raw %>% group_by(week) %>% summarise(val=sum(val),store=n_distinct(store))
# sales <- sales %>% group_by(ppg,brand,pack,store,week) %>% summarise(
#   val=sum(val),vol=sum(vol)
# )
# brand.sales <- sales %>% group_by(brand,week) %>% 
#   summarise(val=sum(val),vol=sum(vol),store=n_distinct(store))
# plot.ts(filter(brand.sales,brand=='SENSODYNE')$store/cat.sales$store)
# plot.ts(filter(brand.sales,brand=='SENSODYNE')$val/cat.sales$val)
# plot.ts((sales %>% group_by(week) %>% summarise(val=sum(val)))$val/cat.sales$val)

# write.csv(sales,'storetracking.csv',row.names=F)

#Output
rm(list=ls())
library(data.table)
library(dplyr)
setwd('E:\\BaiduNetdiskDownload')
sales <- fread('storetracking.csv')
w <- sort(unique(sales$week))
w2 <- rep(1:13,each=8)[-1]
w3 <- c(rep(1,3),rep(2:14,each=8))[1:103]
sales$m1 <- w2[match(sales$week,w)]
sales$m2 <- w3[match(sales$week,w)]

ssd.sales <- sales %>% group_by(brand,store,week) %>% summarise(
  ival=sum(val),ivol=sum(vol)
)
tt.sales <- ssd.sales %>% group_by(store,week) %>% summarise(
  cval=sum(ival),cvol=sum(ivol)
)
ssd.sales <- filter(ssd.sales,brand=='SENSODYNE')[,-1]
# sales <- filter(sales,brand=='SENSODYNE')
sales <- merge(sales,ssd.sales,by=c('store','week'))
sales <- merge(sales,tt.sales,by=c('store','week'))
sales <- select(sales,-brand) %>% mutate(
  avp = val/vol,
  cval = cval-ival, cvol = cvol-ivol, cvp = cval/cvol,
  ival = ival-val, ivol = ivol-vol, ivp = ival/ivol,
) %>% arrange(ppg,store,week)
ssales <- sales %>% group_by(store,ppg) %>% summarise(svol=mean(vol))
sales <- merge(sales,ssales,by=c('store','ppg')) %>% mutate(vol=vol/svol)
ppg <- (sales %>% group_by(ppg) %>% 
          summarise(val=sum(val),vol=sum(vol*svol),avp=val/vol*100) %>% 
          arrange(desc(val)))
rp <- sales %>% group_by(ppg,store,m1,m2) %>% summarise(rp=max(avp))
rp1 <- rp %>% group_by(ppg,store,m=m1) %>% summarise(rp=max(rp))
rp2 <- rp %>% group_by(ppg,store,m=m2) %>% summarise(rp=max(rp))
rp <- rbind(rp1,rp2) %>% group_by(ppg,store,m) %>% summarise(rp=max(rp))

k1 <- paste(sales$store,sales$ppg,sales$m1)
k2 <- paste(sales$store,sales$ppg,sales$m2)
kr <- paste(rp$store,rp$ppg,rp$m)
sales$rp <- apply(cbind(rp$rp[match(k1,kr)],rp$rp[match(k2,kr)]),1,max)
sales <- sales %>% mutate(pp = ifelse(avp/rp>=0.95,1,avp/rp))

map <- fread("store_MBD_map.csv")
colnames(map) <- c('store','channel')
sales <-merge(sales,map,by='store')
sales <- mutate(sales,brand=substr(ppg,1,regexpr("_",ppg)-1))

##################################


# test <- t(sapply(ppg$ppg,function(p){
#   x <- filter(sales,ppg==p) %>% select(
#     vol,rp,pp,avp,ivp,cvp,pack,week
#   ) %>% mutate(
#     vol=log(vol),pp=log(pp),rp=log(rp),avp=log(avp),ivp=log(ivp),cvp=log(cvp),pack=log(pack),ny=(week>20180000)+0
#   )
#   x[x==-Inf] <- NaN
#   x <- x[rowSums(is.na(x))==0,]
#   coef(lm(vol~rp+pp+ivp+cvp+pack+ny+1,data=x))
# }))
# head(test)
# 
# test <- lapply(ppg$ppg,function(p){
#   print(p)
#   x <- filter(sales,ppg==p) %>% select(
#     vol,rp,pp,avp,ivp,cvp,pack,week,svol
#   ) %>% mutate(
#     vol=log(vol*svol),svol=svol,pp=log(pp),rp=log(rp),avp=log(avp),ivp=log(ivp),cvp=log(cvp),pack=log(pack),ny=(week>20180000)+0
#   )
#   x[abs(x)==Inf] <- NaN
#   x <- x[rowSums(is.na(x))==0,]
#   if(nrow(x)==0){return(NULL)}
#   c(p,summary(lm(vol~.,data=x))$r.square)
# })
# head(test)
# 
# 
# head(ppg)
# write.csv(ppg,'clipboard')

# storemap <- cbind(store=unique(sales$store))
# weekmap <- cbind(week=sort(unique(sales$week)))
# brandmap <- cbind(brand=unique(sales$brand))
# sales$store <- match(sales$store,storemap)
# sales$week <- match(sales$week,weekmap)
# sales$brand <- match(sales$brand,brandmap)

pt <- sales %>% mutate(
  rp = ifelse(is.na(rp),avp,rp),week = (week>20180000)+2017,
  vol=vol*svol,rval=vol*rp,pp=ifelse(avp/rp<=0.95,1,0)) %>% select(
    store,ppg,week,val,vol,avp,rp,pp,rval,channel,brand
  ) %>% filter(vol>0)
pt <- pt %>% group_by(ppg,week,channel,brand) %>% summarise(
  freq=mean(pp),
  valoff = sum(val*(1-pp)), valon = sum(val*pp),
  voloff = sum(vol*(1-pp)), volon = sum(vol*pp),
  depth=(valon/volon)/(valoff/voloff),
  rp = (valoff/voloff)*100
)
write.csv(pt,'test.csv')

############

# x <- read.csv('test.csv')
x1 <- filter(x,!is.na(Hyper))
x2 <- filter(x,is.na(Hyper))
