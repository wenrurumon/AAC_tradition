
rm(list=ls())
setwd("/Users/wenrurumon/Documents/GSK/GSK2020/model")
library(data.table)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(reshape2)

write.clip <- function(x){
  print(head(x))
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')   
  close(clip)
}
rp <- function(x,r=2){
  if(length(x)<=(r*2+1)){
    return(rep(max(x),length(x)))
  } else {
    out <- rep(NA,length(x))
    for(i in (r+1):(length(out)-r)){
      out[i] <- max(x[i+(-r:r)])
    }
    out[1:r] <- max(x[1:(r*2)]) 
    out[length(out)-0:(r-1)] <- max(x[length(out)-0:(r*2-1)])
    return(out)
  }
}

################################################################################
# Import
################################################################################

#Sales Data

ec <- read.xlsx("ctims.xlsx",sheet=2)
raw <- read.xlsx("Centrum EC Raw Data_Mar.20.xlsx",sheet=3) %>% filter(SubCategory=="复合维生素"&Channel%in%c('DEC','CBEC'))
refshop <- (raw %>% filter(grepl('善存',Brandname)&Channel%in%c('DEC','CBEC')) %>% group_by(Channel,SubChannel,Shopname) %>% summarise(value=sum(`Value(RMB)`)) %>% arrange(desc(value)) %>% filter(grepl('jd|tmall',SubChannel,ignore.case=T)) %>% as.data.frame() %>% filter(Shopname!='others'))$Shopname[1:10]
raw$packsize <- sapply(strsplit(raw$Skuname,'\\(|\\)'),function(x){as.numeric(x[length(x)])})
colnames(raw) <- c('month','channel','subchannel','shopname','category','subcategory','brand','sku','value','pack','unit','ctsales','packsize')
raw <- raw %>% filter(month>=201801) %>% select(-ctsales)

ctsales <- raw %>% 
  filter(grepl('centrum',brand,ignore.case=T)&shopname%in%refshop&value*pack>0) %>% 
  mutate(avp=value/pack,key=paste(channel,subchannel,shopname,sku)) %>% 
  arrange(sku,shopname,subchannel,channel,month)

ctsales %>% 
  mutate(p=rep(1:5,c(6,6,6,6,3))[match(ctsales$month,unique(ctsales$month))]) %>% 
  filter(channel=='CBEC') %>%
  group_by(sku,p) %>% 
  summarise(value=sum(value),pack=sum(pack),avp=value/pack) %>% 
  acast(sku~p,value.var='avp') %>% 
  as.data.frame

ctsales %>% 
  mutate(p=rep(1:4,c(6,9,3,9))[match(ctsales$month,unique(ctsales$month))]) %>% 
  filter(channel=='CBEC') %>%
  group_by(sku,p) %>% 
  summarise(value=sum(value),pack=sum(pack),avp=value/pack) %>% 
  acast(sku~p,value.var='avp') %>% 
  as.data.frame

ctsales$rp <- unlist(lapply(unique(ctsales$key),function(k){rp(filter(ctsales,key==k)$avp,r=2)}))
ctsales <- ctsales %>% mutate(tprspd=(rp-avp)*pack) %>% 
  group_by(month,channel,subchannel) %>%
  summarise(value=sum(value),volume=sum(pack*packsize,na.rm=T),sku=n_distinct(sku),
            tprspd=sum(tprspd),rsales=sum(rp*pack)) %>%
  filter(grepl('jd|tmall',subchannel,ignore.case=T)) %>% 
  mutate(platform=ifelse(grepl('jd',subchannel,ignore.case=T),'JD','Tmall')) %>%
  group_by(month,channel,platform) %>%
  summarise(value=sum(value),volume=sum(volume),sku=sum(sku),
            rp=sum(rsales)/sum(volume),avp=sum(value)/sum(volume),tprspd=sum(tprspd)) %>% 
  arrange(channel,platform,month)
catsales <- merge(raw %>% group_by(month,channel) %>% summarise(channel_catval=sum(value)),
      raw %>% group_by(month) %>% summarise(catval=sum(value)),
      by='month')
ctsales <- merge(ctsales,catsales,by=c('month','channel')) %>% arrange(channel,platform,month)

#Media Data

raw <- fread("online_media.csv") %>% filter(target!='Caltrate')
raw$month <- raw$year*100+as.numeric(gsub('月','',raw$month))
ctmedia <- raw %>% 
  group_by(month,channel,platform,media,target) %>%
  summarise(spd=sum(spd),imp=sum(imp),click=sum(click)) %>%
  arrange(media,target,platform,channel,month) %>% 
  filter(target!='Caltrate')
massmedia <- fread("massmedia.csv")

################################################################################
# Model - CBEC
################################################################################

#Data

sdata <- filter(ctsales,channel=='CBEC') %>% 
  group_by(month) %>%
  summarise(value=sum(value),volume=sum(volume),sku=sum(sku),
            rp=sum(rp*volume)/sum(volume),avp=value/volume,tprspd=sum(tprspd),
            channel_catval=mean(channel_catval),catval=mean(catval))
mdata <- filter(ctmedia,channel=='CBEC') %>% 
  melt(id=c('month','channel','platform','media','target')) %>% 
  mutate(value=ifelse(target=='GSK',value/2,value)) %>% 
  group_by(month,channel,platform,media,variable) %>% 
  summarise(value=sum(value)) %>% 
  acast(month~media~variable,value.var='value')
mdata[is.na(mdata)] <- 0
temp <- apply(mdata,2:3,sum)
(temp <- data.table(media=rownames(temp),temp) %>% mutate(cpp=spd/click,cpm=spd/imp,imppc=imp/click))

#Prior

onsite <- mdata[,,1]
b.onsite <- 2.36/(sum(rowSums(onsite)*sdata$value/sdata$volume)/sum(onsite)*1.578164588)*0.75
b.onsite <- t(b.onsite * (1+cor(sdata$volume,mdata[,,3])))

mass <- as.matrix(massmedia[,-1])
b.mass <- 0.06/(sum(rowSums(mass)*sdata$value/sdata$volume)/sum(mass)*1.578164588)
b.mass <- t(b.mass * (1+cor(sdata$volume,mass)))

base <- sdata %>% select(rp,catval=channel_catval,tprspd) %>% as.matrix
b.base <- apply(base,2,function(x){coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~x))})[2,]
b.base <- coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~base %*% b.base))[2]*b.base

#Contribution Validation

b.base[ncol(base)] <- 0.4689/(rowSums(t(base) * b.base) / sum(sdata$volume))[ncol(base)] * b.base[ncol(base)] * 1.3
b0 <- cbind(c(
  intercept=coef(lm(I(sdata$volume-base%*%b.base-onsite%*%b.onsite-mass%*%b.mass)~1)),
  b.base,b.onsite,b.mass)
)
X <- cbind(intercept=1,base,onsite,mass)
plot.ts(sdata$volume); lines(X%*%b0,col=2)

#Base Optiization vs Residual

res <- sdata$volume - X%*%b0
sel <- c(1:3)
b.res <- apply(base[,sel],2,function(x){coef(lm(res~x))})[2,]
b0[1] <- b0[1] + coef(lm(res~I(base[,sel] %*% b.res)))[1]
b0[1+sel] <- b0[1+sel] + coef(lm(res~I(base[,sel] %*% b.res)))[2]*b.res
b0

#Optimization

loss <- function(b,b0=b0){
  b <- sign(b0)*abs(b)
  l1 <- sum((sdata$volume - X %*% b)^2)/sum((sdata$volume - X %*% b0)^2)
  l2 <- sum((b/b0-1)^2)
  l1+l2
}
b <- optim(b0,loss,b0=b0,control=list(maxit=20000))
b[[1]] <- sign(b0) * abs(b[[1]])
rownames(b[[1]]) <- colnames(X)
cbind(rowSums(t(X) * as.numeric(b[[1]])) / sum(sdata$volume))
plot.ts(sdata$volume); lines(X%*%b[[1]],col=2)
b[[1]]

tapply(colSums((t(X) * as.numeric(b[[1]]))[colnames(X) %in% ctmedia$media,]),rep(1:3,c(12,12,3)),sum)/
  tapply(sdata$volume,rep(1:3,c(12,12,3)),sum)
tapply(colSums((t(X))[colnames(X) %in% ctmedia$media,]),rep(1:3,c(12,12,3)),sum)/
  tapply(sdata$volume,rep(1:3,c(12,12,3)),sum)
tapply(rowSums(onsite),rep(1:3,c(12,12,3)),sum)

#Calculation

decomp <- as.data.table(t(t(X) * as.numeric(b[[1]]))) 
decomp<- data.table(month=sdata$month,val=sdata$value,vol=sdata$volume,decomp)
rlt.cbec <- list(
  decomp=decomp,
  support=X,
  coef=as.numeric(b[[1]])
)

#Appendix

temp <- sdata %>% group_by(period=rep(1:4,c(6,9,3,9))) %>% 
  summarise(value=sum(value),bvalue=sum(volume*rp),volume=sum(volume),
            avp=value/volume,rp=bvalue/volume)
(temp1 <- temp[4,]/temp[2,]-1)

temp <- sdata %>% group_by(period=rep(1:5,c(6,6,6,6,3))) %>% 
  summarise(value=sum(value),bvalue=sum(volume*rp),volume=sum(volume),
            avp=value/volume,rp=bvalue/volume)
(temp1 <- rbind(temp1,temp[4,]/temp[2,]-1))

temp <- sdata %>% group_by(period=rep(1:4,c(9,6,6,6))) %>% 
  summarise(value=sum(value),bvalue=sum(volume*rp),volume=sum(volume),
            avp=value/volume,rp=bvalue/volume)
(temp1 <- rbind(temp1,temp[4,]/temp[2,]-1))

################################################################################
# Model - DEC JD
################################################################################

#Data

sdata <- filter(ctsales,channel=='DEC'&platform=='JD') %>% 
  group_by(month) %>%
  summarise(value=sum(value),volume=sum(volume),sku=sum(sku),
            rp=sum(rp*volume)/sum(volume),avp=value/volume,tprspd=sum(tprspd),
            channel_catval=mean(channel_catval),catval=mean(catval))
mdata <- filter(ctmedia,channel=='DEC'&platform=='JD') %>% 
  melt(id=c('month','channel','platform','media','target')) %>% 
  mutate(value=ifelse(target=='GSK',value/2,value)) %>% 
  group_by(month,channel,platform,media,variable) %>% 
  summarise(value=sum(value)) %>% 
  acast(month~media~variable,value.var='value')
mdata[is.na(mdata)] <- 0
temp <- apply(mdata,2:3,sum)
(temp <- data.table(media=rownames(temp),temp) %>% mutate(cpp=spd/click,cpm=spd/imp,imppc=imp/click))

#Prior

onsite <- mdata[,,1]
b.onsite <- 4.57/(sum(rowSums(onsite)*sdata$value/sdata$volume)/sum(onsite)*1.350327471)*0.95
b.onsite <- t(b.onsite * (1+cor(sdata$volume,mdata[,,3])))

mass <- as.matrix(massmedia[,-1])
b.mass <- 0.04/(sum(rowSums(mass)*sdata$value/sdata$volume)/sum(mass)*1.350327471)
b.mass <- t(b.mass * (1+cor(sdata$volume,mass)))

base <- sdata %>% select(rp,catval=channel_catval,tprspd) %>% as.matrix
b.base <- apply(base,2,function(x){coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~x))})[2,]
b.base <- coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~base %*% b.base))[2]*b.base

#Contribution Validation

rowSums(t(base) * b.base) / sum(sdata$volume)
b.base[ncol(base)] <- 0.2739/(rowSums(t(base) * b.base) / sum(sdata$volume))[ncol(base)] * b.base[ncol(base)] * 1.350327471
b0 <- cbind(c(
  intercept=coef(lm(I(sdata$volume-base%*%b.base-onsite%*%b.onsite-mass%*%b.mass)~1)),
  b.base,b.onsite,b.mass)
)
X <- cbind(intercept=1,base,onsite,mass)
plot.ts(sdata$volume); lines(X%*%b0,col=2)

#Base Optiization vs Residual

res <- sdata$volume - X%*%b0
sel <- c(1:3)
b.res <- apply(base[,sel],2,function(x){coef(lm(res~x))})[2,]
b0[1] <- b0[1] + coef(lm(res~I(base[,sel] %*% b.res)))[1]
b0[1+sel] <- b0[1+sel] + coef(lm(res~I(base[,sel] %*% b.res)))[2]*b.res

#Optimization

loss <- function(b,b0=b0){
  b <- sign(b0)*abs(b)
  l1 <- sum((sdata$volume - X %*% b)^2)/sum((sdata$volume - X %*% b0)^2)
  l2 <- sum((b/b0-1)^2)
  l1+l2
}
b <- optim(b0,loss,b0=b0,control=list(maxit=20000))
b[[1]] <- sign(b0) * abs(b[[1]])
rownames(b[[1]]) <- colnames(X)
rowSums(t(X) * as.numeric(b[[1]])) / sum(sdata$volume)
plot.ts(sdata$volume); lines(X%*%b[[1]],col=2)

#Calculation

decomp <- as.data.table(t(t(X) * as.numeric(b[[1]]))) 
decomp<- data.table(month=sdata$month,val=sdata$value,vol=sdata$volume,decomp)
rlt.jd <- list(
  decomp=decomp,
  support=X,
  coef=as.numeric(b[[1]])
)

################################################################################
# Model - DEC TM
################################################################################

#Data

sdata <- filter(ctsales,channel=='DEC'&platform=='Tmall') %>% 
  group_by(month) %>%
  summarise(value=sum(value),volume=sum(volume),sku=sum(sku),
            rp=sum(rp*volume)/sum(volume),avp=value/volume,tprspd=sum(tprspd),
            channel_catval=mean(channel_catval),catval=mean(catval))
mdata <- filter(ctmedia,channel=='DEC'&platform=='TM') %>% 
  melt(id=c('month','channel','platform','media','target')) %>% 
  mutate(value=ifelse(target=='GSK',value/2,value)) %>% 
  group_by(month,channel,platform,media,variable) %>% 
  summarise(value=sum(value)) %>% 
  acast(month~media~variable,value.var='value')
mdata[is.na(mdata)] <- 0
temp <- apply(mdata,2:3,sum)
(temp <- data.table(media=rownames(temp),temp) %>% mutate(cpp=spd/click,cpm=spd/imp,imppc=imp/click))

#Prior

onsite <- mdata[,,1]
b.onsite <- 2.12/(sum(rowSums(onsite)*sdata$value/sdata$volume)/sum(onsite)*1.350327471)*0.67
b.onsite <- t(b.onsite * (1+cor(sdata$volume,mdata[,,3])))

mass <- as.matrix(massmedia[,-1])
b.mass <- 0.04/(sum(rowSums(mass)*sdata$value/sdata$volume)/sum(mass)*1.350327471)
b.mass <- t(b.mass * (1+cor(sdata$volume,mass)))

base <- sdata %>% select(rp,catval=channel_catval,tprspd) %>% as.matrix
b.base <- apply(base,2,function(x){coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~x))})[2,]
b.base <- coef(lm(I(sdata$volume-onsite%*%b.onsite-mass%*%b.mass)~base %*% b.base))[2]*b.base

#Contribution Validation

rowSums(t(base) * b.base) / sum(sdata$volume)
b.base[ncol(base)] <- 0.4512/(rowSums(t(base) * b.base) / sum(sdata$volume))[ncol(base)] * b.base[ncol(base)]*0.95
b0 <- cbind(c(
  intercept=coef(lm(I(sdata$volume-base%*%b.base-onsite%*%b.onsite-mass%*%b.mass)~1)),
  b.base,b.onsite,b.mass)
)
X <- cbind(intercept=1,base,onsite,mass)
plot.ts(sdata$volume); lines(X%*%b0,col=2)

#Base Optiization vs Residual

res <- sdata$volume - X%*%b0
sel <- c(1:3)
b.res <- apply(base[,sel],2,function(x){coef(lm(res~x))})[2,]
b0[1] <- b0[1] + coef(lm(res~I(base[,sel] %*% b.res)))[1]
b0[1+sel] <- b0[1+sel] + coef(lm(res~I(base[,sel] %*% b.res)))[2]*b.res
b0

#Optimization

loss <- function(b,b0=b0){
  b <- sign(b0)*abs(b)
  l1 <- sum((sdata$volume - X %*% b)^2)/sum((sdata$volume - X %*% b0)^2)
  l2 <- sum((b/b0-1)^2)
  l1+l2
}
b <- optim(b0,loss,b0=b0,control=list(maxit=20000))
b[[1]] <- sign(b0) * abs(b[[1]])
rownames(b[[1]]) <- colnames(X)
rowSums(t(X) * as.numeric(b[[1]])) / sum(sdata$volume)
plot.ts(sdata$volume); lines(X%*%b[[1]],col=2)

#Calculation

decomp <- as.data.table(t(t(X) * as.numeric(b[[1]]))) %>% mutate(catval=catval)
decomp<- data.table(month=sdata$month,val=sdata$value,vol=sdata$volume,decomp)
rlt.tm <- list(
  decomp=decomp,
  support=X,
  coef=as.numeric(b[[1]])
)

################################################################################
# Calculation
################################################################################

#Contribution Topline

rlts <- list(cbec=rlt.cbec,jd=rlt.jd,tm=rlt.tm)

temp <- lapply(rlts,function(rlt){
  decomp <- rlt$decomp;X <- rlt$support;b <- rlt$coef
  temp <- data.table(
    decomp %>% select(which(colnames(decomp)%in%c('vol',colnames(sdata)))),
    onsite=rowSums(decomp %>% select(which(colnames(decomp)%in%unique(ctmedia$media)))),
    mass=rowSums(decomp %>% select(which(colnames(decomp)%in%colnames(massmedia)[-1])))) %>% as.matrix
  con <- apply(temp[,-1],2,function(x){tapply(x,rep(1:3,c(12,12,3)),sum)})
  con[,colnames(con)%in%c('vol','sku','avp','rp','catval','tprspd','onsite','mass')]/con[,1]
})
do.call(rbind,temp) %>% write.clip

colSums(rlt.cbec$decomp %>% select(which(colnames(rlt.cbec$decomp) %in% unique(ctmedia$media))) *
          rlt.cbec$decomp$val/rlt.cbec$decomp$vol)/
colSums((rlt.cbec$support[,colnames(rlt.cbec$support)%in%unique(ctmedia$media)]))

#CBEC

write.clip(temp1)

rlt <- rlt.cbec
decomp <- rlt$decomp;support <- as.data.table(rlt$support);b <- rlt$coef

temp <- data.table(
  decomp %>% select(which(colnames(decomp)%in%c('val','vol',colnames(sdata)))),
  onsite=rowSums(decomp %>% select(which(colnames(decomp)%in%unique(ctmedia$media)))),
  mass=rowSums(decomp %>% select(which(colnames(decomp)%in%colnames(massmedia)[-1])))) %>% as.matrix
temp <- apply(temp[,-1],2,function(x){tapply(x,rep(1:5,c(6,6,6,6,3)),sum)})
cbind(c(val=temp[4,1]/temp[2,1]-1,(temp[4,-1]-temp[2,-1])/temp[2,2])) %>% write.clip

temp <- data.table(
  support %>% select(which(colnames(support)%in%c('val','vol',colnames(sdata)))),
  onsite=rowSums(support %>% select(which(colnames(support)%in%unique(ctmedia$media)))),
  mass=rowSums(support %>% select(which(colnames(support)%in%colnames(massmedia)[-1])))) %>% as.matrix
temp <- apply(temp,2,function(x){tapply(x,rep(1:5,c(6,6,6,6,3)),sum)})
cbind(temp[2,],temp[4,],temp[4,]/temp[2,]-1) %>% write.clip

#Calc

temp <- lapply(rlts,function(rlt){
  decomp <- rlt$decomp;X <- rlt$support;b <- rlt$coef
  driven <- data.table(
    tprspd=decomp$tprspd,
    decomp %>% select(which(colnames(decomp)%in%ctmedia$media)),
    decomp %>% select(which(colnames(decomp)%in%colnames(massmedia)[-1]))
  )
  sup <- X[,match(colnames(driven),colnames(X))]
  vol <- tapply(decomp$vol,rep(1:3,c(12,12,3)),sum)
  driven <- apply(driven,2,function(x){tapply(x,rep(1:3,c(12,12,3)),sum)})/as.numeric(vol)
  sup <- apply(sup,2,function(x){tapply(x,rep(1:3,c(12,12,3)),sum)})
  cbind(t(driven),t(sup))
})
for(i in 1:3){
  temp[[i]] <- data.table(
    model=names(temp)[i],media=rownames(temp[[i]]),temp[[i]]
  )
}
do.call(rbind,temp) %>% write.clip




