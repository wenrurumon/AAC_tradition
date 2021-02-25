
rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(ggplot2)

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  print(x)
  write.table(x, file=clip, sep = ',')   
  close(clip)
}

# setwd('/Users/wenrurumon/Documents/zirui/ksf2021')
setwd('/Users/wenrurumon/Downloads/ksf2021')
raw <- fread('ksf2021.csv')
map <- read.xlsx("上轮模型参考.xlsx")
map$Decomp.Description[!map$Decomp.Description%in%colnames(raw)]
raw$MONTH <- sapply(strsplit(raw$MONTH,'/'),function(x){
  x <- as.numeric(x)
  x[1] * 10000 + x[2] * 100 + x[3]
})

nat <- function(x){tapply(x,raw$MONTH,sum)}
plot.ts(as.numeric(nat(raw$ksf_sales.vol)))
plot.ts(as.numeric(nat(raw$tea_category.vol)))
nat(raw$ksf_tv_total.grp)
sum(raw$ksf_sales.vol)/sum(raw$tea_category.vol)

map <- map[map$Decomp.Description%in%colnames(raw),]
mdata <- as.data.frame(raw)[,match(c('ksf_sales.vol',map$Decomp.Description),colnames(raw))]

####
# mdata$ksf_tv_total.grp <- log(mdata$ksf_tv_total.grp+1)
mdata$ksf_tv_total.grp <- mdata$ksf_tv_total.grp^0.85
# mdata$ksf_tv_total.grp <- mdata$ksf_tv_total.grp

####

desc <- mdata2 <- as.data.frame(apply(mdata,2,function(x){tapply(x,raw$MONTH,sum)}))
mdata2 <- apply(mdata2,2,function(x){c(sum(x[2:13]),sum(x[2:8]),sum(x[14:20]))})
mdata2 <- cbind(map[,-c(2:3,5)],t(mdata2[,-1]/mdata2[,1]))
mcoef <- mdata2[,5:7]/mdata2[,2:4]
b <- rowMeans(mcoef,na.rm=T)

mdata2[,2:4]
data.frame(mdata2$Decomp.Description,apply(mdata2[,5:7],2,function(x){x/b}))
colSums(apply(mdata2[,5:7],2,function(x){x/b}))

test <- apply(t(t(mdata[,-1])/b),2,nat) %>% as.data.frame
test <- apply(cbind(val=nat(raw$ksf_sales.val),vol=nat(raw$ksf_sales.vol),test),2,function(x){c(sum(x[2:13]),sum(x[14:25]),sum(x[2:12]),sum(x[14:24]),sum(x[26:36]))})
test[,-1:-2]/test[,2]
write.clip(test%>%t)

#######################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(openxlsx)
library(ggplot2)

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  print(x)
  write.table(x, file=clip, sep = ',')   
  close(clip)
}

setwd('/Users/wenrurumon/Downloads/ksf2021')
raw <- fread('ksf2021.csv')
raw$MONTH <- sapply(strsplit(raw$MONTH,'/'),function(x){
  x <- as.numeric(x)
  x[1] * 10000 + x[2] * 100 + x[3]
})

v2t <- 'ksf_sales.vol
ksf_tv_total.grp
ksf_tg_otv_total.imp
ksf_nat_otv_total.imp
ksf_tg_otv_ott.imp
ksf_digital.banner.imp
ksf_digital.others.imp
ksf_digital.info.imp
ksf_ip.support
ksf_social_total.spd
ksf_ooh.mspd
nat_teapie_tv.mspd
ksf.avp
ksf_sales.wd
avg_temprature'

v2t <- strsplit(v2t,'\n')[[1]]
raw$ksf_ip.support <- log(raw$ksf_ip.support+1)
raw$ksf_ip.support <- log(raw$ksf_ip.support+1)
fdata <- as.data.frame(
  apply(as.data.frame(raw)[,-2],2,function(x){tapply(x,raw$MONTH,sum)})
)
write.clip(raw <- apply(as.data.frame(raw)[,match(v2t,colnames(raw))],2,function(x){tapply(x,raw$MONTH,sum)}))
coefs <- as.data.frame(openxlsx::read.xlsx('本轮calc.xlsx',sheet=6))
ret <- function(x,r=0.3){
  x.raw<-x
  for(i in 2:length(x)){
    x[i]<-x[i-1]*r+x[i]
  }
  x/sum(x)*sum(x.raw)
}
raw[,2:11] <- apply(raw[,2:11],2,ret)
ksfwd <- (fdata[,grep('wd',colnames(fdata))]/rowSums(fdata[,grep('wd',colnames(fdata))]))[,1]
untwd <- (fdata[,grep('wd',colnames(fdata))]/rowSums(fdata[,grep('wd',colnames(fdata))]))[,2]

v2a <- c(2:11,12:13,15)
colnames(raw)[v2a]
decomp <- raw[,v2a,drop=F]
dcoefs <- coefs[v2a-1,]
for(i in 1: ncol(decomp)){
  decomp[1:13,i] <- dcoefs[i,2] * decomp[1:13,i]
  decomp[14:25,i] <- dcoefs[i,3] * decomp[14:25,i]
  decomp[26:36,i] <- dcoefs[i,4] * decomp[26:36,i]
}

decomp <- cbind(decomp,ksfwd=ksfwd*53395.44,untwd=untwd*-161616.42)
ho <- rowSums(decomp)
y <- raw[,1]-ho


model <- lm(y~
            +paste(rep(1:12,length=nrow(raw)))
            +rep(0:1,c(26,10))
          )
summary(model)
plot.ts(raw[,1]); lines(predict(model)+ho,col=2)

e <- raw[,1]-predict(model)-ho
# plot.ts(e <- colMeans(rbind(e,
#                        c(NA,e[-length(e)]),
#                        c(e[-1],NA),
#                        c(NA,NA,e[-(length(e)+0:-1)]),
#                        c(e[-1:-2],NA,NA)
#                        ),na.rm=T))

write.clip(cbind(base=predict(model),decomp,fit=predict(model)+rowSums(decomp),actual=raw[,1]))

##############





