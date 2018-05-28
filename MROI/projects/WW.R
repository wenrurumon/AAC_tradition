rm(list=ls())
library(data.table)
# par(mfrow=c(3,2))
setwd("/Users/wenrurumon/Documents/GSK/model")
library(openxlsx)
library(dplyr)
f <- function(x,i){
  x <- try(read.xlsx(x,i))
  print(i)
  if(class(x)=="try-error"){
    print('close')
    return(NA)
  }else{
    x
  }
}
databtb <- lapply(1:17,function(i){
  f(dir(pattern='_model_data')[1],i)
})
datavtr <- lapply(1:13,function(i){
  f(dir(pattern='_model_data')[2],i)
})
xbtb <- lapply(databtb,function(x){
  s <- which(is.na(x[,1]))
  map <- t(x[c(s,max(s)+1),])
  coln <- x[max(s)+1,]
  colnames(x) <- coln
  key <- x[-c(s,s+1),]
  key <- arrange(key,city,month)
  data <- key[,-1:-2]
  data <- apply(data,2,as.numeric)
  list(map=map,data=data)
})
xvtr <- lapply(datavtr,function(x){
  s <- which(is.na(x[,1]))
  map <- t(x[c(s,max(s)+1),])
  coln <- x[max(s)+1,]
  colnames(x) <- coln
  key <- x[-c(s,s+1),]
  key <- arrange(key,city,month)
  data <- key[,-1:-2]
  data <- apply(data,2,as.numeric)
  list(map=map,data=data)
})
names(xbtb) <- getSheetNames(dir(pattern='_model_data')[1])
names(xvtr) <- getSheetNames(dir(pattern='_model_data')[2])

keyvtr <- datavtr[[1]][,1:2]
keyvtr <- keyvtr[!is.na(keyvtr[,1]),][-1,]
xvtr <- lapply(xvtr,function(x){
  map <- x$map
  data <- x$data[as.numeric(keyvtr[,2])>42339,]
  list(map=map,data=data)
})
keyvtr <- keyvtr[as.numeric(keyvtr[,2])>42339,]

keybtb <- databtb[[1]][,1:2]
keybtb <- keybtb[!is.na(keybtb[,1]),][-1,]
xbtb <- lapply(xbtb,function(x){
  map <- x$map
  data <- x$data[as.numeric(keybtb[,2])>=42005,]
  list(map=map,data=data)
})
keybtb <- keybtb[as.numeric(keybtb[,2])>=42005,]

#################
# Voltaren
# 313313 as total shipment value
#################

tm <- function(x,m=m.dum){
  as.numeric(tapply(x,m,sum))
}
ret <- function(x,d=c.dum,r=0.3){
  for(i in 2:length(x)){
    if(c.dum[i]==c.dum[i-1]){
      x[i] <- x[i-1] * r + x[i]
    }
  }
  x
}
lag <- function(x,d=c.dum,l=3){
  x2 <- rep(0,length=length(x))
  for(i in (l+1):length(x)){
    if(c.dum[i]==c.dum[i-l]){
      x2[i] <- x[i-l]
    }
  }
  return(x2)
}

check <- function(x,y,pf=1,spd=1){
  # x <- vtr.grp1*7.762889e-02+vtr.grp2*1.096288e-01
  # y <- ve.vol
  sales <- sum(x)
  pie <- sales / sum(y)
  roi <- (sales * pf) / spd
  list(pie=pie,roi=roi,sc=round(tm(x)/tm(y),4))
}

plot.tm <- function(x,col=1){plot.ts(tm(x),col=col)}
lines.tm <- function(x,col=1){lines(tm(x),col=col)}

##################################################################
##################################################################
#################
# Bactroban
#################

c.dum <- keybtb[,1]
m.dum <- rep(1:36,length=length(c.dum))
m.dum2 <- rep(1:12,length=length(c.dum))
y.dum <- rep(rep(1:3,each=12),length=length(c.dum))
y1 <- as.numeric(y.dum==1);y2 <- as.numeric(y.dum==2);y3 <- as.numeric(y.dum==3)
br.sales <- xbtb$BTB_sales$data[,grep('VAL',colnames(xbtb$BTB_sales$data))]
br.catval <- rowSums(br.sales)
btb.sales <- xbtb$BTB_sales$data[,grep('BAC',colnames(xbtb$BTB_sales$data))]
ww.nd <- btb.sales[,1]
ww.vol <- btb.sales[,2]
ww.val <- btb.sales[,3]
ww.wd <- btb.sales[,4]
o10.nd <- btb.sales[,5]
o10.vol <- btb.sales[,6]
o10.val <- btb.sales[,7]
o10.wd <- btb.sales[,8]
o5.nd <- btb.sales[,9]
o5.vol <- btb.sales[,10]
o5.val <- btb.sales[,11]
o5.wd <- btb.sales[,12]
ww.avp <- ww.val/ww.vol; o10.avp <- o10.val/o10.vol; o5.avp <- o5.val/o5.vol
ww.avp <- ifelse(is.na(ww.avp),0,ww.avp)
o10.avp <- ifelse(is.na(o10.avp),0,o10.avp)
o5.avp <- ifelse(is.na(o5.avp),0,o5.avp)
hos.val <- lag(rowSums(xbtb$Hospital$data[,grep('BAC',colnames(xbtb$Hospital$data))[c(3,6)]]),1)
hh <- paste(c.dum,rep(rep(1:3,each=12),length=length(c.dum)))
ohh <- rep(tapply(o10.vol+o5.vol,hh,sum),each=12)
whh <- rep(tapply(ww.vol,hh,sum),each=12)
ww.catval <- xbtb$BTB_sales$data[,grepl('wwcatval',colnames(xbtb$BTB_sales$data))]
pdb.data <- xbtb$BTB_sales$data[,grep('PI DE BANG',colnames(xbtb$BTB_sales$data),value=T)]
pdb.val <- pdb.data[,grepl('VAL',colnames(pdb.data))]
x <- xbtb$BTB_sales$data[,grep('RYTHROMYCIN',colnames(xbtb$BTB_sales$data),value=T)]
hys.val <- x[,3]
hys.wd <- x[,4]

#Process ATL
o.grp <- rowSums(xbtb$`TV and SPONSORSHIP`$data[,grep('BTB',colnames(xbtb$`TV and SPONSORSHIP`$data))])
o.grp1 <- ret(o.grp*y1,c.dum,0.3)
o.grp2 <- ret(o.grp*y2,c.dum,0.3)
o.grp3 <- ret(o.grp*y3,c.dum,0.3)
o.grp <- o.grp1 + o.grp2 + o.grp3
ww.grp <- ret(rowSums(xbtb$`TV and SPONSORSHIP`$data[,grep('WW',colnames(xbtb$`TV and SPONSORSHIP`$data))]),c.dum,0.3)
ar.grp <- ret(xbtb$`TV and SPONSORSHIP`$data[,1],c.dum,0.3)
o.igrp <- ret(rowSums(xbtb$otv$data[,grepl('oint',colnames(xbtb$otv$data))&grepl('GRP',colnames(xbtb$otv$data))]),c.dum,0.3)
w.igrp <- ret(rowSums(xbtb$otv$data[,grepl('WW',colnames(xbtb$otv$data))&grepl('GRP',colnames(xbtb$otv$data))]),c.dum,0.3)
x <- xbtb$`digital(Banner)`$data[,grepl('Impressions',colnames(xbtb$`digital(Banner)`$data))]
w.nmimp <- ret(rowSums(x[,c(10,13)]),c.dum,0.3)
btb.nmimp <- ret(rowSums(x[,14:15]),c.dum,0.3)
o.bimp <- ret(
  rowSums(x[,(!grepl('OTV',colnames(x)))&(grepl('Oint',colnames(x)))])+
    rowSums(x[,(!grepl('OTV',colnames(x)))&(grepl('Oint',colnames(x)))]) * y1 * 0.8
  ,c.dum,0.3)
w.bimp <- ret(rowSums(x[,(!grepl('OTV',colnames(x)))&(grepl('ww',colnames(x)))]),c.dum,0.3)
x <- xbtb$`digital（Social）`$data[,grep('Impression',colnames(xbtb$`digital（Social）`$data))]
o.weibo <- ret(rowSums(x[,grepl('OINT_Weibo',colnames(x)),drop=F]),c.dum,0.5)
w.weibo <- ret(rowSums(x[,grepl('WW_Weibo',colnames(x)),drop=F]),c.dum,0.3)
o.wechat <- ret(rowSums(x[,grepl('OINT_Wechat',colnames(x)),drop=F]),c.dum,0.3)
w.wechat <- ret(rowSums(x[,grepl('WW_Wechat',colnames(x)),drop=F]),c.dum,0.3)
btb.weibo <- ret(rowSums(x[,grepl('BTBBOTH_Weibo',colnames(x)),drop=F]),c.dum,0.5)
btb.wechat <- ret(rowSums(x[,grepl('BTBBOTH_Wechat',colnames(x)),drop=F]),c.dum,0.3)
x[rep(rep(1:3,each=12),length=length(c.dum))==1,] <- 0
o.social <- ret(rowSums(x[,grepl('OINT_Weibo',colnames(x)),drop=F]),c.dum,0.3)
ww.social <- ret(rowSums(x[,grepl('WW',colnames(x)),drop=F]),c.dum,0.3)
btb.social <- ret(rowSums(x[,grepl('BOTH',colnames(x)),drop=F]),c.dum,0.3)
x <- xbtb$`digital(SEM)`$data[,grepl('Click',colnames(xbtb$`digital(SEM)`$data))]
o.sem <- ret(rowSums(x[,grepl('软膏',colnames(x)),drop=F]),c.dum,0.3)
o.sem3 <- ret(rowSums(x[,grepl('软膏',colnames(x)),drop=F])*y3,c.dum,0.3)
ww.sem <- ret(rowSums(x[,grepl('喷雾',colnames(x)),drop=F]),c.dum,0.3)
ww.sem3 <- ret(rowSums(x[,grepl('喷雾',colnames(x)),drop=F])*y3,c.dum,0.3)
btb.sem <- ret(rowSums(x[,grepl('Master',colnames(x)),drop=F]),c.dum,0.3)
btb.sem3 <- ret(rowSums(x[,grepl('Master',colnames(x)),drop=F])*y3,c.dum,0.3)
x <- xbtb$posm_0410$data[,grepl('item',colnames(xbtb$posm_0410$data))]
o.2dp <- x[,grepl('Oint_Display_二次',colnames(x))]
w.2dp <- rowSums(x[,grepl('WW_Display_二次',colnames(x))])
o.ms <- x[,grepl('Oint_Display_主货架',colnames(x))]
w.ms <- x[,grepl('WW_Display_主货架',colnames(x))]
# o.rmd2c <- x[,grepl('Oint_Display_面向消费者',colnames(x)),drop=F]*y1
w.rmd2c <- rowSums(x[,grepl('Display_面向消费者',colnames(x)),drop=F]*y1)
o.rmd2b <- x[,grepl('Oint_Display_面向门店',colnames(x)),drop=F]*y1
w.rmd2b <- x[,grepl('WW_Display_面向门店',colnames(x)),drop=F]*y1
x <- xbtb$reminder$data
w.rmd2c <- ret(lag(w.rmd2c + rowSums(x[,grepl('面向消费者',colnames(x)),drop=F]),c.dum,0),c.dum,0.3)
o.rmd2b <- ret(lag(o.rmd2b + rowSums(x[,grepl('Onit_KASC_面向门店',colnames(x)),drop=F]),c.dum,1),c.dum,0.3)
w.rmd2b <- ret(lag(w.rmd2b + rowSums(x[,grepl('WW_KASC_面向门店',colnames(x)),drop=F]),c.dum,1),c.dum,0.3)
o10.app <- xbtb$APP$data[,2]
o5.app <- xbtb$APP$data[,3]
o.yxt <- xbtb$Traning$data[,1]
ww.yxt <- xbtb$Traning$data[,3]
x <- xbtb$KASC$data[,grep('门店数',colnames(xbtb$KASC$data))]
o.kasc <- ret(rowSums(x[,1:7]),c.dum,0.3)
w.kasc <- ret(rowSums(x[,9:12]),c.dum,0.3)
o.oos <- xbtb$OOS$data[,1]
w.oos <- xbtb$OOS$data[,2]
o.kasc2 <- ret(rowSums(xbtb$kasc2$data),c.dum,0.3)
o.kasc <- o.kasc2 + o.kasc
x <- xbtb$BTB_sales$data
x <- x[,grepl('PI DE',colnames(x))&grepl('VAL',colnames(x))]
x <- rowSums(x)

smi <- function(x,t=1){
  for(i in 1:t){
    x <- apply(cbind(c(x[-1],NA),x,c(NA,x[-length(x)])),1,mean,na.rm=T)  
  }
  return(x)
}
sm <- function(x,t=1){
  x <- lapply(unique(c.dum),function(c){
    smi(x[c.dum==c],t)
  })
  return(do.call(c,x))
}

ww.wd_sm <- sm(ww.wd,2)
fes.dum <- rep(rep(c(0,1,0,1,0,1,0),c(7,1,10,1,11,1,5)),length=length(ww.vol))

#######################
#Wound Wash
#64984/sum(ww.vol);2.904578
#Pending KACC
#######################

check(o.grp * 0.007027743, ww.vol, 2.904578, 93600)
check(ww.grp * 0.015693721, ww.vol*y3, 2.904578, 18500)
check(o.igrp * 0.06262665, ww.vol, 2.904578, 12700)
check(w.igrp * 0.02116713, ww.vol*y3, 2.904578, 10100)
check(w.nmimp * 0.91274e-08, ww.vol*y3, 2.904578, 4780)
check(btb.nmimp * 0.3034e-08, ww.vol*y3, 2.904578, 970)
check(ww.sem * 2.237401e-05, ww.vol, 2.904578, 12300)
check(w.weibo * 1.774524e-08,ww.vol*(y2+y3),2.904578, 46696.560)
check(w.wechat * 2.092103e-08,ww.vol*(y2+y3),2.904578, 1133.035)
check(w.bimp * 3.911974e-09, ww.vol,2.904578,4620)
check(w.2dp * 1.221433e-03, ww.vol, 2.904578,1740)
check(w.ms * 4.358183e-05, ww.vol, 2.904578,2220)
check(w.rmd2c*2.509272e-04+w.rmd2b*5.113741e-04,ww.vol,2.904578,10260)
check(ww.yxt*0.006870075,ww.vol,1,1)
check(w.kasc*0.001388341,ww.vol,1,1)
check(btb.weibo*1.223381e-07+btb.wechat*0.788291e-07,ww.vol,2.904578,725)

hold <- rowSums(
  cbind(   o.grp = o.grp * 0.003027743 + o.grp1 * 1.590168e-04
           ,ww.grp = (ww.grp * 0.015693721) * 0.8 * 1.2
           ,o.igrp = (o.igrp * 0.06262665) / 6
           ,w.igrp = w.igrp * 0.02116713 * 2
           ,w.nmimp = (w.nmimp * 0.91274e-08) * 2 * 2.25
           ,btb.nmimp = btb.nmimp * 0.3034e-08
           ,ww.sem = (ww.sem * (y3) * 2.49956e-06 + ww.sem * 9.24934e-06)/2
           ,w.weibo = (w.weibo * 2.774524e-08 - w.weibo*y2*2.01e-08)/2
           ,w.wechat = (w.wechat * 2.092103e-08) * 20
           ,w.bimp = w.bimp * 3.911974e-09 * 2
           ,w.ms = w.ms * 4.358183e-05
           ,w.2dp = w.2dp * (y1 * 1.221433e-03 + y2 * .821433e-03 + y3 * 1.821433e-03)
           ,ww.yxt = ww.yxt*0.006870075
           ,w.rmd = w.rmd2c*.309272e-03+w.rmd2b*1.113741e-04+y1*w.rmd2b*1.113741e-04
           ,w.kasc = w.kasc*0.0001388341 + 0.0001388341*1.8 * w.kasc * y2
           ,hys.val = hys.val * -0.005214611
           ,w.oos = w.oos * -0.017027936
           ,btb.weibo = btb.weibo*1.223381e-07
           ,btb.wechat = btb.wechat*0.788291e-07
           ,ww.avp = ww.avp *  -.010001073
  ))
hold_ww <- hold
# coef(lm(ww.vol~w.bimp+paste(c.dum, m.dum2)-1))[1:3]
# ww.wd_mc <- ww.wd / rep(tapply(ww.wd,c.dum,mean),each=sum(c.dum==c.dum[[1]]))
# ww.wd_mc <- rep(tapply(ww.wd,paste(c.dum,y.dum),mean),each=12)/rep(tapply(ww.wd,c.dum,mean),each=36)
ww.vol2 <- ww.vol - hold
coef(summary(xlm <- lm(ww.vol2~
                         +ww.wd
                       + fes.dum
                       + paste(c.dum) + paste(m.dum2) -1
                       # + paste(c.dum,m.dum2) -1
)))[1:10,];
plot.ts(as.numeric(tapply(ww.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

decomp <- as.data.frame(apply(cbind(
  o.grp = o.grp * 0.003027743 + o.grp1 * 1.590168e-04
  ,ww.grp = (ww.grp * 0.015693721) * 0.8 * 1.2
  ,o.igrp = (o.igrp * 0.06262665) / 6
  ,w.igrp = w.igrp * 0.02116713 * 2
  ,w.nmimp = (w.nmimp * 0.91274e-08) * 2 * 2.25
  ,btb.nmimp = btb.nmimp * 0.3034e-08
  ,ww.sem = (ww.sem * (y3) * 2.49956e-06 + ww.sem * 9.24934e-06)/2
  ,w.weibo = (w.weibo * 2.774524e-08 - w.weibo*y2*2.01e-08)/2
  ,w.wechat = (w.wechat * 2.092103e-08) * 20
  ,w.bimp = w.bimp * 3.911974e-09 * 2
  ,w.ms = w.ms * 4.358183e-05
  ,w.2dp = w.2dp * (y1 * 0.621433e-03 + y2 * .821433e-03 + y3 * 1.821433e-03)
  ,ww.yxt = ww.yxt*0.006870075
  ,w.rmd = w.rmd2c*.309272e-03+w.rmd2b*1.113741e-04+y1*w.rmd2b*3.113741e-04
  ,w.kasc = w.kasc*0.0001388341 + 0.0001388341*0.5 * w.kasc * y2
  ,hys.val = hys.val * -0.005214611
  ,w.oos = w.oos * -0.017027936
  ,btb.weibo = btb.weibo*1.223381e-07
  ,btb.wechat = btb.wechat*0.788291e-07
  ,ww.avp = ww.avp *  -.010001073
  ,ww.wd = ww.wd * 0.114379
  ,fes.dum = fes.dum * 2.938615
  ,actual = as.numeric(ww.vol)
),2,tm)) * 64984/sum(ww.vol)

res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc_ww <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc_ww,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp
decomp[,3]/decomp[,2]
decomp[,2]/decomp[,1]
(decomp[,2]-decomp[,1])[21]/decomp[24,1]

setwd('/Users/wenrurumon/Desktop')
write.csv(decomp,'dc_ww_total.csv')
write.csv(sc_ww,'sc_ww_total.csv')

#########

x <- xbtb$otv$data
x <- x[,grepl('_GRP',colnames(x))]
x <- x[,grepl('WW',colnames(x))]
lm(xlm$residual ~ rowSums(x[,1:2]) + rowSums(x[,-1:-2])-1)

x <- xbtb$`digital(SEM)`$data
x <- x[,grepl('喷雾剂',colnames(x))&grepl('Clicks',colnames(x))]
lm(xlm$residual~rowSums(x[,c(1,3,5,7)])+rowSums(x[,c(-1,-3,-5,-7)])-1)
lm(xlm$residual ~ rowSums(x[,1:2])+rowSums(x[,3:4])+rowSums(x[,5:6])+rowSums(x[,7:8])-1)
cor(cbind(ww.vol,xlm$residual,rowSums(x[,1:2]),rowSums(x[,3:4]),rowSums(x[,5:6]),rowSums(x[,7:8])))

x <- xbtb$`digital(Banner)`$data
x <- x[,grep('ww_',colnames(x))]
x <- x[,grep('Banner_Impressions',colnames(x))]
lm(xlm$residual~x)
cor(xlm$residual,x)
