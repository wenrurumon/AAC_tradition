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

####################################################################################
####################################################################################

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
#Oint total
#######################

# o.vol <- o5.vol * 572864/sum(o5.vol) + o10.vol * 290066/sum(o10.vol)
# o.val <- o5.val * 572864/sum(o5.vol) + o10.val * 290066/sum(o10.vol)
o.vol <- (o5.vol + o10.vol) * (572864+290066) / sum(o10.val+o5.val)
o.val <- (o5.val + o10.val) * (572864+290066) / sum(o10.val+o5.val)

o10.idx <- o10.vol/o5.vol
o.avp <- o.val/o.vol
o.app <- o5.app + o10.app
o.wd <- (o5.wd * 572864/sum(o5.vol) + o10.wd * 290066/sum(o10.vol))/(572864/sum(o5.vol)+o10.wd * 290066/sum(o10.vol))
y <- o.vol
hold_ototal <- hold <- cbind(
  o.grp = o.grp * 0.2701472 + o.grp1 * 5.495535e-02 + o.grp2 * 0.02066978
  ,o.igrp = o.igrp * 0.9473182 + o.igrp * y3 * - 0.2213737 
  ,o.weibo = o.weibo * (1.337016e-07 + y2 * 0.37016e-07)
  ,o.wechat = o.wechat * 1.228420e-05
  ,o.sem = o.sem * 4.132780e-05
  ,o.app = o.app * 0.653169e-03
  ,o.yxt = o.yxt * 0.078180902
  ,o.2dp = o.2dp * 1.398475e-03 + o.2dp*y2*0.113259e-03 + o.2dp * y3 * 1.0543502e-03
  ,o.ms = o.ms * 1.385050e-04
  ,o.rmd2b = o.rmd2b * 0.01631934 # - o.rmd2b * 0.01631934  * y1
  ,o.bimp = o.bimp * 4.114150e-08+ o.bimp*y1*4.578263e-08
  ,btb.nmimp = btb.nmimp*1.470612e-07
  ,btb.weibo = btb.weibo*4.223381e-06
  ,btb.wechat = btb.wechat*3.288291e-06
  ,btb.sem = btb.sem * 3.405585e-05 + btb.sem3 * -0.864641e-05
  ,w.nmimp = w.nmimp * 0.51274e-07
  ,ww.grp = ww.grp*0.06598499
  ,w.igrp = w.igrp*0.1377784
  ,o.kasc = o.kasc*0.04736858 / 1.5
  ,o.oos = o.oos*-1.012834
  ,ctv999 = xbtb$cTV$data[,1] * -0.01077211
  ,cotvpdb = xbtb$cDigital$data[, 3] * -7.937059e-04
  ,ar.grp = ar.grp*0.3432135
)
y_2 <- y - rowSums(hold)
o10.wdsm <- sm(o10.wd)
coef(summary(xlm <- lm(y_2~ o5.wd + o10.wd 
                       + I(o10.wd * y3) + I(o10.wd*y2)
                       # +ar.grp
                       +paste(c.dum) + paste(m.dum2) -1)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold)),col=2)

decomp <- as.data.frame(apply(cbind(
  o.grp = o.grp * 0.2701472 + o.grp1 * 5.495535e-02 + o.grp2 * 0.02066978
  ,o.igrp = o.igrp * 0.9473182 + o.igrp * y3 * - 0.2213737 
  ,o.weibo = o.weibo * (1.337016e-07 + y2 * 0.37016e-07)
  ,o.wechat = o.wechat * 1.228420e-05
  ,o.sem = o.sem * 4.132780e-05
  ,o.app = o.app * 0.653169e-03
  ,o.yxt = o.yxt * 0.078180902
  ,o.2dp = o.2dp * 1.398475e-03 + o.2dp*y2*0.113259e-03 + o.2dp * y3 * 1.0543502e-03
  ,o.ms = o.ms * 1.385050e-04
  ,o.rmd = o.rmd2b * 0.01631934  - o.rmd2b * 0.01631934  * y1
  ,o.bimp = o.bimp * 4.114150e-08+ o.bimp*y1*4.578263e-08
  ,btb.nmimp = btb.nmimp*1.470612e-07
  ,btb.weibo = btb.weibo*4.223381e-06
  ,btb.wechat = btb.wechat*3.288291e-06
  ,btb.sem = btb.sem * 3.405585e-05 + btb.sem3 * -0.864641e-05
  ,w.nmimp = w.nmimp * 0.51274e-07
  ,ww.grp = ww.grp*0.06598499
  ,w.igrp = w.igrp*0.1377784
  ,o.kasc = o.kasc*0.04736858 / 1.5
  ,o.oos = o.oos*-1.012834
  ,ctv999 = xbtb$cTV$data[,1] * -0.01077211
  ,cotvpdb = xbtb$cDigital$data[, 3] * -7.937059e-04
  ,ar.grp = ar.grp*0.3432135
  ,o5.wd = o5.wd * 6.7093706
  ,o10.wd = o10.wd * ( -1.7099470 + y3 * -0.5926010 + y2 * -0.3358272)
  ,actual = as.numeric(o.vol)
),2,tm))

res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc_o <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc_o,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp
1-decomp[,3,drop=F]/decomp[,2,drop=F]
1-decomp[,2,drop=F]/decomp[,1,drop=F]

##############
check(o.2dp * 1.398475e-03,o.vol,1,2210)
check(o.yxt * 0.38180902, o.vol,1,1)
check(o.app * 0.653169e-03, o.vol, 1, 890)
check(o.grp * 0.2701472 + o.grp1 * 5.495535e-02 + o.grp2 * 0.02066978, o.vol, 1, 83600)
check(o.igrp * 0.9473182, o.vol, 1, 12700)
check(o.sem * 4.132780e-05, o.vol, 1, 3600)
check(o.weibo * 0.437016e-07, o.vol,1,137)
check(o.wechat * 1.228420e-05, o.vol*y1, 1, 280)
check(o.kasc*0.04736858,o.vol,1,7800)
check(o.ms * 1.385050e-04,o.vol,1,9550)
check(o.rmd2b * 0.01631934  - o.rmd2b * 0.01631934  * y1,o.vol,1,250+125.496+783.14)
check(btb.sem * 1.270093e-05,o.vol,1,2200)
check(o.bimp * 1.114150e-08+ o.bimp*y1*1.578263e-08 ,o.vol,1,8880)
check(btb.nmimp*1.470612e-07,o.vol,1,970)
check(btb.weibo*4.223381e-06,o.vol,1,701)
check(btb.wechat*3.288291e-06,o.vol,1,366)
check(btb.sem * 4.405585e-05 + btb.sem3 * -1.364641e-05, o.vol,1,2190)
check(ww.grp*0.06598499, o.vol, 1, 18500)
check(w.igrp*0.1377784,  o.vol, 1, 10100)
check(w.nmimp * 1.750280e-08,  o.vol, 1, 4780)
check(ar.grp*0.3432135,o.vol,1,1)

check(ww.grp * 0.015693721 * 2.904578 + ww.grp*0.06598499,o.vol,1,18500 )
check(w.igrp * 0.02116713 * 2.904578 + w.igrp*0.1377784,o.vol,1,10100 )
check(w.nmimp * 0.51274e-07 * 2.904578 + w.nmimp * 1.750280e-08,  o.vol, 1, 4780)

# ,ww.grp = ww.grp * 0.015693721
# ,w.igrp = w.igrp * 0.02116713
# ,w.nmimp = w.nmimp * 0.91274e-08

#######################
#Oint 5g
o5.m <- o5.vol * 572864/sum(o5.vol)
o10.m <- o10.vol * 290066/sum(o10.vol)
#######################

o5.hold <- (hold_ototal * o5.vol/(o5.vol+o10.vol))/(572864/sum(o5.vol))
o10.hold <- (hold_ototal * o10.vol/(o5.vol+o10.vol))/(290066/sum(o10.vol))

o5.v2 <- o5.vol - rowSums(o5.hold)
xlm <- lm(o5.v2~o5.wd + o10.wd + I(o10.wd*y1) + I(o10.wd*y2) +paste(c.dum)+paste(m.dum2)-1)
coef(summary(xlm))[1:10,]
plot.ts(tm(o5.vol));lines(tm(predict(xlm)+rowSums(o5.hold)),col=2)
decomp <- as.data.frame(apply(cbind(o5.hold
                                    ,o5.wd = o5.wd * 0.88035951
                                    ,o10.wd = o10.wd * (-0.69741653 + y1 * 0.14420070 + y2 * 0.06835121)
                                    ,actual = o5.vol),2,tm)) * 572864/sum(o5.vol)
res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc_o5 <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc_o5,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp5 <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))

o10.v2 <- o10.vol - rowSums(o10.hold)
xlm <- lm(o10.v2 ~ o10.wd + paste(c.dum) + paste(m.dum2) -1 )
coef(summary(xlm))[1:10,]
plot.ts(tm(o10.vol));lines(tm(predict(xlm)+rowSums(o10.hold)),col=2)
decomp <- as.data.frame(apply(cbind(o10.hold
                                    ,o10.wd = o10.wd * 1.071823
                                    ,actual = o10.vol),2,tm)) * 290066/sum(o10.vol)
res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc_o10 <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc_o10,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp10 <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))

decomp_os <- rbind(
  data.table(model = 'o5',var=rownames(decomp5),decomp5)
  ,data.table(model = 'o10',var=rownames(decomp10),decomp10)
)
colnames(decomp_os) <- c('model','var','drive1','drive2','drive3','pie1','pie2','pie3')
colSums(filter(decomp_os,var=='predict') %>% select(drive1,drive2,drive3))
