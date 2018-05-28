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
o10.vol <- btb.sales[,6] * 1
o10.val <- btb.sales[,7]
o10.wd <- btb.sales[,8]
o5.nd <- btb.sales[,9]
o5.vol <- btb.sales[,10] * .5
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

##########
#Projection

# f5 <- (572864000/11.95 * 5) / sum(o5.vol)
# f10 <- (290066000/20.29 * 10) / sum(o10.vol)
# o5.val <- o5.val * f5
# o10.val <- o10.val * f10
# o5.vol <- o5.vol * f5
# o10.vol <- o10.vol * f10
# o.price <- (572864+290066)/ ((572864000/11.95 * 5) + (290066000/20.29 * 10)) * 1000
# o.vol <- (o5.vol + o10.vol)/1000
# o.val <- (o5.val + o10.val)/1000
# o.vol <- o.vol * (572864+290066) / sum(o.vol)

f <- (572864/11.95 * 5)+(290066/20.29 * 10)
f <- f/sum(o5.vol+o10.vol)
o.price <- (572864+290066)/((572864/11.95 * 5)+(290066/20.29 * 10))
o.vol <- (o5.vol+o10.vol)*f
o.val <- (o5.val + o10.val) * f

#######################
#Oint total
#######################

check(o.grp * (0.3507619 + y3 * - 0.0821582 + y3 * - 0.048075), o.vol, 1, 93600)
check(o.igrp * 1.0473182 + o.igrp * y3 - 0.013235 + o.igrp * y3 * - 0.0213737 , o.vol, 1, 12700)
check(o.weibo * (1.337016e-07 + y2 * 0.37016e-07), o.vol,1,137)
check(o.wechat * 1.228420e-05, o.vol*y1, 1, 280)

# f5 <- 572864/sum(o5.val)
# f10 <- 290066/sum(o10.val)
# o.vol <- o5.vol * f5 + o10.vol * f10
# o.val <- o5.val * f5 + o10.val * f10
# o.vol <- (o5.vol + o10.vol) * (572864+290066) / sum(o10.vol+o5.vol)
# o.val <- (o5.val + o10.val) * (572864+290066) / sum(o10.vol+o5.vol)
# o.vol <- o.vol * (572864+290066) / sum(o.vol)

o.avp <- o.val/o.vol
o.app <- o5.app + o10.app
y <- o.vol * (572864+290066)/sum(o.vol)
hold_ototal <- hold <- cbind(0
                             ,o.grp = o.grp * (0.3507619 + y3 * - 0.0821582 + y2 * - 0.028075)
                             ,o.igrp = o.igrp * 1.299197 + o.igrp * y3 * (-0.42135)
                             ,o.weibo = o.weibo * (1.337016e-07 + y2 * 0.37016e-07)/2
                             ,o.wechat = o.wechat * 1.228420e-05/2
                             ,o.sem = o.sem * 4.132780e-05
                             ,o.app = o.app * 0.653169e-03
                             ,o.yxt = o.yxt * (0.078180902 * y2 + y3 * 0.05343383)
                             ,o.2dp = o.2dp * 2.398475e-03 - o.2dp * 0.0018989107  + o.2dp * y3 * 1.0543502e-03
                             ,o.ms = o.ms * 1.385050e-04
                             ,o.rmd2b = o.rmd2b * 0.01631934  - o.rmd2b * 0.014631934  * y1 - o.rmd2b * 0.00543978 * y3
                             ,o.bimp = o.bimp * 4.114150e-08+ o.bimp*y1*4.578263e-08
                             ,btb.nmimp = btb.nmimp*3.870612e-07
                             ,btb.weibo = btb.weibo*3.223381e-06
                             ,btb.wechat = btb.wechat*2.288291e-06
                             ,btb.sem = btb.sem * 3.405585e-05 + btb.sem3 * -0.864641e-05
                             ,w.nmimp = w.nmimp * 9.418267e-08
                             ,ww.grp = ww.grp*0.06598499
                             ,w.igrp = w.igrp*0.1877784
                             ,o.kasc = o.kasc*0.04736858 / 1.5
                             ,o.oos = o.oos*-1.012834
                             ,ctv999 = xbtb$cTV$data[,1] * -0.01077211
                             ,cotvpdb = xbtb$cDigital$data[, 3] * -7.937059e-04
                             ,ar.grp = ar.grp*0.3432135
                             ,o5.wd = o5.wd * 3.605184
                             ,o10.wd = o10.wd * 5.752033
                             ,o.avp = o.avp * -62.41468
)
y_2 <- y - rowSums(hold)
o10.wdsm <- sm(o10.wd)
coef(summary(xlm <- lm(y_2~ 
                       # + o.avp
                       # +I(o10.wd * y1) + I(o10.wd * y2) + I(o10.wd * y3)
                       # +I(o10.wd * y3)
                       # +I(o.grp* y3) + I(o.grp * y2)
                       +paste(c.dum)
                       +paste(m.dum2)
                       -1
)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold)),col=2)
# test <- apply(cbind(y,predict(xlm)+rowSums(hold)),2,function(x){tapply(x,y.dum,sum)})
# test[2,]/test[1,];test[3,]/test[2,]
# dummy <- o.vol - rowSums(hold) - o5.wd * 1.161080 - o10.wd * 5.885792 - o.avp * -47.246780

decomp <- as.data.frame(apply(cbind(
  o.grp = o.grp * (0.3507619 + y3 * - 0.0821582 + y2 * - 0.028075)
  ,o.igrp = o.igrp * 1.299197 + o.igrp * y3 * (-0.42135)
  ,o.weibo = o.weibo * (1.337016e-07 + y2 * 0.37016e-07)/2
  ,o.wechat = o.wechat * 1.228420e-05/2
  ,o.sem = o.sem * 4.132780e-05
  ,o.app = o.app * 0.653169e-03
  ,o.yxt = o.yxt * (0.078180902 * y2 + y3 * 0.05343383)
  ,o.2dp = o.2dp * 2.398475e-03 - o.2dp * 0.0018989107  + o.2dp * y3 * 1.0543502e-03
  ,o.ms = o.ms * 1.385050e-04
  ,o.rmd2b = o.rmd2b * 0.01631934  - o.rmd2b * 0.014631934  * y1 - o.rmd2b * 0.00543978 * y3
  ,o.bimp = o.bimp * 4.114150e-08+ o.bimp*y1*4.578263e-08
  ,btb.nmimp = btb.nmimp*3.870612e-07
  ,btb.weibo = btb.weibo*3.223381e-06
  ,btb.wechat = btb.wechat*2.288291e-06
  ,btb.sem = btb.sem * 3.405585e-05 + btb.sem3 * -0.864641e-05
  ,w.nmimp = w.nmimp * 9.418267e-08
  ,ww.grp = ww.grp*0.06598499
  ,w.igrp = w.igrp*0.1877784
  ,o.kasc = o.kasc*0.04736858 / 1.5
  ,o.oos = o.oos*-1.012834
  ,ctv999 = xbtb$cTV$data[,1] * -0.01077211
  ,cotvpdb = xbtb$cDigital$data[, 3] * -7.937059e-04
  ,ar.grp = ar.grp*0.3432135
  ,o5.wd = o5.wd * 3.605184
  ,o10.wd = o10.wd * 5.752033
  ,o.avp = o.avp * -62.41468
  ,ss = predict(xlm)
  ,actual = as.numeric(y)
),2,tm))
plot.ts(rowSums(decomp)-decomp$actual,col=2)
lines(decomp$actual)

round(t(apply(decomp,2,function(x){tapply(x,rep(1:3,each=12),sum)})),2)

################################################################################################
################################################################################################

if(abs(sum(o5.vol)-35099.39)<0.01){
  o5.vol <- o5.vol * f
  o10.vol <- o10.vol * f
}
hold_ototal2 <- hold_ototal[,1:24]
hold_t5 <- (coef(summary(lm(o5.vol~hold_ototal2)))[-1,3]/40+1) * sum(o5.vol)/sum(o.vol)/2
hold_t5[6] <- 0.093333
hold_5 <- t(t(hold_ototal2[,-1]) * hold_t5);sum(hold_5)/sum(o5.vol*o.price)
hold_10 <- hold_ototal2[,-1]-hold_5

y <- o5.vol*o.price
canni <- o10.vol*o.price
canni2 <- o5.vol * o.price
hold5 <- cbind(hold_5
               ,o5.wd = o5.wd * 2.277716
               ,mis = 8.332459*y1
               ,o10.canni = canni * (y1 * -0.2137619 + y2 * -0.2381158 + y3 * -0.2609454)
               # ,o10.canni = canni * (y1*-0.007302877+y2* -0.02771741 +y3* -0.08521422 )
)
o5.wdr <- ifelse(o5.wd - o10.wd >0,o5.wd - o10.wd,0)
y_2 <- y - rowSums(hold5)
canni.c <- sapply(unique(c.dum),function(i){canni*(c.dum==i)})
coef(summary(xlm <- lm(y_2~ 
                         # o5.wd
                         # +o5.wdr
                       # +o10.wdsm
                       # +I(canni*y1) + I(canni*y2) + I(canni*y3)
                       # +y1
                         # o5.vol
                       +paste(c.dum,m.dum2)
                       -1
)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold5)),col=2)
decomp <- cbind(hold5,ss=predict(xlm))
decomp <- cbind(decomp,predict=rowSums(decomp),actual=y)
sc5 <- as.data.frame((apply(decomp,2,function(x){tapply(x,m.dum,sum)})))
decomp <- apply(sc5,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp5 <- decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
# decomp5
decomp5[nrow(decomp5)-3,1:3]/tapply(o10.vol*o.price,y.dum,sum)
round(decomp[,3]/decomp[,2]-1,2)
round(decomp[,2]/decomp[,1]-1,2)

################################################################################################
################################################################################################

y <- o10.vol * o.price
hold10 <- cbind(hold_10
                # ,o10.wd = o10.wdsm *  8.644428
                ,mis = -8.332459*y1
                # ,o5.canni = canni2 * -0.1434865
                # ,o5.canni = o5.wd * (y1*-3.858670+y2* -3.223479+y3*-2.684617)
                ,o10.wd = o10.wdsm*(6.869047*y1+ 7.501126*y2+8.102901*y3)
                # ,mis = y1 * -47.24351
                # ,o5.canni = canni2 * -0.7161362
)
y_2 <- y - rowSums(hold10)
coef(summary(xlm <- lm(y_2~ 
                         # +canni2
                         # o10.wdsm
                         # +I(o10.wd * (c.dum=='beijing'))
                         # +I(o10.wdsm*y1)+I(o10.wdsm*y2)+I(o10.wdsm*y3)
                       # +I(o5.vol*o.price)
                         # # +canni.c
                         # + I(canni2*y1) + I(canni2*y2) + I(canni2*y3)
                         # +y1+y2+y3
                         +paste(c.dum)
                       +paste(m.dum2)
                       -1
)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold10)),col=2)
decomp <- cbind(hold10,ss=predict(xlm))
decomp <- cbind(decomp,predict=rowSums(decomp),actual=y)
sc10 <- as.data.frame((apply(decomp,2,function(x){tapply(x,m.dum,sum)})))
decomp <- apply(sc10,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp10 <- decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp10
round(decomp[,3]/decomp[,2],2)
round(decomp[,2]/decomp[,1],2)

plot.ts(sc5$actual+sc10$actual);lines(sc5$predict+sc10$predict,col=2)

write.csv(sc5,'test.csv')
write.csv(sc10,'test.csv')

###############################

decomp <- rbind(
  data.frame(model='o5',var=rownames(decomp5),decomp5),
  data.frame(model='o10',var=rownames(decomp10),decomp10)
)
test <- filter(decomp,var%in%c('predict','actual')) %>% group_by(var) %>% summarise(sum(X1),sum(X2),sum(X3))
test[,3]/test[,2]
test[,4]/test[,3]
# decomp <- data.frame(model=decomp[,1:2],define=NA,decomp[,-1:-2])
setwd('/Users/wenrurumon/Desktop')
write.csv(decomp,'test.csv',quote=F,row.names=F)

test <- (lm(log(o.vol)~paste(m.dum2)-1))
plot.tm(predict(test))
test <- coef(test)
names(test) <- (gsub('paste(m.dum2)','',names(test)))
