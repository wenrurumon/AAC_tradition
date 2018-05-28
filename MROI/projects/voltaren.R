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

#################################
#Voltaren
#################################

#Process sales data
c.dum <- keyvtr[,1]
m.dum <- rep(1:24,length=length(c.dum))
m.dum2 <- rep(1:12,length=length(c.dum))
y.dum <- rep(1:2,each=12,length=length(c.dum))
y1 <- as.numeric(y.dum==1); y2 <- as.numeric(y.dum==2)
x <- xvtr$VTR_sales
ve.vols <- x$data[,grepl('vol',x$map[-1:-2,4])&grepl('EMULGEL',x$map[-1:-2,3]),drop=F]
ve.vol <- as.matrix(ve.vols) %*% matrix(c(20,50,0),nrow=3)
ve.vals <- (x$data[,grepl('val',x$map[-1:-2,4])&grepl('EMULGEL',x$map[-1:-2,3]),drop=F])
cat.val <- x$data[,19]
cat.vol <- x$data[,20]
ve.val <- rowSums(ve.vals[,1:2])
ve.avp <- as.numeric(ve.val/ve.vol)
vt.vols <- x$data[,grepl('vol',x$map[-1:-2,4])&grepl('TAB EC',x$map[-1:-2,3]),drop=F]
vt.vol <- as.matrix(vt.vols) %*% matrix(c(10,20,30),nrow=3) * 25
ve.nd <- apply(x$data[,grepl('ND',colnames(x$data))],1,max,na.rm=T)
ve.wds <- x$data[,grepl('WD',colnames(x$data))]
v20.wd <- ve.wds[,1,drop=F]
v50.wd <- ifelse(is.na(ve.wds[,2,drop=F]),0,ve.wds[,2,drop=F])
ve.wd <- apply(x$data[,grepl('WD',colnames(x$data))],1,max,na.rm=T)
hos.vol <- ret(lag(x$data[,21],2),c.dum,0.3)
vtcat.val <- x$data[,23]
vt.nd <- x$data[,24]
vt.wd <- x$data[,25]
ve.ai <- v20.wd + v50.wd

#Process ATL
vtr.grp <- rowSums(xvtr$tv$data)
vtr.grp1 <- ret(vtr.grp*y1,c.dum,0.3)
vtr.grp2 <- ret(vtr.grp*y2,c.dum,0.3)
vtr.otvimp <- rowSums(xvtr$otv$data[,grepl('impression',colnames(xvtr$otv$data))])
vtr.otvimp1 <- ret(vtr.otvimp*y1,c.dum,0.3)
vtr.otvimp2 <- ret(vtr.otvimp*y2,c.dum,0.3)
x <- xvtr$digital$data[,grepl('Impression',colnames(xvtr$digital$data))]
vtr.dgt_camp <- ret(rowSums(xvtr$digital$data[,1:11]),c.dum,0.3)
vtr.dgt_ist <- ret(rowSums(xvtr$digital$data[,12:17]),c.dum,0.3)
vtr.sem <- rowSums(xvtr$SEM$data[,grepl('Clicks',colnames(xvtr$SEM$data))])
vtr.sem1 <- ret(vtr.sem*y1,c.dum,0.3)
vtr.sem2 <- ret(vtr.sem*y2,c.dum,0.3)
x <- xvtr$social$data[,grepl('Impression',colnames(xvtr$social$data))]
vtr.social_wb <- ret(rowSums(x[,grepl('Weibo',colnames(x))]),c.dum,0.3)
vtr.social_wc <- ret(rowSums(x[,grepl('Wechat',colnames(x))]),c.dum,0.3)
vtr.social <- ret(rowSums(xvtr$social$data[,grepl('Impression',colnames(xvtr$social$data))]),c.dum,0.3)
vtr.ooh <- ret(xvtr$ooh$data[,2],c.dum,.3)
vtr.reminder <- rowSums(xvtr$`display(POSM)`$data[,grepl('reminder',colnames(xvtr$`display(POSM)`$data))])
vtr.rmd1 <- ret(vtr.reminder * y1, c.dum, 0.3)
vtr.rmd2 <- ret(vtr.reminder * y2, c.dum, 0.3)
vtr.rmd <- ret(vtr.reminder,c.dum,0.3)
vtr.posm <- rowSums(xvtr$`display(POSM)`$data[,grepl('posm',colnames(xvtr$`display(POSM)`$data))])
vtr.posm1 <- vtr.posm * y1
vtr.posm2 <- vtr.posm * y2
vtr.instore <- rowSums(xvtr$KASC$data[,grepl('店内促销_store',colnames(xvtr$KASC$data))])
vtr.onshelf <- rowSums(xvtr$KASC$data[,grepl('上架_store',colnames(xvtr$KASC$data))])
vtr.app <- rowSums(xvtr$VTR_APP$data[,grepl('Scanned',colnames(xvtr$VTR_APP$data))])
vtr.yxt <- (xvtr$VTR_traning$data)[,1]
x <- xvtr$VTR_cTV$data
ynby.grp <- ret(rowSums(x[,8:9]),c.dum,0.3)

#################################
#Model1
#313313 / sum(ve.vol)
#################################

check(vtr.grp1*0.3417+vtr.grp2*0.3847,ve.vol,313313/sum(ve.vol),80000)
check(vtr.otvimp1*1.076799e-05+vtr.otvimp2*9.743027e-06,ve.vol,313313/sum(ve.vol),14000)
check(vtr.dgt_ist*3.521235e-06,ve.vol,313313/sum(ve.vol),6000)
check(vtr.dgt_camp*9.233591e-08,ve.vol,313313/sum(ve.vol),8000)
check(vtr.dgt_ist*3.521235e-06+vtr.dgt_camp*9.233591e-08,ve.vol,313313/sum(ve.vol),14000)
check(vtr.sem*3.175415e-04,ve.vol,313313/sum(ve.vol),1550)
check(vtr.social_wb*7.256070e-07,ve.vol,313313/sum(ve.vol),810)
check(vtr.social_wc*3.072282e-05,ve.vol,313313/sum(ve.vol),3840)
check(vtr.ooh * 1.901025e-04,ve.vol*y1,313313/sum(ve.vol),1552)
check(vtr.rmd * 0.714486e-02,ve.vol,313313/sum(ve.vol),1035)
check(vtr.posm1 * 3.196959e-03 + vtr.posm2 * 4.535670e-03,ve.vol,313313/sum(ve.vol),1103)
check(vtr.instore * 4.363851e-02,ve.vol,313313/sum(ve.vol),1650.8)
check(vtr.onshelf * 1.518378e-02,ve.vol,313313/sum(ve.vol),242.5)
check(vtr.app * 0.933507e-03,ve.vol,313313/sum(ve.vol),750)
check(vtr.yxt * 0.920051,ve.vol,313313/sum(ve.vol),1)
check(cat.val * 0.0855161,ve.vol,1,1)
check(v20.wd * 0.1368,ve.vols[,1],1,1)
check(v50.wd * 1.3235,ve.vols[,2],1,1)
check(ve.ai * 5.065491,ve.vol,1,1)
check(hos.vol * 2.102793e-05,ve.vol,1,1)

#distribution pie too big?
#dgt roi too small? wierd numbers: double check
#on-shelf roi: grouping? too high?
#ooh: too small?
#instore: what is it?

# lm(ve.vol~vtr.dgt_camp)

hold <- rowSums(
  cbind(
    0
    ,vtr.grp = vtr.grp1*0.3417+vtr.grp2*0.4047
    ,vtr.otvimp = vtr.otvimp1*1.076799e-05+vtr.otvimp2*1.1743027e-05
    ,vtr.dgt_ist*2.521235e-06
    ,vtr.dgt_camp*3.233591e-07
    ,vtr.sem*3.175415e-04 + vtr.sem2 * 6.35083e-05
    ,vtr.social_wb*7.256070e-07
    ,vtr.social_wc*3.072282e-05
    ,vtr.ooh * 1.901025e-04
    ,vtr.rmd * 0.514486e-02 + vtr.rmd * y2 * 0.414486e-02
    ,vtr.posm1 * 3.196959e-03 + vtr.posm2 * 4.535670e-03
    ,vtr.instore * 4.363851e-02
    ,vtr.onshelf * 1.518378e-02
    ,vtr.app * 0.933507e-03
    ,vtr.yxt * 0.920051 + vtr.yxt * y2 * 0.120051
    ,cat.val * 0.0255161
    ,ynby.grp * -0.00586702
    ,hos.vol * 2.102793e-05
    ,v20.wd * 1.038157
  )
)
hold_ve <- hold

ve.vol2 <- ve.vol - hold
summary(xlm <- lm(ve.vol2~
                    vtr.yxt+
                  # I(v50.wd+v20.wd)
                    ve.ai + v50.wd
                  + paste(c.dum,m.dum2) - 1
));xlm_ve <- xlm;coef(summary(xlm))[1:5,];plot.ts(as.numeric(tapply(ve.vol,m.dum,sum)));lines(as.numeric(tapply(predict(xlm)+hold,m.dum,sum)),col=2)

#####################

decomp <- as.data.frame(apply(cbind(
  # vtr.grp = vtr.grp1*0.3417+vtr.grp2*0.4047
  vtr.grp =  vtr.grp1 * 0.3417 + vtr.grp2 * 0.3017
  ,vtr.otvimp = vtr.otvimp1*1.076799e-05+vtr.otvimp2*1.1743027e-05
  ,vtr.dgt_ist = vtr.dgt_ist*2.521235e-06
  ,vtr.dgt_camp = vtr.dgt_camp*3.233591e-07
  ,vtr.sem = vtr.sem*3.175415e-04 + vtr.sem2 * 6.35083e-05
  ,vtr.social_wb = vtr.social_wb*7.256070e-07
  ,vtr.social_wc = vtr.social_wc*3.072282e-05
  ,vtr.ooh = vtr.ooh * 1.901025e-04
  ,vtr.rmd = vtr.rmd * 0.514486e-02 + vtr.rmd * y2 * 0.414486e-02
  ,vtr.posm = (vtr.posm1 * 3.196959e-03 + vtr.posm2 * 4.535670e-03)*2
  ,vtr.instore = vtr.instore * 4.363851e-02
  ,vtr.onshelf = vtr.onshelf * 1.518378e-02
  ,vtr.app = vtr.app * 0.933507e-03
  ,vtr.yxt = (vtr.yxt * 0.920051 + vtr.yxt * y2 * 0.220051)/3
  ,vtr.catval = cat.val * 0.0355161
  ,ynby.grp = ynby.grp * -0.00586702
  ,hos.vol = hos.vol * 2.102793e-05 + y1 * 1.3
  ,ve.ai = as.numeric(ve.ai * 0.4548609)
  ,v50.wd = as.numeric(v50.wd * 4.3334000)
  ,actual = as.numeric(ve.vol)
),2,tm)) * 313313/sum(ve.vol)
res <- decomp$actual*2 - rowSums(decomp)
# res <- rep(0,length(res))
res <- predict(lm(res~paste(rep(1:12,2))))
sc_ve <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- rbind(0,(apply(sc_ve,2,function(x){tapply(x,rep(1:2,each=12),sum)})))
decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp
decomp[,3]/decomp[,2]
# decomp[,3]/decomp[,2]

setwd('/Users/wenrurumon/Desktop')
write.csv(decomp,'test.csv')

#################################
#Model2
#51270/sum(vt.vol1)
#################################

# check(vtr.grp*1.315619e+00,vt.vol,51270/sum(vt.vol),80000)
# check(vtr.otvimp*2.794909e-05,vt.vol,51270/sum(vt.vol),14000)
# check(vtr.ooh * 8.530562e-03, vt.vol,51270/sum(vt.vol),16000)
# 
# tapply(vtr.ooh * 8.530562e-03* 51270/sum(vt.vol),y.dum,sum)
# 
# coef(summary(xlm <- lm(vt.vol ~ vt.wd
#                        + vtr.grp + vtr.otvimp + vtr.ooh
#                        + paste(c.dum) + paste(m.dum2))))[1:10,]
# plot.ts(tm(vt.vol)); lines(tm(predict(xlm)),col=2)

#################################
#Outboard
#################################

x <- xvtr$tv$data
0.38 + sapply(1:3,function(i){(coef(lm(xlm$residuals~x[,i]-1)))})

x <- xvtr$otv$data[,grepl('impression',colnames(xvtr$otv$data))]
x <- cbind(PC=rowSums(x[,grepl('PC',colnames(x))]),Mobile=rowSums(x)-rowSums(x[,grepl('PC',colnames(x))]))
1.076799e-05+sapply(1:2,function(i){coef(lm(xlm$residuals~x[,i]-1))})

x <- xvtr$SEM$data[,grepl('Clicks',colnames(xvtr$SEM$data))]
3.675415e-04+sapply(1:2,function(i){coef(lm(xlm$residuals~x[,i]-1))})

x <- round(vtr.grp / rep(tapply(vtr.grp,c.dum,mean),each=sum(c.dum==c.dum[1]))-1,1)
# barplot(y <- tapply(xlm$residuals,x,mean)[-1])
y <- ve.vol
barplot(m <- tapply(x,x,length)[-1])
x <- rep(0,length=length(m))
for(i in 3:(length(x)-2)){
  x[i+2] <- sum(y[(i-2):(i+2)]*m[(i-2):(i+2)])/sum(m[(i-2):(i+2)])
}
barplot(x)
cbind(m,x)

x <- sapply(1:10,function(i){pweibull(seq(0,2,0.1),i)})
plot.ts(x)
