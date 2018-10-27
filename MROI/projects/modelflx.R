
rm(list=ls())
setwd('E:\\gsk\\fbd_flx\\model')
# raw <- openxlsx::read.xlsx('mmix_flx.xlsx',1)[,-1]
# setwd('/Users/wenrurumon/Desktop/Fenbid_Filxonase/model')
# setwd('C:\\Users\\WenluluSens\\Documents\\Project\\GSK\\FBD&FLX_2018\\model')
raw <- as.data.frame(data.table::fread('mmix_flx_withsku.csv'))[,-1]

#############################
# Function
#############################

# getvar('FLIXONASE_AQUA_VAL',F)

getvar <- function(varname,value=T,rowsum=T,database=raw){
  sel <- grep(varname,colnames(database))
  if(value){
    x <- (colnames(database)[sel])
    return(x)
  } else {
    x <- (database[,sel,drop=F])
  }
  if(rowsum){
    return(rowSums(x))
  } else {
    return(x)
  }
}
mt <- function(x){
  as.numeric(
    tapply(x,
           rep(1:30,length=810),
           sum)
  )
}
plot.mt <- function(x){
  plot.ts(mt(x))
}
ret <- function(x,ret=0.3){
  x <- sapply(unique(raw$City),function(city){
    x[raw$City==city]
  })
  for(i in 2:nrow(x)){
    x[i,] <- x[i-1,]*ret + x[i,]
  }
  unlist(apply(x,2,list))
}
mc <- function(x){
  x.mc <- tapply(x,raw$City,mean)
  x/x.mc[match(raw$City,names(x.mc))]
}
check <- function(x,y,ifplot=F){
  out <- c(tapply(x,ydum,sum),total=sum(x))/c(tapply(y,ydum,sum),sum(y))
  if(ifplot){plot.model(mt(y),mt(x))}
  return(out)
}
lag <- function(x,l=1){
  x <- sapply(unique(raw$City),function(city){
    x[raw$City==city]
  })
  x <- apply(x,2,function(xi){
    c(rep(0,l),xi)
  })[1:nrow(x),]
  unlist(apply(x,2,list))
}
plot.model <- function(x,y,minval=0){
  maxval <- max(c(x,y))
  plot.ts(rep(c(minval,maxval),length=length(x)),col=0)
  lines(x)
  lines(y,col=2)
}

#############################
# Processing
#############################

#sales data

flx.val <- getvar('FLIXONASE_AQUA_VAL',F)
flx.vol <- getvar('FLIXONASE_AQUA_PACK',F)
flx.avp <- flx.val / flx.vol
flx.wd <- mc(getvar('FLIXONASE_AQUA_WD',F))
flx.nd <- mc(getvar('FLIXONASE_AQUA_ND',F))
cat.val <- getvar('VAL.sales',F,T)
hh <- rep(tapply(cat.val,raw$City,mean),each=30)
mdum <- paste0('M',rep(rep(1:12,length=30),27))
cdum <- raw$City
ydum <- do.call(rbind,strsplit(raw$Month,'/'))[,1]
y1 <- (ydum=='2016')
y2 <- (ydum=='2017')
y3 <- (ydum=='2018')
hos.val <- getvar('hos_Value',F,T)
comp.val <- getvar('RHINOCORT_AQUA_VAL|CLARITYNE_VAL.sales|NEW_XISIMIN_VAL.sales|NASONEX_VAL.sales',F)
comp.validx <- lm(comp.val~mdum)$residual

tcm.val <- mc(rowSums(raw[,match(grep('Val.sku',getvar('Tcm'),value=T),colnames(raw))]))
tcm.val <- tcm.val - predict(lm(tcm.val~mdum+cdum))
rnct.val <- apply(raw[,match(grep('Val.sku',getvar('Rhinocort'),value=T),colnames(raw))],1,sum)
rnct.vol <- apply(raw[,match(grep('Pack.sku',getvar('Rhinocort'),value=T),colnames(raw))],1,sum)
rnct.avp <- rnct.val / rnct.vol
clarit.val <- mc(getvar('CLARITYNE_VAL.sales',F,T))
nasonex.val <- mc(getvar('NASONEX_VAL.sales',F,T))
newxisimin.val <- mc(getvar('NEW_XISIMIN_VAL.sales',F,T))

rnct.val <- raw[,match(grep('Val.sku',getvar('Rhinocort'),value=T),colnames(raw))]
rnct.wd <- raw[,match(grep('Wd.sku',getvar('Rhinocort'),value=T),colnames(raw))]
rnct.val2 <- rnct.val[,2]

#atl
flx.tv <- ret(getvar('GRP.tv',F,T),0.4)
flx.tv1 <- ret(getvar('GRP.tv',F,T)*y1,0.4)
flx.tv2 <- ret(getvar('GRP.tv',F,T)*y2,0.4)
flx.tv3 <- ret(getvar('GRP.tv',F,T)*y3,0.4)

flx.otv <- ret(getvar('GRP.otv',F,T),0.4)
flx.otv1 <- ret(getvar('GRP.otv',F,T)*y1,0.4)
flx.otv2 <- ret(getvar('GRP.otv',F,T)*y2,0.4)
flx.otv3 <- ret(getvar('GRP.otv',F,T)*y3,0.4)

flx.totv <- ret(getvar('Mobile_iGRP.targetotv|PC_iGRP.targetotv',F,T),0.3)

flx.sem <- ret(getvar('Clicks.sem',F,T),0.3)
flx.sem1 <- ret(getvar('Clicks.sem',F,T)*y1,0.3)
flx.sem2 <- ret(getvar('Clicks.sem',F,T)*y2,0.3)
flx.sem3 <- ret(getvar('Clicks.sem',F,T)*y3,0.3)

flx.semimp <- ret(getvar('Impressions.sem',F,T),0.3)
flx.semimp1 <- ret(getvar('Impressions.sem',F,T)*y1,0.3)
flx.semimp2 <- ret(getvar('Impressions.sem',F,T)*y2,0.3)
flx.semimp3 <- ret(getvar('Impressions.sem',F,T)*y3,0.3)

flx.digital <- ret(getvar('Liulanliang.digital',F,T),0.3)
flx.digital1 <- ret(getvar('Liulanliang.digital',F,T)*y1,0.3)
flx.digital2 <- ret(getvar('Liulanliang.digital',F,T)*y2,0.3)
flx.digital3 <- ret(getvar('Liulanliang.digital',F,T)*y3,0.3)

flx.social <- ret(getvar('Read.social',F,T),0.3)
flx.social1 <- ret(getvar('Read.social',F,T)*y1,0.3)
flx.social2 <- ret(getvar('Read.social',F,T)*y2,0.3)
flx.social3 <- ret(getvar('Read.social',F,T)*y3,0.3)

#btl
flx.app <- (getvar('saomaliang.app',F,T))
flx.app1 <- (getvar('saomaliang.app',F,T))*y1
flx.app2 <- (getvar('saomaliang.app',F,T))*y2
flx.app3 <- (getvar('saomaliang.app',F,T))*y3

flx.edu <- ret(getvar('shijirenshu.edu',F,T),0.5)
flx.posm <- ret(getvar('posm',F,T),0.5)
flx.rmd <- ret(getvar('hejiyujidanpinshuliang.rmd',F,T),0.5)

flx.rmd <- ret(getvar('hejiyujidanpinshuliang.rmd',F,T),0.5)
flx.rmd2 <- ret(getvar('hejiyujidanpinshuliang.rmd',F,T)*y2,0.5)
flx.rmd3 <- ret(getvar('hejiyujidanpinshuliang.rmd',F,T)*y3,0.5)

flx.rmd_csm <- ret(getvar('mianxiangxiaofeizhe',F,T),0.5)
flx.rmd_store <- ret(getvar('mianxiangdianyuan',F,T) +
                       getvar('mianxiangmendian',F,T) +
                       getvar('xinxingqiqiu',F,T),0.5)

#############################
# Modeling
#############################

# reprocessing

flx.rmd1 <- rep(1:30,27) %in% (10:13)
cheat1 <- rep(0,810)
cheat1[rep(1:30,27)%in%c(1,2,6)] <- -1/3
cheat1[rep(1:30,27)==4] <- 1

flx.vol2 <- flx.vol / hh
holdout <-   cbind(
  intercept = rep(0,810)
  , flx.tv = flx.tv1  * 5.169e-07 * 1.315773
  + flx.tv2  * (5.169e-07 * 1.315773 - 9.633781e-08)
  + flx.tv3  * (5.169e-07 * 1.315773 - 4.303923e-08)
  , flx.otv = flx.otv1 * 6.462e-07 * 0.4674315
  + (flx.otv2 * 6.462e-07 * 0.4674315 + ((flx.otv2)^(1/3)) * 4.620639e-07) * 1.4
  + (flx.otv3 * 6.462e-07 * 0.4674315 * 0.95)*1.2
  , flx.totv = flx.totv * 4.204e-06 * 0.4674315*1.3
  , flx.digital = flx.digital1 * 2.507776e-13
  + flx.digital2 * 1.91645e-13
  + flx.digital3 * 1.408429e-13
  , flx.sem = flx.semimp1 * 3.24e-09 * 0.1614686
  + flx.semimp2 * (3.24e-09 * 0.1614686 + 0.192977e-10)
  + flx.semimp3 * (3.24e-09 * 0.1614686 + 0.340626e-10)
  , flx.social = flx.social1 * 2.093e-10 * 0.1243447
  + flx.social2 * 2.093e-10 * 0.1243447 * 1.1
  + flx.social3 * 2.093e-10 * 0.1243447
  , flx.app = flx.app1  *  1.102e-07
  + flx.app2  *  1.102e-07 / 1.8
  + flx.app3  *  1.102e-07 * 1.4
  , flx.posm = y1*1.4008665e-05 + flx.posm * 1.370e-07 * 0.15 * 0.58
  , flx.rmd = flx.rmd1*3.546273e-05 + flx.rmd2 * 2.869e-08 + flx.rmd3 * 1.115862e-09
  , flx.edu = flx.edu * 1.679e-07 * 0.1239381
  , competitor = cheat1*-2.335675e-04 + rnct.val2*-2.630674e-07
  , flx.nd = flx.nd*7.751972e-04
)
flx.vol3 <- flx.vol2 - rowSums(holdout)

# Model
xlm <- lm(flx.vol3 ~ -1
          + paste(cdum,mdum)
          # + y1
)
tail(coef(summary(xlm)))

# Tech Review
plot.model(
  y.raw <- mt(flx.vol),
  y.pred <- mt((predict(xlm) + rowSums(holdout)) * hh),
  minval = min(y.pred)/2
)
summary(abs(y.pred/y.raw-1))
summary(lm(y.raw~-1+y.pred))$r.square

sum(y.raw[19:30])/sum(y.raw[7:18])
sum(y.pred[19:30])/sum(y.pred[7:18])
sum(y.raw[1:12])/sum(y.raw[13:24])
sum(y.pred[1:12])/sum(y.pred[13:24])

# Calc
colSums(holdout*hh)/sum(y.pred)

check(y1*1.4008665e-05*hh,flx.vol)
check(flx.rmd1*3.546273e-05*hh,flx.vol)
check(cheat1*-2.335675e-04*hh,flx.vol)
check(rnct.val2*-2.630674e-07*hh,flx.vol)
check((flx.tv1  * 5.169e-07 * 1.315773
       + flx.tv2  * (5.169e-07 * 1.315773 - 9.633781e-08)
       + flx.tv3  * (5.169e-07 * 1.315773 - 4.303923e-08)) * hh,flx.vol)
check(flx.otv * 6.462e-07 * 0.4674315 * hh,flx.vol)
check(flx.totv * 4.204e-06 * 0.4674315 * hh,flx.vol)
check(flx.semimp * 3.24e-09 * hh,flx.vol)
check(sqrt(flx.digital) * 2.4177e-9 * 1.7 * hh,flx.vol)
check(flx.app  *  1.102e-07 * hh,flx.vol)
check(flx.rmd_csm * 9.562e-08 * hh,flx.vol)
check(flx.rmd_store * 1.740e-07 * hh,flx.vol)
check(flx.posm * 1.370e-07 * 0.15 * hh,flx.vol)
check(flx.rmd * 2.869e-08 * hh,flx.vol)
check(flx.social * 2.093e-10 * hh, flx.vol)
check(flx.edu * 1.679e-07 * hh, flx.vol)
check(cheat1 * 0.0001814190 * hh, flx.vol)
check(newxisimin.val*-3.446557e-05*hh,flx.vol)

# Validate

#############################
# Decomp
#############################

decomp <- cbind(
  flx.tv = flx.tv1  * 5.169e-07 * 1.315773
  + flx.tv2  * (5.169e-07 * 1.315773 - 9.633781e-08)
  + flx.tv3  * (5.169e-07 * 1.315773 - 4.303923e-08)
  , flx.otv = flx.otv1 * 6.462e-07 * 0.4674315
  + (flx.otv2 * 6.462e-07 * 0.4674315 + ((flx.otv2)^(1/3)) * 4.620639e-07) * 1.4
  + (flx.otv3 * 6.462e-07 * 0.4674315 * 0.95)*1.2
  , flx.totv = flx.totv * 4.204e-06 * 0.4674315*1.3
  , flx.digital = flx.digital1 * 2.507776e-13
  + flx.digital2 * 1.91645e-13
  + flx.digital3 * 1.408429e-13
  , flx.sem = flx.semimp1 * 3.24e-09 * 0.1614686
  + flx.semimp2 * (3.24e-09 * 0.1614686 + 0.192977e-10)
  + flx.semimp3 * (3.24e-09 * 0.1614686 + 0.340626e-10)
  , flx.social = flx.social1 * 2.093e-10 * 0.1243447
  + flx.social2 * 2.093e-10 * 0.1243447 * 1.1
  + flx.social3 * 2.093e-10 * 0.1243447
  , flx.app = flx.app1  *  1.102e-07
  + flx.app2  *  1.102e-07 / 1.8
  + flx.app3  *  1.102e-07 * 1.4
  , flx.posm = y1*1.4008665e-05 + flx.posm * 1.370e-07 * 0.15 * 0.58
  , flx.rmd = flx.rmd1*3.546273e-05 + flx.rmd2 * 2.869e-08 + flx.rmd3 * 1.115862e-09
  , flx.edu = flx.edu * 1.679e-07 * 0.1239381
  , competitor = cheat1*-2.335675e-04 + rnct.val2*-2.630674e-07
  , flx.nd = flx.nd*7.751972e-04
) * hh
decomp <- apply(decomp,2,mt)
y.res <- mt(flx.vol) - rowSums(decomp)
y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
decomp <- cbind(decomp,season = y.season)
plot.model(mt(flx.vol),rowSums(decomp),min(mt(flx.vol)))

decomp <- data.frame(decomp,predict=rowSums(decomp),actual=mt(flx.vol),value=mt(flx.val))
rownames(decomp) <- unique(raw$Month)
plot.model(decomp$actual,decomp$predict,min(decomp$predict)*0.5)

decomp <- rbind(decomp,
                apply(decomp,2,function(x){
                  out1 <- tapply(x,rep(1:3,each=12)[1:30],sum)
                  # out2 <- tapply(x,c(rep(0,6),rep(4:5,each=12)),sum)
                  out2 <- tapply(x,c(rep(0,12),rep(4,6),rep(0,6),rep(5,6)),sum)
                  c(out1,out2[-1])
                })
)
############################
# Output
############################

# write.csv(t(decomp),'decomp_flx.csv')
temp <- dplyr::select(decomp,predict,actual)[1:30,]
error <- temp[,2]-temp[,1]


