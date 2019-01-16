
rm(list=ls())
setwd('E:\\gsk\\fbd_flx\\model')
# raw <- openxlsx::read.xlsx('mmix_flx.xlsx',1)[,-1]
# setwd('/Users/wenrurumon/Desktop/Fenbid_Filxonase/model')
# setwd('C:\\Users\\WenluluSens\\Documents\\Project\\GSK\\FBD&FLX_2018\\model')
raw <- as.data.frame(data.table::fread('mmix_flx_withsku.csv'))[,-1]
cat27.val <- read.table('clipboard',header=T)
'''
PainCat	GMCat	CLARITYNE	XISIMIN	RHINOCORT
462836.1622	51829.47751	11512.9407	5381.484196	831.1151261
405439.0785	47928.34847	11148.72798	5587.099997	870.4827166
460697.4148	63888.93766	14602.40493	6621.780661	1396.361957
449360.4662	68374.95135	15877.83161	7799.399329	1786.358039
448262.785	67411.98746	15638.04128	7853.534288	2101.032885
426215.6829	60902.37793	15060.47122	6849.838992	1851.575964
415323.4771	60861.30923	15008.02212	6670.504691	1967.709703
419803.873	71156.22126	16724.75855	6912.48082	3213.723612
456220.7842	72329.42462	16139.23415	6920.999955	3408.617855
480492.7507	64448.99386	14469.22327	7097.216485	3467.302821
482529.4355	61895.81589	13380.01458	6164.505212	3431.9435
513098.042	58797.41078	12840.99041	5794.10371	2972.769121
563908.2828	55002.37803	12295.89557	5988.052064	2646.501034
423732.3408	51136.07852	10663.46884	6014.323199	2583.615053
491389.0824	66655.70169	14408.26212	7061.766921	2991.4544
488689.8731	76680.18209	17226.29812	8888.748111	2740.639115
493427.6441	74803.66074	16785.4883	8733.687316	2525.187282
459788.793	61948.2011	14053.49264	7590.611728	1766.436953
461563.6282	64293.94559	14619.41513	8187.08635	1848.35381
463685.6882	73255.31491	16706.71876	8407.243024	1878.248374
505864.2248	78969.42111	17209.10295	8344.62923	2219.311112
515604.2413	70042.53004	15245.71479	7845.44401	2359.696155
523030.409	64406.102	13428.14765	6990.226203	1981.651849
538429.5562	62791.99669	13506.52005	6782.675452	1955.773051
509782.6496	56224.01469	11855.95748	6606.8205	1834.563436
484851.7128	50856.03851	11265.77805	6993.366275	1651.186525
470263.9657	69866.36665	14537.28074	8717.975189	2850.7114
451635.7668	79179.3158	17431.22717	9702.585311	3503.723734
418335.9593	73550.05469	16413.02285	9262.620817	3176.082207
408120.5237	64491.81452	14362.36966	8475.840971	2483.736663
'''

cat27.val <- apply(cat27.val,2,function(x){rep(x,27)})

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

ec.val <- '720567	457112	824482	1051324	1021869	865758	1009593	939943	764087	1303571	1589617	970861	624044	827010	1324979	1660868	2542916	1453290	1575827	1981953	3392455	2385435	3519818	1733936	1452124	748255	1578784	1999537	1851765	2650754'
ec.val <- as.numeric(strsplit(ec.val,'\t')[[1]])
ec.val <- rep(ec.val,27)
# comp.val2 <- rowSums(raw[,colnames(raw)%in%grep('Nasal',getvar('Val.sku'),value=T)[-3]])
# cat.val <- rowSums(raw[,colnames(raw)%in%grep('Nasal',getvar('Val.sku'),value=T)])


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

# comp.res <- comp.val2 - predict(lm(comp.val2~paste(cdum,mdum)))
hh <- flx.vol / mc(flx.vol) * 501.9995316
flx.rmd1 <- rep(1:30,27) %in% (10:13)
cheat1 <- rep(0,810)
cheat1[rep(1:30,27)%in%c(1,2,6)] <- -1/3
cheat1[rep(1:30,27)==4] <- 1
cheat2 <- c(rep(0,14),rep(-1,1),rep(1,2),rep(-1,1),rep(0,12))
cheat2 <- rep(cheat2,27)
GMcat.val <- cat27.val[,2]
crt.val <- cat27.val[,3]
xsm.val <- cat27.val[,4]
lnct.val  <- cat27.val[,5]

flx.vol2 <- flx.vol / hh
holdout <-   cbind(
  intercept = rep(0,810)
  , flx.tv = (flx.tv1  * 5.169e-07 * 1.315773
               + flx.tv2  * (5.169e-07 * 1.315773 - 9.633781e-08)
               + flx.tv3  * (5.169e-07 * 1.315773 - 4.303923e-08)) * 0.97
  , flx.otv = (flx.otv1 * 6.462e-07 * 0.4674315
               + (flx.otv2 * 6.462e-07 * 0.4674315 + ((flx.otv2)^(1/3)) * 4.620639e-07) * 1.4
               + (flx.otv3 * 6.462e-07 * 0.4674315 * 0.95)*1.2)*0.93
  , flx.totv = flx.totv * 4.204e-06 * 0.4674315*1.3*0.93
  , flx.digital = (flx.digital1 * 2.507776e-13
                   + flx.digital2 * 1.91645e-13
                   + flx.digital3 * 1.408429e-13*1.5/1.7)*0.97
  , flx.sem = (flx.semimp1 * 3.24e-09 * 0.1614686
               + flx.semimp2 * (3.24e-09 * 0.1614686 + 0.192977e-10)
               + flx.semimp3 * (3.24e-09 * 0.1614686 + 0.340626e-10))*0.9
  , flx.social = (flx.social1 * 2.093e-10 * 0.1243447
                  + flx.social2 * 2.093e-10 * 0.1243447 * 1.1
                  + flx.social3 * 2.093e-10 * 0.1243447)*1.02
  , flx.app = (flx.app1  *  1.102e-07
               + flx.app2  *  1.102e-07 / 1.8
               + flx.app3  *  1.102e-07 * 1.4)*0.9
  , flx.posm = (y1*1.4008665e-05 + flx.posm * 1.370e-07 * 0.15 * 0.58)*1.02
  , flx.rmd = (flx.rmd1*3.546273e-05 + flx.rmd2 * 2.869e-08 + flx.rmd3 * 1.115862e-09)*1.03
  , flx.edu = flx.edu * 1.679e-07 * 0.1239381
  ,xms.val = xsm.val*-2.969309e-08
  ,crt.val = crt.val*-9.700495e-09
  ,lnct.val = lnct.val*-4.460597e-08
  , flx.wd = flx.wd * 2.668474e-03  + cheat1 * -1.447050e-04
  , flx.avp = flx.avp * -0.0001313869
  , ec.val = ec.val * -1.284366e-11
  , cat.val = GMcat.val*2.502769e-08
)

flx.vol3 <- flx.vol2 - rowSums(holdout)

# Model
xlm <- lm(flx.vol3 ~ -1
          + paste(cdum,mdum)  
          # + cheat1
          +y3
          # + flx.wd
          # + flx.avp
          # + cheat1
          # + GMcat.val
          # + ec.val
          # + cheat2
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
  flx.tv = (flx.tv1  * 5.169e-07 * 1.315773
            + flx.tv2  * (5.169e-07 * 1.315773 - 9.633781e-08)
            + flx.tv3  * (5.169e-07 * 1.315773 - 4.303923e-08)) * 0.97
  , flx.otv = (flx.otv1 * 6.462e-07 * 0.4674315
               + (flx.otv2 * 6.462e-07 * 0.4674315 + ((flx.otv2)^(1/3)) * 4.620639e-07) * 1.4
               + (flx.otv3 * 6.462e-07 * 0.4674315 * 0.95)*1.2)*0.93
  , flx.totv = flx.totv * 4.204e-06 * 0.4674315*1.3*0.93
  , flx.digital = (flx.digital1 * 2.507776e-13
                   + flx.digital2 * 1.91645e-13
                   + flx.digital3 * 1.408429e-13*1.5/1.7)*0.97
  , flx.sem = (flx.semimp1 * 3.24e-09 * 0.1614686
               + flx.semimp2 * (3.24e-09 * 0.1614686 + 0.192977e-10)
               + flx.semimp3 * (3.24e-09 * 0.1614686 + 0.340626e-10))*0.9
  , flx.social = (flx.social1 * 2.093e-10 * 0.1243447
                  + flx.social2 * 2.093e-10 * 0.1243447 * 1.1
                  + flx.social3 * 2.093e-10 * 0.1243447)*1.02
  , flx.app = (flx.app1  *  1.102e-07
               + flx.app2  *  1.102e-07 / 1.8
               + flx.app3  *  1.102e-07 * 1.4)*0.9
  , flx.posm = (y1*1.4008665e-05 + flx.posm * 1.370e-07 * 0.15 * 0.58)*1.02
  , flx.rmd = (flx.rmd1*3.546273e-05 + flx.rmd2 * 2.869e-08 + flx.rmd3 * 1.115862e-09)*1.03
  , flx.edu = flx.edu * 1.679e-07 * 0.1239381
  ,xms.val = xsm.val*-2.969309e-08
  ,crt.val = crt.val*-9.700495e-09
  ,lnct.val = lnct.val*-4.460597e-08
  , flx.wd = flx.wd * 2.668474e-03  + cheat1 * -1.447050e-04
  , flx.avp = flx.avp * -0.0001313869
  , ec.val = ec.val * -1.284366e-11
  , cat.val = GMcat.val*2.502769e-08
)
decomp <- apply(decomp,2,function(x){mt(x*hh)})
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

write.csv(t(decomp),'decomp_flx.csv')
temp <- dplyr::select(decomp,predict,actual)[1:30,]
error <- temp[,2]-temp[,1]

compare <- function(x){
  t1 <- rep(c(rep(1:2,each=12),rep(3,each=6)),length=810)
  t2 <- rep(c(rep(0,12),rep(4,6),rep(0,6),rep(5,6)),length=810)
  c(tapply(x,t1,sum),tapply(x,t2,sum)[-1])
}
compare(flx.val)
compare(flx.vol)
compare(flx.val)/compare(flx.vol)
