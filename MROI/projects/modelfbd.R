rm(list=ls())
setwd('E:\\gsk\\fbd_flx\\model')
# setwd('/Users/wenrurumon/Desktop/Fenbid_Filxonase/model')
# setwd('C:\\Users\\WenluluSens\\Documents\\Project\\GSK\\FBD&FLX_2018\\model')
# raw <- openxlsx::read.xlsx('mmix_fbd.xlsx',1)[,-1]
raw <- as.data.frame(data.table::fread('mmix_fbd_withsku.csv'))[,-1]
raw2 <- as.data.frame(data.table::fread('mmix_fbd_sku2.csv'))[,-1]
pcat1.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('GeneralPainRelief',colnames(raw2),value=T),value=T))])
pcat2.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('MuscularPainRelief',colnames(raw2),value=T),value=T))])
pcat3.val <- (raw2[,colnames(raw2)%in%c(grep('Val.sku',grep('100_Pain',colnames(raw2),value=T),value=T))])
pcat.val <- rowSums(pcat1.val)+rowSums(pcat2.val)+rowSums(pcat3.val)
tcm.val <- rowSums(cbind(pcat1.val,pcat2.val,pcat3.val)[,grep('Tcm',colnames(cbind(pcat1.val,pcat2.val,pcat3.val)))])
# grep('RX',colnames(raw2),value=T)

pcat27.val <- as.numeric(rep(strsplit('422633.4052
                                      370567.7639
                                      419852.032
                                      410312.285
                                      409362.6127
                                      387998.4946
                                      377675.327
                                      381880.0912
                                      418660.1586
                                      441812.3965
                                      444653.0236
                                      473834.3401
                                      524041.1098
                                      388313.4321
                                      450610.1424
                                      447815.0381
                                      450773.3011
                                      419630.8815
                                      420095.9693
                                      422922.7846
                                      464617.418
                                      475248.7342
                                      482814.1005
                                      497183.3324
                                      468123.7459
                                      447681.3311
                                      426285.2324
                                      406705.0132
                                      372833.903
                                      364725.996','\n')[[1]],27))


#############################
# Function
#############################

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
compare <- function(x){
  t1 <- rep(c(rep(1:2,each=12),rep(3,each=6)),length=810)
  t2 <- rep(c(rep(0,12),rep(4,6),rep(0,6),rep(5,6)),length=810)
  c(tapply(x,t1,sum),tapply(x,t2,sum)[-1])
}

#############################
# Processing
#############################

plot.ts(apply(raw[,colnames(raw)%in%grep('FENBID',getvar('VAL.sales'),value=T)[-1:-2]],2,mt))

#Dummy
ydum <- do.call(rbind,strsplit(paste(raw$Month),'/'))[,1]
cdum <- raw$City
mdum <- paste0('M',rep(rep(1:12,length=30),length=810))
y1 <- as.numeric(ydum=='2016')
y2 <- as.numeric(ydum=='2017')
y3 <- as.numeric(ydum=='2018')

#sales data
fbd.val <- getvar('FENBID_VAL.sales|FENBID_FAST_VAL.sales',F,T) - getvar('RX',F,F)[,3]
frx.val <- getvar('RX',F,F)[,3]
f400.val <- getvar('400MG_x_24_VAL.sales',F,T) - getvar('RX',F,F)[,3]
f300.val <- fbd.val - f400.val
f400.ul <- f400.val * 348315.7339/sum(f400.val)
f300.ul <- f300.val * 1082580.493/sum(f300.val)
fbd.ul <- f400.ul + f300.ul
compare(fbd.val)
compare(fbd.ul)

fbd_sku.vol <- raw[,match(grep('PACK.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd_rx.vol <- fbd_sku.vol[,grep('RX',colnames(fbd_sku.vol))]*24
fbd_sku.vol <- fbd_sku.vol[,-grep('RX',colnames(fbd_sku.vol)),drop=F]
f400.vol <- fbd_sku.vol[,grep('400', colnames(fbd_sku.vol))]*24
fbd.vol <- rowSums(as.matrix(fbd_sku.vol) %*% cbind(as.numeric(sapply(strsplit(colnames(fbd_sku.vol),'_'),function(x){x[length(x)-1]}))))
f300.vol <- fbd.vol-f400.vol
compare(fbd.vol)
fbd_sku.pack <- raw[,match(grep('PACK.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd_sku.pack <- fbd_sku.pack[,-grep('RX',colnames(fbd_sku.pack)),drop=F]
fbd.pack <- rowSums(fbd_sku.pack)
compare(fbd.pack)
fbd_sku.val <- raw[,match(grep('VAL.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd_sku.val <- fbd_sku.val[,-grep('RX',colnames(fbd_sku.val)),drop=F]
fbd.val <- rowSums(fbd_sku.val); plot.mt(fbd.val)
compare(fbd.val)
fbd_sku.wd <- raw[,match(grep('WD.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.wd <- apply(fbd_sku.wd,1,max)
fbd_sku.nd <- raw[,match(grep('ND.sales',getvar('FENBID')[-1:-8],value=T),colnames(raw)),drop=F]
fbd.nd <- apply(fbd_sku.nd,1,max)
fbd.avp <- fbd.val/fbd.vol

f400.wd <- getvar('400MG_x_24_WD',F,F)[,1]
f400.nd <- getvar('400MG_x_24_ND',F,F)[,1]
f300.avp <- f300.val / f300.vol
f400.avp <- f400.val / f400.vol
f34avp.idx <- f300.avp/f400.avp


#ATL
fbd.tv <- ret(getvar('GRP.tv',F,F)[,1],0.3)
fbd.tv1 <- ret(getvar('GRP.tv',F,F)[,1]*y1,0.3)
fbd.tv2 <- ret(getvar('GRP.tv',F,F)[,1]*y2,0.3)
fbd.tv3 <- ret(getvar('GRP.tv',F,F)[,1]*y3,0.3)

fbd.otv <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T),0.3)
fbd.otv1 <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T)*y1,0.3)
fbd.otv2 <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T)*y2,0.3)
fbd.otv3 <- ret(getvar('PC_GRP.otv|Mobile_GRP.otv',F,T)*y3,0.3)

fbd.semclick <- ret(getvar('Clicks.sem',F,T),0.3)
fbd.sem1 <- ret(getvar('Clicks.sem',F,T)*y1,0.3)
fbd.sem2 <- ret(getvar('Clicks.sem',F,T)*y2,0.3)
fbd.sem3 <- ret(getvar('Clicks.sem',F,T)*y3,0.3)

fbd.digital <- ret(rowSums(getvar('Impression.digital',F,F)),0.3)
fbd.digital1 <- ret(rowSums(getvar('Impression.digital',F,F))*y1,0.3)
fbd.digital2 <- ret(rowSums(getvar('Impression.digital',F,F))*y2,0.3)
fbd.digital3 <- ret(rowSums(getvar('Impression.digital',F,F))*y3,0.3)

fbd.social <- ret(getvar('Impressions.social|Impressions_.social',F,T),0.3)
fbd.social1 <- ret(getvar('Impressions.social|Impressions_.social',F,T)*y1,0.3)
fbd.social2 <- ret(getvar('Impressions.social|Impressions_.social',F,T)*y2,0.3)
fbd.social3 <- ret(getvar('Impressions.social|Impressions_.social',F,T)*y3,0.3)
# fbd.social[fbd.social==59597928] <- sum(c(1474320,112301))
# fbd.social <- ret(fbd.social,0.3)

#BTL
fbd.posm <- getvar('hejiyujidanpinshuliang.posm',F,T)
fbd.posm1 <- rep(1:30,27)%in%(5:12)
fbd.rmd <- getvar('hejiyujidanpinshuliang.rmd',F,T)


fbd.edu <- getvar('shijirenshu.edu',F,T)
fbd.app <- getvar('saomaliang.app',F,T)

cat.val <- rowSums(raw[,colnames(raw)%in%grep('Pain',getvar('Val.sku'),value=T)])


#############################
# Modeling
#############################

baselog <- rep(log(1:30),27)
base <- fbd.vol - predict(lm(fbd.vol~paste(cdum,mdum)))

# reprocessing
y.vol <- fbd.vol
hh <- y.vol / mc(y.vol)
y.vol2 <- y.vol / hh
rx.dum <- rep(c(1.5,.5,-.5,-.5,-.5,-.5,rep(0,24)),length=810)
holdout <-   cbind(
  intercept = rep(0,810)
  ,fbd.tv=(fbd.tv1*9.826528e-05*2*1.199585
           +fbd.tv2*9.826528e-05*1.8*1.199585
           +fbd.tv3*9.826528e-05*1.8*1.199585)* 0.9 
  ,fbd.otv=(fbd.otv1*5.890516e-04*0.84*1.527221
            +fbd.otv2*5.890516e-04*0.8*1.527221
            +fbd.otv3*5.890516e-04*1.2*1.527221)* 0.8
  ,fbd.sem=(fbd.sem1*4.911940e-06/3*1.657327
            +fbd.sem2*4.911940e-06/2.7*0.9*1.657327
            +fbd.sem3*4.911940e-06/2.6*1.657327)*0.9
  ,fbd.digital = (fbd.digital1*6.815926e-12*30*1.86647
                  +fbd.digital2*2.815926e-12*27*1.717152
                  +fbd.digital3*0.815926e-12*27*1.5*1.679823)*0.8
  ,fbd.social = (fbd.social1*4.007322e-11*4*1.53527
                 +fbd.social2*4.007322e-11*200*1.53527
                 +fbd.social3*4.007322e-11*40*1.53527)*0.8
  ,fbd.posm = (fbd.posm1*0.0084719*1.4826+fbd.posm*5.052831e-06/2*1.4826*0.8)*0.8
  ,fbd.rmd = (fbd.posm1*0.0064719/3*1.6131+fbd.rmd*1.781199e-05/2*1.6131*0.9)*0.8
  ,fbd.app = fbd.app*1.007945e-06*0.8
  ,fbd.edu = fbd.edu*9.573258e-04*0.02013463*0.8
  ,f400.wd = sqrt(baselog)*0.18814143
  ,fbd.wd = mc(fbd.wd) * 0.52794934
  ,f400.canni = +mc(f400.wd)*-0.08682037 #f300.avp * -0.1403404 + f34avp.idx * -0.05405017
  ,rx.dum = rx.dum*-0.01543696# + y1 * -0.0107422750 + y2 * 0.0007546546
  ,cat.val = mc(pcat27.val) * 0.21915082
)
y.vol3 <- y.vol2 - rowSums(holdout)

# Model
xlm <- lm(y.vol3 ~ -1
          # + cdum + mdum
          + paste(cdum,mdum)
          # + I(sqrt(baselog))
          # +baselog
          # + mc(f400.wd)
          # + I(f400.vol/hh)
          # + mc(fbd.wd)
          # + mc(pcat27.val)
          # + f34avp.idx
          # + mc(f300.avp) + mc(f400.avp)
          # + rx.dum
          # +y1+y2+y3
          # + fbd.semclick
          # +fbd.sem1+fbd.sem2+fbd.sem3
          # + f300.avp + f400.avp
)
tail(coef(summary(xlm)),10)
plot.model(mt(y.vol),mt((predict(xlm)+rowSums(holdout))*hh),min(mt(y.vol))*0.5)

# Tech Review
y.raw <- mt(y.vol)
y.pred <- mt((predict(xlm)+rowSums(holdout))*hh)
plot.model(y.raw,y.pred)
summary(abs(y.pred/y.raw-1),2)
summary(lm(y.raw~y.pred-1))$r.square


# Calc
check(mc(pcat27.val) * 0.14299284*hh,fbd.vol)
check((sqrt(baselog)*0.13937019+mc(f400.wd)*-0.0814061+baselog*-0.0002225878)*hh,fbd.vol)
check(sqrt(baselog)*0.08678244*hh+mc(f400.wd)*-0.04839218*hh+baselog*0.02165816*hh,fbd.vol)
check((mc(f400.wd) * 0.04302708 + f400.vol/hh*0.1600186 + baselog*0.01016852 +sqrt(baselog)*-0.007889076 )*hh,fbd.vol)
check(baselog*0.08536940*hh,fbd.vol)
check(fbd.tv*9.826528e-05*2*hh,fbd.vol)
check(fbd.otv*5.890516e-04*0.7*hh,fbd.vol)
check(fbd.semclick*4.911940e-06/3*hh,fbd.vol)
check((fbd.digital1*6.815926e-12+fbd.digital2*2.815926e-12+fbd.digital3*0.815926e-12)*hh,fbd.vol)
check(fbd.social*4.007322e-11*4*hh,fbd.vol)
check(fbd.app*1.007945e-06*hh,fbd.vol)
check(fbd.edu*9.573258e-04*0.02013463*hh,fbd.vol)
check((fbd.posm1*0.0164719+fbd.posm*5.052831e-06/2)*hh,fbd.vol)
check((fbd.posm1*0.0064719+fbd.rmd*1.781199e-05)*hh,fbd.vol)
check(pcat.val*6.39825e-05*hh,fbd.vol)
check(rx.dum * -0.08778013*hh,fbd.vol)

#############################
# Decomp
#############################

decomp <- cbind(
  fbd.tv=(fbd.tv1*9.826528e-05*2*1.199585
          +fbd.tv2*9.826528e-05*1.8*1.199585
          +fbd.tv3*9.826528e-05*1.8*1.199585)* 0.9 
  ,fbd.otv=(fbd.otv1*5.890516e-04*0.84*1.527221
            +fbd.otv2*5.890516e-04*0.8*1.527221
            +fbd.otv3*5.890516e-04*1.2*1.527221)* 0.8
  ,fbd.sem=(fbd.sem1*4.911940e-06/3*1.657327
            +fbd.sem2*4.911940e-06/2.7*0.9*1.657327
            +fbd.sem3*4.911940e-06/2.6*1.657327)*0.9
  ,fbd.digital = (fbd.digital1*6.815926e-12*30*1.86647
                  +fbd.digital2*2.815926e-12*27*1.717152
                  +fbd.digital3*0.815926e-12*27*1.5*1.679823)*0.8
  ,fbd.social = (fbd.social1*4.007322e-11*4*1.53527
                 +fbd.social2*4.007322e-11*200*1.53527
                 +fbd.social3*4.007322e-11*40*1.53527)*0.8
  ,fbd.posm = (fbd.posm1*0.0084719*1.4826+fbd.posm*5.052831e-06/2*1.4826*0.8)*0.8
  ,fbd.rmd = (fbd.posm1*0.0064719/3*1.6131+fbd.rmd*1.781199e-05/2*1.6131*0.9)*0.8
  ,fbd.app = fbd.app*1.007945e-06*0.8
  ,fbd.edu = fbd.edu*9.573258e-04*0.02013463*0.8
  ,f400.wd = sqrt(baselog)*0.18814143
  ,fbd.wd = mc(fbd.wd) * 0.52794934
  ,f400.canni = +mc(f400.wd)*-0.08682037 #f300.avp * -0.1403404 + f34avp.idx * -0.05405017
  ,rx.dum = rx.dum*-0.01543696# + y1 * -0.0107422750 + y2 * 0.0007546546
  ,cat.val = mc(pcat27.val) * 0.21915082
)
decomp <- apply(decomp,2,function(x){mt(x*hh)})
y.res <- mt(y.vol) - rowSums(decomp)
y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
decomp <- cbind(decomp,season = y.season)
plot.model(mt(y.vol),rowSums(decomp),min(mt(y.vol))/3)

decomp <- data.frame(decomp,predict=rowSums(decomp),actual=mt(fbd.vol),value=mt(fbd.val))
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

write.csv(t(decomp),'decomp_fbd.csv')

############################
# FBD300
############################
#
# y.vol <- f300.vol
# hh <- y.vol / mc(y.vol)
#
# holdout <- cbind(
#   fbd.tv=fbd.tv1*9.826528e-05*2
#   +fbd.tv2*9.826528e-05*1.8
#   +fbd.tv3*9.826528e-05*1.8
#   ,fbd.otv=fbd.otv1*5.890516e-04*0.84
#   +fbd.otv2*5.890516e-04*0.8
#   +fbd.otv3*5.890516e-04*1.2
#   ,fbd.sem=fbd.sem1*4.911940e-06/3
#   +fbd.sem2*4.911940e-06/2.7*0.9
#   +fbd.sem3*4.911940e-06/2.6
#   ,fbd.digital = fbd.digital1*6.815926e-12*30
#   +fbd.digital2*2.815926e-12*27
#   +fbd.digital3*0.815926e-12*27*1.5
#   ,fbd.social = fbd.social1*4.007322e-11*4
#   +fbd.social2*4.007322e-11*200
#   +fbd.social3*4.007322e-11*40
#   ,fbd.posm = fbd.posm1*0.0084719+fbd.posm*5.052831e-06/2
#   ,fbd.rmd = fbd.posm1*0.0064719/2+fbd.rmd*1.781199e-05/2
#   ,fbd.app = fbd.app*1.007945e-06
#   ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
# )
# check(rowSums(holdout)*hh,fbd.vol)
# holdout <- rowSums(holdout)
# y.vol2 <- mc(y.vol)
# check(holdout*hh,f300.vol)
# y.vol3 <- y.vol2 - holdout
#
# tail(coef(summary(
#   xlm <- lm(y.vol3~cdum+mdum + f300.avp + f400.wd + rx.dum + fbd.wd
#   )
# ))); plot.model(mt(y.vol),mt(predict(xlm)*hh+holdout*hh))
#
# decomp <- cbind(
#   fbd.tv=fbd.tv1*9.826528e-05*2
#   +fbd.tv2*9.826528e-05*1.8
#   +fbd.tv3*9.826528e-05*1.8
#   ,fbd.otv=fbd.otv1*5.890516e-04*0.84
#   +fbd.otv2*5.890516e-04*0.8
#   +fbd.otv3*5.890516e-04*1.2
#   ,fbd.sem=fbd.sem1*4.911940e-06/3
#   +fbd.sem2*4.911940e-06/2.7*0.9
#   +fbd.sem3*4.911940e-06/2.6
#   ,fbd.digital = fbd.digital1*6.815926e-12*30
#   +fbd.digital2*2.815926e-12*27
#   +fbd.digital3*0.815926e-12*27*1.5
#   ,fbd.social = fbd.social1*4.007322e-11*4
#   +fbd.social2*4.007322e-11*200
#   +fbd.social3*4.007322e-11*40
#   ,fbd.posm = fbd.posm1*0.0084719+fbd.posm*5.052831e-06/2
#   ,fbd.rmd = fbd.posm1*0.0064719+fbd.rmd*1.781199e-05
#   ,fbd.app = fbd.app*1.007945e-06
#   ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
#   ,f300.avp = f300.avp * -8.451082346
#   ,f400.wd = f400.wd * -0.008542408
#   ,rx.dum = rx.dum * -0.192382389
#   ,fbd.wd = fbd.wd * 0.007093139
# )
# decomp <- apply(decomp,2,function(x){mt(x*hh)})
# y.res <- mt(y.vol) - rowSums(decomp)
# y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
# # y.season <- 0
# decomp <- cbind(decomp,season = y.season)
# plot.model(mt(y.vol),rowSums(decomp),min(mt(y.vol))/3)
#
# decomp <- data.frame(decomp,predict=rowSums(decomp),actual=mt(f300.vol),value=mt(f300.val))
# rownames(decomp) <- unique(raw$Month)
# plot.model(decomp$actual,decomp$predict,min(decomp$predict)*0.5)
#
# decomp <- rbind(decomp,
#                 apply(decomp,2,function(x){
#                   out1 <- tapply(x,rep(1:3,each=12)[1:30],sum)
#                   out2 <- tapply(x,c(rep(0,6),rep(4:5,each=12)),sum)
#                   c(out1,out2[-1])
#                 })
# )
# # write.csv(t(decomp),'decomp_f300.csv')
#
# ############################
# # FBD400
# ############################
#
# y.vol <- f400.vol
# hh <- y.vol / mc(y.vol)
#
# plot.ts(temp <- (1:30)^(1/20))
# summary(lm(mt(y.vol)~temp))
# temp <- rep(temp,27)
#
# holdout <- cbind(
#   fbd.tv=fbd.tv1*9.826528e-05*2
#   +fbd.tv2*9.826528e-05*1.8
#   +fbd.tv3*9.826528e-05*1.8
#   ,fbd.otv=fbd.otv1*5.890516e-04*0.84
#   +fbd.otv2*5.890516e-04*0.8
#   +fbd.otv3*5.890516e-04*1.2
#   ,fbd.sem=fbd.sem1*4.911940e-06/3
#   +fbd.sem2*4.911940e-06/2.7*0.9
#   +fbd.sem3*4.911940e-06/2.6
#   ,fbd.digital = fbd.digital1*6.815926e-12*30
#   +fbd.digital2*2.815926e-12*27
#   +fbd.digital3*0.815926e-12*27*1.5
#   ,fbd.social = fbd.social1*4.007322e-11*4
#   +fbd.social2*4.007322e-11*200
#   +fbd.social3*4.007322e-11*40
#   ,fbd.posm = fbd.posm1*0.0084719+fbd.posm*5.052831e-06/2
#   ,fbd.rmd = fbd.posm1*0.0064719+fbd.rmd*1.781199e-05
#   ,fbd.app = fbd.app*1.007945e-06
#   ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
#   ,fbd400.wd = temp * 5.816138056 + f400.wd * 0.004073018
#   ,f300.avp = f300.avp * -0.41222899
#   ,rx.dum = rx.dum  * 0.05666099
# )
# holdout <- rowSums(holdout)
# y.vol2 <- mc(y.vol)
# y.vol3 <- y.vol2 - holdout
# tail(coef(summary(xlm <- lm(
#   y.vol3~cdum + mdum
# ))))
# plot.model(mt(y.vol),mt(predict(xlm)*hh+holdout*hh))
#
# decomp <- cbind(
#   fbd.tv=fbd.tv1*9.826528e-05*2
#   +fbd.tv2*9.826528e-05*1.8
#   +fbd.tv3*9.826528e-05*1.8
#   ,fbd.otv=fbd.otv1*5.890516e-04*0.84
#   +fbd.otv2*5.890516e-04*0.8
#   +fbd.otv3*5.890516e-04*1.2
#   ,fbd.sem=fbd.sem1*4.911940e-06/3
#   +fbd.sem2*4.911940e-06/2.7*0.9
#   +fbd.sem3*4.911940e-06/2.6
#   ,fbd.digital = fbd.digital1*6.815926e-12*30
#   +fbd.digital2*2.815926e-12*27
#   +fbd.digital3*0.815926e-12*27*1.5
#   ,fbd.social = fbd.social1*4.007322e-11*4
#   +fbd.social2*4.007322e-11*200
#   +fbd.social3*4.007322e-11*40
#   ,fbd.posm = fbd.posm1*0.0084719+fbd.posm*5.052831e-06/2
#   ,fbd.rmd = fbd.posm1*0.0064719+fbd.rmd*1.781199e-05
#   ,fbd.app = fbd.app*1.007945e-06
#   ,fbd.edu = fbd.edu*9.573258e-04*0.02013463
#   ,fbd400.wd = temp * 5.816138056 + f400.wd * 0.004073018
#   ,f300.avp = f300.avp * -0.41222899
#   ,rx.dum = rx.dum  * 0.05666099
# )
# decomp <- apply(decomp,2,function(x){mt(x*hh)})
# y.res <- mt(y.vol) - rowSums(decomp)
# y.season <- predict(lm(y.res ~ paste(rep(1:12,length=30))-1))
#
# # y.season <- 0
# decomp <- cbind(decomp,season = y.season)
# y.res <- mt(y.vol) - rowSums(decomp)
# plot.model(mt(y.vol),rowSums(decomp),min(mt(y.vol))/3)
#
# decomp <- data.frame(decomp,predict=rowSums(decomp),actual=mt(f400.vol),value=mt(f400.val))
# rownames(decomp) <- unique(raw$Month)
# plot.model(decomp$actual,decomp$predict,min(decomp$predict)*0.5)
#
# decomp <- rbind(decomp,
#                 apply(decomp,2,function(x){
#                   out1 <- tapply(x,rep(1:3,each=12)[1:30],sum)
#                   out2 <- tapply(x,c(rep(0,6),rep(4:5,each=12)),sum)
#                   c(out1,out2[-1])
#                 })
# )
# # write.csv(t(decomp),'decomp_f400.csv')
