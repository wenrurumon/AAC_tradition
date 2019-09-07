
rm(list=ls())
library(data.table)
library(dplyr)
raw <- fread('/Users/wenrurumon/Desktop/GSK2018/FSS.csv') %>% as.data.frame()
colnames(raw) <- gsub('\\.| ','_',tolower(colnames(raw)))
ref <- 'sp_catval	0.090829174	0.116605221
tv_grp	0.013067879	0.011204238
digital_grp	0.001008769	4.55811E-05
digital_imp	0.002860871	0.004359724
sp_otv	0	0.072214619
otv_grp	0.02174158	0.022141593
dm_imp	0.003016588	0.009154239
sem_click	0.012245156	0.008209248
weibo_imp	0.001098422	0.0022976
sp_thrutrain	0	0.039444097
sp_ddis	0	0.01895221
sp_tprspd	0.498158925	0.426797027
content_marketing	0	0.011327014'
ref <- do.call(rbind,strsplit(strsplit(ref,'\n')[[1]],'\t'))
ref <- matrix(apply(ref[,-1],2,as.numeric),ncol=2,dimnames=list(ref[,1],NULL))
y <- raw$sp_value
dfile <- raw[,match(rownames(ref),colnames(raw))]

#Sales decomposition by contribution

nmf <- sapply(1:ncol(dfile),function(i){
  xi <- dfile[,i]
  for(j in 2:length(xi)){
    xi[j] <- xi[j-1]*0.75+xi[j]
  }
  print(colnames(dfile)[i])
  sel <- dfile[1:53,i]>0
  y1 <- y[1:53][sel]
  x1 <- xi[1:53][sel]
  x1 <- ((x1-min(x1))/(max(x1)-min(x1)))/5+1
  x1 <- ref[i,1]*sum(y[1:53])/sum(y1) * x1/mean(x1)
  tmp1 <- rep(0,53)
  tmp1[sel] <- x1
  sel <- dfile[-1:-53,i]>0
  y1 <- y[-1:-53][sel]
  x1 <- xi[-1:-53][sel]
  x1 <- ((x1-min(x1))/(max(x1)-min(x1)))/5+1
  x1 <- ref[i,1]*sum(y[-1:-53])/sum(y1) * x1/mean(x1)  
  tmp2 <- rep(0,52)
  tmp2[sel] <- x1
  tmp <- c(tmp1,tmp2)
  tmp 
})
colnames(nmf) <- colnames(dfile)
nmf <- nmf * ifelse(rowSums(nmf)>0.8,0.8,rowSums(nmf))/rowSums(nmf) * y

#Seasonality imputation
hold <- rowSums(nmf)
res <- y-hold
month <- paste0('m',do.call(rbind,strsplit(raw$week,'/'))[,2])
k <- c(7,21)
dummy <- sapply(k,function(k){
 paste0('d',rep(1:(length(y)/k),each=k))
})  
dummy <- data.frame(res,dummy,month)
model <- (lm(res~.,data=dummy))
plot.ts(res); lines(predict(model),col=2)
plot.ts(y); lines((fit<-hold+predict(model)),col=2)
summary(lm(y~fit-1))

#dummy the value outlier 
fit <- predict(model) + hold
d1 <- (abs(fit-y)>min(abs(quantile((fit-y),c(0.01,1))))) * (y-fit)
plot.ts(y); lines(predict(lm(y~fit+d1-1)),col=2)
nmf <- cbind(nmf,festival_payback=d1*coef(lm(y~fit+d1-1))[2])
dfile <- cbind(dfile,festival_payback=d1)

#dummy the errirate outlier 
fit <- predict(model) + hold + nmf[,ncol(nmf)]
d2 <- ifelse(abs(fit/y-1)>=quantile(abs(fit/y-1),99),sign(y-fit),0)
fit2 <- predict(lm(y~fit+d2-1))
nmf <- cbind(nmf,fit_dummy=d2*coef(lm(y~fit+d2-1))[2])
dfile <- cbind(dfile,fit_dummy=d2)

#Model Summary
fdata <- data.frame(y=y,intercept=predict(model),nmf)
fdata.sum <- do.call(rbind,lapply(1:ncol(nmf),function(i){
  res <- y-nmf[,i]
  xi <- dfile[,i]
  x1 <- xi * rep(c(1,0),c(53,52))
  x2 <- xi * rep(c(0,1),c(53,52))
  xi <- cbind(x1,x2)
  colnames(xi) <- paste0(colnames(dfile)[i],c('_Y1',"_Y2"))
  xi <- data.frame(res,xi)
  coef(summary(lm(res~.,data=xi)))[-1,,drop=F]
}))
fdata.sum <- abs(rbind(coef(summary(lm(y~.,data=fdata)))[1:2,],fdata.sum))
fdata.sum[,3] <- fdata.sum[,3] + 1
fdata.sum[,2] <- fdata.sum[,1] / fdata.sum[,2]
fdata.sum[,4] <- 1-pnorm(fdata.sum[,3])

fit <- rowSums(fdata)-y
plot.ts(y); lines(fit,col=2)
summary(abs(fit/y-1))
summary(lm(y~fit-1))
lmtest::dwtest(lm(y~fit-1))

clip <- pipe('pbcopy','w')
write.table(fdata.sum, file=clip, sep = '\t')   
close(clip)

fdata <- cbind(actual=fdata$y,predict=rowSums(fdata)-fdata$y,fdata[,-1])

clip <- pipe('pbcopy','w')
write.table(fdata, file=clip, sep = '\t')   
close(clip)
