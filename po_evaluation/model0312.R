
rm(list=ls())
library(data.table)
library(dplyr)
library(reshape2)
library(kernlab)
library(ggplot2)
library(forecast)
setwd('/Users/huzixin/Documents/baolelijia/anya/0312')

##############################
#Module
##############################

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')
  close(clip)
}

links <- function(y,h=48,maxr=1.5){
  y <- y+1
  x <- 1:length(y)
  loss <- function(r){
    y0 <- c(y[1],y[length(y)])
    x0 <- c(x[1],(x[length(x)])^r)
    x0r1 <- c(x[1],(x[length(x)]))
    sum(abs(y-cbind(1,(1:length(x))^r) %*% cbind(coef(lm(y0~x0)))))/
      sum(abs(y-cbind(1,(1:length(x))^1) %*% cbind(coef(lm(y0~x0r1))))) + 
      r/10
  }
  #Linear Fit
  r <- 1
  y0 <- c(y[1],y[length(y)])
  x0 <- c(x[1],(x[length(x)])^r)
  p <- cbind(1,(1:(length(x)))^r) %*% cbind(coef(lm(y0~x0)))
  # plot.ts(y); lines(p,col=2)
  sel <- y/p>0.9
  #Organic Fit
  (r <- optim(1,loss,control=list(maxit=10000))$par)
  r <- min(r,maxr)
  y0 <- c(y[sel][1],y[sel][sum(sel)])
  x0 <- c(x[sel][1],(x[sel][sum(sel)])^r)
  p2 <- cbind(1,(1:(length(x)))^r) %*% cbind(coef(lm(y0~x0)))
  # plot.ts(y); lines(p,col=2); lines(p2,col=4)
  #Generate Curve
  out <- cbind(1,(1:(length(x)+h))^r) %*% cbind(coef(lm(y0~x0)))
  out[1:length(y)] <- y
  plot.ts(out,col=2); lines(p2,col=2); lines(y)
  out-1
}

links0 <- function(y,h=48,maxr=1.5){
  y <- y+1
  x <- 1:length(y)
  loss <- function(r){
    y0 <- c(y[1],y[length(y)])
    x0 <- c(x[1],(x[length(x)])^r)
    x0r1 <- c(x[1],(x[length(x)]))
    sum(abs(y-cbind(1,(1:length(x))^r) %*% cbind(coef(lm(y0~x0)))))/
      sum(abs(y-cbind(1,(1:length(x))^1) %*% cbind(coef(lm(y0~x0r1))))) + 
      r/10
  }
  #Linear Fit
  r <- 1
  y0 <- c(y[1],y[length(y)])
  x0 <- c(x[1],(x[length(x)])^r)
  p <- cbind(1,(1:(length(x)))^r) %*% cbind(coef(lm(y0~x0)))
  # plot.ts(y); lines(p,col=2)
  sel <- y/p>0.9
  #Organic Fit
  (r <- optim(1,loss,control=list(maxit=10000))$par)
  r <- min(r,maxr)
  y0 <- c(y[sel][1],y[sel][sum(sel)])
  x0 <- c(x[sel][1],(x[sel][sum(sel)])^r)
  p2 <- cbind(1,(1:(length(x)))^r) %*% cbind(coef(lm(y0~x0)))
  # plot.ts(y); lines(p,col=2); lines(p2,col=4)
  #Generate Curve
  out <- cbind(1,(1:(length(x)))^r) %*% cbind(coef(lm(y0~x0)))
  # out[1:length(y)] <- y
  plot.ts(y); lines(out,col=2)
  out-1
}

##############################
#Raw sales and storemaster process
##############################

#a map of months
monthlist <- rep(2000:2030,each=12)*100+rep(1:12,length=372)
getmonths <- function(start,end){
  monthlist[match(start,monthlist):match(end,monthlist)]
}

#sales[store,brand,month,vol]

raw_vol_report <- lapply(dir(pattern='vol_report'),fread)
raw_vol_report <- lapply(raw_vol_report,function(x){
  colnames(x)　<- tolower(colnames(x))
  x
})
names(raw_vol_report) <- dir(pattern='vol_report')

raw_vol_report <- lapply(raw_vol_report,function(x){
  colmiss <- unique(unlist(lapply(raw_vol_report,colnames))) %in% colnames(x)
  colmiss <- unique(unlist(lapply(raw_vol_report,colnames)))[!colmiss]
  xmiss <- matrix(NA,nrow=nrow(x),ncol=length(colmiss))
  colnames(xmiss) <- colmiss
  x <- data.frame(x,xmiss) 
  x[,match(unique(unlist(lapply(raw_vol_report,colnames))),colnames(x))]
})

# lapply(raw_vol_report,function(x){
#   x %>%
#     filter(l1=='PT',l3=='On Trade') %>%
#     group_by(po_number=='NULL') %>%
#     summarise(sum(total_quantity_9l_intake,na.rm=T))
# })

sales <- do.call(rbind,raw_vol_report) %>%
  select(store=outlet_code,date=std_date,brand=subbrand_code,
         vol=total_quantity_9l_intake,po=po_number) %>%
  filter(!is.na(vol)) %>%
  as.data.table

#storemaster[store,region,channel,division]
#always use the latest info for each store

storemaster <- do.call(rbind,raw_vol_report) %>%
  select(date=std_date,store=outlet_code,region=region_eng_name,
         channel=channel_group,l1=l1,l3=l3,falcon=falconflag,
         parent=parent_outletcode,
         division=division_eng_name) %>%
  unique()

storemaster <- storemaster %>%
  merge(
    storemaster %>%
      group_by(store) %>%
      summarise(date=max(date))
  )

storemaster <- storemaster %>%
  mutate(region=tolower(region)) %>%
  mutate(region=gsub(' region','',region)) %>%
  mutate(region=ifelse(grepl('head',region),'headquarter',region)) %>%
  mutate(region=ifelse(grepl('hainan',region),'south',region)) %>%
  mutate(region=ifelse(grepl('west',region),'west',region))

#Final sales [sales,storemaster for PT on trade only]

fsales <- sales
fsales <- data.table(fsales,
                     storemaster[match(fsales$store,storemaster$store),-1:-2]) %>%
  # filter(date!=max(sales$month)) %>%
  filter(l1=='PT',l3=='On Trade') %>%
  mutate(fy=floor(date/100)+1,fy=ifelse(date%%100<=6,fy-1,fy)) %>%
  mutate(po=ifelse(po=='NULL',NA,po))

storemaster2 <- storemaster %>% filter(store%in%fsales$store)

####################################################
#Data Review

nat.sales <- fsales %>%
  group_by(region='total',date) %>%
  summarise(vol=sum(vol)) %>%
  acast(date~region,value.var='vol')

nat.mat <- data.table(
  month=rownames(nat.sales)[-1:-11],
  (sapply(12:nrow(nat.sales),function(i){
    colSums(nat.sales[1:12+i-12,,drop=F])
  }))
)

test <- cbind(nat.mat,links0(nat.mat$V2))[,-1]

region.sales <- fsales %>%
  group_by(region,date) %>%
  summarise(vol=sum(vol)) %>%
  acast(date~region,value.var='vol') 

region.mat <- data.table(
  month=as.numeric(rownames(region.sales)[-1:-11]),
  t(sapply(12:nrow(region.sales),function(i){
    colSums(region.sales[1:12+i-12,])
  }))
)

write.clip(cbind(region.mat,apply(region.mat[,-1],2,links0)))

fsales %>%
  group_by(month=date,po=!is.na(po)) %>%
  summarise(vol=sum(vol)) %>%
  acast(month~po,value.var='vol') %>%
  write.clip


####################################################
#Nat Sales

nat.sales <- fsales %>%
  group_by(region='total',date) %>%
  summarise(vol=sum(vol)) %>%
  acast(date~region,value.var='vol')

nat.mat <- data.table(
  month=rownames(nat.sales)[-1:-11],
  (sapply(12:nrow(nat.sales),function(i){
    colSums(nat.sales[1:12+i-12,,drop=F])
  }))
)

nat.fcast <- apply(nat.mat[,-1],2,links,h=48)
rownames(nat.fcast) <- c(nat.mat$month,rep(NA,48))
for(i in 13:nrow(nat.fcast)){
  rownames(nat.fcast)[i] <- as.numeric(rownames(nat.fcast)[i-12])+100
}

for(i in 1:48){
  nat.sales <- rbind(
    nat.sales,
    nat.fcast[nrow(nat.mat)+i,]-colSums(nat.sales[nrow(nat.sales)-0:10,,drop=F])
  )
  rownames(nat.sales)[nrow(nat.sales)] <-
    as.numeric(rownames(nat.sales)[nrow(nat.sales)-12])+100
}

####################################################
#By Region Sales

region.sales <- fsales %>%
  group_by(region,date) %>%
  summarise(vol=sum(vol)) %>%
  acast(date~region,value.var='vol') 

region.mat <- data.table(
  month=as.numeric(rownames(region.sales)[-1:-11]),
  t(sapply(12:nrow(region.sales),function(i){
    colSums(region.sales[1:12+i-12,])
  }))
)

region.fcast <- apply(region.mat[,-1],2,links,h=48)
region.mat.cagr <- (region.fcast[nrow(region.mat),]/region.fcast[1,])^(1/(nrow(region.mat)))
region.fcast.cagr <- (region.fcast[nrow(region.fcast),]/region.fcast[1,])^(1/(nrow(region.fcast)))
rownames(region.fcast) <- c(region.mat$month,rep(NA,48))
for(i in 13:nrow(region.fcast)){
  rownames(region.fcast)[i] <- as.numeric(rownames(region.fcast)[i-12])+100
}

for(i in 1:48){
  region.sales <- rbind(
    region.sales,
    region.fcast[nrow(region.mat)+i,]-colSums(region.sales[nrow(region.sales)-0:10,])
  )
  rownames(region.sales)[nrow(region.sales)] <-
    as.numeric(rownames(region.sales)[nrow(region.sales)-12])+100
}

####################################################
#By Brand Sales

brand.sales <- fsales %>%
  group_by(fy,brand,date) %>%
  summarise(vol=sum(vol))
brand.sel <- unique((brand.sales %>% filter(fy==max(brand.sales$fy),vol>0))$brand)

brand.sales <- brand.sales %>%
  filter(brand %in% brand.sel) %>%
  acast(date~brand,value.var='vol',fill=0)

brand.mat <- data.table(
  month=rownames(brand.sales)[-1:-11],
  t(sapply(12:nrow(brand.sales),function(i){
    colSums(brand.sales[1:12+i-12,])
  }))
)

brand.fcast <- apply(brand.mat[,-1],2,function(x){
  out <- links(x,h=48,maxr=1.5)
  ifelse(out<0,0,out)
})

brand.mat.cagr <- ((brand.fcast+1)[nrow(brand.mat),]/(brand.fcast+1)[1,])^(1/(nrow(brand.mat)))
brand.fcast.cagr <- ((brand.fcast+1)[nrow(brand.fcast),]/(brand.fcast+1)[1,])^(1/(nrow(brand.fcast)))
data.table(actual=(brand.mat.cagr)^12,predict=(brand.fcast.cagr)^12) %>% arrange(desc(predict))

rownames(brand.fcast) <- c(brand.mat$month,rep(NA,48))
for(i in 13:nrow(brand.fcast)){
  rownames(brand.fcast)[i] <- as.numeric(rownames(brand.fcast)[i-12])+100
}

for(i in 1:48){
  brand.sales <- rbind(
    brand.sales,
    brand.fcast[nrow(brand.mat)+i,]-colSums(brand.sales[nrow(brand.sales)-0:10,])
  )
  rownames(brand.sales)[nrow(brand.sales)] <-
    as.numeric(rownames(brand.sales)[nrow(brand.sales)-12])+100
}

####################################################
#By Brand By Region

brand.sales2 <- fsales %>%
  group_by(fy,brand=paste(region,brand),date) %>%
  summarise(vol=sum(vol))
brand.sel <- unique((brand.sales2 %>% filter(fy==max(brand.sales2$fy),vol>0))$brand)

brand.sales2 <- brand.sales2 %>%
  filter(brand %in% brand.sel) %>%
  acast(date~brand,value.var='vol',fill=0)

brand.mat2 <- data.table(
  month=rownames(brand.sales2)[-1:-11],
  t(sapply(12:nrow(brand.sales2),function(i){
    colSums(brand.sales2[1:12+i-12,])
  }))
)

brand.fcast2 <- apply(brand.mat2[,-1],2,function(x){
  out <- links(x,h=48,maxr=1.2)
  ifelse(out<0,0,out)
})
rownames(brand.fcast2) <- c(brand.mat2$month,rep(NA,48))
for(i in 13:nrow(brand.fcast2)){
  rownames(brand.fcast2)[i] <- as.numeric(rownames(brand.fcast2)[i-12])+100
}

brand.mat2.cagr <- ((brand.fcast2+1)[nrow(brand.mat2),]/(brand.fcast2+1)[1,])^(1/(nrow(brand.mat2)))
brand.fcast2.cagr <- ((brand.fcast2+1)[nrow(brand.fcast2),]/(brand.fcast2+1)[1,])^(1/(nrow(brand.fcast2)))

refcagr <- data.table(
  brand=colnames(brand.fcast2),
  actual=(brand.mat2.cagr)^12,
  predict=(brand.fcast2.cagr)^12) %>% arrange(desc(predict))

for(i in 1:48){
  brand.sales2 <- rbind(
    brand.sales2,
    brand.fcast2[nrow(brand.mat2)+i,]-colSums(brand.sales2[nrow(brand.sales2)-0:10,])
  )
  rownames(brand.sales2)[nrow(brand.sales2)] <-
    as.numeric(rownames(brand.sales2)[nrow(brand.sales2)-12])+100
}

####################################################
#Error Rate

(check <- data.table(
  month=rownames(nat.sales),
  nat=nat.sales,
  region=rowSums(region.sales),
  brand=rowSums(brand.sales),
  brand2=rowSums(brand.sales2)
)) %>%
  write.clip

check %>%
  melt(id='month') %>%
  ggplot() + 
  geom_line(aes(x=match(month,check$month),y=value,colour=variable))

check <- check %>%
  # mutate(nat.total=brand2) %>%
  melt(id=1:2) %>%
  mutate(rate=nat.total/value) %>%
  select(month,model=variable,rate)

region.sales <- melt(region.sales) %>%
  select(month=Var1,region=Var2,value) %>%
  merge(check %>% filter(model=='region')) %>%
  mutate(value=value*rate) %>%
  acast(month~region,value.var='value') 

brand.sales <- melt(brand.sales) %>%
  select(month=Var1,region=Var2,value) %>%
  merge(check %>% filter(model=='brand')) %>%
  mutate(value=value*rate) %>%
  acast(month~region,value.var='value') 

brand.sales2 <- melt(brand.sales2) %>%
  select(month=Var1,region=Var2,value) %>%
  merge(check %>% filter(model=='brand2')) %>%
  mutate(value=value*rate) %>%
  acast(month~region,value.var='value') 

nat.sales <- apply(nat.sales,2,function(x){
  out <- rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  ifelse(out<0,0,out)
})

region.sales <- apply(region.sales,2,function(x){
  out <- rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  ifelse(out<0,0,out)
})

brand.sales <- apply(brand.sales,2,function(x){
  out <- rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  ifelse(out<0,0,out)
})

brand.sales2 <- apply(brand.sales2,2,function(x){
  out <- rowMeans(cbind(x,c(NA,x[-length(x)]),c(x[-1],NA)),na.rm=T)
  ifelse(out<0,0,out)
})

####################################################
#By PO Sales

fsales %>%
  group_by(fy,po=!is.na(po)) %>%
  # summarise(vol=sum(vol)) %>%
  summarise(vol=n_distinct(store)) %>%
  acast(fy~po,value.var='vol') %>%
  write.clip

po.sales <- fsales %>%
  group_by(po) %>%
  summarise(min=min(date),max=max(date),n=n_distinct(date),
            vol24=sum(vol[fy==2024]),vol=sum(vol)) 

#Project Map

map <- melt(brand.sales2)
map <- do.call(rbind,strsplit(paste(map$Var2),' ')) %>%
  as.data.table %>%
  select(region=1,brand=2) %>%
  cbind(month=map$Var1,value=map$value) %>%
  mutate(fy=floor(month/100)+1,fy=ifelse(month%%100<=6,fy-1,fy)) %>%
  mutate(actual=month%in%sales$date)
map <- map %>%
  filter(actual,fy==max(fsales$fy)) %>%
  group_by(region,brand) %>%
  summarise(base=sum(value)) %>%
  merge(
    map %>%
      filter(!actual,fy<max(map$fy)) %>%
      group_by(region,brand,fy) %>%
      summarise(predict=sum(value)) %>%
      mutate(predict=ifelse(predict<0,0,predict))
  ) %>%
  mutate(rate=(predict+1)/(base+1)) %>%
  select(region,brand,fy,rate) %>%
  mutate(rate=ifelse(rate<0,0,rate))

out <- fsales %>%
  filter(po%in%po.sales$po,fy==max(fsales$fy)) %>%
  group_by(po,region,brand) %>%
  summarise(vol=sum(vol)) %>%
  merge(map) %>%
  mutate(predict=(vol+1)*rate-1) %>%
  mutate(predict=ifelse(predict<0,0,predict))

out %>%
  group_by(fy) %>%
  summarise(predict=sum(predict)) 

###############################################
#PO Info

poinfo <- fread('/Users/huzixin/Documents/baolelijia/anya/0312/ROI_Database.csv') %>%
  select(po=1,invest=3,outlet=`Outlet Name`,target=`Full Contract Volume (9L)`,
         start=`Start Date`,end=`End Date`,postore=CRMNO,storename=`Outlet Name`) %>%
  unique()

poinfo <- poinfo %>%
  mutate(start=as.numeric(gsub('-','',substr(start,1,7)))) %>%
  mutate(end=as.numeric(gsub('-','',substr(end,1,7)))) %>%
  filter(po%in%fsales$po,!is.na(po),!is.na(target)) %>%
  filter(end>202006) %>%
  arrange(desc(target))

dim(poinfo)

outs <- lapply(1:nrow(poinfo),function(i){
  print(i)
  poi <- poinfo[i]
  storelist <- storemaster2 %>% filter(parent==poi$postore)
  salesi <- fsales %>%
    filter(parent%in%poi$postore) %>%
    filter(date>=poi$start,date<=poi$end) %>%
    group_by(key=paste(region,brand)) %>%
    summarise(vol=sum(vol))
  mapi <- brand.sales2 %>%
    melt() %>%
    select(date=1,key=2,p=3) %>%
    filter(date>=poi$start,date<=poi$end) %>%
    mutate(status=ifelse(date<=max(fsales$date),'A','P')) %>%
    group_by(key,status) %>%
    summarise(p=sum(p,na.rm=T)) %>%
    dcast(key~status,value.var='p')
  if(!'P'%in%colnames(mapi)){mapi <- mapi %>% mutate(P=0)}
  salesi <- salesi %>%
    merge(mapi,all.x=T) %>%
    mutate(A=ifelse(is.na(A),1,A)) %>%
    mutate(P=ifelse(is.na(P),0,P)) %>%
    mutate(pred=ifelse(vol<=0,0,vol)) %>%
    mutate(pred=P/(A+1)*(pred+1)+vol) %>%
    mutate(parent=poi$postore,po=poi$po,name=poi$storename)
  list(sales=salesi,store=storelist)
})

out <- poinfo %>%
  select(po,start,end,target,invest) %>%
  merge(
    do.call(rbind,lapply(outs,function(x){x$sales})) %>%
      group_by(po,parent,name) %>%
      summarise(pred=sum(pred),vol=sum(vol)),all.x=T    
  ) %>%
  mutate(pred=ifelse(is.na(pred),0,pred)) %>%
  mutate(vol=ifelse(is.na(vol),0,vol))

out$region <- sapply(outs,function(x){
  x <- unique(x$store$region)
  if(length(x)==0){
    NA
  } else {
    x
  }
})

out <- out %>%
  select(po,parent,region,name,start,end,target,finished=vol,prediction=pred,invest)

out %>%
  mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>%
  group_by(fy,region='Total') %>%
  summarise(n(),sum(prediction),sum(target),sum(finished),mean(finished>target),mean(prediction>target)) %>%
  write.clip

out %>%
  mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>%
  group_by(fy,region) %>%
  summarise(sum(prediction),sum(target),sum(finished),mean(finished>target),mean(prediction>target)) %>%
  rbind(
    out %>%
      mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>%
      group_by(fy,region='Total') %>%
      summarise(sum(prediction),sum(target),sum(finished),mean(finished>target),mean(prediction>target))
  ) %>%
  arrange(fy,region) %>%
  write.clip

out %>%
  mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>%
  group_by(fy,region) %>%
  summarise(sum(prediction),sum(target),sum(invest)) %>%
  rbind(
    out %>%
      mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>%
      group_by(fy,region='Total') %>%
      summarise(sum(prediction),sum(target),sum(invest))
  ) %>%
  arrange(fy,region) %>%
  write.clip

test <- lapply(unique(out$region),function(i){
  outi <- out %>% 
    filter(region==i) %>%
    mutate(fy=floor(end/100)+ifelse(end%%100<=6,0,1)) %>% 
    filter(fy==2024) %>%
    arrange(desc(target)) 
  outi$salesprop <- outi$prediction/sum(outi$prediction)
  outi$targetprop <- outi$target/sum(outi$target)
  outi
})

do.call(rbind,lapply(test,function(test){
  do.call(rbind,lapply((1:18)/20,function(p){
    test[1:(nrow(test)*p),] %>%
      group_by(region,p=p) %>%
      summarise(n=n(),sum(prediction>target),sum(finished),sum(prediction),sum(target))
  }))
})) %>% write.clip

do.call(rbind,lapply(test,head,5)) %>%
  write.clip

  
