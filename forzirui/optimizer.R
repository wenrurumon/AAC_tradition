
rm(list=ls())
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(openxlsx)

################################################
# 曲线模型设计
################################################

#Base Data

setwd('/Users/huzixin/Documents/mcd/moma/result')
load('modelfiles.rda')

#Curves

curves <- raw %>%
  group_by(y,x=mapping,quantile) %>%
  summarise(mspd=sum(spending),mdrive=sum(accumulate)) %>%
  select(-quantile) 

curves <- rbind(
  curves,
  do.call(rbind,lapply(1:nrow(robsroi),function(i){
    curvei <- curves %>% 
      filter(x==robsroi$mapping[i]) %>%
      mutate(y='AP Curve')
    if(length(which(diff(round(curvei$mdrive,2))==0))==0){
      curvei$mdrive <- curvei$mspd * robsroi$roi[i]
      curvei
    } else {
      j <- min(which(diff(round(curvei$mdrive,2))==0))
      curvei$mdrive <- curvei$mspd * robsroi$roi[i]
      curvei$mdrive[-1:-j] <- curvei$mdrive[j]  
      curvei
    }
  }))
) %>%
  mutate(key=paste(y,x,sep=':')) %>%
  arrange(key)

MODELS <- lapply(seq(1,2.5,0.1),function(threshold){
  models <- lapply(unique(curves$key),function(keyi){
    curvei <- curves %>% filter(key==keyi)
    funi <- splinefun(curvei$mspd,curvei$mdrive)
    funi2 <- function(x){
      ifelse(x>max(curvei$mspd)|x<0,x,funi(x))
    }
    roi <- mean((roisummary %>% filter(target=='Sales',key==curvei$x[1]))$roi)
    roi <- roi/(funi2(filter(spt,x==curvei$x[1])$exe)/filter(spt,x==curvei$x[1])$exe)
    if(sum(curvei$mspd>=filter(spt,x==curvei$x[1])$exe*threshold)>0){
      curvei$mdrive[curvei$mspd>=filter(spt,x==curvei$x[1])$exe*threshold] <- min(curvei$mdrive[curvei$mspd>=filter(spt,x==curvei$x[1])$exe*threshold])  
    }
    funi <- splinefun(curvei$mspd,curvei$mdrive)
    funi
  }) 
  names(models) <- unique(curves$key)
  models
})
names(MODELS) <- paste0('t',seq(1,2.5,0.1)*100)

################################################
# 优化参照设计
################################################

#Pathtable

paths <- lapply(MODELS,function(models){
  rangei <- seq(0,3,length.out=3001)
  models.user <- models[grep('User:',names(models))]
  models.sales <- models[grep('AP Curve:',names(models))]
  rlts <- do.call(rbind,lapply(1:nrow(spt),function(i){
    out <- data.table(
      x=spt$x[i],
      idx=rangei,
      user=models.user[[i]](spt$exe[i]*rangei),
      sales=models.sales[[i]](spt$exe[i]*rangei),
      spd=spt$exe[i]*rangei
    ) %>%
      mutate(usereffect=user/spd,roi=sales/spd)
  }))
  rlts
})

maxpaths <- lapply(paths,function(rlts){
  maxroi <- rlts %>%
    merge(
      rlts %>%
        group_by(x) %>%
        summarise(roi=max(roi,na.rm=T))
    ) %>%
    arrange(desc(roi))
  maxuser <- rlts %>%
    merge(
      rlts %>%
        group_by(x) %>%
        summarise(usereffect=max(usereffect,na.rm=T))
    ) %>%
    arrange(desc(usereffect))
  maxroi <- maxroi %>%
    merge(
      maxroi %>%
        group_by(x) %>%
        summarise(spd=max(spd)),
      by=c('x','spd')
    ) %>%
    arrange(desc(roi))
  maxuser <- maxuser %>%
    merge(
      maxuser %>%
        group_by(x) %>%
        summarise(spd=max(spd)),
      by=c('x','spd')
    ) %>%
    arrange(desc(usereffect))
  out <- list(maxroi=maxroi,maxuser=maxuser)
  out
  # lapply(out,function(x){
  #   x$cspd <- cumsum(x$spd)
  #   x$csales <- cumsum(x$sales)
  #   x$cuser <- cumsum(x$user)
  #   x
  # })
})[-1]

maxpaths.roi <- lapply(maxpaths,function(x){x$maxroi})
maxpaths.user <- lapply(maxpaths,function(x){x$maxuser})

################################################################################################
################################################################################################

# Benchmark

models0 <- MODELS[[1]]
models0.user <- models0[grep('User:',names(models0))]
models0.sales <- models0[grep('AP Curve:',names(models0))]

b0 <- spt$exe
sales0 <- sapply(1:length(b0),function(i){
  models0.sales[[i]](b0[i])
})
user0 <- sapply(1:length(b0),function(i){
  models0.user[[i]](b0[i])
})
sum(sales0)/sum(b0) 
sum(user0)/sum(b0)
#确认我们的curve模型跑出来的roi和effectivenss与真实情况基本一致

#Set Target

budget_target <- sum(spt$mean)
sales_target <- budget_target * (sum(sales0)/sum(b0)*1.1)
user_target <- budget_target * (sum(user0)/sum(b0)*1.1)

################################################
# 优化路线
################################################

#Scenarios

scenarios <- lapply(maxpaths,function(pathi){
  maxroi <- pathi$maxroi
  maxuser <- pathi$maxuser
  do.call(rbind,lapply(1:nrow(spt),function(roi_prior){
    step1 <- maxroi[1:roi_prior,]
    step2 <- maxuser %>% filter(!x%in%step1$x)
    out <- data.table(
      roi_prior=roi_prior,
      step=1:nrow(spt),
      rbind(step1,step2)
    )
    out$cumspd <- cumsum(out$spd)
    out$cumuser <- cumsum(out$user)
    out$cumsales <- cumsum(out$sales)
    out
  }))
})

# 找到那些scenario可以实现目标

steps0 <- lapply(scenarios,function(scenarioi){
  sel <- (scenarioi %>%
            filter(cumsales>=sales_target,cumuser>=user_target,cumspd<=budget_target) %>%
            arrange(cumsales))$roi_prior[1]
  scenarioi %>%
    filter(roi_prior==sel,step<=min((scenarioi %>%
                                       filter(roi_prior==sel) %>%
                                       filter(cumsales>=sales_target,cumuser>=user_target))$step))
})

# 选一个scenario做下一步

sel0 <- which.min(sapply(steps0[1:4],nrow)) #到底选哪个threshold
# sel0 <- 1
step0 <- steps0[[sel0]]

#检查还剩多少钱

budget_left <- budget_target-sum(step0$spd)

#往下放两个更aggresive的模型来投放

sel1 <- min(sel0+2,length(paths)) #怎么算更aggresive的路径
maxroi1 <- maxpaths.roi[[sel1]]
maxuser1 <- maxpaths.user[[sel1]]

step1.roi <- maxroi1 %>%
  as.data.frame %>%
  merge(step0 %>% select(x,used_spd=spd),by='x',all=T) %>%
  mutate(used_spd=ifelse(is.na(used_spd),0,used_spd)) %>%
  mutate(spd2go=spd-used_spd) %>%
  filter(spd2go>0) %>%
  arrange(desc(roi)) 

step1.user <- maxuser1 %>%
  as.data.frame %>%
  merge(step0 %>% select(x,used_spd=spd),by='x',all=T) %>%
  mutate(used_spd=ifelse(is.na(used_spd),0,used_spd)) %>%
  mutate(spd2go=spd-used_spd) %>%
  filter(spd2go>0) %>%
  arrange(desc(user))

step1 <- step1.user #选到底最优roi还是最优user
step1 <- step1[1:which(cumsum(step1$spd2go)>=budget_left)[1],]
step1$spd2go[nrow(step1)] <- budget_left-sum(step1$spd2go)+step1$spd2go[nrow(step1)]

#Summarise Strategy

step2 <- spt %>%
  select(x,mean=mean,exe=exe) %>%
  merge(
    step0 %>%
      as.data.frame %>%
      select(x,spd0=spd),
    all.x=T    
  ) %>%
  mutate(spd0=ifelse(is.na(spd0),0,spd0)) %>%
  merge(
    step1 %>%
      as.data.frame %>%
      select(x,spd1=spd2go),
    all.x=T
  ) %>%
  mutate(spd1=ifelse(is.na(spd1),0,spd1)) %>%
  mutate(spd=spd0+spd1) %>%
  mutate(spd/mean,spd/exe)

print(step2)

#Simulation

b1 <- step2$spd

models1 <- MODELS[[sel1]]
models1.user <- models1[grep('User:',names(models1))]
models1.sales <- models1[grep('AP Curve:',names(models1))]

sales1 <- sapply(1:length(b1),function(i){
  models1.sales[[i]](b1[i])
})
user1 <- sapply(1:length(b1),function(i){
  models1.user[[i]](b1[i])
})
sum(sales1)/sales_target;sum(sales1)/sum(b1)
sum(user1)/user_target;sum(user1)/sum(b1)

data.table(x=spt$x,previous=b0,scenario=b1) %>%
  melt(id=1) %>%
  ggplot() + 
  geom_point(aes(y=x,x=value,colour=variable))
