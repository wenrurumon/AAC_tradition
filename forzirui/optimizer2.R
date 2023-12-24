
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

robsroi$roi[6:7] <- robsroi$roi[6:7]/2

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

MODELS <- lapply(seq(1,2,0.25),function(threshold){
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
names(MODELS) <- paste0('t',seq(1,2,0.25)*100)
MODELS <- MODELS[-1]

################################################################################################
################################################################################################

# Validate Model

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
(benchroi <- sum(sales0)/sum(b0))
(benchueff <- sum(user0)/sum(b0))

#get paths to optimize specific weights of outcome

getpaths <- function(weights){
  paths <- lapply(MODELS,function(models){
    rangei <- seq(0,2.5,length.out=251)
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
    rlts <- rlts %>%
      mutate(score=weights[1]*sales+weights[2]*user) %>%
      mutate(scoreeffect=score/spd) %>%
      arrange(desc(scoreeffect))
    rlts$sel <- c(0,sapply(2:nrow(rlts),function(i){
      sum(rlts$spd[1:(i-1)][rlts$x[1:(i-1)]==rlts$x[i]]>rlts$spd[i])
    }))
    rlts <- rlts %>% filter(sel==0) %>% select(-sel)
    test <- data.table(id=1:nrow(rlts),rlts) %>%
      acast(id~x,value.var='spd',fill=0)
    rlts$cumspd <- sapply(1:nrow(test),function(i){
      sum(apply(test[1:i,,drop=F],2,max))
    })
    rlts
  })
  paths
}

getpaths2 <- function(weights,weights2){
  system.time(paths1 <- getpaths(weights))
  system.time(paths2 <- getpaths(weights2))
  system.time(
    rlts2 <- lapply(1:length(paths1),function(i){
      pathi1 <- paths1[[i]] 
      pathi2 <- paths2[[i]]
      pathi1 <- pathi1[1:which(pathi1$cumspd>budget_target*1.1)[1],]
      pathi2 <- pathi2[1:which(pathi2$cumspd>budget_target*1.1)[1],]
      scenarios <- lapply(1:nrow(pathi1),function(j){
        rlts <- rbind(pathi1[1:j,],pathi2)
        rlts$sel <- c(0,sapply(2:nrow(rlts),function(i){
          sum(rlts$spd[1:(i-1)][rlts$x[1:(i-1)]==rlts$x[i]]>rlts$spd[i])
        }))
        rlts <- rlts %>% 
          filter(sel==0) %>% 
          select(x,idx,user,sales,spd)
        test <- data.table(id=1:nrow(rlts),rlts) %>%
          acast(id~x,value.var='spd',fill=0)
        rlts$cumspd <- sapply(1:nrow(test),function(i){
          sum(apply(test[1:i,,drop=F],2,max))
        })
        test <- data.table(id=1:nrow(rlts),rlts) %>%
          acast(id~x,value.var='sales',fill=0)
        rlts$cumsales <- sapply(1:nrow(test),function(i){
          sum(apply(test[1:i,,drop=F],2,max))
        })
        test <- data.table(id=1:nrow(rlts),rlts) %>%
          acast(id~x,value.var='user',fill=0)
        rlts$cumuser <- sapply(1:nrow(test),function(i){
          sum(apply(test[1:i,,drop=F],2,max))
        })
        rlts <- rlts %>%
          mutate(
            step=1:nrow(rlts),
            strategy=j,
            cumsales=cumsales,
            cumuser=cumuser,
            score1=cumsales*weights[1]+cumuser*weights[2],
            score2=cumsales*weights2[1]+cumuser*weights2[2]
          ) 
        rlts[1:which(rlts$cumspd>budget_target*1.1)[1],]
      })
      scenarios <- do.call(rbind,scenarios)
      strategies <- scenarios %>%
        select(strategy,step,cumspd,cumsales,cumuser,score1,score2)
      list(scenarios=scenarios,strategies=strategies)
    })
  )
  rlts2
}

# Targets and Weights (Input)

budget_target <- sum(spt$mean) #定budget
sales_target <- budget_target*benchroi*1.2
user_target <- budget_target*benchueff*1.1

# Dicision

# weights1 <- c(sales=1,user=0) #优化方向1
# weights2 <- c(sales=0,user=1) #优化方向2
# system.time(scenarios <- getpaths2(weights1,weights2))
# names(scenarios) <- names(MODELS)
# save(scenarios,file='scenarios_model1.rda')
load('scenarios_model1.rda')

thresmodel <- scenarios$t125 #选一个媒体可以接受的最大变化量
scenario <- thresmodel$scenarios
strategy <- thresmodel$strategies

#搜索所有满足constrain的策略,找某一个kpi最大最大 (Input)

strategyi <- strategy %>%
  filter(cumsales>=sales_target,cumuser>=user_target,cumspd<=budget_target) %>%
  arrange(desc(cumsales*cumuser/cumspd^2/benchroi/benchueff)) 
  # arrange(desc(cumuser/cumspd)) #满足条件后user最大
  # arrange(desc(cumsales/cumspd)) #满足条件后roi最大

strategyi <- strategy %>%
  filter(cumsales>=sales_target,cumuser>=user_target) %>%
  arrange(cumspd)

(scenarioi <- scenario %>%
    filter(strategy==strategyi$strategy[1],step<=strategyi$step[1]) %>%
    group_by(x) %>%
    summarise(spd=max(spd),sales=max(sales),user=max(user)) %>%
    mutate(roi=sales/spd,ueff=user/spd) %>%
    arrange(desc(spd)))

scenarioi %>%
  select(1:4) %>%
  melt(id=c('x','spd')) %>%
  group_by(variable) %>%
  summarise(value=sum(value),spd=sum(spd)) %>%
  mutate(roi=value/spd)
  
#Outcome

(rlti <- data.table(media=spt$x,previous=spt$mean,exe=spt$exe,sales=sales0/b0,user=user0/b0) %>%
  mutate(sales=sales*previous,user=user*previous) %>%
  merge(
    scenarioi %>%
      select(media=x,scenario=spd,sales2=sales,user2=user),
    all.x=T
  ) %>%
  mutate(spdidx=scenario/exe,roiidx=sales2/scenario/sales*previous,useridx=user2/user/scenario*previous))

rlti %>%
  select(media,previous,scenario) %>%
  melt(id=1) %>%
  ggplot() + 
  geom_point(aes(y=media,x=value,colour=variable))
