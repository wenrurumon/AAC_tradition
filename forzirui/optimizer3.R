
rm(list=ls())
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(openxlsx)

################################################
# 数据库
################################################

#Base Data

setwd('/Users/huzixin/Documents/mcd/moma/version1231')
load('curves.rda')
load('roisummary.rda')

# #Curves
# 
# curves <- curves %>% mutate(key=paste(y,x,sep=':'))
# tabs <- unique(curves$key)
# 
# #Models for curve
# 
# models <- lapply(tabs,function(tab){
#   thres <- 1.5 #ROI Threshold
#   curvei <- filter(curves,key==tab)
#   curvei <- curvei %>%
#     mutate(roi=ifelse(spdidx>0.1,roi,(curvei %>% filter(spdidx>0.1))$roi[1])) %>%
#     mutate(drive=roi*spd)
#   curvei <- curvei %>%
#     mutate(drive=ifelse(spdidx>thres,max((curvei %>% filter(spdidx<thres))$drive),drive)) %>%
#     mutate(roi=drive/spd)
#   funi <- splinefun(curvei$spd,curvei$drive)
#   funi
# })
# names(models) <- tabs
# 
# #Validate Curves
# 
# benchroi <- do.call(rbind,lapply(1:nrow(spt),function(i){
#   tab <- spt$x[i]
#   out <- sapply(models[grep(paste0(':',tab),names(models))],function(fun){
#     fun(spt$exe[i])
#   })
#   out <- data.table(x=tab,y=names(out),drive=out,spd=spt$exe[i]) %>%
#     mutate(y=substr(y,1,regexpr(':',y)-1))
# })) %>%
#   group_by(y) %>%
#   summarise(drive=sum(drive),spd=sum(spd)) %>%
#   mutate(roi=drive/spd) %>%
#   mutate(spd=sum(spt$mean)) %>%
#   mutate(drive=roi*spd)
# 
# #Curves
# 
# curves <- do.call(rbind,lapply(1:nrow(spt),function(i){
#   tab <- spt$x[i]
#   spdi <- (1:150)/100*spt$exe[i]
#   out <- lapply(grep(paste0(':',tab),names(models)),function(j){
#     data.table(x=tab,y=names(models)[j],spd=spdi,drive=models[[j]](spdi)) %>%
#       mutate(y=substr(y,1,regexpr(':',y)-1)) %>%
#       mutate(roi=drive/spd)
#   })
#   do.call(rbind,out)
# })) %>%
#   melt(id=1:3) %>%
#   dcast(x+spd~variable+y,value.var='value')
# 
# #Adjust ROI
# 
# curves$roi_Sales[curves$x=='Digital Display - Weibo OP'] <-
#   ifelse(curves$roi_Sales[curves$x=='Digital Display - Weibo OP']>4.47841234,
#          4.47841234,curves$roi_Sales[curves$x=='Digital Display - Weibo OP'])
# 
# curves$drive_Sales[curves$x=='Digital Display - Weibo OP'] <-
#   curves$spd[curves$x=='Digital Display - Weibo OP'] *
#   curves$roi_Sales[curves$x=='Digital Display - Weibo OP']
# 
# #Paths.maxsales
# 
# paths.maxsales <- curves %>% arrange(desc(roi_Sales))
# system.time(
#   paths.maxsales <- unique(lapply(1:nrow(paths.maxsales),function(i){
#     pathi <- paths.maxsales[1:i,]
#     pathi %>%
#       group_by(x) %>%
#       summarise(spd=max(spd)) %>%
#       merge(pathi)
#   }))
# )
# paths.maxsales <- do.call(c,list(list(paths.maxsales[[1]][-1,,drop=F]),paths.maxsales))
# paths.maxsales <-lapply(1:length(paths.maxsales),function(i){
#   data.table(step=i,paths.maxsales[[i]]) %>%
#     mutate(strategy='Sales') %>%
#     arrange(desc(roi_Sales))
# })
# strategy.maxsales <- do.call(rbind,paths.maxsales) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd)
# 
# #Paths.maxuser
# 
# paths.maxuser <- curves %>% arrange(desc(roi_User))
# system.time(
#   paths.maxuser <- unique(lapply(1:nrow(paths.maxuser),function(i){
#     pathi <- paths.maxuser[1:i,]
#     pathi %>%
#       group_by(x) %>%
#       summarise(spd=max(spd)) %>%
#       merge(pathi)
#   }))
# )
# paths.maxuser <- do.call(c,list(list(paths.maxuser[[1]][-1,,drop=F]),paths.maxuser))
# paths.maxuser <- lapply(1:length(paths.maxuser),function(i){
#   data.table(step=i,paths.maxuser[[i]]) %>%
#     mutate(strategy='User') %>%
#     arrange(desc(roi_User))
# })
# strategy.maxuser <- do.call(rbind,paths.maxuser) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd)
# 
# #Paths.maxreach
# 
# paths.maxreach <- curves %>% arrange(desc(roi_Reach))
# system.time(
#   paths.maxreach <- unique(lapply(1:nrow(paths.maxreach),function(i){
#     pathi <- paths.maxreach[1:i,]
#     pathi %>%
#       group_by(x) %>%
#       summarise(spd=max(spd)) %>%
#       merge(pathi)
#   }))
# )
# paths.maxreach <- do.call(c,list(list(paths.maxreach[[1]][-1,,drop=F]),paths.maxreach))
# paths.maxreach <- lapply(1:length(paths.maxreach),function(i){
#   data.table(step=i,paths.maxreach[[i]]) %>%
#     mutate(strategy='Reach') %>%
#     arrange(desc(roi_Reach))
# })
# strategy.maxreach <- do.call(rbind,paths.maxreach) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd)
# 
# save(benchroi,spt,strategy.maxreach,strategy.maxuser,strategy.maxsales,file='benchmarks.rda')
load('benchmarks.rda')

################################################
# 算法
################################################

# #Input to generate scenarios
# 
# budget_target <- sum(spt$mean) * 1.5
# 
# #New Paths to Max Sales
# 
# temppath <- curves %>% arrange(desc(roi_Sales))
# rlts <- list()
# for (i in 1:nrow(temppath)) {
#   pathi <- temppath[1:i,]
#   pathi <- pathi %>%
#     group_by(x) %>%
#     summarise(spd = max(spd)) %>%
#     merge(pathi)
#   if (sum(pathi$spd) > budget_target) {
#     break  
#   } else {
#     rlts[[i]] <- pathi
#   }
# }
# rlts <- unique(rlts)
# rlts <- do.call(c,list(list(rlts[[1]][-1,,drop=F]),rlts))
# paths.maxsales <- lapply(1:length(rlts),function(i){
#   data.table(step=i,rlts[[i]]) %>%
#     mutate(strategy='Sales') %>%
#     arrange(desc(roi_Sales))
# })
# strategy.maxsales <- do.call(rbind,paths.maxsales) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(reach=ifelse(reach>100,100,reach)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd) 
# 
# #New Paths to Max User
# 
# temppath <- curves %>% arrange(desc(roi_User))
# rlts <- list()
# for (i in 1:nrow(temppath)) {
#   pathi <- temppath[1:i,]
#   pathi <- pathi %>%
#     group_by(x) %>%
#     summarise(spd = max(spd)) %>%
#     merge(pathi)
#   if (sum(pathi$spd) > budget_target) {
#     break  
#   } else {
#     rlts[[i]] <- pathi
#   }
# }
# rlts <- unique(rlts)
# rlts <- do.call(c,list(list(rlts[[1]][-1,,drop=F]),rlts))
# paths.maxuser <- lapply(1:length(rlts),function(i){
#   data.table(step=i,rlts[[i]]) %>%
#     mutate(strategy='User') %>%
#     arrange(desc(roi_User))
# })
# strategy.maxuser <- do.call(rbind,paths.maxuser) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(reach=ifelse(reach>100,100,reach)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd) 
# 
# #New Paths to Max Reach
# 
# temppath <- curves %>% arrange(desc(roi_Reach))
# rlts <- list()
# for (i in 1:nrow(temppath)) {
#   pathi <- temppath[1:i,]
#   pathi <- pathi %>%
#     group_by(x) %>%
#     summarise(spd = max(spd)) %>%
#     merge(pathi)
#   if (sum(pathi$spd) > budget_target) {
#     break  
#   } else {
#     rlts[[i]] <- pathi
#   }
# }
# rlts <- unique(rlts)
# rlts <- do.call(c,list(list(rlts[[1]][-1,,drop=F]),rlts))
# paths.maxreach <- lapply(1:length(rlts),function(i){
#   data.table(step=i,rlts[[i]]) %>%
#     mutate(strategy='Reach') %>%
#     arrange(desc(roi_Reach))
# })
# strategy.maxreach <- do.call(rbind,paths.maxreach) %>%
#   group_by(step,strategy) %>%
#   summarise(spd=sum(spd),sales=sum(drive_Sales),reach=sum(drive_Reach),user=sum(drive_User)) %>%
#   mutate(reach=ifelse(reach>100,100,reach)) %>%
#   mutate(dsales=sales/spd,dreach=reach/spd,duser=user/spd) 
# 
# #Path3
# 
# strategys <- list(reach=strategy.maxreach,sales=strategy.maxsales,user=strategy.maxuser)
# paths <- list(reach=paths.maxreach,sales=paths.maxsales,user=paths.maxuser)
# paths <- paths[rank(sapply(strategys,nrow))]
# strategys <- strategys[rank(sapply(strategys,nrow))]
# 
# system.time(
#   path3 <- do.call(c,lapply(1:length(paths[[1]]),function(i){
#     print(paste(i,Sys.time()))
#     do.call(c,lapply(1:length(paths[[2]]),function(j){
#       path0 <- rbind(paths[[1]][[i]],paths[[2]][[j]])
#       lapply(1:length(paths[[3]]),function(k){
#         cbind(code=paste(i,j,k,sep='-'),rbind(path0,paths[[3]][[k]]))
#       })
#     }))
#   }))
# )
# path3 <- do.call(rbind,path3) 
# path3 <- path3 %>%
#   select(-step,-strategy) %>%
#   unique() %>%
#   merge(
#     path3 %>%
#       filter(!is.na(x)) %>%
#       group_by(x,code) %>%
#       summarise(spd=max(spd))
#   )
# 
# strategy3 <- path3 %>%
#   group_by(code) %>%
#   summarise(spd=sum(spd),reach=sum(drive_Reach),sales=sum(drive_Sales),user=sum(drive_User)) %>%
#   mutate(reach=ifelse(reach>100,100,reach)) %>%
#   mutate(dreach=reach/spd,dsales=sales/spd,duser=user/spd) %>%
#   mutate(score_reach=reach/unlist(max.benchmark)[4],
#          score_sales=sales/unlist(max.benchmark)[5],
#          score_user=user/unlist(max.benchmark)[6]) 
# 
# save(path3,strategy3,file='strategy3.rda')
system.time(load('strategy3.rda'))

################################################################################################################

#Inputs for 2-step optimization

budget_target <- sum(spt$mean) #input
targets.uplift <- c(reach=1.0,sales=1.1,user=1.1) #input
weights <- c(reach=0.1,sales=0.8,user=0.1) #input

#Calculate Max Sales/User/Reach

max.benchmark <- data.table(
  max.reachroi = (strategy.maxreach %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$dreach[1],
  max.salesroi = (strategy.maxsales %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$dsales[1],
  max.userroi = (strategy.maxuser %>% filter(spd<=budget_target) %>% arrange(desc(spd)))$duser[1]
) %>%
  mutate(
    max.reach = max.reachroi * budget_target,
    max.sales = max.salesroi * budget_target,
    max.user = max.userroi * budget_target
  ) %>%
  mutate(max.reach = ifelse(max.reach>100,100,max.reach))

#Set Target

targets <- budget_target*benchroi$roi*targets.uplift
targets.prop <- targets/unlist(max.benchmark)[4:6]

#Strategy 1

strategyi <- strategy3 %>%
  filter(spd<=budget_target,reach>=targets[1],sales>=targets[2],user>=targets[3]) %>%
  arrange(spd)

pathi <- path3 %>% filter(code==strategyi$code[1]) %>% arrange(desc(spd))

(pathi <- rbind(
  pathi %>%
    summarise(x='Total',spd=sum(spd),
              drive_Reach=sum(drive_Reach),drive_Sales=sum(drive_Sales),drive_User=sum(drive_User)) %>%
    mutate(roi_Reach=drive_Reach/spd,roi_Sales=drive_Sales/spd,roi_User=drive_User/spd),
  pathi %>% select(-code)
) %>%
  select(`Media Type`=x,Spending=spd,
         Sales=drive_Sales,User=drive_User,Reach=drive_Reach,
         roi_Sales,roi_User,roi_Reach))

#Strategy 2

strategyi <- strategy3 %>%
  filter(spd<=budget_target,reach>=targets[1],sales>=targets[2],user>=targets[3]) %>%
  mutate(score_reach=reach/unlist(max.benchmark)[4],
         score_sales=sales/unlist(max.benchmark)[5],
         score_user=user/unlist(max.benchmark)[6]) %>%
  mutate(score=score_reach*weights[1]+score_sales*weights[2]+score_user*weights[3]) %>%
  arrange(desc(score))

pathi <- path3 %>% filter(code==strategyi$code[1]) %>% arrange(desc(spd))

(pathi <- rbind(
  pathi %>%
    summarise(x='Total',spd=sum(spd),
              drive_Reach=sum(drive_Reach),drive_Sales=sum(drive_Sales),drive_User=sum(drive_User)) %>%
    mutate(roi_Reach=drive_Reach/spd,roi_Sales=drive_Sales/spd,roi_User=drive_User/spd),
  pathi %>% select(-code)
) %>%
  select(`Media Type`=x,Spending=spd,
         Sales=drive_Sales,User=drive_User,Reach=drive_Reach,
         roi_Sales,roi_User,roi_Reach))

#plot

spt %>%
  select(x,average_spending=mean,executive_spending=exe) %>%
  merge(pathi %>% select(x=1,suggested_spending=Spending) %>% filter(x!='Total'),all=T) %>%
  melt() %>%
  ggplot() + 
  geom_point(aes(x=value/1000000,y=x,colour=variable,shape=variable),size=3) +
  theme_bw() + 
  labs(x='Spending (MRMB)',y='',colour='',shape='') +
  theme(text=element_text(size=15))

#Growth

out <- unlist(pathi %>% filter(`Media Type`=='Total') %>% select(-1))/
  c(benchroi$spd[1],benchroi[c(2,3,1),]$drive,benchroi[c(2,3,1),]$roi)



