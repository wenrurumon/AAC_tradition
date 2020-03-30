
###################################
# Module
###################################

rm(list=ls())
library(data.table)
library(dplyr)
library(keras)

#Get data from github
setwd('/Users/wenrurumon/Documents/posdoc/wuhan/data/github')
raw <- lapply(dir(pattern='model_'),function(x){
  key <- tolower(strsplit(x,'_')[[1]][2:3])
  x1 <- fread(x)
  for(i in (nrow(x1)-1):1){
    x1[i,] <- ifelse(x1[i,]>x1[i+1,],x1[i+1,],x1[i,])
  }
  x2 <- data.table(V1=x1$V1,apply(rbind(0,as.matrix(x1[,-1])),2,diff))
  x <- cbind(melt(x1,id='V1'),melt(x2,id='V1')$value)
  colnames(x) <- c('date','state','accum','new')
  cbind(scope=key[1],fact=key[2],x)
})
raw <- do.call(rbind,raw)
raw <- merge(raw,(raw %>% group_by(scope,state,fact) %>% summarise(peak=max(new))),by=c('scope','state','fact')) %>% 
  filter(new==peak) %>% group_by(scope,state,fact) %>% summarise(peak=min(date)) %>% 
  merge(raw,by=c('scope','state','fact')) %>% mutate(d=ifelse(date>peak&(scope=='china'|state=='China'),1,0)) %>%
  select(-peak) %>% mutate(key=paste(scope,fact,state),
                           i=as.numeric((as.POSIXct(date)-as.POSIXct(min(raw$date)))/3600/24)+1)
raw <- raw %>% mutate(d=ifelse((scope=='china'|state=='China')&d==0&i>10,0.5,d))

#Module
getlog <- function(I,keyI,log,d=NULL){
  logi <- filter(log,key==keyI&i%in%(0:8+I))
  x <- logi[1:8,]
  y <- logi[-1:-8,]
  list(x=x,y=y)
}

###################################
# Whether Peak
###################################

log <- filter(raw,scope%in%c('china','global')&fact=='confirmed')
mfile <- do.call(c,lapply(unique(log$key),function(K){
  lapply(1:(max(log$i)-8),function(I){
    getlog(I,K,log)
  })
}))
X <- (t(sapply(mfile,function(x){
  c(x$x$new,mean(x$x$d))
})))
Y <- cbind(sapply(mfile,function(x){
  # x$y$new/(mean(x$x$new)+1)
  x$y$new
}))

e.input <- layer_input(shape=ncol(X))
e.layer <- layer_dense(e.input,16,activation='relu')
l.layer <- layer_dense(e.layer,4,activation='relu')
d.output <- layer_dense(units=ncol(Y))
model <- keras_model(e.input,d.output(l.layer))
model %>% compile(loss = "mean_squared_error", optimizer = "adam",metrics = c('mae'))
system.time(model %>% fit(x = X,y = Y,batch = 128,epochs = 1000,verbose = 0))
model %>% fit(x = X,y = Y,batch = 128,epochs = 100,verbose = 2)
keras::save_model_hdf5(model,paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model1.model'),overwrite = TRUE,include_optimizer = TRUE)

temp <- filter(raw,state=='US'&fact=='confirmed') %>% arrange(i)
D <- NA
for(j in 1:300){
  print(j)
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    # Di <- mean(logi$x$d)
    Di <- 1
    newi <- model %>% predict(matrix(c(logi$x$new,Di),nrow=1))
    accumi <- max(logi$x$accum)+newi
    rlti <- data.frame(
      scope=unique(logi$x$scope),state=unique(logi$x$state),fact=unique(logi$x$fact),
      date=paste(as.POSIXct(max(paste(logi$x$date)))+3600*24),
      accum = accumi,
      new = newi,
      d=round(Di),
      key=unique(logi$x$key),i=max(logi$x$i)+1
    ) %>% as.data.table
  }))
  temp <- rbind(temp,tempi)
}
plot.ts((temp %>% arrange(i))$new)
sum(temp$new)

###################################
# Stacked Model
###################################

models <- lapply(
  paste0('/Users/wenrurumon/Documents/posdoc/wuhan/model/model',1:2,'.model'),function(x){
    keras::load_model_hdf5(x)
  })

rlts <- list()
temp <- filter(raw,state=='US'&fact=='confirmed') %>% arrange(i)
D <- NA
j <- 0
for(j in 1:300){
  print(j)
  tempi <- do.call(rbind,lapply(unique(temp$key),function(K){
    logi <- getlog(max(temp$i)-7,K,temp)
    # logiDi <- models[[1]] %>% predict(matrix(c(logi$x$new/(mean(logi$x$new)+1),mean(logi$x$d)),nrow=1))
    Di <- 
      newi <- models[[2]] %>% predict(matrix(c(logi$x$new,round(Di)),nrow=1))
    accumi <- max(logi$x$accum)+newi
    rlti <- data.frame(
      scope=unique(logi$x$scope),state=unique(logi$x$state),fact=unique(logi$x$fact),
      date=paste(as.POSIXct(max(paste(logi$x$date)))+3600*24),
      accum = accumi,
      new = newi,
      d=round(Di),
      key=unique(logi$x$key),i=max(logi$x$i)+1
    ) %>% as.data.table
  }))
  temp <- rbind(temp,tempi)
}

plot.ts((temp %>% arrange(i))$new)
sum(temp$new)
