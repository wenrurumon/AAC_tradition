
rm(list=ls())
library(kernlab)
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)

#Module

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')
  close(clip)
}

fcast <- function(y,h=48,pen14=c(0.8,0.9),k=0.2){
  #Setup
  # y <- raw
  # pen14 <- c(0.3,0.5)
  # h <- 48
  bna <- sum(is.na(y))
  y <- y[!is.na(y)]
  #Y ~ Season + Time
  x <- (1:length(y))^mean(pen14)
  monthid <- outer(rep(1:12,length=length(y)),1:12,'==')
  X <- cbind(month=x,season=monthid)
  summary(model1 <- lm(y~X-1))
  b <- c(coef(model1),nls=mean(pen14))
  #Y ~ Season + Time | Nonlinear, Robust
  loss <- function(b,pen14=pen14){
    X[,1] <- (1:length(y))^abs(b[14])
    pred <- X %*% cbind(b[-14])
    gap <- (pred - y)^2
    if(b[14] < min(pen14)){
      pen14 <- abs(b[14]-min(pen14))+1
    } else if (b[14] > max(pen14)){
      pen14 <- abs(b[14]-max(pen14))+1
    } else {
      pen14 <- 1
    }
    w <- sqrt(gap)/y
    w <- ifelse(w<k,1,k/w)
    (sum((pred - y)^2 * w) / sum((y-mean(y))^2)) * pen14
  }
  bnew <- optim(b,loss,pen14=pen14)$par
  #Summarise
  monthid <- outer(rep(1:12,length=(length(y)+h)),1:12,'==')
  Xnew <- cbind((1:nrow(monthid))^bnew[14],monthid)
  pred <- Xnew %*% cbind(bnew[-14])
  y <- c(rep(NA,bna),y)
  pred <- c(rep(NA,bna),pred)
  rlt <- data.table(
    id = 1:length(pred),
    month = rep(1:12,length=length(pred)),
    year = rep(1:ceiling(length(pred)/12),each=12)[1:length(pred)],
    actual = c(y,rep(NA,h)),
    fit = as.numeric(pred)
  ) %>%
    mutate(predict=ifelse(is.na(actual),fit,actual)) 
  print(rlt %>% melt(id=1:3) %>%
          filter(variable!='predict',!is.na(value)) %>%
          ggplot() + 
          geom_line(aes(x=id,y=value,colour=variable)) +
          theme_bw())
  test <- (rlt %>% group_by(year) %>% summarise(fit=sum(predict)))$fit
  print(test[-1]/test[-length(test)]-1)
  list(
    coef=bnew,
    decomp=rlt
  )
}

################################################################
# Data
################################################################

library(forecast)
setwd('/Users/wenrurumon/Documents/gsk/gsk2023/forecasting/modeling')
raw <- lapply(1:2,function(i){
  openxlsx::read.xlsx('wellotc.xlsx',sheet=i)
})
raw <- do.call(rbind,raw) %>%
  melt(id=1:3) %>%
  mutate(variable=as.numeric(paste(variable))) %>%
  filter(variable>=201800) %>%
  mutate(key=paste(subcategory,channel))

map <- openxlsx::read.xlsx('wellotc.xlsx',sheet=3)
map$category <- map$Row.Labels[1]

for ( i in 1:nrow(map) ){
  if(is.na(map$`2018`[i])){
    map$category[i] <- map$`Row.Labels`[i]
  } else {
    map$category[i] <- map$category[i-1]
  }
}
colnames(map)[1] <- 'channel'
map <- map %>% 
  filter(!is.na(`2018`)) %>%
  melt(id=c('category','channel')) %>%
  mutate(value=ifelse(value==0,NA,value)) %>%
  select(subcategory=category,channel,year=variable,ttl=value) %>%
  mutate(key=paste(subcategory,channel))

##########

foral <- function(y,h=47,e=3,target=c(0.027,0.027,0.019,0.017)){
  
  y[is.na(y)] <- 0
  
  yf <- forecast(ts(y,frequency=12),h=h)
  pred0 <- pred <- c(y,yf$mean)
  
  pred[62:72] <- pred[62:72] * (sum(pred[49:60])*(1+target[1])-sum(pred[61]))/sum(pred[62:72])
  pred[73:84] <- pred[73:84] * (sum(pred[61:72])*(1+target[2]))/sum(pred[73:84])
  pred[85:96] <- pred[85:96] * (sum(pred[73:84])*(1+target[3]))/sum(pred[85:96])
  pred[97:108] <- pred[97:108] * (sum(pred[85:96])*(1+target[4]))/sum(pred[97:108])
  
  predg <- (pred-pred0)[-1:-length(y)]
  pred[-1:-length(y)] <- pred0[-1:-length(y)] + predict(lm(predg~poly(1:length(predg),e)))
  
  pred[1:length(y)] <- y
  
  out <- tapply(
    pred,
    rep(1:9,each=12),
    sum
  )
  
  print((out[-1]/out[-9]-1)[5:8])
  print((out[-1]/out[-9]-1)[5:8]-target)
  
  plot.ts(pred,col=2); lines(y)
  
  pred %>% as.numeric
  
}

out <- lapply(unique(raw$key),function(i){
  print(i)
  rawi <- raw %>% filter(key==i)
  mapi <- map %>% filter(key==i)
  y <- rawi$value
  data.frame(
    key=i,
    month=rep(2018:2026,each=12)*100+rep(1:12,9),
    value=foral(y,47,3,mapi$ttl[6:9]/mapi$ttl[6:9-1]-1)
  )
})

out <- do.call(rbind,out) %>%
  merge(
    raw %>% select(1,2,3,6) %>% unique()
  ) %>%
  mutate(value=ifelse(value==0,NA,value),
         year=floor(month/100)) %>%
  mutate(bu=ifelse(category%in%c('MV','Joint','Calcium'),'Wellness','OTC'))

# write.clip(out)

################################################################
# Polish
################################################################

test <- lapply(unique(out$key),function(i){

  print(i)

  outi <- out %>% filter(key==i)
  y <- outi$value
  yna <- sum(is.na(y))
  y <- y[!is.na(y)]

  x <- 1:length(y)
  x2 <- rep(1:12,length=length(x))
  x2 <- outer(x2,1:12,'==')+0
  p0 <- sapply(1:100,function(s){
    set.seed(s)
    predict(gausspr(cbind(x),y))
  }) %>% rowMeans
  p1 <- sapply(1:100,function(s){
    set.seed(s)
    predict(gausspr(cbind(p0,x2),y))
  }) %>% rowMeans
  p0 <- c(rep(NA,yna),p0)
  p1 <- c(rep(NA,yna),p1)
  y <- c(rep(NA,yna),y)

  data.frame(
    year=2018:2026,
    y=tapply(y,rep(1:9,each=12),sum) %>% as.numeric,
    p0=tapply(p0,rep(1:9,each=12),sum) %>% as.numeric,
    p1=tapply(p1,rep(1:9,each=12),sum) %>% as.numeric
  )

  out2 <- data.table(
    outi,
    pred=p0,
    pred1=p1
  ) %>%
    merge(
      data.frame(
        year=2018:2026,
        y=tapply(y,rep(1:9,each=12),sum) %>% as.numeric,
        p0=tapply(p0,rep(1:9,each=12),sum) %>% as.numeric,
        p1=tapply(p1,rep(1:9,each=12),sum) %>% as.numeric
      ) %>%
        mutate(gap=(y-p1)/p0),by='year'
    ) %>%
    mutate(pred=pred*gap+pred1) %>%
    select(-pred1,-gap)

  out2 %>%
    group_by(year) %>%
    summarise(y=sum(value),p=sum(pred))

  out2 <- out2 %>%
    mutate(pred=ifelse(month<=202302,value,pred)) %>%
    select(colnames(outi))

  outi

})

test <- do.call(rbind,test)
test_bk <- test
test <- test_bk

write.clip(test)

################################################################
# Finalize
################################################################

vali <- c()

for (i in unique(test$key)){
  
  testi <- test_bk[test_bk$key==i,]
  
  x <- testi %>%
    group_by(year) %>%
    summarise(value=sum(value))
  r <- x$value[7:9]/x$value[7:9-1]
  
  if(prod(diff(r-1))<0){vali <- c(vali,i)}
  
  b <- c(coef(lm(r~I(scale((1:3)^0.9)))),0.9)
  loss <- function(b){
    p <- cbind(1,scale((1:3)^abs(b[3]))) %*% cbind(b[1:2])
    sum((r-p)^2)*ifelse(abs(b[3]-0.8)>=0.2,abs(b[3]-0.8)+1,1)
  }
  b <- optim(b,loss)$par
  b <- scale(as.numeric(cbind(1:3)^abs(b[3])))
  r2 <- splinefun(b[-2],r[-2])(b)
  
  x$target <- c(x$value[1:6],x$value[6] * cumprod(r2))
  x$gap <- x$target/x$value
  test2 <- testi %>% 
    merge(x %>% select(year,rate=gap)) %>%
    mutate(value=rate*value)
  
  test$value[test$key==i] <- test2$value
  
}

write.clip(test)


