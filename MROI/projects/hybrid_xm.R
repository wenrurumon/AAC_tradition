
rm(list=ls())
setwd('/Users/wenrurumon/Documents/zirui/xiaomi')
library(data.table)
library(dplyr)
library(ggplot2)

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  print(x)
  write.table(x, file=clip, sep = ',',row.names=T)   
  close(clip)
}

raw <- fread("xiaomi_model_data.csv")
raw[raw==''] <- 0
raw[is.na(raw)] <- 0
colnames(raw)[2] <- 'month'
raw$month <- sapply(strsplit(raw$month,'/'),function(x){
  x <- as.numeric(x)
  x[1] * 100 + x[2]
})
code <- unique(raw$code)
raw$code <- match(raw$code,unique(raw$code))
raw <- apply(raw,2,function(x){as.numeric(gsub(',','',x))}) %>% as.data.frame

raw <- raw %>% merge(
  raw[match(unique(raw$code),raw$code),] %>% 
    select(code=code,start=y)) %>%
  mutate(idx=y/start)

temp <- raw[,4:10]
for(i in 2:nrow(temp)){
  if(raw$code[i]==raw$code[i-1]){
    temp[i,] <- temp[i-1,]*0.3 + temp[i,]  
  }
}
raw[,4:10] <- temp

raw0 <- raw[match(unique(raw$code),raw$code),] %>%
  select(y,digital_display,digital_social_other,digital_otv,digital_search,
         digital_ecommerce,ooh,tv,brand_xiaomi,price_index)
coef0 <- apply(raw0[,-1],2,function(x){coef(lm(raw0[,1]~x))[2]})
coef0[coef0<0] <- 0
coef0[is.na(coef0)] <- 0

raw1 <- raw[-match(unique(raw$code),raw$code),] %>%
  select(idx,digital_display,digital_social_other,digital_otv,digital_search,
         digital_ecommerce,ooh,tv,brand_xiaomi,price_index)
coef1 <- apply(raw1[,-1],2,function(x){coef(lm(raw1[,1]~x))[2]})
coef1[coef1<0] <- 0
coef1[is.na(coef1)] <- 0

coefs <- list(
  coef(lm(raw0[,1] ~ as.matrix(raw0[,-1]) %*% cbind(coef0)))[1],
  coef(lm(raw0[,1] ~ as.matrix(raw0[,-1]) %*% cbind(coef0)))[2] * coef0,
  coef(lm(raw1[,1] ~ as.matrix(raw1[,-1]) %*% cbind(coef1)))[1],
  coef(lm(raw1[,1] ~ as.matrix(raw1[,-1]) %*% cbind(coef1)))[2] * coef1
)

raw0 <- raw %>% filter(`new launch`==1)
raw0 <- raw0[,colnames(raw0)%in%c('code',names(which(coef0>0)))]
colnames(raw0)[-1] <- paste0(colnames(raw0)[-1],'_0')
raw1 <- raw[,colnames(raw)%in%c('code','month',names(which(coef1>0)))]
colnames(raw1)[-1:-2] <- paste0(colnames(raw1)[-1:-2],'_1')

datafile <- merge(raw0,raw1) %>% select(-code,-month)
y <- raw$y
x0 <- cbind(1,datafile[,1:5] %>% as.matrix)
x1 <- cbind(1,datafile[,-1:-5] %>% as.matrix)
b0 <- unlist(coefs[1:2])
b0 <- b0[b0!=0]
b1 <- unlist(coefs[3:4])
b1 <- b1[b1!=0]

pred <- as.numeric(x0 %*% b0 * x1 %*% b1)
summary(model <- lm(y~pred+paste(raw$brand_redmi)))
plot.ts(y); lines(predict(model),col=2)

write.clip(cbind(actual=raw$y,predict=predict(model)))
write.clip(cbind(unlist(coefs[1:2]),unlist(coefs[3:4])))

##################################################
#Curve
##################################################

datafile <- select(raw,idx,4:10)
y <- datafile$idx

model <- lapply(2:(ncol(datafile)-1),function(i){
  print(i)
  x <- datafile[,i]+1
  model1 <- lm(y~x)
  y1 <- predict(model1)
  model2 <- lm(y~log(x))
  y2 <- predict(model2)
  model3 <- lm(log(y)~x)
  y3 <- exp(predict(model3))
  out <- data.frame(x,y1,y2,y3) %>% arrange(x) %>% melt(id=1)
  ggplot() + geom_line(data=out,aes(x=x,y=value,colour=variable))
  x <- floor(min(x)):ceiling(max(x))
  x <- quantile(x,(0:20)/20)
  y1 <- coef(model1)[1] + coef(model1)[2]*x
  y2 <- coef(model2)[1] + coef(model2)[2]*log(x)
  y3 <- exp(coef(model3)[1] + coef(model3)[2]*x)
  x <- cbind(x,apply(cbind(y1,y2,y3),1,min))
  
})
do.call(cbind,model) %>% write.clip
