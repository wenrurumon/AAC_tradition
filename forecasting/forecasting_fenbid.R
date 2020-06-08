
rm(list=ls())

library(openxlsx)
library(dplyr)
library(data.table)
library(forecast)
library(ggplot2)

ret <- function(x,r=0.3,l=0){
  out <- x
  for(i in 2:length(x)){
    out[i] <- out[i-1]*r+x[i]
  }
  out <- c(rep(0,l),out)[1:length(out)]
  out * sum(x)/sum(out)
}
rsq <- function(x,y){
  summary(lm(y~x))$r.square
}
scalelim <- function(x,f=1.5){
  x <- as.numeric(unlist(x))
  (range(x,na.rm=T)-mean(x,na.rm=T))*f+mean(x,na.rm=T)
}
comval <- function(x){
  x <- round(x,0)
  x <- strsplit(paste(x),'')[[1]]
  x[(length(x):1)%%3==0] <- paste0(',',x[(length(x):1)%%3==0])
  x <- paste(x,collapse='')
  if(substr(x,1,1)==','){x <- substr(x,2,nchar(x))}
  return(x)
}

setwd('/Users/wenrurumon/Documents/GSK/forecasting/')
temp <- openxlsx::read.xlsx('Fenbid_workingfile.xlsx')
temp$Month <- as.Date(temp$Month)

################################################
#Data Review
#After client upload the file of Fenbid_workingfile.xlsx
#Generate data review RMD
#Client can modify the data via the website
################################################

X <- data.table(
  select(temp,1,2),
  select(temp,5,6,7),
  select(temp,8:13)
)

cat(paste('Base Factors\n'))
p <- melt(select(X,1:3),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_line(data=p,aes(x=Month,y=value,colour=Status),size=1) +
  labs(x='Month',y=unique(p$variable),colour='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=mean(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',round(p$value[4],2),
           '% in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,4)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_line(data=p,aes(x=Month,y=value,colour=Status),size=1) +
  labs(x='Month',y=unique(p$variable),colour='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=mean(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',round(p$value[4],2),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,5)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_line(data=p,aes(x=Month,y=value,colour=Status),size=1) +
  labs(x='Month',y=unique(p$variable),colour='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

cat(paste('ATL Factors\n'))

p <- melt(select(X,c(1:2,6)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,7)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,8)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,9)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

cat(paste('BTL Factors\n'))

p <- melt(select(X,c(1:2,10)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

p <- melt(select(X,c(1:2,11)),id=c('Month','Status')) %>% mutate(variable=gsub('\\.',' ',variable))
ggplot() + geom_col(data=p,aes(x=Month,y=value,fill=Status),size=1,stat='identity') +
  labs(x='Month',y=unique(p$variable),fill='',title=paste('Data review -',unique(p$variable)))
p <- p %>% group_by(Month=year(Month),variable) %>% summarise(value=sum(value))
ggplot() + geom_bar(data=p,aes(x=Month,y=value),fill='steelblue',stat='identity') +
  labs(x='Year',y=unique(p$variable),title=paste('Data review -',unique(p$variable))) 
reshape2::acast(p,Month~variable)
cat(paste0(unique(p$variable),' for Fenbid is expected at about ',comval(p$value[4]),
           ' in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the value in 2019.\n'))

################################################
#Prediction Review
#After client confirms the data, activate model and generate report
################################################

#Modeling

b <- read.csv('coef_summary.csv',row.names=1)
X <- data.table(
  select(temp,1,2),
  intercept = 1,
  select(temp,5,6,7),
  dummy.vc = ifelse(temp$Month>='2019-4-1',temp$Weighted.Distribution,0),
  select(temp,8:13)
)
for(i in 8:13){X[[i]] <- ret(X[[i]])}
decomp <- as.matrix(select(X,-1:-2))
for(i in 1:ncol(decomp)){decomp[,i] <- decomp[,i] * unlist(b)[i]}
decomp <- as.data.table(decomp)
decomp$Weighted.Distribution <- decomp$Weighted.Distribution + decomp$dummy.vc
decomp <- select(decomp,-dummy.vc)

#Volume prediction

p <- data.frame(month=temp$Month,Actual=temp$Fenbid.DU,Predict=rowSums(decomp)) %>% mutate(
  Predict=ifelse(is.na(Actual),Predict,NA)
) %>% melt(id='month')
ggplot() +
  geom_line(data=p,aes(x=month,y=value,colour=variable),size=1) +
  ylim(scalelim(p$value))+
  labs(title='Consumption Forecasting in Sales Volume',x='Month',y='Volume (MDU)',colour='')
p <- p %>% group_by(month=year(month),variable) %>% summarise(value=sum(value,na.rm=T)/1000)
ggplot() +
  geom_bar(data=p,aes(x=month,y=value,fill=variable),stat='identity',
           position=position_stack(reverse=T)) +
  labs(title='Consumption Forecasting in Sales Volume',x='Year',y='Volume (MMDU)',fill='')
p <- p %>% group_by(month) %>% summarise(value=sum(value,na.rm=T))

cat(paste0('Total sales volume for Fenbid is estimated as about ',round(p$value[4],2),
      ' MMDU in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the sales volume in 2019.'))

#Value prediction

p <- data.frame(month=temp$Month,Actual=temp$Fenbid.Value,Predict=rowSums(decomp)*temp$DU.Price) %>% 
  mutate(Predict=ifelse(is.na(Actual),Predict,NA)) %>% 
  melt(id='month')
ggplot() +
  geom_line(data=p,aes(x=month,y=value,colour=variable),size=1) +
  ylim(scalelim(p$value))+
  labs(title='Consumption Forecasting in Sales Value',x='Month',y='Volume (MRMB)',colour='')
p <- p %>% group_by(month=year(month),variable) %>% summarise(value=sum(value,na.rm=T)/1000)
ggplot() +
  geom_bar(data=p,aes(x=month,y=value,fill=variable),stat='identity',
           position=position_stack(reverse=T)) +
  labs(title='Consumption Forecasting in Sales Value',x='Year',y='Value (MMRMB)',fill='')
p <- p %>% group_by(month) %>% summarise(value=sum(value,na.rm=T))

cat(paste0('Total sales value for Fenbid is estimated as about ',round(p$value[4],2),
           ' MMRMB in CY2020.\nWhich is ',round(p$value[4]/p$value[3]*100,2),'% of the sales value in 2019.'))

#Volume Due

p <- data.frame(month=year(temp$Month),decomp) 
p <- apply(p[,-1],2,function(x){tapply(x,p[,1],sum)})[,-1]
p <- data.frame(month=rownames(p)[-1],(p[-1,]-p[-nrow(p),])/rowSums(p)[-nrow(p)]) %>% 
  melt(id='month') %>% mutate(value=value*100,variable=gsub('\\.',' ',variable))  %>% filter(
    month==2020
  )
p$variable <- factor(p$variable,unique(p$variable))
p$variable <- c('Base','Base','Base','ATL','ATL','ATL','ATL','BTL','BTL')[p$variable]
p$variable <- factor(p$variable,unique(p$variable)[length(unique(p$variable)):1])
p <- p %>% group_by(month,variable) %>% summarise(value=sum(value))
ggplot() +
  geom_bar(data=p,aes(x=variable,y=value),stat='identity',fill='steelblue') +
  coord_flip() +
  ylim(max(abs(p$value))*c(-1.2,1.2)) +
  labs(x='',y='Due to %',title='Source of Volume Change')

p <- data.frame(month=year(temp$Month),decomp) 
p <- apply(p[,-1],2,function(x){tapply(x,p[,1],sum)})[,-1]
p <- data.frame(month=rownames(p)[-1],(p[-1,]-p[-nrow(p),])/rowSums(p)[-nrow(p)]) %>% 
  melt(id='month') %>% mutate(value=value*100,variable=gsub('\\.',' ',variable))  %>% filter(
    month==2020
  )
p$variable <- factor(p$variable,unique(p$variable)[length(unique(p$variable)):1])
ggplot() +
  geom_bar(data=p,aes(x=variable,y=value),stat='identity',fill='steelblue') +
  coord_flip() +
  ylim(max(abs(p$value))*c(-1.2,1.2)) +
  labs(x='',y='Due to %',title='Source of Volume Change')

p <- data.frame(month=temp$Month[-1:-12],
  (decomp[-1:-12,]-decomp[-(nrow(decomp)-0:11),])/rowSums(decomp)[-(nrow(decomp)-0:11)]) %>%
  melt(id='month') %>% filter(variable!='intercept') %>%
  mutate(value=value*100,variable=gsub('\\.',' ',variable)) 
p$variable <- factor(p$variable,unique(p$variable))
p <- p %>% group_by(month,variable) %>% summarise(value=sum(value))
ggplot() +
  geom_bar(data=p,aes(x=month,y=value,fill=variable),stat='identity') +
  ylim(range(p$value)*2) + 
  labs(x='Month',y='Due to %',fill='Buckets',title='Volume Due to by Buckets') 

#Value Due

decomp2 <- decomp
decomp2$DU.Price <- rowSums(decomp) * temp$DU.Price - rowSums(decomp)

p <- data.frame(month=year(temp$Month),decomp2) 
p <- apply(p[,-1],2,function(x){tapply(x,p[,1],sum)})[,-1]
p <- data.frame(month=rownames(p)[-1],(p[-1,]-p[-nrow(p),])/rowSums(p)[-nrow(p)]) %>% 
  melt(id='month') %>% mutate(value=value*100,variable=gsub('\\.',' ',variable))  %>% filter(
    month==2020
  )
p$variable <- factor(p$variable,unique(p$variable))
p$variable <- c('Base','Base','Base','ATL','ATL','ATL','ATL','BTL','BTL')[p$variable]
p$variable <- factor(p$variable,unique(p$variable)[length(unique(p$variable)):1])
p <- p %>% group_by(month,variable) %>% summarise(value=sum(value))
ggplot() +
  geom_bar(data=p,aes(x=variable,y=value),stat='identity',fill='steelblue') +
  coord_flip() +
  ylim(max(abs(p$value))*c(-1.2,1.2)) +
  labs(x='',y='Due to %',title='Source of Value Change')

p <- data.frame(month=year(temp$Month),decomp2) 
p <- apply(p[,-1],2,function(x){tapply(x,p[,1],sum)})[,-1]
p <- data.frame(month=rownames(p)[-1],(p[-1,]-p[-nrow(p),])/rowSums(p)[-nrow(p)]) %>% 
  melt(id='month') %>% mutate(value=value*100,variable=gsub('\\.',' ',variable))  %>% filter(
    month==2020
  )
p$variable <- factor(p$variable,unique(p$variable)[length(unique(p$variable)):1])
ggplot() +
  geom_bar(data=p,aes(x=variable,y=value),stat='identity',fill='steelblue') +
  coord_flip() +
  ylim(max(abs(p$value))*c(-1.2,1.2)) +
  labs(x='',y='Due to %',title='Source of Value Change')

p <- data.frame(month=temp$Month[-1:-12],
  (decomp[-1:-12,]-decomp[-(nrow(decomp)-0:11),])/rowSums(decomp)[-(nrow(decomp)-0:11)]) %>%
  melt(id='month') %>% filter(variable!='intercept') %>%
  mutate(value=value*100,variable=gsub('\\.',' ',variable)) 
p$variable <- factor(p$variable,unique(p$variable))
p <- p %>% group_by(month,variable) %>% summarise(value=sum(value))
ggplot() +
  geom_bar(data=p,aes(x=month,y=value,fill=variable),stat='identity') +
  ylim(range(p$value)*2) + 
  labs(x='Month',y='Due to %',fill='Buckets',title='Value Due to by Buckets') 


################################################
#Tech Review
################################################

p <- data.frame(month=temp$Month,Actual=temp$Fenbid.DU,Fit=rowSums(decomp)) %>% filter(!is.na(Actual))
ggplot() + 
  geom_line(data=melt(p,id='month'),aes(x=month,y=value,colour=variable),size=1) + 
  ylim(scalelim(p[,2])) +
  labs(
    title='Fit Chart in Sales Volume',
    subtitle=
         paste0('R square: ',round(rsq(p[,3],p[,2]) * 100,2),'%\nMAPE: ',round(mean(abs(p[,3]/p[,2]-1))*100,2),'%'),
    x='Month',y='Volume (MDU)',colour='')

p <- data.frame(month=p$month[-1:-12],(p[-1:-12,-1]/p[-(nrow(p)-0:11),-1]-1)*100)
ggplot() + 
  geom_line(data=melt(p,id='month'),aes(x=month,y=value,colour=variable),size=1) + 
  ylim(scalelim(p[,-1])) +
  labs(
    title='Fit Chart in Volume Growth%',
    subtitle=
      paste0('R square: ',round(rsq(p[,3],p[,2]) * 100,2),'%\nMAPE: ',round(mean(abs(p[,3]-p[,2]-1)),2),'%'),
    x='Month',y='Volume Growth (%)',colour='')

p <- data.frame(month=temp$Month,Actual=temp$Fenbid.Value,Fit=rowSums(decomp)*temp$DU.Price) %>% filter(!is.na(Actual))
ggplot() + 
  geom_line(data=melt(p,id='month'),aes(x=month,y=value,colour=variable),size=1) + 
  ylim(scalelim(p[,-1])) +
  labs(
    title='Fit Chart in Sales Value',
    subtitle=
      paste0('R square: ',round(rsq(p[,3],p[,2]) * 100,2),'%\nMAPE: ',round(mean(abs(p[,3]/p[,2]-1))*100,2),'%'),
    x='Month',y='Volume (MRMB)',colour='')

p <- data.frame(month=p$month[-1:-12],(p[-1:-12,-1]/p[-(nrow(p)-0:11),-1]-1)*100)
ggplot() + 
  geom_line(data=melt(p,id='month'),aes(x=month,y=value,colour=variable),size=1) + 
  ylim(scalelim(p[,-1])) +
  labs(
    title='Fit Chart in Value Growth%',
    subtitle=
      paste0('R square: ',round(rsq(p[,3],p[,2]) * 100,2),'%\nMAPE: ',round(mean(abs(p[,3]-p[,2]-1)),2),'%'),
    x='Month',y='Value Growth (%)',colour='')







