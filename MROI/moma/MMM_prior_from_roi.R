
rm(list=ls())
library(dplyr)
library(data.table)
setwd('/Users/huzixin/Documents/mcd/moma')

################################################################################################
################################################################################################

#Process data

raw <- openxlsx::read.xlsx('副本New User_by Market Spending Data_1120.xlsx')
map <- openxlsx::read.xlsx('副本New User_by Market Spending Data_1120.xlsx',sheet=2)
map$Media <- tolower(gsub(' ','',map$Media))
user <- openxlsx::read.xlsx('byWeek_byMkt_New_Total数量与客单价.xlsx')
natdata <- apply(raw[,-1:-3],2,function(x){
  tapply(x,raw$Week,sum,na.rm=T)
})
user <- user %>%
  mutate(newsales=new_user.y*`new_user.y客单价`,allsales=All.User.Count*`All.User客单价`)
userdata <- apply(user[,c(3,5,7,8)],2,function(x){
  tapply(x,user$Week,sum,na.rm=T)
})

natdata <- melt(natdata)
userdata <- melt(userdata[,-1])

#Calculate to previous MMM

mfile <- rbind(natdata,userdata) %>%
  merge(
      data.table(Var2=map$Spending命名,code=map$Media),
    all=T
  ) %>%
  mutate(code=ifelse(is.na(code),paste0('Y_',Var2),code))

mfile <- mfile %>%
  group_by(week=Var1,code) %>%
  summarise(value=sum(value)) %>%
  acast(week~code,value.var='value')

Y <- mfile[,grep('Y',colnames(mfile))]  
X <- mfile[,-grep('Y',colnames(mfile))]  
roi <- data.table(
  var=colnames(X),
  roi=c(4.1,3.34,3.33,3.88,3.48,5.36,1.19)
)

#New Model

allbase <- melt(X) %>%
  merge(roi %>% select(Var2=1,roi=2)) %>%
  mutate(sales=value*roi) %>%
  group_by(week=Var1) %>%
  summarise(atl=sum(sales)) %>%
  data.table(Y) %>%
  mutate(base=Y_allsales-atl) %>%
  select(week,base)

rate <- merge(
  Y %>% melt(),
  Y %>% melt(),
  by='Var1'
) %>%
  select(week=Var1,x1=Var2.x,x2=Var2.y,value1=value.x,value2=value.y) %>%
  mutate(rate=value2/value1)

Y <- data.table(week=rownames(Y),Y)

X <- melt(X) %>%
  merge(roi %>% select(Var2=1,roi=2)) %>%
  mutate(sales=value*roi) %>%
  dcast(Var1~Var2,value.var='sales') %>%
  select(week=1,2:8)

Xnew <- cbind(X[,-1])*Y$Y_newsales/Y$Y_allsales
test <- sapply(1:ncol(Xnew),function(i){
  coef(lm(I(Y$Y_newsales-rowSums(Xnew[,-i]))~Xnew[,i]+allbase$base))[2]
})
test[test>3] <- 3
test[test<1/3] <- 1/3
test[4] <- test[4]/2
test[2] <- test[2]*0.8
names(test) <- colnames(X)[-1]

for (i in 1:ncol(Xnew)){
  Xnew[,i] <- Xnew[,i] * test[i]
}

Xnew$base <- predict(lm(I(Y$Y_newsales-rowSums(Xnew))~allbase$base))
Xold <- (as.matrix(cbind(X[,-1],allbase$base))-as.matrix(Xnew))

Xnuser <- as.matrix(Xnew)/(Y$Y_newsales/Y$Y_new_user.y)
plot.ts(Y$Y_new_user.y); lines(rowSums(Xnuser),col=2)
Xauser <- as.matrix(cbind(X[,-1],allbase$base))/(Y$Y_allsales/Y$Y_All.User.Count)

summary(lm(Y$Y_new_user.y~rowSums(Xnuser)))
data.table(
  colnames(Xnuser),
  new=round(colSums(Xnuser)/sum(Xnuser)*100,2),
  all=round(colSums(Xauser)/sum(Xauser)*100,2)
) %>%
  mutate(newuser_rate=new/all*sum(Y$Y_new_user.y)/sum(Y$Y_All.User.Count))

################################################################################################
################################################################################################

#Outboard

sepdata <- natdata %>% 
  select(week=1,var=2,spd=3) %>%
  merge(
    map %>% select(var=2,media=Media)
  ) 

mediai <- unique(map$Media)[1]

subrlt <- do.call(rbind,lapply(unique(map$Media),function(mediai){
  print(mediai)
  yi <- Y$Y_new_user.y-rowSums(Xnuser[,!colnames(Xnuser)%in%c(mediai)])
  yi <- Xnuser[,colnames(Xnuser)%in%c(mediai,'base')] %>% rowSums()
  xi <- sepdata %>%
    filter(media==mediai) %>%
    acast(week~var,value.var='spd')
  bi <- rep(1,ncol(xi))
  loss <- function(b){
    b <- abs(b)
    r <- -cor(xi %*% cbind(b),yi)  
    v <- var(log(abs(b)))
    r+v
  }
  if(length(bi)==1){
    b <- list(1) 
  } else {
    b <- optim(bi,loss,control=list(maxit=10000))
  }
  data.table(
    media=mediai,
    submedia = colnames(xi),
    coef=abs(b[[1]]),
    pie = (rowSums(t(xi) * abs(b[[1]])))/sum(rowSums(t(xi) * abs(b[[1]])))
  )
}))


################################################################################################
################################################################################################

#Output

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')
  close(clip)
}

data.table(
  week = Y$week,
  actual=Y$Y_new_user.y,
  predict=rowSums(Xnuser),
  Xnuser
) %>% write.clip

data.table(
  colnames(Xnuser),
  new=round(colSums(Xnuser)/sum(Xnuser)*100,2),
  all=round(colSums(Xauser)/sum(Xauser)*100,2)
) %>%
  mutate(newuser_rate=new/all*sum(Y$Y_new_user.y)/sum(Y$Y_All.User.Count)) %>% 
  write.clip

data.table(
  media=colnames(Xnuser),
  new=round(colSums(Xnuser)/sum(Xnuser)*100,2),
  all=round(colSums(Xauser)/sum(Xauser)*100,2)
) %>%
  mutate(rate=new/all*sum(Y$Y_new_user.y)/sum(Y$Y_All.User.Count)) %>%
  merge(subrlt,by='media',all=T) %>%
  mutate(coef=ifelse(is.na(coef),1,coef),pie=ifelse(is.na(pie),1,pie)) %>%
  mutate(pie=pie*new,rate2=coef*rate) %>%
  write.clip

