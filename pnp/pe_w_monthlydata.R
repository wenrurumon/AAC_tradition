
rm(list=ls())
library(data.table)
library(openxlsx)
library(dplyr)
setwd('/Users/huzixin/Documents/nielsen/bi/bimmm')
raw.iqvia <- read.xlsx('venndata.xlsx')

write.clip <- function(x){
  clip <- pipe('pbcopy','w')
  write.table(x, file=clip, sep = ',')
  close(clip)
}

data.iqvia <- raw.iqvia %>%
  select(month=3,brand=4,sku=5,channel=Channel,channel2=Channel2,segment=Segment,species=Species,
         format=Unit,lprice=`List.Price(RMB/Dose)`,
         value=`Value(RMB.'000)`,volume=`Volume(Dose.'000)`) %>%
  group_by(month,brand,sku,channel,channel2,segment,species,format) %>%
  summarise(lvalue=sum(volume*lprice),value=sum(value),volume=sum(volume)) %>%
  mutate(price=value/volume,lprice=lvalue/volume) %>%
  mutate(sku=paste(brand,sku)) #%>%
  # mutate(pidx=price/rprice,tprspd=(rprice-price)*volume)

data.rp <- data.iqvia %>%
  group_by(channel,channel2,brand,sku,segment,species,month) %>%
  summarise(value=sum(value),volume=sum(volume),price=value/volume) %>%
  dcast(channel+channel2+brand+sku+segment+species~month,value.var='price')

for(i in 1:nrow(data.rp)){
  x <- unlist(data.rp[i,-1:-6])
  x <- apply(
    cbind(x
          ,c(NA,x[-length(x)])
          ,c(x[-1],NA)
          ,c(x[-1:-2],NA,NA)
          ,c(NA,NA,x[-length(x)+0:1])
    ),
    1,max,na.rm=T)
  x[x==-Inf] <- NA
  data.rp[i,-1:-6] <- x
}

data.iqvia <- data.iqvia %>%
  merge(
    data.rp %>%
      melt() %>%
      select(1:6,month=7,rprice=8),
    all.x=T
  ) %>%
  mutate(price=ifelse(price>rprice,rprice,price)) %>%
  mutate(pidx=price/rprice,
         tprspd=(rprice-price)*volume)

##########################################################################################
# Iqivia Pricing
##########################################################################################

data.category <- data.iqvia %>%
  group_by(month,segment,species) %>%
  summarise(catval=sum(value),catvol=sum(volume),catprice=catval/catvol)
# 
# data.category <- data.category %>%
#   select(-catval) %>%
#   merge(
#     data.category %>%
#       group_by(month,segment,species) %>%
#       summarise(catval=sum(catval))    
#   )

data.bi <- data.iqvia %>%
  filter(grepl('来恩|可信',brand,ignore.case=T)) %>%
  filter(channel2=='天猫旗舰') %>%
  merge(data.category)

test <- do.call(rbind,lapply(unique(data.bi$sku),function(i){
  data.brandi <- data.bi %>% filter(sku==i)
  if(nrow(data.brandi %>%
          select(channel,channel2) %>%
          unique())>1){
    model <- lm(volume~tprspd+catval+paste(channel,channel2),data=data.brandi)  
  } else {
    model <- lm(volume~tprspd+catval,data=data.brandi)
  }
  data.brandi$dvol <- as.matrix(data.brandi[,c('tprspd'),drop=F]) %*% cbind(coef(model)[2])
  data.brandi$dvol[data.brandi$dvol<0] <- 0
  data.brandi %>% mutate(rvol=volume-dvol,rval=rvol*rprice)
}))

test %>%
  filter(!grepl('Others',sku)) %>%
  filter(channel2=='天猫旗舰') %>%
  group_by(brand) %>%
  summarise(
    lval=sum(lprice*volume),
    volume=sum(volume),value=sum(value),dvol=sum(dvol),rval=sum(rval),
    avp=value/volume
  ) %>%
  mutate(
    rvol=volume-dvol,lprice=lval/volume
  ) %>%
  mutate(rprice=rval/rvol) %>%
  mutate(pidx=avp/rprice-1,pidx2=avp/lprice-1,uplift=dvol/rvol) %>%
  mutate(pe=uplift/pidx,pe2=uplift/pidx2) %>%
  # select(brand,volume,value,avp,rp=rprice,lp=lprice,uplift,ppi_rp=pidx,pe_rp=pe,ppi_lp=pidx2,pe_lp=pe2) %>%
  write.clip
