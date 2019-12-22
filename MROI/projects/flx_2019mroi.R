
# rm(list=ls())
library(dplyr)

plotmodel <- function(model){
  plot.ts((actual<-raw$volume))
  lines(fit<-(predict(model)+rowSums(hold)),col=2)
  print(
    c(actual1=sum(actual[13:24])/sum(actual[1:12]),
      fit1=sum(fit[13:24])/sum(fit[1:12]),
      actual2=sum(actual[25:30])/sum(actual[13:18]),
      fit2=sum(fit[25:30])/sum(fit[13:18])
    )
  )
  return(model)
}
contri <- function(x,coef=1,value=T){
  x <- coef * x
  x1 <- tapply(x,rep(c(1,2,3),c(12,12,6)),sum)/
    tapply(modelfile$volume,rep(c(1,2,3),c(12,12,6)),sum)
  x2 <- sum(x[13:18])/sum(modelfile$volume[13:18])
  if(value){
    return(x)
  }else{
    return(c(x1[1:2],x2,x1[3]))
  }
}

# Load data
# raw <- read.table(pipe("pbpaste"), sep="\t", header=T)

modelfile <- select(raw,-value)

# model

hold <- cbind(
  tv=contri(modelfile$tv,rep(c(1.06e+01,1.06e+01,.56e+01),c(12,12,6))),
  otv=contri(modelfile$tv,rep(c(2.209 ,2.209 ,2.209 ),c(12,12,6))),
  target_otv = 0,
  digital=contri(modelfile$digital,rep(c(22.475e-01,12.475e-01,12.475e-01),c(12,12,6))),
  sem=contri(modelfile$sem,rep(c(2.419e-03,2.419e-03,2.419e-03),c(12,12,6))),
  social=contri(modelfile$social,rep(c(1.499e-04,1.499e-04,1.499e-04),c(12,12,6))),
  posm=contri(modelfile$posm,rep(c(.0046,.0046,.0046),c(12,12,6))),
  brand_reminder=contri(modelfile$brand_reminder,rep(c(11.36,11.36,11.36),c(12,12,6))),
  training=contri(modelfile$brand_reminder,rep(c(5.936e-01,5.936e-01,5.936e-01),c(12,12,6))),
  advisory=contri(modelfile$advisory,1.342e-03),
  avp = 0,
  nd = 0,
  wd = contri(modelfile$wd,193.5109),
  hospital = 0,
  ec = contri(modelfile$ec,-0.2018),
  category = contri(modelfile$category,0.1582),
  clarityne = 0,
  rhino = contri(modelfile$rhino,-0.1495),
  NewXisimin = 0
)

modelfile <- mutate(modelfile,
                    y=volume-rowSums(hold),
                    month=paste(rep(1:12,3))[1:30])

(model <- lm(y~month,data=modelfile)) %>% plotmodel
fit <- predict(model)+rowSums(hold)
summary(lm(modelfile$volume~fit-1))

write.csv(apply(hold,2,contri,value=F),pipe("pbcopy"))
write.csv(decomp <- cbind(
  intercept=predict(model),
  hold,
  fit=predict(model)+rowSums(hold),
  actual=modelfile$volume
),pipe('pbcopy'))

lmtest::dwtest(lm(modelfile$volume~fit-1))

#Summary
contri(modelfile$ec,-0.2018  ,value=F)
contri(modelfile$sem,rep(c(2.419e-03,2.419e-03,2.419e-03),c(12,12,6)),value=F)

###################

decomp <- dplyr::select(as.data.frame(decomp),-fit,-actual)
i <- 2
colnames(decomp)[i]
x <- raw[,colnames(raw)==colnames(decomp)[i]]
x1 <- x2 <- x3 <- x
x1[-1:-12] <- 0
x2[-13:-24] <- 0
x3[1:24] <- 0
hold <- decomp[,-i]
y <- raw$volume-rowSums(hold)
coef(lm(y~x1+x2+x3-1))

out <- lapply(1:ncol(decomp),function(i){
  print(i)
  x <- raw[,colnames(raw)==colnames(decomp)[i]]
  if(length(x)==0){return(matrix(rep(NA,4),nrow=1))}
  x1 <- x2 <- x3 <- x
  x1[-1:-12] <- 0
  x2[-13:-24] <- 0
  x3[1:24] <- 0
  hold <- decomp[,-i]
  y <- raw$volume-rowSums(hold)
  cbind(coef(summary(lm(y~x1+x2+x3-1))))
})
names(out) <- colnames(decomp)
for(i in 1:length(out)){
  rownames(out[[i]]) <- rep(names(out)[i],nrow(out[[i]]))
}
write.csv(do.call(rbind,out),pipe('pbcopy'))

write.csv(cbind(1-(pnorm(abs(raw[[1]])))),pipe('pbcopy'))

########

x <- fread('sem.csv')
x <- x[,1:9]
colnames(x) <- tolower(colnames(x))
x$impression <- as.numeric(gsub('¥|,','',x$impression))
x$spending <- as.numeric(gsub('¥|,','',x$spending))
x$click <- as.numeric(gsub('¥|,','',x$click))

x <- x %>% group_by(month,keyword,ocp,device) %>% summarise(sum(impression),sum(click),sum(spending))
write.csv(x,pipe('pbcopy'))
