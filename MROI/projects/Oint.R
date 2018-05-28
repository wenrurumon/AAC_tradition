
################################

o.price <- (572864+290066)/ ((572864000/11.95 * 5) + (290066000/20.29 * 10)) * 1000
hold_t5 <- (coef(summary(lm(o5.vol~hold_ototal)))[-1,3]/40+1) * ((572864000/11.95 * 5) / ((572864000/11.95 * 5) + (290066000/20.29 * 10)))
hold_t5[6] <- 0.093333
hold5 <- t(t(hold_ototal[,-1]) * hold_t5)
hold10 <- hold_ototal[,-1] - hold5

y <- 572864 * o5.vol / sum(o5.vol)
canni <- 290066 * o10.vol / sum(o10.vol)
hold5 <- cbind(hold5
               ,o5.wd = o5.wd * 5.35600 + o5.wd * y1 * 0.1154883
               )
y_2 <- y - rowSums(hold5)
coef(summary(xlm <- lm(y_2~ 
                      # +I(o5.wd)
                       + I(canni*y1) + I(canni*y2) + I(canni*y3)
                      +y1
                       +paste(c.dum)
                       +paste(m.dum2)
                       -1
)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold5)),col=2)
decomp <- cbind(hold5,
                o10.wd_canni = (-0.3556491*y1 +-0.4355374 *y2 + -0.5974095*y3) * canni + 50.5565828 * y1
                ,actual = as.numeric(y))
decomp <- as.data.frame((apply(decomp,2,tm)))
# decomp <- decomp * sum(o5.vol)/572864 * o.price
res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc5 <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc5,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp5 <- decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp[,3,drop=F]/decomp[,2,drop=F]-1
decomp[,2,drop=F]/decomp[,1,drop=F]-1

(decomp[nrow(decomp)-3,1:3]+tapply(290066 * o10.vol / sum(o10.vol),y.dum,sum))/tapply(290066 * o10.vol / sum(o10.vol),y.dum,sum)
decomp

##############################

y <- 290066 * o10.vol / sum(o10.vol)
y_2 <- y - rowSums(hold10)
o10.wdsm <- sm(o10.wd)
coef(summary(xlm <- lm(y_2~ 
                         # + I(o10.wd*y1) + I(o10.wd*y2) + I(o10.wd*y3)
                       +paste(c.dum)
                       +paste(m.dum2)
                       -1
)))[1:10,]
plot.ts(tm(y));lines(tm(predict(xlm)+rowSums(hold10)),col=2)
decomp <- cbind(hold10,
                o10.wd = o10.wd * (y1*3.045919+y2*3.394174+y3*3.656856),
                actual = as.numeric(y))
decomp <- as.data.frame((apply(decomp,2,tm)))
# decomp <- decomp * sum(o10.vol)/290066 * o.price
res <- decomp$actual*2 - rowSums(decomp)
res <- predict(lm(res~paste(rep(1:12,3))))
sc10 <- decomp <- cbind(decomp[,-ncol(decomp)],intercept=res,predict=rowSums(decomp)-decomp$actual+res,actual=decomp$actual)
plot.ts(decomp$actual); lines(decomp$predict,col=2)
decomp <- apply(sc10,2,function(x){tapply(x,rep(1:3,each=12),sum)})
decomp10 <- decomp <- t(rbind(decomp,round(decomp / decomp[,ncol(decomp)-1],4)))
decomp[,3,drop=F]/decomp[,2,drop=F]-1
decomp[,2,drop=F]/decomp[,1,drop=F]-1

################################

decomp <- rbind(
  data.frame(model='o5',var=rownames(decomp5),decomp5),
  data.frame(model='o10',var=rownames(decomp10),decomp10)
)
decomp <- data.frame(model=decomp[,1:2],define=NA,decomp[,-1:-2])
setwd('/Users/wenrurumon/Desktop')
write.csv(decomp,'test.csv',quote=F,row.names=F)



