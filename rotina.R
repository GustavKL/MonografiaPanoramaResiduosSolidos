library(car)
library(lmtest)
setwd("E:\\Trabalhos\\Monografia")
rs<-read.csv("RS5.csv")

names(rs)<-c("cidade","quantidade","quantidade_reci","relacao_reci","pib","pop","desp_saude","desp_coleta","desp_manejo","desp_servico","politica","plano","coleta_selet")

mod1<-lm(quantidade~pib,data=rs)
summary(mod1)
#diagrama de dispersão
ehat1<-resid(mod1)
yhat1<-fitted(mod1)
plot(yhat1,ehat1,xlab="PIB",ylab="Resíduos",ylim=c(-700000,500000),xlim=c(0,1000000))
abline(h=0,col="red")
s<-2
res<-lm(u2~pib,data=rs)
N<-nobs(res)
u2<-resid(mod1)^2
white<-lm(u2~pib+I(pib^2),data=rs)  #I-para transf. algébrica
R2white<-summary(white)$r.squared
chi2white<-R2white*N
chi2white
valor_pwhite<-pchisq(chi2white,s-1,lower.tail=F)
valor_pwhite
linearHypothesis(mod1, hypothesis.matrix = "pib = 1")$'Pr(>F)'[2] < 0.05
bptest(mod1)

mod2<-lm(quantidade~pop,data=rs)
summary(mod2)
#diagrama de dispersão
ehat2<-resid(mod2)
yhat2<-fitted(mod2)
plot(yhat2,ehat2,xlab="População",ylab="Resíduos",ylim=c(-700000,500000),xlim=c(0,1000000))
abline(h=0,col="red")
bptest(mod2)

mod3<-lm(desp_coleta~quantidade,data=rs)
summary(mod3)
bptest(mod3)
coeftest(mod3)

mod4<-lm(desp_manejo~quantidade,data=rs)
summary(mod4)
bptest(mod4)

mod5<-lm(desp_saude~quantidade,data=rs)
summary(mod5)

rs$politica<-as.factor(rs$politica)
mod6<-lm(desp_manejo~politica,data=rs)
summary(mod6)

rs$plano<-as.factor(rs$plano)
mod7<-lm(desp_manejo~plano,data=rs)
summary(mod7)

rs$coleta_selet<-as.factor(rs$coleta_selet)
mod8<-lm(desp_saude~coleta_selet,data=rs)
summary(mod8)

###################################################################################
mod1<-lm(quantidade~pib,data=rs)
summary(mod1)
bptest(mod1)

mod2<-lm(quantidade~pop,data=rs)
summary(mod2)
bptest(mod2)

mod3<-lm(desp_coleta~quantidade,data=rs)
summary(mod3)
bptest(mod3)
coeftest(mod3)

mod4<-lm(desp_manejo~quantidade,data=rs)
summary(mod4)
bptest(mod4)
coeftest(mod4)

mod5<-lm(desp_saude~quantidade,data=rs)
summary(mod5)
bptest(mod5)
coeftest(mod5)

rs$politica<-as.factor(rs$politica)
mod6<-lm(desp_coleta~politica,data=rs)
summary(mod6)
bptest(mod6)

rs$politica<-as.factor(rs$politica)
mod7<-lm(desp_manejo~politica,data=rs)
summary(mod7)
bptest(mod7)

rs$plano<-as.factor(rs$plano)
mod8<-lm(desp_coleta~plano,data=rs)
summary(mod8)
bptest(mod8)

rs$plano<-as.factor(rs$plano)
mod9<-lm(desp_manejo~plano,data=rs)
summary(mod9)
bptest(mod9)


###################################################################################

setwd("C:\\Users\\gusta\\Desktop\\Banco")
rs<-read.csv("RS5provisorio.csv")

names(rs)<-c("cidade","desp_coleta","desp_manejo","desp_servico","coleta_selet")

rs$coleta_selet<-as.factor(rs$coleta_selet)
mod9<-lm(desp_coleta~coleta_selet,data=rs)
summary(mod9)