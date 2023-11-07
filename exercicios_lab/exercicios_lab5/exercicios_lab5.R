pacman::p_load(tidyverse,survival,AdequacyModel,rms)

dados <- read.csv("~/R Documents/R Repositories/sobrevivencia/exercicios_lab/exercicios_lab5/adesao.txt", sep="") %>%
  mutate(censura = status) %>%
  select(-status)

#### Analise Descritiva - Variavel resposta ####

#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)


#funcao de risco
HHt = -log(KM$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)

#curva TTT

TTT(tempo)

#### Analise Descritiva - Variavel resposta e covariaveis ####

# Resposta x Linha

KM = survfit(Surv(dados$tempo,dados$censura)~dados$linha)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

summary(KM)

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ linha, data=dados, rho = 1)

#Linhas TTT por categorias da variavel linha
dad2<-data.frame("tempo" = dados$tempo, "linha" = dados$linha)
tempo1<-dad2[dad2$linha == "1",]
TTT(tempo1$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo2<-dad2[dad2$linha == "2",]
TTT(tempo2$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo3<-dad2[dad2$linha == "3",]
TTT(tempo3$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)


#### Modelo Parametrico #### 

#create a Surv object
s <- with(dados,Surv(tempo,censura))

#plot kaplan-meier estimate, per sex
fKM <- survfit(s ~ 1,data=dados)
plot(fKM)

#plot Cox PH survival curves, per sex
sCox <- coxph(s ~ 1,data=dados)
lines(survfit(sCox),col='green')


#plot weibull survival curves, per sex,
sWei <- survreg(s ~ 1 ,dist='weibull',data=dados)

lines(predict(sWei,type="quantile",p=seq(.01,.99,by=.01))[1,],seq(.99,.01,by=-.01),col="red")


#create a Surv object
s <- with(dados,Surv(tempo,censura))
## Kaplan-Meier estimator without grouping
km.null <- survfit(data = dados, s ~ 1)
plot(km.null)

## Overplot estimation from Cox regression by Efron method
cox.null <- coxph(data = dados, s ~ 1)
lines(survfit(cox.null), col = "purple", mark.time = FALSE)

## Parametric estimation with Weibull distribution
weibull.null <- survreg(data = dados, s ~ 1, dist = "weibull")
lines(x = predict(weibull.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "red")

## Parametric estimation with log-logistic distribution
loglogistic.null <- survreg(data = lung, s ~ 1, dist = "loglogistic")
lines(x = predict(loglogistic.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "blue")

## Add legends
legend(x = "topright",
       legend = c("Kaplan-Meier", "Cox (Efron)", "Weibull", "Log-logistic"),
       lwd = 2, bty = "n",
       col = c("black", "purple", "red", "blue"))

#a funcao suvreg com weibull retorna o valor extremo, para isso temos que fazer uma transformacao para encontrar os parametrod da weibull padrao.

gama_weibull = 1/weibull.null$icoef[1]

alpha_weibull = exp(weibull.null$icoef[2])
