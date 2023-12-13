pacman::p_load(readr,tidyverse,survival,AdequacyModel,rms,BART,randomForestSRC,StepReg)

#### 1. Ler o banco de dados `adesao` que está disponível no Sigaa.


dados <- read_csv("dados/heart_failure_clinical_records_dataset.csv") %>%
  mutate(censura = DEATH_EVENT,
         tempo = time,
         age = round(age,0)) %>% 
  select(-c(DEATH_EVENT,time))



head(dados)


#### 2. Fazer uma análise exploratória apenas da variável resposta: estimativa de Kaplan-Meier (KM), gráfico da função de risco acumulado e gráfico do Tempo Total em Teste (gráfico TTT).


#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)


summary(KM)
#funcao de risco
HHt = -log(KM$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#autoplot(KM,fun = "cumhaz",xlab = "Tempo",ylab = "Risco Acumulado H(t)")

#curva TTT
TTT(dados$tempo)



# Resposta x Sex

KM = survfit(Surv(dados$tempo,dados$censura)~dados$sex)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ sex, data=dados, rho = 1)


# Resposta x diabetes

KM = survfit(Surv(dados$tempo,dados$censura)~dados$diabetes)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))


#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ diabetes, data=dados, rho = 1)


# Resposta x anaemia

KM = survfit(Surv(dados$tempo,dados$censura)~dados$anaemia)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ anaemia, data=dados, rho = 1)


# Resposta x blood pressure

KM = survfit(Surv(dados$tempo,dados$censura)~dados$high_blood_pressure)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ high_blood_pressure, data=dados, rho = 1)


# Resposta x smoking

KM = survfit(Surv(dados$tempo,dados$censura)~dados$smoking)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ smoking, data=dados, rho = 1)




#Linhas TTT por categorias da variavel linha
par(mfrow = c(1,2))
dad2<-data.frame("tempo" = dados$tempo, "high_blood_pressure" = dados$high_blood_pressure)
tempo1<-dad2[dad2$high_blood_pressure == "0",]
TTT(tempo1$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)
tempo2<-dad2[dad2$high_blood_pressure == "1",]
TTT(tempo2$tempo, col="red", lwd=2.5, grid=TRUE, lty=2)



#create a Surv object
s <- with(dados,Surv(tempo,censura))
## Kaplan-Meier estimator without grouping
km.null <- survfit(data = dados, s ~ 1)
plot(km.null, ylim = c(0.5, 1),conf.int = F)

## Parametric estimation with Weibull distribution
weibull.null <- survreg(data = dados, s ~ 1, dist = "weibull")
lines(x = predict(weibull.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "red")

## Parametric estimation with log-logistic distribution
loglogistic.null <- survreg(data = dados, s ~ 1, dist = "loglogistic")
lines(x = predict(loglogistic.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "blue")

## Parametric estimation with log-normal distribution
lognormal.null <- survreg(data = lung, s ~ 1, dist = "lognorm")
lines(x = predict(loglogistic.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "green")

## Parametric estimation with log-normal distribution
normal.null <- survreg(data = lung, s ~ 1, dist = "gaussian")
lines(x = predict(normal.null, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "purple")

## Add legends
legend(x = "bottomleft",
       legend = c("Kaplan-Meier", "Weibull", "log-logistic","log-normal","normal"),
       lwd = 2, bty = "n",
       col = c("black", "red", "blue","green","purple"))




#a funcao suvreg com weibull retorna o valor extremo, para isso temos que fazer uma transformacao para encontrar os parametrod da weibull padrao.
n = length(dados$tempo) #n de obs

gama_weibull = 1/weibull.null$scale

alpha_weibull = exp(weibull.null$icoef[1])


pws = 2 #numero de parametros da distribuicao

AICws = (-2*weibull.null$loglik[1])+(2*pws)

AICcws = AICws + ((2*pws*(pws+1))/(n-pws-1))

BICws = (-2*weibull.null$loglik[1])+(pws*log(n))

medidasw = cbind(AICws,AICcws,BICws)




cat("Weibull ~(",gama_weibull,",",alpha_weibull,")")

medidasw

summary(weibull.null)




sigma_lognormal = lognormal.null$scale

mi_lognormal = lognormal.null$icoef[1]


plns = 2 #descobrir o que é isso

AIClns = (-2*lognormal.null$loglik[1])+(2*plns)

AICclns = AIClns + ((2*plns*(plns+1))/(n-plns-1))

BIClns = (-2*lognormal.null$loglik[1])+(plns*log(n))

medidalns = cbind(AIClns,AICclns,BIClns)




cat("Log-Normal ~(",mi_lognormal,",",sigma_lognormal,")")

medidalns

summary(loglogistic.null)



gama_loglogistica = 1/loglogistic.null$scale

alpha_loglogistica = exp(loglogistic.null$icoef[1])


plls = 2 #descobrir o que é isso

AIClls = (-2*loglogistic.null$loglik[1])+(2*plls)

AICclls = AIClls + ((2*plls*(plls+1))/(n-plls-1))

BIClls = (-2*loglogistic.null$loglik[1])+(plls*log(n))

medidalls = cbind(AIClls,AICclls,BIClls)




cat("Log-Logistica ~(",gama_loglogistica,",",alpha_loglogistica,")")

medidalls

summary(loglogistic.null)



medidasw
medidalns
medidalls


######

lognormal.1 <- survreg(data = dados, s ~ 1, dist = "lognorm")
summary(lognormal.1)

lognormal.2 <- survreg(data = dados, s ~ age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking  , dist = "lognorm")
summary(lognormal.2)

lognormal.3 <- survreg(data = dados, s ~ age+anaemia+creatinine_phosphokinase+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium, dist = "lognorm")
summary(lognormal.3)

lognormal.4 <- survreg(data = dados, s ~ , dist = "lognorm")
summary(lognormal.4)

lnorm4 = lognormal.4$loglik[2] #log da verossimilhanca do modelo com 2 variaveis
lnorm2 = lognormal.2$loglik[2] #log da verossimilhanca do modelo com propatraso

TRV = 2*(lnorm4 - lnorm2)
TRV

1-pchisq(TRV,4)


###################

#create a Surv object
s <- with(dados,Surv(tempo,censura))
## Kaplan-Meier estimator without grouping
km.null <- survfit(data = dados, s ~ 1)
plot(km.null, ylim = c(0, 1),conf.int = F)

## Parametric estimation with log-logistic distribution
lognormal.completa <- survreg(data = dados, s ~ age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking, dist = "lognorm")
lines(x = predict(lognormal.completa, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "red")

## Parametric estimation with log-normal distribution
lognormal.reduzida <- survreg(data = dados, s ~ age+anaemia+creatinine_phosphokinase+ejection_fraction+high_blood_pressure+serum_creatinine+serum_sodium, dist = "lognorm")
lines(x = predict(lognormal.reduzida, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "blue")


## Add legends
legend(x = "bottomleft",
       legend = c("Kaplan-Meier", "log-normal completa","log-normal reduzida"),
       lwd = 2, bty = "n",
       col = c("black", "red", "blue","green"))

summary(lognormal.reduzida)


### residuos de cox snell
y = log(dados$tempo)
mip = lognormal.reduzida$linear.predictors
Smod = 1-pnorm((y-mip)/lognormal.reduzida$scale)
ei = (-log(Smod))


Kmew = survfit(Surv(ei,dados$censura)~1, conf.int = F)
te = Kmew$time
ste = Kmew$surv
sexp = exp(-te)

par(mfrow = c(1,1))

plot(ste,sexp, xlab = "S(ei): Kaplan-Meier", ylab = "S(ei): Exponencial Padrao")
plot(Kmew,conf.int = F, xlab = "Residuos de Cox-Snell", ylab = "Sobrevivencia estimada")
lines(te,sexp,lty=2,col=2)
legend(0.6,1.0,lty=c(1,2),c("Kaplan-Meier","Exponencial padrao"),cex=0.8, bty = "n")



#martingal
martingal = dados$censura - ei

plot(y,martingal)

#deviance 

#devw = (martingal/abs(martingal))*(-2*(martingal+censura*log(censura-martingal)))

##### Aplicando a fração de cura

#verossimilhanca exponencial
x0 = c(rep(1,n))
x1 = dados$age
t = dados$tempo
censura = dados$censura

lognorm_vero = function(param){
  
  sigma =  param[1] 
  beta0 =  param[2]
  beta1 =  param[3]
  phi = param[4]
  
  mip = beta0*x0+beta1*x1
  
  if((sigma>0) &&(phi>0) && (phi<1)) {
    
    densi = ((1/(t*sigma*sqrt(2*pi)))*exp(-((log(t)-mip)^2/(2*sigma^2))))
    sobrevi = 1-pnorm((y-mip)/sigma)
    spop = phi + (1-phi)*sobrevi
    fpop = (1-phi)*densi
    
    max = sum(censura * log(fpop) + (1-censura)*log(spop))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results_expo = optim(c(1.63,-0.336261,-0.046719,0.5),lognorm_vero,NULL,hessian = T,method = "BFGS")

results_expo$convergence
results_expo$par
logLE = (-1)*results_expo$value
invRE = solve(results_expo$hessian)
varianciaE = diag(invRE)
eppE = sqrt(varianciaE)

phi = results_expo$par[4]


### residuos de cox snell
y = log(dados$tempo)
mip = results_expo$par[2]*x0+results_expo$par[3]*x1
Smod = phi+(1-phi)*(1-pnorm((y-mip)/results_expo$par[1]))
ei = (-log(Smod))


Kmew = survfit(Surv(ei,dados$censura)~1, conf.int = F)
te = Kmew$time
ste = Kmew$surv
sexp = exp(-te)

par(mfrow = c(1,1))

plot(ste,sexp, xlab = "S(ei): Kaplan-Meier", ylab = "S(ei): Exponencial Padrao")
plot(Kmew,conf.int = F, xlab = "Residuos de Cox-Snell", ylab = "Sobrevivencia estimada")
lines(te,sexp,lty=2,col=2)
legend(0.6,1.0,lty=c(1,2),c("Kaplan-Meier","Exponencial padrao"),cex=0.8, bty = "n")

























