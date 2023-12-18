pacman::p_load(readr,tidyverse,survival,AdequacyModel,rms,BART,randomForestSRC,StepReg,cuRe,survminer,dplyr)


#### 1. Ler o banco de dados `adesao` que está disponível no Sigaa.


dados <- read_csv("dados/heart_failure_clinical_records_dataset.csv") %>%
  mutate(censura = DEATH_EVENT,
         tempo = time,
         age = round(age,0)) %>% 
  dplyr::select(-c(DEATH_EVENT,time))



head(dados)

dados2 = dados %>%
  dplyr::select(censura) %>%
  drop_na() %>%
  count(censura) %>%
  mutate(freq = round((n/sum(n)*100),1)) %>% 
  mutate(censura = as.factor(censura))%>%
  mutate(freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
         label = str_c(n," (", freq, ")") %>% str_squish()) 

  
ggplot(dados2) +
  aes(x = fct_reorder(str_wrap(censura), n, .desc=T), y = n , label = label) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3 ) + 
  labs(x = "Censura", y = "Frequencia")
  

ggplot(data=dados, aes(x=age)) +
  geom_histogram(fill="steelblue", color="black",bins = 14) +
  ggtitle("Histograma de Idade (Age)")

ggplot(data=dados, aes(x=creatinine_phosphokinase)) +
  geom_histogram(fill="steelblue", color="black",bins = 14) +
  ggtitle("Histograma de Creatinine Phosphokinase")

ggplot(data=dados, aes(x=ejection_fraction)) +
  geom_histogram(fill="steelblue", color="black",bins = 14) +
  ggtitle("Histograma de Ejection Fraction")

ggplot(data=dados, aes(x=platelets)) +
  geom_histogram(fill="steelblue", color="black",bins = 15) +
  ggtitle("Histograma de Platelets")

ggplot(data=dados, aes(x=serum_creatinine)) +
  geom_histogram(fill="steelblue", color="black",bins = 15) +
  ggtitle("Histograma de Serum Creatinine")

ggplot(data=dados, aes(x=serum_sodium)) +
  geom_histogram(fill="steelblue", color="black",bins = 15) +
  ggtitle("Histograma de Serum Sodium")




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


#### Escolhendo a Distribuicao ----

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




#### Calculando as medidas AIC, AICc e BIC ----


#### WEIBULL
#a funcao suvreg com weibull retorna o valor extremo, para isso temos que fazer uma transformacao para encontrar os parametrod da weibull padrao.
n = length(dados$tempo) #n de obs

gama_weibull = 1/weibull.null$scale

alpha_weibull = exp(weibull.null$icoef[1])

pws = 2 #numero de parametros da distribuicao

AICws = (-2*weibull.null$loglik[1])+(2*pws)

AICcws = AICws + ((2*pws*(pws+1))/(n-pws-1))

BICws = (-2*weibull.null$loglik[1])+(pws*log(n))

medidasw = cbind(AICws,AICcws,BICws)

#### LOG-NORMAL
sigma_lognormal = lognormal.null$scale

mi_lognormal = lognormal.null$icoef[1]

plns = 2 #numero de parametros para ser estimado no modelo

AIClns = (-2*lognormal.null$loglik[1])+(2*plns)

AICclns = AIClns + ((2*plns*(plns+1))/(n-plns-1))

BIClns = (-2*lognormal.null$loglik[1])+(plns*log(n))

medidalns = cbind(AIClns,AICclns,BIClns)

#### LOG-LOGISTICA

gama_loglogistica = 1/loglogistic.null$scale

alpha_loglogistica = exp(loglogistic.null$icoef[1])

plls = 2 #numero de parametros para ser estimado no modelo

AIClls = (-2*loglogistic.null$loglik[1])+(2*plls)

AICclls = AIClls + ((2*plls*(plls+1))/(n-plls-1))

BIClls = (-2*loglogistic.null$loglik[1])+(plls*log(n))

medidalls = cbind(AIClls,AICclls,BIClls)


#resultados das medidas de cada distribuicao
medidasw
medidalns
medidalls






#### Aplicando a fração de cura ----

#verossimilhanca exponencial
x0 = c(rep(1,n))
x1 = dados$age
x2 = dados$ejection_fraction
x3 = dados$high_blood_pressure
x4 = dados$serum_creatinine
t = dados$tempo
censura = dados$censura

lognorm_vero = function(param){
  
  sigma =  param[1] 
  phi = param[2]
  beta0 =  param[3]
  beta1 =  param[4]
  beta2 =  param[5]
  beta3 =  param[6]
  beta4 =  param[7]
  
  
  
  mip = cbind(x0,x1,x2,x3,x4) %*% c(beta0,beta1,beta2,beta3,beta4)
  
  if((sigma>0) && (phi>0) && (phi<1)) {
    
    densi = ((1/(t*sigma*sqrt(2*pi)))*exp(-((log(t)-mip)^2/(2*sigma^2))))
    sobrevi = 1-pnorm((t-mip)/sigma)
    spop = phi + (1-phi)*sobrevi
    fpop = (1-phi)*densi
    
    max = sum(censura * log(fpop) + (1-censura)*log(spop))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results_expo = optim(c(1.63,0.5,-0.336261,0.0403,-0.5081,0.0732,-0.046719),lognorm_vero,NULL,hessian = T,method = "BFGS")



results_expo$convergence
results_expo$par
logLE = (-1)*results_expo$value
invRE = solve(results_expo$hessian)
varianciaE = diag(invRE)
eppE = sqrt(varianciaE)

phi = results_expo$par[2]

## Medidas

plns = 2 #descobrir o que é isso

AIClns = (-2*logLE)+(2*plns)

AICclns = AIClns + ((2*plns*(plns+1))/(n-plns-1))

BIClns = (-2*logLE)+(plns*log(n))

(medidalns1 = cbind(AIClns,AICclns,BIClns))

### residuos de cox snell
y = log(dados$tempo)
mip = cbind(x0,x1,x2,x3,x4) %*% results_expo$par[3:length(results_expo$par)] 
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



# Residuo Martingal
martingal = dados$censura - ei

par(mfrow = c(1,2))
plot(y,martingal,xlab = "log(tempo)", ylab = "Residuo Martingal", pch = censura+1)
plot(rank(y),martingal,xlab = "Rank das Observacoes", ylab = "Residuo Martingal", pch = censura+1)


#deviance 

devw = (martingal/abs(martingal))*(-2*(martingal+censura*log(censura-martingal)))^(1/2)
plot(y,devw,xlab = "log(tempo)",ylab="Residuo Deviance", pch = censura+1)
plot(rank(y),devw,xlab = "Rank das Observacoes",ylab="Residuo Deviance", pch = censura+1)

###################

#verossimilhanca exponencial
x0 = c(rep(1,n))
x1 = dados$age
x2 = dados$ejection_fraction
x3 = dados$serum_creatinine
t = dados$tempo
censura = dados$censura

lognorm_vero = function(param){
  
  sigma =  param[1] 
  phi = param[2]
  beta0 =  param[3]
  beta1 =  param[4]
  beta2 =  param[5]
  beta3 =  param[6]
  
  
  
  mip = cbind(x0,x1,x2,x3) %*% c(beta0,beta1,beta2,beta3)
  
  if((sigma>0) && (phi>0) && (phi<1)) {
    
    densi = ((1/(t*sigma*sqrt(2*pi)))*exp(-((log(t)-mip)^2/(2*sigma^2))))
    sobrevi = 1-pnorm((t-mip)/sigma)
    spop = phi + (1-phi)*sobrevi
    fpop = (1-phi)*densi
    
    max = sum(censura * log(fpop) + (1-censura)*log(spop))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results_expo = optim(c(1.63,0.5,-0.336261,0.0403,-0.5081,0.0732),lognorm_vero,NULL,hessian = T,method = "BFGS")


results_expo$convergence
results_expo$par
logLE = (-1)*results_expo$value
invRE = solve(results_expo$hessian)
varianciaE = diag(invRE)
eppE = sqrt(varianciaE)

phi = results_expo$par[2]

## Medidas

plns = 2 #descobrir o que é isso

AIClns = (-2*logLE)+(2*plns)

AICclns = AIClns + ((2*plns*(plns+1))/(n-plns-1))

BIClns = (-2*logLE)+(plns*log(n))

(medidalns2 = cbind(AIClns,AICclns,BIClns))

#teste da razao de verossimilhança

#TRV = 2*(logLW-logLE)
#1-pchisq(TRV,1)


### residuos de cox snell
y = log(dados$tempo)
mip = cbind(x0,x1,x2,x3) %*% results_expo$par[3:length(results_expo$par)] 
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
  


# Residuo Martingal
martingal = dados$censura - ei

par(mfrow = c(1,2))
plot(y,martingal,xlab = "log(tempo)", ylab = "Residuo Martingal", pch = censura+1)
plot(rank(y),martingal,xlab = "Rank das Observacoes", ylab = "Residuo Martingal", pch = censura+1)


#deviance 

devw = (martingal/abs(martingal))*(-2*(martingal+censura*log(censura-martingal)))^(1/2)
plot(y,devw,xlab = "log(tempo)",ylab="Residuo Martingal", pch = censura+1)
plot(rank(y),devw,xlab = "Rank das Observacoes",ylab="Residuo Martingal", pch = censura+1)
















