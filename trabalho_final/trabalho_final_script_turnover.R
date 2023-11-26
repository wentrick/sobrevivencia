pacman::p_load(readr,tidyverse,survival,AdequacyModel,rms)

#### 1. Ler o banco de dados `adesao` que está disponível no Sigaa.


dados <- read_csv("dados/turnover.csv") %>%
  mutate(censura = event,
         tempo = stag) %>% 
  select(-c(event,stag))

head(dados)


km_model <- survfit(Surv(dados$tempo, dados$censura) ~ 1)
#summary(km_model)

plot(km_model, xlab="Time", ylab="Probability of Employee Churn", mark.time = T)


# Resposta x gender

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$gender) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ gender, data=dados, rho = 1)



# Resposta x industry

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$industry) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ industry, data=dados, rho = 1)


# Resposta x profession

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$profession) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ profession, data=dados, rho = 1)


# Resposta x coach

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$coach) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ coach, data=dados, rho = 1)


# Resposta x head_gender

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$head_gender) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ head_gender, data=dados, rho = 1)


# Resposta x grey_wage

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$greywage) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ greywage, data=dados, rho = 1)


# Resposta x way

km_model2 <- survfit(Surv(dados$tempo, dados$censura) ~ dados$way) # independent var must be categorical
#summary(km_model2)

plot(km_model2, xlab="Time", ylab="Probability of Employee Churn", mark.time = T,col = c("red","blue","green","yellow","purple","cyan","black"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ way, data=dados, rho = 1)


#create a Surv object
s <- with(dados,Surv(tempo,censura))
## Kaplan-Meier estimator without grouping
km.null <- survfit(data = dados, s ~ 1)
plot(km.null, ylim = c(0, 1),conf.int = F)

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

## Add legends
legend(x = "bottomleft",
       legend = c("Kaplan-Meier", "Cox (Efron)", "Weibull", "log-logistic","log-normal"),
       lwd = 2, bty = "n",
       col = c("black", "purple", "red", "blue","green"))




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




cat("Log-Normal ~(",gama_loglogistica,",",alpha_loglogistica,")")

medidalls

summary(loglogistic.null)



medidasw
medidalns
medidalls


######

lognormal.1 <- survreg(data = dados, s ~ linha, dist = "lognorm")
summary(lognormal.1)

lognormal.2 <- survreg(data = dados, s ~ propatraso, dist = "lognorm")
summary(lognormal.2)

lognormal.3 <- survreg(data = dados, s ~ comprimdia, dist = "lognorm")
summary(lognormal.3)


lognormal.4 <- survreg(data = dados, s ~ comprimdia + propatraso, dist = "lognorm")
summary(lognormal.4)

lnorm4 = lognormal.4$loglik[2] #log da verossimilhanca do modelo com 2 variaveis
lnorm2 = lognormal.2$loglik[2] #log da verossimilhanca do modelo com propatraso

TRV = 2*(lnorm4 - lnorm2)
TRV

1-pchisq(TRV,1)




lnorm4 = lognormal.4$loglik[2] #log da verossimilhanca do modelo com 2 variaveis
lnorm3 = lognormal.3$loglik[2] #log da verossimilhanca do modelo com comprimdia

TRV = 2*(lnorm4 - lnorm3)
TRV

1-pchisq(TRV,1)






lognormal.5 <- survreg(data = dados, s ~ comprimdia + propatraso + linha, dist = "lognorm")
summary(lognormal.5)

lnorm4 = lognormal.4$loglik[2] #log da verossimilhanca do modelo com 2 variaveis
lnorm5 = lognormal.5$loglik[2] #log da verossimilhanca do modelo com todas as variaveis

TRV = 2*(lnorm5 - lnorm4)
TRV

1-pchisq(TRV,2)

lognormal.6 <- survreg(data = dados, s ~ comprimdia + propatraso + comprimdia*propatraso, dist = "lognorm")
summary(lognormal.6)

lnorm4 = lognormal.4$loglik[2] #log da verossimilhanca do modelo com 2 variaveis
lnorm6 = lognormal.6$loglik[2] #log da verossimilhanca do modelo com 2 variaveis e interacao entre elas

TRV = 2*(lnorm6 - lnorm4)
TRV

1-pchisq(TRV,1)


lognormal.4 <- survreg(data = dados, s ~ comprimdia + propatraso, dist = "lognorm")
summary(lognormal.4)




























































