pacman::p_load(tidyverse,survival)

tempo = c(3,6,8,9,9,10,11,12,12,12,1,1,2,3,4,5,5,7,9,12)
censura = c(rep(1,4),0,1,1,0,0,0,rep(1,6),0,1,1,0)
grupo = c(rep(1,10),rep(2,10))

dados = data.frame(tempo,censura,grupo) %>%
  arrange(tempo) %>%
  mutate(grupo = as.factor(grupo))

#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)

KM = survfit(Surv(dados$tempo,dados$censura)~dados$grupo)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

summary(KM)

logrank = survdiff(Surv(tempo, censura) ~ grupo, data=dados, rho = 0)

wilcoxon = survdiff(Surv(tempo, censura) ~ grupo, data=dados, rho = 1)
# intervalo de confianca 

KM = survfit(Surv(dados$tempo,dados$censura)~1,conf.type = "plain",conf.int = T)

plot(KM)

#funcao de risco
HHt = -log(KM$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#risco relativo

RR = (logrank$obs[1]/logrank$exp[1])/(logrank$obs[2]/logrank$exp[2])





