pacman::p_load(tidyverse,survival)

tempo = c(7,34,42,63,64,74,83,84,91,108,112,129,133,133,139,140,140,146,149,154,157,160,160,
          165,173,176,185,218,225,241,248,273,279,297,319,405,417,420,440,523,523,
          583,694,1101,1116,1146,1226,1349,1412,1417)
censura = c(rep(1,5),0,rep(1,20),0,rep(1,5),0,1,0,rep(1,5),0,1,1,1,0,1,0,0,0,1)

dados = data.frame(tempo,censura) %>%
  arrange(tempo)


#modelo de sobrevivencia Kaplan-meier

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)

summary(KM)

#funcao de risco Kaplan-Meier
HHt = -log(KM$surv)

plot(stepfun(KM$time[1:(length(KM$time) - 1)],c(0,HHt[1:(length(HHt) - 1)])),do.points = F)


#Nelson-Aalen

ENA = survfit(coxph(Surv(tempo,censura)~1, method = "breslow"))
summary(ENA)
plot(ENA, conf.int = F,)


#funcao de risco Nelson-Aalen
HHt = -log(ENA$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#tempo mediano

KM


################################################################################
#Questoa 2 


pacman::p_load(tidyverse,survival)

tempo = c(28,89,175,195,309,377,393,421,447,462,709,744,770,1106,1206,34,88,137,
          199,280,291,299,300,309,351,358,369,369,370,375,382,392,429,451,1119)
censura = c(rep(1,5),rep(0,4),)

dados = data.frame(tempo,censura) %>%
  arrange(tempo)




























