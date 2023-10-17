pacman::p_load(tidyverse,survival,ggfortify)

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

plot(stepfun(KM$time,c(0,HHt)),do.points = F)
#Nelson-Aalen

ENA = survfit(coxph(Surv(tempo,censura)~1, method = "breslow"))
summary(ENA)
plot(ENA, conf.int = F,)


#funcao de risco Nelson-Aalen
HHt = -log(ENA$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#tempo mediano

KM


<<<<<<< HEAD
################################################################################
#Questoa 2 


pacman::p_load(tidyverse,survival)

tempo = c(28,89,175,195,309,377,393,421,447,462,709,744,770,1106,1206,34,88,137,
          199,280,291,299,300,309,351,358,369,369,370,375,382,392,429,451,1119)
censura = c(rep(1,5),rep(0,4),)

dados = data.frame(tempo,censura) %>%
  arrange(tempo)
=======
###############################################################################
# exercicio 2 

tempo = c(28,89,175,195,309,377,393,421,447,462,709,744,770,1106,1206,34,88,137,
          199,280,291,299,300,309,309,351,358,369,370,375,382,392,429,451,1119)
censura = c(rep(1,5),rep(0,4),1,rep(0,5),rep(1,6),0,0,rep(1,9),0,1,0)
grupo = c(rep(1,15),rep(2,20))

dados = data.frame(tempo,censura,grupo) %>%
  arrange(tempo)%>%
  mutate(grupo = as.factor(grupo))

#modelo de sobrevivencia Kaplan-meier

KM = survfit(Surv(dados$tempo,dados$censura)~1)

autoplot(KM,conf.int = F, mark.time = T)

summary(KM)

#funcao de risco Kaplan-Meier
HHt = -log(KM$surv)

autoplot(stepfun(KM$time,c(0,HHt)),do.points = F)


#Nelson-Aalen

ENA = survfit(coxph(Surv(tempo,censura)~1, method = "breslow"))
summary(ENA)
autoplot(ENA, conf.int = F,)


#funcao de risco Nelson-Aalen
HHt = -log(ENA$surv)

autoplot(stepfun(KM$time,c(0,HHt)),do.points = F)

#analise por grupo ----- 

#kaplan-meier

KM = survfit(Surv(dados$tempo,dados$censura)~dados$grupo)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue"))

#Nelson ALlen

ENA = survfit(coxph(Surv(dados$tempo,dados$censura)~dados$grupo, method = "breslow"))
summary(ENA)

plot(ENA,conf.int = F, mark.time = T, col = c("red","blue"))

autoplot(ENA)

#tempo mediano

KM





###############################################################################
# Exercicio 4

tempo = c(140,177,50,65,86,153,181,191,77,84,87,56,66,73,119,140,rep(200,14),124,
          58,56,68,79,89,107,86,142,110,96,142,86,75,117,98,105,126,43,46,81,133,
          165,170,rep(200,5),112,68,84,109,153,143,60,70,98,164,63,63,77,91,91,66,
          70,77,63,66,66,66,94,101,105,108,112,115,126,161,178)
censura = c(rep(1,15),rep(0,15),rep(1,23),rep(0,7),rep(1,30))
grupo = c(rep(1,30),rep(2,30),rep(3,30))

dados = data.frame(tempo,censura,grupo) %>%
  arrange(tempo)%>%
  mutate(grupo = as.factor(grupo))


####### 
# a)

KM = survfit(Surv(tempo,censura)~1,data = dados)

autoplot(KM,conf.int = F, mark.time = T)

summary(KM)

#funcao de risco Kaplan-Meier
HHt = -log(KM$surv)

autoplot(stepfun(KM$time,c(0,HHt)),do.points = F)

#######
# b)

KM = survfit(Surv(tempo,censura)~grupo,data = dados)

autoplot(KM,conf.int = F, mark.time = T)

summary(KM)

#######
# c)

survdiff(Surv(tempo, censura) ~ grupo, data=dados, rho = 0)

survdiff(Surv(tempo, censura) ~ grupo, data=dados, rho = 1)


#######
# d)


>>>>>>> 897fb13bbee408fac585fd08ba66f3d6d9ae18ab




























