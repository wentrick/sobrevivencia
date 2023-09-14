pacman::p_load(tidyverse,survival)

tempo = c(7,rep(8,4),12,12,17,18,22,rep(30,6),8,8,9,10,10,14,15,15,18,19,21,22,22,23,25,rep(8,6),9,rep(10,3),11,17,19)
censura = c(rep(1,4),0,rep(1,2),0,rep(1,2),rep(0,6),1,0,rep(1,2),0,rep(1,23))
grupo = c(rep(1,16),rep(2,15),rep(3,13))

dados = data.frame(tempo,censura,grupo) %>%
  arrange(tempo) %>%
  mutate(grupo = as.factor(grupo))



#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)



KM = survfit(Surv(dados$tempo,dados$censura)~dados$grupo)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))


summary(KM)

 
# intervalo de confianca 

KM = survfit(Surv(dados$tempo,dados$censura)~1,conf.type = "plain",conf.int = T)

plot(KM)

#funcao de risco
HHt = -log(KM$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#Nelson ALlen

ENA = survfit(coxph(Surv(tempo,censura)~1, method = "breslow"))
summary(ENA)
plot(ENA, conf.int = F,)
