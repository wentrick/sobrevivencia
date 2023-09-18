pacman::p_load(tidyverse,survival)

tempo = c(6,4,12,3,1,3,1,2,3,9,10,11,5,5,1)
censura = c(1,1,0,0,1,1,1,1,1,0,1,1,0,1,1)

dados = data.frame(tempo,censura) %>%
  arrange(tempo)


#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)

summary(KM)


length(tempo)
