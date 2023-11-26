pacman::p_load(readr,tidyverse,survival,AdequacyModel,rms)

#### 1. Ler o banco de dados `adesao` que está disponível no Sigaa.


dados <- read_csv("dados/Breast Cancer METABRIC.csv") %>%
  mutate(censura = `Overall Survival Status`,
         tempo = `Overall Survival (Months)`,
         censura = recode(censura, 'Living' = 0, "Deceased" = 1)) %>% 
  select(-c(`Overall Survival Status`,`Overall Survival (Months)`))




head(dados)



#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)


# Resposta x breast surgery

KM = survfit(Surv(dados$tempo,dados$censura)~dados$`Type of Breast Surgery`)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ dados$`Type of Breast Surgery`, data=dados, rho = 1)


# Resposta x Celullarity

KM = survfit(Surv(dados$tempo,dados$censura)~dados$Cellularity)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ dados$Cellularity, data=dados, rho = 1)



# Resposta x chemotherapy

KM = survfit(Surv(dados$tempo,dados$censura)~dados$Chemotherapy)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

#teste para diferenca de curvas
survdiff(Surv(tempo, censura) ~ dados$Chemotherapy, data=dados, rho = 1)



