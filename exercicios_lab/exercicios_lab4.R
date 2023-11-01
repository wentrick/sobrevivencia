pacman::p_load(tidyverse,survival,fastDummies)

tempo = c(7,rep(8,4),12,12,17,18,22,rep(30,6),8,8,9,10,10,14,15,15,18,19,21,22,22,23,25,rep(8,6),9,rep(10,3),11,17,19)
censura = c(rep(1,4),0,rep(1,2),0,rep(1,2),rep(0,6),1,0,rep(1,2),0,rep(1,23))
grupo = c(rep(1,16),rep(2,15),rep(3,13))

dados = data.frame(tempo,censura,grupo) %>%
  arrange(tempo) %>%
  mutate(grupo = as.factor(grupo))

#modelo de sobreviencia

KM = survfit(Surv(dados$tempo,dados$censura)~dados$grupo)

plot(KM,conf.int = F, mark.time = T, col = c("red","blue","green"))

summary(KM)

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)

#curva TTT

TTT(tempo)

#verossimilhanca weibull

weibull_vero = function(param){
  
  alfa =  param[1] 
  gama =  param[2]
  
  if((alfa>0) && (gama>0)) {
    density = (gama/(alfa^gama))* tempo^(gama-1)*exp(-(tempo/alfa)^gama)
    survival = exp(-(tempo/alfa)^gama)
    
    max = sum(censura * log(density) + (1-censura)*log(survival))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results = optim(c(1,1),weibull_vero,NULL,hessian = T)

results$convergence
results$par
logLW = (-1)*results$value
invRW = solve(results$hessian)
varianciaW = diag(invRW)
eppW = sqrt(varianciaW)

#verossimilhanca exponencial

exponencial_vero = function(param){
  
  alfa =  param[1] 
  gama =  1
  
  if((alfa>0)) {
    density = (gama/(alfa^gama))* tempo^(gama-1)*exp(-(tempo/alfa)^gama)
    survival = exp(-(censura/alfa)^gama)
    
    max = sum(censura * log(density) + (1-censura)*log(survival))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results_expo = optim(c(1),exponencial_vero,NULL,hessian = T,method = "Brent",lower = 0.0001,upper = 50)

results_expo$convergence
results_expo$par
logLE = (-1)*results_expo$value
invRE = solve(results_expo$hessian)
varianciaE = diag(invRE)
eppE = sqrt(varianciaE)


#teste da razao de verossimilhança

TRV = 2*(logLW-logLE)
1-pchisq(TRV,1)

# Metodo grafico para selecao de distribuicao

St_exponencial = exp(-(KM$time/results_expo$par))

St_weibull = exp(-(KM$time/results$par[1])^results$par[2])

plot(KM,conf.int = F, mark.time = T)
lines(c(0,KM$time),c(1, St_exponencial),col="red")
lines(c(0,KM$time),c(1,St_weibull),col = "blue")
legend(15,1, legend = c("KM","Exponencial", "Weibull"), lty = c(1, 1), col = c("black","red", "blue"))

#usando fuancao ja pronta (nem todas as funcoes de sobrevivencia vao ter aplicadas no R logo é normal ter que fazer manulamente como antes)

mwe = survreg(Surv(tempo,censura)~1,dist = "weibull")

summary(mwe)

mwe = survreg(Surv(log(tempo),censura)~1,dist = "extreme")
summary(mwe)

mex = survreg(Surv(tempo,censura)~1,dist = "exponential")
summary(mex)

#fazedno pela regressao

dados_reg = dados %>%
  dummy_cols(select_columns = "grupo", remove_selected_columns = TRUE, remove_first_dummy  = TRUE) %>%
  mutate(intecepto = 1) %>%
  select(c(5,3,4))
  

#verossimilhanca weibull

weibull_vero = function(param){
  
  beta_0 = param[1]
  beta_1 = param[2]
  beta_2 = param[3]
  gama =  param[4]
  alfa =  exp(as.matrix(dados_reg) %*% c(beta_0,beta_1,beta_2)) 
  
  
  if((gama>0)) {
    density = (gama/(alfa^gama))* tempo^(gama-1)*exp(-(tempo/alfa)^gama)
    survival = exp(-(tempo/alfa)^gama)
    
    max = sum(censura * log(density) + (1-censura)*log(survival))
    return(-1*max)
  }
  
  else {
    return (NA)}
  
}

results = optim(c(1,1,1,1),weibull_vero,NULL,hessian = T)

estimativasregw = results$par
hessianregw = results$hessian
inveregw = solve(hessianregw)
varianciaregw = diag(inveregw)
eppregw = sqrt(varianciaregw)
Zregw = estimativasregw[1:3]/eppregw[1:3]
pvalor = 2*(1-pnorm(abs(Zregw)))
pvalor


cbind(estimativasregw[1:3],eppregw[1:3],Zregw,pvalor)

















