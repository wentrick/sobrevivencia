pacman::p_load(tidyverse,survival,AdequacyModel)

tempo = c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
censura = c(rep(1,7),0,1,1,0,rep(1,8),0)

dados = data.frame(tempo,censura) %>%
  arrange(tempo) 

#modelo de sobrevivencia

KM = survfit(Surv(dados$tempo,dados$censura)~1)

plot(KM,conf.int = F, mark.time = T)


#funcao de risco
HHt = -log(KM$surv)

plot(stepfun(KM$time,c(0,HHt)),do.points = F)


#curva TTT

TTT(tempo)

#verossimilhanca weibull

weibull_vero = function(param){
  
  alfa =  param[1] 
  gama =  param[2]
  
  if((alfa>0) && (gama>0)) {
    density = (gama/(alfa^gama))* tempo^(gama-1)*exp(-(tempo/alfa)^gama)
    survival = exp(-(censura/alfa)^gama)
    
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
    survival = exp(-(tempo/alfa)^gama)
    
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

#usando fuancao ja pronta (nem todas as funcoes de sobrevivencia vao ter aplicadas no R logo é normal ter que fazer manulamente como antes)

mwe = survreg(Surv(tempo,censura)~1,dist = "weibull")
summary(mwe)

mwe = survreg(Surv(log(tempo),censura)~1,dist = "extreme")
summary(mwe)

mex = survreg(Surv(tempo,censura)~1,dist = "exponential")
summary(mex)

# Metodo grafico para selecao de distribuicao

St_exponencial = exp(-(KM$time/results_expo$par))

St_weibull = exp(-(KM$time/results$par[1])^results$par[2])

plot(KM,conf.int = F, mark.time = T)
lines(c(0,KM$time),c(1, St_exponencial),col="red")
lines(c(0,KM$time),c(1,St_weibull),col = "blue")
legend(25,1, legend = c("KM","Exponencial", "Weibull"), lty = c(1, 1), col = c("black","red", "blue"))













