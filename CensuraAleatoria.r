library(dplyr)
library(tidyverse)
library(LindleyR)
library(GoFKernel)

# censura aleatoria -------------------------------------------------------
# x <- randcensor(n = 100, pcens = 0.14, timedistr = 'lnorm', censordistr = 'unif', meanlog = 1, sdlog = 1)

t = 1:100/10
pAlpha = 1.2
pLambda = 0.8
pPhi = 2
pPi = 0.4

# Densidade Distribution Function (DF) -------------------------------------------------------
func_f <- function(t){
  Alpha = pAlpha
  Lambda = pLambda
  Phi = pPhi
  Pi = pPi
  (1-Pi)*Alpha*Lambda^(Alpha*Phi)*t^(Alpha*Phi-1)*(Lambda+(Lambda*t)^Alpha)*exp(-(Lambda*t)^Alpha)/((Lambda+Phi)*gamma(Phi))
}

plot(t, func_f(t), type="l", col="red", lwd=2, xlab="time (t)", ylab="f(t)", main="DF")

min(func_f(t))
max(func_f(t))

# Cumulative Distribution Function (CDF) -------------------------------------------------------
func_F <- function(t){
  Alpha = pAlpha
  Lambda = pLambda
  Phi = pPhi
  Pi = pPi
  (1-Pi)*(pgamma((Lambda*t)^Alpha,Phi,lower=TRUE)*gamma(Phi)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/((Lambda+Phi)*gamma(Phi))
}

plot(t, func_F(t), type="l", col="red", lwd=2, xlab="time (t)", ylab="F(t)", main="CDF")

min(func_F(t))
max(func_F(t))

# gera numeros aleatoriamente uniforme entre 0 e 1 --------------
n = 1000
r <- runif(n, 0, 0.6)
hist(r)

F.inv = inverse(func_F, lower = 0, upper = 30)

# simulação tempo de falha
t_falha= {}
index = 1
for (i in r) {
  print(c(index, i))
  t_falha = c(t_falha, F.inv(i))
  index = index + 1
}

hist(t_falha, breaks=20, freq=FALSE)
min(t_falha)
max(t_falha)

