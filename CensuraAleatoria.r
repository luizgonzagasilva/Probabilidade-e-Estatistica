# !diagnostics off
library(dplyr)
library(tidyverse)
library(LindleyR)
library(GoFKernel)

# censura aleatoria -------------------------------------------------------
# x <- randcensor(n = 100, pcens = 0.14, timedistr = 'lnorm', censordistr = 'unif', meanlog = 1, sdlog = 1)

t = 1:100/10
Alpha = 1.2
Lambda = 1.5
Phi = 2
Pi = 0

# Densidade Distribution Function (DF) -------------------------------------------------------
func_f <- function(t, Alpha, Lambda, Phi){
  (1-Pi)*Alpha*Lambda^(Alpha*Phi)*t^(Alpha*Phi-1)*(Lambda+(Lambda*t)^Alpha)*exp(-(Lambda*t)^Alpha)/(Lambda+Phi)*gamma(Phi)
}

plot(t, func_f(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="f(t)", main="DF")

# Cumulative Distribution Function (CDF) -------------------------------------------------------
func_F <- function(t){
  Alpha = 1.2
  Lambda = 1.5
  Phi = 2
  Pi = 0
  ( pgamma((Lambda*t)^Alpha,Phi,lower=TRUE)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/((Lambda+Phi)*gamma(Phi))
}

plot(t, func_F(t), type="l", col="red", lwd=2, xlab="time (t)", ylab="F(t)", main="CDF")

# Survival function (1 - CDF) -------------------------------------------------------
func_C <- function(t, Alpha, Lambda, Phi){
  1 - (pgamma((Lambda*t)^Alpha,Phi, lower=TRUE)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/(Lambda+Phi)*gamma(Phi)
}

plot(t,func_C(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="1-F(t)", main="1 - CDF")


# gera numeros aleatoriamente uniforme entre 0 a 1 --------------
n = 1000
r <- runif(n, 0, 1)
hist(r)

min(func_F(r))
max(func_F(r))


F.inv = inverse(func_F, lower = min(func_F(r)), upper = max(func_F(r)))


# simulação tempo de falha
t_falha= {}
t_censura= {}
index = 0
for (i in r) {
  print(c(index, i))
  t_falha = c(t_falha, F.inv(i))
  #t_censura = c(t_censura, C.inv(i))
  index = index + 1
}

hist(t_falha)
hist(t_censura)



