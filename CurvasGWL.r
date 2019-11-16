library(GoFKernel)
library(dplyr)
library(tidyverse)
library(LindleyR)
library(GoFKernel)

# valores do parametros
pAlpha = 3
pLambda = 0.7
pPhi = 0.5
pPi = 0.3

t = 1:300/10
Alpha = pAlpha
Lambda = pLambda
Phi = pPhi
Pi = pPi



# funcao incompleta upper/lower gamma -----------------------------------
ugamma<-function(a,b){
  (pgamma(a,b, lower=FALSE)*gamma(b))
}

lgamma<-function(a,b){
  (pgamma(a,b, lower=TRUE)*gamma(b))
}
  

# Densidade Distribution Function (DF) -------------------------------------------------------
func_f <- function(t, Alpha, Lambda, Phi, Pi){
  (1-Pi)*Alpha*Lambda^(Alpha*Phi)*t^(Alpha*Phi-1)*(Lambda+(Lambda*t)^Alpha)*exp(-(Lambda*t)^Alpha)/((Lambda+Phi)*gamma(Phi))
}

#lines(t, func_f(t, Alpha, Lambda, Phi, Pi), col="blue", lwd=2)

plot(t, func_f(t, Alpha, Lambda, Phi, Pi), type="l", col="red", lwd=2, xlab="time (t)", ylab="f(t)", main="DF")

# Cumulative Distribution Function (CDF) -------------------------------------------------------
func_F <- function(t, Alpha, Lambda, Phi, Pi){
  aux <- (1-Pi)*(lgamma((Lambda*t)^Alpha,Phi)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/((Lambda+Phi)*gamma(Phi))
  return(aux)
}

plot(t, func_F(t, Alpha, Lambda, Phi, Pi), type="l", col="red", lwd=2, xlab="time (t)", ylab="F(t)", main="CDF")

# Survival function (1 - CDF) -------------------------------------------------------
func_C <- function(t, Alpha, Lambda, Phi){
  1 - (pgamma((Lambda*t)^Alpha,Phi, lower=TRUE)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/ ((Lambda+Phi)*gamma(Phi))
}

plot(t,func_C(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="1-F(t)", main="1 - CDF")

# Survival function (S) -------------------------------------------------------
func_S <- function(t, Alpha, Lambda, Phi){
  ( Pi*(Alpha+Phi)*gamma(Phi) + (1 - Pi)*(pgamma((Lambda*t)^Alpha,Phi, lower=FALSE)*(Lambda+Phi) + (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha)) ) / ((Lambda+Phi)*gamma(Phi))
}

plot(t,func_S(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="S(t)", main="Survival"
     
     





