# !diagnostics off
library(dplyr)
library(tidyverse)
library(LindleyR)
library(GoFKernel)

# valores do parametros
t = 1:100/10
Alpha = 0.7771233
Lambda = 1.0553040
Phi = 1.1809489
Pi = 0.5176842

# Densidade Distribution Function (DF) -------------------------------------------------------
func_f <- function(t, Alpha, Lambda, Phi){
  (1-Pi)*Alpha*Lambda^(Alpha*Phi)*t^(Alpha*Phi-1)*(Lambda+(Lambda*t)^Alpha)*exp(-(Lambda*t)^Alpha)/(Lambda+Phi)*gamma(Phi)
}

plot(t, func_f(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="f(t)", main="DF")

# Cumulative Distribution Function (CDF) -------------------------------------------------------
func_F <- function(t, Alpha, Lambda, Phi){
  ( pgamma((Lambda*t)^Alpha,Phi,lower=TRUE)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/((Lambda+Phi)*gamma(Phi))
}

plot(t, func_F(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="F(t)", main="CDF")

# Survival function (1 - CDF) -------------------------------------------------------
func_C <- function(t, Alpha, Lambda, Phi){
  1 - (pgamma((Lambda*t)^Alpha,Phi, lower=TRUE)*(Lambda+Phi) - (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha))/ ((Lambda+Phi)*gamma(Phi))
}

plot(t,func_C(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="1-F(t)", main="1 - CDF")

# Survival function (S) -------------------------------------------------------
func_S <- function(t, Alpha, Lambda, Phi){
  ( Pi*(Alpha+Phi)*gamma(Phi) + (1 - Pi)*(pgamma((Lambda*t)^Alpha,Phi, lower=FALSE)*(Lambda+Phi) + (Lambda*t)^(Alpha*Phi)*exp(-(Lambda*t)^Alpha)) ) / ((Lambda+Phi)*gamma(Phi))
}

plot(t,func_S(t, Alpha, Lambda, Phi), type="l", col="red", lwd=2, xlab="time (t)", ylab="S(t)", main="Survival")


