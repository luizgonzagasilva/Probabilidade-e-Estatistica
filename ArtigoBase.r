
# funcao probabilidade INK ----------------------------------------------------
Mi = 0.5
Omega = 10
t = (1:6000)/1000
f = (2/gamma(Mi))*((Mi/Omega)^Mi)*t^(-2*Mi-1)*exp(-Mi/(Omega*t^2))

plot(t,f)


# funcao probabilidade GWL ------------------------------------------------

# unimodal
alpha = 2
phi = 3
lambda = 5
t = (1:8000)/1000
f = ( (alpha*lambda^(alpha*phi))/((lambda+phi)*gamma(phi)) )*t^(alpha*(phi-1))*(lambda+(lambda*t)^alpha)*exp(-(lambda*t)^alpha)

plot(t,f)

# unimodal?
alpha = 0.26
phi = 3
lambda = 5
t = (1:8000)/1000
f = ( (alpha*lambda^(alpha*phi))/((lambda+phi)*gamma(phi)) )*t^(alpha*(phi-1))*(lambda+(lambda*t)^alpha)*exp(-(lambda*t)^alpha)

plot(t,f)




# funcao sobrevivencia ----------------------------------------------------
mi = 0.5
Pi = 0.3
omega = 2
t = (1:6000)/1000


S1 = (Pi*gamma(mi) + (1-Pi)*pgamma(mi/(omega*t^2), mi, lower=TRUE)*gamma(mi))/gamma(mi)

plot(t,S1, type="l", col="green", lwd=2, xlab="time (t)", ylab="S(t)", main="Exponential decay")


mi = 4
Pi = 0.3
omega = 2
t = (1:6000)/1000

S2 = (Pi*gamma(mi) + (1-Pi)*pgamma(mi/(omega*t^2), mi, lower=TRUE)*gamma(mi))/gamma(mi)
lines(t,S2, col="red")

legend("topleft",
       c("sin(x)","cos(x)"),
       fill=c("blue","red")
)