# f1 = Alpha*sum(delta*log(t)) + d*Alpha*log(Lambda) - n*gamma(Phi) - n/(Lambda+Phi)
f1 = expression(log(x))
df1 = D(f1,"x")
x=1.8
eval(df1)

dnum <- function(a, h){
  (log(a+h)-log(a))/h
}