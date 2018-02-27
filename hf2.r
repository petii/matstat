#HF:exp(lambda) elo. paraméterre becslések: 1/átlag
## és 1/[n*min(X1,...Xn)]
#megvizsgálni, melyik a jobb
##tobb mintaelemszamra, ismetlesre

la=3
n=100
ism=100
becs1=rep(0,times=ism)
becs2=becs1
for (i in 1:ism)
{
  #Mintat veszunk a exp(lambda)-bol
  x=rexp(n,la)
  #1/atlag
  becs1[i]=1/mean(x)
  #1/(n*min)
  becs2[i]=1/( n*min(x))
}
summary(becs1)
summary(becs2)
