
#HF:exp(lambda) elo. paraméterre becslések: 1/átlag
## és 1/[n*min(X1,...Xn)]
#megvizsgálni, melyik a jobb
##tobb mintaelemszamra, ismetlesre

#hibafuggveny
err <- function(lambda, estimations) {
  return (min(c(abs(lambda-mean(estimations)),min(abs(lambda-quantile(estimations))))))
}

la=3

ns=10*c(1:30) 
dim = length(ns)

meansMean = rep(0,times=dim)
meansMin = rep(0,times=dim)
medianMean = rep(0,times=dim)
medianMin = rep(0,times=dim)
errsMean = rep(0,times=dim)
errsMin = rep(0,times=dim)

#for (ism in ns) {
  for (n in ns) {
    ism = n
    
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
    print(paste("Mintaelemszam:",toString(n),"Ismetlesszam:",toString(ism)))
    i = n/10
    meansMean[i] = mean(becs1)
    medianMean[i] = median(becs1)
    meansMin[i] = mean(becs2)
    medianMin[i] = median(becs2)
    errsMean[i] = err(la,becs1)
    errsMin[i] = err(la,becs2)
    if (err(la,becs1)<err(la,becs2)) {
      print(" 1/atlag a jobb")
    }
    else {
      print(" 1/n*min a jobb")
    }
  }
#}

par(mfrow=c(1,1))
plot(ns/10,rep(la,times=dim),type="l",ylim=c(0,max(medianMin)),ylab = "")
points(meansMean)
#points(medianMean)
#points(meansMin)
points(medianMin)
lines(errsMean)
lines(errsMin)

