setwd("D:\\Karosszék\\Google Drive\\elte\\2017_2018_2\\stat\\matstat")

##########
#2. gyakorlat
##########
#2 fajta becslés a papíros példasor 3. feladatára
n=100
la=3
par=exp(-la)
ism=100
becs1=rep(0,times=ism)
becs2=becs1
for (i in 1:ism)
{
  x=rpois(n,la)
  becs1[i]=exp(-mean(x))
  becs2[i]=sum(x==0)/n
}
summary(becs1)
summary(becs2)
####
#az 1. a jobb, az x átlagból
####

x=c(21,35,45,61)
xx=rep(0,times=9)
for(i in 1:9)
  xx[i]=quantile(x,0.33,type=i)

#a saját adataink

diak=read.table("diak_18a.csv",sep=";",header=T,dec=",")

summary(diak)
#alapstat


par(mfrow=c(2,2))

u1=hist(diak[,7],xlab="jegy",ylab="gyakoriság",main="Valszám jegy",breaks=c(1:6)-1/2)
hist(diak[,5],xlab="óra",ylab="gyakoriság",main="Tan.id\u151",breaks=2*c(0:15)-1/2)
hist(diak[,1],xlab="cm",ylab="gyakoriság",main="Testmagasság",breaks=5*c(0:10)+152.5)
hist(diak[,3],xlab="",ylab="gyakoriság",main="Cip\u151méret",breaks=c(0:12)+73/2)


par(mfrow=c(2,1))
hist(diak[,2],xlab="kg",ylab="gyakoriság",main="Testsúly",breaks=5*c(10:19)+2.5)
hist(diak[,6],xlab="perc",ylab="gyakoriság",main="Utazási idő",breaks=15*c(1:14)+2.5)

par(mfrow=c(2,2))

boxplot(diak[,2]~diak[,4],ylab="cm",xlab="nem",main="Testsúly")
boxplot(diak[,1]~diak[,4],ylab="kg",xlab="nem",main="Testmagasság")
boxplot(diak[,5]~diak[,4],ylab="h",xlab="nem",main="Tan.id\u151")
boxplot(diak[,7]~diak[,4],ylab="jegy",xlab="nem",main="Jegy")

####
#kvantilisek
####

sort(diak[diak[,4]=="n",2])

quantile(diak[diak[,4]=="n",2],type=1)

quantile(diak[diak[,4]=="n",2],type=2)

quantile(diak[diak[,4]=="n",2],type=3)

quantile(diak[diak[,4]=="n",2],type=4)

quantile(diak[diak[,4]=="n",2],type=5)

quantile(diak[diak[,4]=="n",2],type=6)

quantile(diak[diak[,4]=="n",2],type=7) #ez a default

quantile(diak[diak[,4]=="n",2],type=8)

quantile(diak[diak[,4]=="n",2],type=9)



#tapasztalati elo.fv
par(mfrow=c(1,1))
x <- rnorm(12)
Fn <- ecdf(x)
plot(Fn,xlim=c(-3.2,3.2))
x=c(1:200)
x=(x-100)/6
lines(x,pnorm(x),col=2)

x <- rnorm(120)
Fn <- ecdf(x)
#lines(Fn,col=3,pch=0.5)

lines(Fn, verticals=F, col.points='blue',
      col.hor='red', col.vert='bisque',cex=0.3)

x <- rnorm(480)
Fn <- ecdf(x)
#lines(Fn,col=3,pch=0.5)

lines(Fn, verticals=F, col.points='green',
      col.hor='red', col.vert='bisque',cex=0.1)

#HF:exp(lambda) elo. paraméterre becslések: 1/átlag
## és 1/[n*min(X1,...Xn)]
#megvizsgálni, melyik a jobb
##tobb mintaelemszamra, ismetlesre
