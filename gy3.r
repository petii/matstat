####
#3.gyak
#HF1: papíros/2
##################
#Parzen-Rosenblatt
##################

#kicsi mintára, hogy lássuk

x=c(1,4,6)
plot(density(x,bw=1/3,kernel="r")) #egyenletes elo. a magfv.

x=c(1,4,6,8,9)
plot(density(x,bw=1/3,kernel="r"))
#bw a magfv szórása
#

x=rnorm(100)
plot(density(x))
plot(density(x,kernel="r"))

#####
#becslések vizsgálata
#más eloszlásokra (pl. Pareto)
#boxplot abrazolással összehasonlitani

n=c(10,40,160,640)
est=matrix(0,1000,4)
alpha=4
xs<-list()
for(j in 1:1000) {
  for (i in 1:length(n))  {
    x=runif(n[i])^{-1/alpha} 
    #HF2:milyen eloszlású az x?
    est[j,i]=var(x)
  }
}
x=runif(1000)^{-1/alpha} 
#x=x[x<20]
hist(x,freq = FALSE,nclass = 30)
lines(density(x))
xx=c(0:(max(x)*100))/100
lines(dexp(xx,))

v<-list()
v$"10"<-est[,1]
v$"40"<-est[,2]
v$"160"<-est[,3]
v$"640"<-est[,4]

boxplot(v,main="Becslések")

n=c(10,40,160,640)
est=matrix(0,1000,4)
alpha=2
#ennek már nem véges a szórása
for(j in 1:1000)
{
  for (i in 1:length(n))
  {x=runif(n[i])^{-1/alpha}
  est[j,i]=var(x)}
}

v<-list()
v$"10"<-est[,1]
v$"40"<-est[,2]
v$"160"<-est[,3]
v$"640"<-est[,4]


boxplot(v,main="Becslések")
#itt nincs javulás

#HF3: más eloszlásokra is megnézni a szórásbecslés változását az elemszám
#függvényében

