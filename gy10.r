
#####
#10.gyak.
#####
#Wilcoxon proba
###############

x=rnorm(50)
y=rnorm(50,0.5)
t.test(x,y)

wilcox.test(x, y)

##de: valtoztassuk meg az adatokat
y[50]=40

t.test(x,y)

wilcox.test(x, y)

####
#chi-négyzet próba
####
diak<-read.table("D:\\OKTATAS\\2016\\inf_a\\dat16.csv",sep=";",header=T)

#nem
#H0: egyenletes elo.
chisq.test(table(diak[,3]))
#tovabbi komponensek
chisq.test(table(diak[,3]))$observed
chisq.test(table(diak[,3]))$expected
chisq.test(table(diak[,3]))$residuals

#szimulalt krit ertekkel
chisq.test(table(diak[,3]),simulate.p.value=T)

#normalitasvizsgalat
#itt nincs tablazat
u1=hist(diak[,5],xlab="jegy",ylab="gyakoriság",main="Valszám",breaks=c(1:6)-1/2)
hist(diak[,1],xlab="kg",ylab="gyakoriság",main="Súly",breaks=10*c(1:10)+25)
hist(diak[,2],xlab="cm",ylab="gyakoriság",main="Testmagasság",breaks=5*c(1:12)+142.5)
hist(diak[,4],xlab="",ylab="gyakoriság",main="Cipőméret",breaks=c(1:14)+67/2)

hist(diak[,6],xlab="",ylab="gyakoriság",main="Tan, idő",breaks=2*c(0:11))
hist(diak[,7],xlab="perc",ylab="gyakoriság",main="Utazási ido",breaks=30*c(1:9)-30)


n<-4;j<-1 #testsuly
#intervallum: 4 db ill. a valtozo sorszama
q<-c(1:n)/n
h<-c(-Inf,qnorm(q))
mu<-mean(diak[,j])
sig=sd(diak[,j])
h<-h*sig+mu
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p

####
#nezzuk 5 osztalyra

n<-5;j<-1 #testmagassag
#intervallum: 4 db ill. a valtozo sorszama
q<-c(1:n)/n
h<-c(-Inf,qnorm(q))
mu<-mean(diak[,j])
sig=sd(diak[,j])
h<-h*sig+mu
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p
##############
#HF szimulált értékekkel is ellenőrizni a p-értéket
##############

n<-4;j<-2 #magasság
#intervallum: 4 db ill. a valtozo sorszama
q<-c(1:n)/n
h<-c(-Inf,qnorm(q))
mu<-mean(diak[,j])
sig=sd(diak[,j])
h<-h*sig+mu
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p

###

n<-4;j<-6 #tan.idő
#diak[1,j]=50
#intervallum: 4 db ill. a valtozo sorszama
q<-c(1:n)/n
h<-c(-Inf,qnorm(q))
mu<-mean(diak[,j])
sig=sd(diak[,j])
h<-h*sig+mu
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p

#illesszünk rá más eloszlást
#exp

n<-4;j<-6 #tan.idő
#diak[1,j]=50
#intervallum: 4 db ill. a valtozo sorszama
q<-c(1:n)/n
h<-c(-Inf,qexp(q))
mu<-mean(diak[,j])
#sig=sd(diak[,j])
h<-h*mu
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p #még nem az igazi

#gamma

n<-4;j<-6 #tan.idő
#diak[1,j]=50
#intervallum: 4 db ill. a valtozo sorszama
q<-c(0:n)/n

mu<-mean(diak[,j])
v=var(diak[,j])

h<-c(-Inf,qexp(q))
#sig=sd(diak[,j])
h<-qgamma(q,mu^2/v,mu/v)
#ezek hataroljak 
nu<-rep(0,times=n)
for (i in 1:n)
  nu[i]<-
  sum((diak[,j]<h[i+1]) & (diak[,j]>=h[i]))


#vart gyak:
np<-sum(nu)*rep(1/n,times=n)
chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,n-3)
p #még nem az igazi

########
#HF probálkozzunk még más eloszlással
########

nu=c(80,113,77,27,3)
np=dbinom(c(0:4),4,0.25)*sum(nu)

chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,4)
p

############
#p becsles
############

phat=sum(nu*c(0:4))/(sum(nu)*4)

nu=c(80,113,77,27,3)
np=dbinom(c(0:4),4,phat)*sum(nu)

chi<-sum((nu-np)^2/np)


#a p-ertek:
p<-1-pchisq(chi,3)
p


#####
#ftlensegvizsg
#####

table(diak[,5],diak[,3])
chisq.test(table(diak[,5],diak[,3]))
chisq.test(table(diak[,5],
                 diak[,3]),simulate.p.value=T)

#osszevonunk osztalyokat
tan_2=diak[,6]<2.5
tan_2=diak[,6]>=2.5
table(tan_2,diak[,3])
chisq.test(table(tan_2,diak[,3]))
chisq.test(table(tan_2,diak[,3]),simulate.p.value=T)

table(diak[,6],diak[,3])

####
#suly es testmag
suly_2=diak[,2]>median(diak[,2])
mag_2=diak[,1]>median(diak[,1])
table(suly_2,mag_2)
chisq.test(table(suly_2,mag_2))
chisq.test(table(suly_2,mag_2),simulate.p.value=T)

#cipo es testmag
suly_2=diak[,4]>median(diak[,4])
mag_2=diak[,1]>median(diak[,1])
table(suly_2,mag_2)
chisq.test(table(suly_2,mag_2))
chisq.test(table(suly_2,mag_2),simulate.p.value=T)

x=c(3692,232,65,7,3,1,0)
m=c(0.25,1,2,3,4,5,6)
sum(x*m)/sum(x)


y=c(3542,284,135,24,9,5,1)
chi=sum((x-y)^2/(x+y))

p<-1-pchisq(chi,6)