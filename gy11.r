
################
#11.óra
################


##############
#linearis regresszio
##############
#X,Y , minta: (x_i,y_i)
#Y = aX+b
#az "lm" megadja a-t és b-t

homb<-read.table("nyir-51-88jav.hom")
homk<-read.table("karc-51-88.hom")

r=c(0:37)
#vajon egyenletesen no-e az atlaghom. marciusban?
atl<-rep(0,times=31)
for (i in 1:31) atl[i]=mean(homb[365*r+59+i,1])
atl
t<-c(1:31)
lm1=lm(atl~t)
summary(lm1)
#intercept: b egyutthato
#t: a egyutthato
#F-statistics: p-value: mennyivel lesz ez jobb, mintha csak az átlagot becsülnénk

plot(t,atl,xlab = "Day",ylab="Mean temp.")
lines(lm1$fitted.values) #nem egyenes, hanem pontok osszekotve
#a pontok és az egyenes közti eltérések a reziduálisok (residuals)
#ezek eloszlásai benne vannak a modelben

#a konstans tagnak nincs olyan naagy szerepe
#kivesszük
lm2=lm(atl~-1+t)
summary(lm2)
plot(t,atl,xlab = "Day",ylab="Mean temp.")
lines(lm1$fitted.values,col=1)
lines(lm2$fitted.values,col=2)

#vajon egyenletesen no-e az atlaghom. aprilisban?

t<-c(1:30)
atl=t
for (i in 1:30) atl[i]=mean(homb[365*r+90+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl,xlab = "Day",ylab="Mean temp.")
lines(lm1$fitted.values)
#sokkal valtozekonyabb

#majus
t<-c(1:31)
atl=t
for (i in 1:31) atl[i]=mean(homb[365*r+120+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl,xlab = "Day",ylab="Mean temp.")
lines(lm1$fitted.values) 

#junius
t<-c(1:30)
atl=t
for (i in 1:30) atl[i]=mean(homb[365*r+151+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)

#az egesz egyben (julius is):
t<-c(1:153)
atl=t
for (i in 1:153) atl[i]=mean(homb[365*r+59+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)

#reziduálisoknál elég nagy eltérés a legnagyobb és legkisebb között
summary(lm1$residuals)
#plot(lm1$residuals)
plot(density(lm1$residuals))

#jobb lenne kvadratikus
#masodfoku közelítés
#Y = a_1*X^2 + a_2*X + b

t<-c(1:153)
t2=t^2
atl=t
for (i in 1:153) atl[i]=mean(homb[365*r+59+i,1])

lm1=lm(atl~t+t2)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values,col=2)
#ez az igazi

lm1=lm(atl~t2)
summary(lm1)
plot(t,atl)
lines(lm1$fitted.values)
#ez nem jo

###
#HF: minél jobb közelítést adni az egész évre
###

##########
#11.gyak
##########

#homb<-read.table("d:\\oktatas\\2016\\inf_a\\nyir-51-88jav.hom")
#homk<-read.table("d:\\oktatas\\2016\\inf_a\\karc-51-88.hom")

r=c(0:37)

t<-c(1:30)
atl=t
for (i in 1:30) atl[i]=mean(homb[365*r+90+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)
#sokkal valtozekonyabb


#de lehet nemparameteresen is simitani (Nadarajah-módszer)
lines(loess.smooth(t, atl,  degree = 1,
                   family = "gaussian", evaluation = 150),col=2)
lines(loess.smooth(t, atl, span = 1/3, degree = 1,
                   family = "gaussian", evaluation = 150),col=4)


#sajat adataink
diak<-read.table("diak_18a.csv",sep=";",header=T)
diak
#elobb csinaljunk szamot a nembol
nem=rep(0,times=dim(diak)[1])
nem[diak[,4]=="f"]=1

#probaljuk a testmagassagot elore jelezni:
lm1=lm(diak[,1]~diak[,2]+nem+diak[,3]+diak[,5]+diak[,6]+diak[,7])
summary(lm1)
#hagyjuk el a legrosszabbat
#ami nem az 5. oszlop, hanem a 3.
#lm1=lm(diak[,1]~diak[,2]+nem+diak[,3]+diak[,6]+diak[,7])
lm1=lm(diak[,1]~diak[,2]+nem+diak[,5]+diak[,6]+diak[,7])
summary(lm1)
#itt az 5. oszlop a legrosszabb (legnagyobb p érték (Pr(>|t|)))
lm1=lm(diak[,1]~diak[,2]+nem+diak[,6]+diak[,7])
summary(lm1)
#6.
lm1=lm(diak[,1]~diak[,2]+nem+diak[,7])
summary(lm1)
#a valszám jegy hogyan befolyásolja a testmagasságot?
#ábrázoljuk, de valahogy több dimenzióban kéne
plot(diak$magasság)
lines(lm1$fitted.values)
#becslés hibaja
plot(abs(lm1$fitted.values-diak$magasság),type='l')

#kivesszük a valszám jegyet
lm1=lm(diak[,1]~diak[,2]+nem)
summary(lm1)

plot(diak[,2],diak[,1])
#a kiugró érték vajon valódi-e?
# hogyan ábrázoljuk? 
#lines(diak[,2],lm1$fitted.values)
plot(lm1$fitted.values,lm1$resid)
hist(lm1$resid)
plot(abs(lm1$fitted.values-diak$magasság),type='l')



pairs(diak)
