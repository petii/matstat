
################
#11.óra
################


##############
#linearis regresszio
##############

homb<-read.table("d:\\oktatas\\2016\\inf_a\\nyir-51-88jav.hom")
homk<-read.table("d:\\oktatas\\2016\\inf_a\\karc-51-88.hom")

r=c(0:37)

#vajon egyenletesen no-e az atlaghom. marciusban?
atl<-rep(0,times=31)
for (i in 1:31) atl[i]=mean(homb[365*r+59+i,1])
t<-c(1:31)
lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)
#egesz jo

#vajon egyenletesen no-e az atlaghom. aprilisban?

t<-c(1:30)
atl=t
for (i in 1:30) atl[i]=mean(homb[365*r+90+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)
#sokkal valtozekonyabb

#majus
t<-c(1:31)
atl=t
for (i in 1:31) atl[i]=mean(homb[365*r+120+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)

#junius
t<-c(1:30)
atl=t
for (i in 1:30) atl[i]=mean(homb[365*r+151+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)

#no es az egesz egyben (julius is):
t<-c(1:153)
atl=t
for (i in 1:153) atl[i]=mean(homb[365*r+59+i,1])

lm1=lm(atl~t)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)

#jobb lenne kvadratikus

t<-c(1:153)
t2=t^2
atl=t
for (i in 1:153) atl[i]=mean(homb[365*r+59+i,1])

lm1=lm(atl~t+t2)
summary(lm1)

plot(t,atl)
lines(lm1$fitted.values)
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

homb<-read.table("d:\\oktatas\\2016\\inf_a\\nyir-51-88jav.hom")
homk<-read.table("d:\\oktatas\\2016\\inf_a\\karc-51-88.hom")

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
#elobb csinaljunk szamot a nembol

diak<-read.table("D:\\OKTATAS\\2018\\inf_a\\diak_18a.csv",sep=";",header=T,dec=",")

#probaljuk a testmagassagot elore jelezni:
nem=rep(0,times=dim(diak)[1])
nem[diak[,4]=="f"]=1

lm1=lm(diak[,1]~diak[,2]+nem+diak[,3]+diak[,5]+diak[,6]+diak[,7])
summary(lm1)
#hagyjuk el a legrosszabbat

lm1=lm(diak[,1]~diak[,2]+nem+diak[,3]+diak[,6]+diak[,7])
summary(lm1)

lm1=lm(diak[,1]~diak[,2]+nem+diak[,3]+diak[,7])
summary(lm1)

lm1=lm(diak[,1]~diak[,2]+nem+diak[,7])
summary(lm1)

lm1=lm(diak[,1]~diak[,2]+nem)
summary(lm1)


plot(diak[,2],diak[,1])
#a kiugró érték vajon valódi-e?
# hogyan ábrázoljuk? 
#lines(diak[,2],lm1$fitted.values)
plot(lm1$fitted.values,lm1$resid)
hist(lm1$resid)



pairs(diak)
