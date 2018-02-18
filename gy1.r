setwd("D:\\Karosszék\\Google Drive\\elte\\2017_2018_2\\stat\\matstat")

diak <- read.csv(file="diak_18a.csv", header=TRUE, sep=";")
diak <- read.table(file = "diakok.txt", header = TRUE)

summary(diak)
#alapstat


par(mfrow=c(2,2)) #2x2 ábra egy oldalon

hist(diak[,1],xlab="jegy",ylab="gyakoriság",main="Matek") #hisztogram
hist(diak[,2],xlab="jegy",ylab="gyakoriság",main="Jog")
hist(diak[,5],xlab="cm",ylab="gyakoriság",main="Testmagasság")
hist(diak[,6],xlab="",ylab="gyakoriság",main="Cipőméret")
#nem jo


par(mfrow=c(2,2))

u1=hist(diak[,1],xlab="jegy",ylab="gyakoriság",main="Matek",breaks=c(1:6)-1/2) #manuálisan megadott osztópontok
hist(diak[,2],xlab="jegy",ylab="gyakoriság",main="Jog",breaks=c(1:6)-1/2,x)



hist(diak[diak[,7],5],xlab="cm",ylab="gyakoriság",main="Testmagasság",breaks=5*c(1:10)+142.5)
hist(diak[,6],xlab="",ylab="gyakoriság",main="Cipőméret",breaks=c(1:12)+69/2)


hist(diak[diak[,8]== "F",5],xlab="cm",ylab="gyakoriság",main="Testmagasság",sub="Nok", breaks=5*c(1:10)+142.5)
hist(diak[diak[,8]== "M",5],xlab="cm",ylab="gyakoriság",main="Testmagasság",sub="Ferfiak",breaks=5*c(1:10)+142.5)



#ez a jo
#probalkozzunk más változókkal, ertelmezzuk

##############################
#napi kozephomerseklet adatok

homb<-read.table("nyir-51-88m.hom.txt") #beolvasás
homk<-read.table("karc-51-88.hom.txt")  #az eredmény 1 oszlopos mátrix

r=c(0:37) #az egész számok 0 és 37 között

jan1b<-homb[365*r+1,1] 
#minden év első napja
jan1k<-homk[365*r+1,1]
summary(jan1b)

#ez a jan 1 
# mi lenne a feb 14?
feb14b<-homb[365*r+45,1]
feb14k<-homk[365*r+45,1]

par(mfrow=c(1,1))
v<-list()
v$" Nyíregyháza "<-jan1b
v$" Karcag "<-jan1k
v$" Nyíregyháza "<-jan1b
v$" Karcag "<-jan1k
boxplot(v,at=c(1:2)+0.1,boxwex=0.5,border=1, main="Január 1-i középhőmérsékletek")
#PROBALKOZZUNK  mas beallitasokkal

v<-list()
v$" jan1 "<-jan1b
v$" feb14 "<-feb14b
boxplot(v,at=c(1:2)+0.1,boxwex=0.5,border=1, main="középhőmérsékletek Nyír")
#PROB

par(mfrow=c(1,2))
hist(homb[,1],main="Nyíregyháza",xlab="",ylab="Gyakoriság")
hist(homk[,1],main="Karcag")

#keressuk meg minden napra a minimum es #maximum homersekleteket
#abrazoljuk is

minb=rep(0, times=365)
maxb=minb
for (i in 1:365)
{bpi<-homb[365*r+i,1]
minb[i]=min(bpi)
maxb[i]=max(bpi)
}

par(mfrow=c(1,1))
plot(minb,type="l",col=4,ylim=c(min(minb),max(maxb)))
lines(maxb,col=2)
#hoppa! Keressuk meg a hibat
#hogyan?
homb[order(homb[,1])[dim(homb)[1]],1] #mennyi a max - kicsit bonyolultan, mert így azt is tudjuk...
order(homb[,1])[dim(homb)[1]] #hogy melyik megfigyelés a hibás

<-order(homb[,1],decreasing = TRUE)[1]
homb[id,]

#hogyan csinaljuk?
k=order(homb[,1])[dim(homb)[1]] 
i=k-365*trunc(k/365) 
r=c(c(1:trunc(k/365)-1),c(trunc(k/365)+1,37))
bpi<-mean(homb[365*r+i,1])

plot(homb[(k-5):(k+5),1],type="l") #vonaldiagram
lines(homk[(k-5):(k+5),1],col=2)

homb[k,1]=NA #hiányzó adat
plot(homb[(k-5):(k+5),1],type="l",ylim=c(20,26))
lines(homk[(k-5):(k+5),1],col=2)


#tobb lehetoseg
#szomszedosakbol
homb[k,1]=(homb[(k-1),1]+homb[k+1,1])/2
plot(homb[(k-5):(k+5),1],type="l",ylim=c(20,26))
lines(homk[(k-5):(k+5),1],col=2)

homb[k,1]=homb[k-1,1]+homk[k,1]- homk[k-1,1]
lines(homb[(k-5):(k+5),1],lty=2)

homb[k,1]=(homb[(k-1),1]+homb[k+1,1])/2+ homk[k,1]-(homk[(k-1),1]+homk[k+1,1])/2
lines(homb[(k-5):(k+5),1],lty=3)

homb[k,1]

#es mi volt az igazi????

homb[k,1]=23.2
#mentsuk is el
write.table(homb,"nyir-51-88jav.hom",quote=F,col.names=F,row.names=F)
#masik lehetőség: R adatként
save(homb,file="nyir-51-88jav.RData")


#ujra a min-max
minb=rep(0, times=365)
maxb=minb
for (i in 1:365)
{bpi<-homb[365*r+i,1]
minb[i]=min(bpi)
maxb[i]=max(bpi)
}
plot(minb,type="l",col=4,ylim=c(min(minb),max(maxb)))
lines(maxb,col=2)

#megkereses

xx=homb[,1]
for (i in 1:38)
{xx[365*(i-1)+c(1:149)]=NA
xx[365*(i-1)+c(201:365)]=NA
}

#vajon a nyari alacsony minimum hiba-e????


##sajnos a helyzet nem olyan egyszeru mint multkor mert, nem abszolut minimumrol
#van szo
#tehat ahhoz, hogy alkalmazni tudjuk az elozoekben hasznalt modszert kell talalnuk egy olyan
#intervallumot ahol az adott ertek minimalis
#ez ranezesre peldaul a 150 - 200 kozotti napokra lehetseges
plot(minb,type="l",col=4,ylim=c(min(minb),max(maxb)))
plot(minb[150:200],type="l",col=4,ylim=c(min(minb[150:200]),max(maxb)))
#tehat ha  150 - 200. nap kozotti minimum helyet kellene megkeresni
order(minb[150:200])[1]
#tehat megvan hogy a kiugro minimum a 150+21. (22-1 az indexeles miatt, de melyik evben?

xx[order(xx)[1]] #mennyi a max - kicsit bonyolultan, mert így azt is tudjuk...
k<-order(homb[,1])[dim(homb)[1]] #hogy melyik megfigyelés a hibás

#tobb lehetoseg
#szomszedosakbol
homb[k,1]=(homb[(k-1),1]+homb[k+1,1])/2
plot(homb[(k-5):(k+5),1],type="l",ylim=c(min(homb[(k-5):(k+5),1]),max(maxb)))
lines(homk[(k-5):(k+5),1],col=2)

homb[k,1]=homb[k-1,1]+homk[k,1]- homk[k-1,1]
lines(homb[(k-5):(k+5),1],lty=2)

homb[k,1]=(homb[(k-1),1]+homb[k+1,1])/2+ homk[k,1]-(homk[(k-1),1]+homk[k+1,1])/2
lines(homb[(k-5):(k+5),1],lty=3)

##########
#2. gyakorlat
##########

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

#HF1 megvizsgálni a sup erteket
##tobb mintaelemszamra, ismetlesre

##################
#Parzen-Rosenblatt
##################



x=c(1,4,6)
plot(density(x,bw=1/3,kernel="r"))

x=c(1,4,6,8,9)
plot(density(x,bw=1/3,kernel="r"))
#bw is the standard deviation of the kernel
#


x=rexp(100)
plot(density(x))

plot(density(x),ylim=c(0,1))
lines(c(1:100)/20,dexp(c(1:100)/20),col=2)
lines(density(x,from=0, to=6),col=4)
lines(density(x,cut=4*min(x)),col=3)
#ezek mind csak vágnak


######
#HF2 hogy lehetne elerni, hogy tenyleg pozitiv tartomanyra koncentralodjon, ez mintha csak levagna

