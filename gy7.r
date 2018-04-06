
########
#7.gyak
########
dat<-read.table("diak_18a.csv",sep=";",header=T,dec=",")

xdat=dat[,1]
n=dim(dat)[1]
#magasság
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error
t.test(xdat,mu=175)
left 
right


n=dim(dat)[1]
xdat=dat[,3]
#cipo
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error
sd(xdat)
t.test(xdat,mu=40,alternative="l")
t.test(xdat,mu=40,alternative="t")


#t-próba az időjárási adatokra
homb<-read.table("nyir-51-88jav.hom")
homk<-read.table("karc-51-88.hom")

r<-c(0:37)

jan1b<-homb[365*r+1,1]
jan1k<-homk[365*r+1,1]

#ezek a jan. 1-i adatok

t.test(jan1b,alternative="l",mu=0)
#H1:mu<0 less

t.test(jan1b,alternative="t",mu=0)
#H1:mu=0 two sided

t.test(jan1b,alternative="t",mu=-4)
#H1:mu>-2 greater

############
###HF a kovetkezo blokk es az eredmeny ertelmezese -es tovabbi hasonlo vizsgalat 
p.v=rep(0,times=365)
for (i in 1:365) {
  jan1b<-homb[365*r+i,1]
  jan1k<-homk[365*r+i,1]
  p.v[i]=t.test(jan1b,jan1k,paired=T,alternative="t")$p.value
}
##################


#ez a korrekt, hiszen parositott megfigyeleseink vannak

###
#nezzunk tovabbi kerdeseket
###

############
#ketmintas t-proba
############

#magasabbak-e az informatikusok, mint a survey stat-osok

dat_ss=read.csv("diak_18t.csv",header=T,sep=";")
i=1
sdat=dat_ss[,i]


xdat=dat[,i]
t.test(sdat,xdat)

i=2
sdat=dat_ss[,i]

xdat=dat[,i]
t.test(sdat,xdat)

i=3
sdat=dat_ss[,i]

xdat=dat[,i]
t.test(sdat,xdat)

i=5
#jegy
sdat=dat_ss[,i]

xdat=dat[,i]
t.test(sdat,xdat)

i=6
sdat=dat_ss[,i]

xdat=dat[,i]
t.test(sdat,xdat)

i=7
sdat=dat_ss[,i]

xdat=dat[,i]
t.test(sdat,xdat)
t.test(sdat,xdat)

