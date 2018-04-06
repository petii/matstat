
homb<-read.table("nyir-51-88jav.hom")
homk<-read.table("karc-51-88.hom")

r<-c(0:37)

jan1b<-homb[365*r+1,1]
jan1k<-homk[365*r+1,1]
p <- t.test(jan1b,jan1k,paired=T,alternative="t")

############
###HF a kovetkezo blokk es az eredmeny ertelmezese -es tovabbi hasonlo vizsgalat 
p.v=rep(0,times=365)
for (i in 1:365) {
  dayOfYearb<-homb[365*r+i,1]
  dayOfYeark<-homk[365*r+i,1]
  p.v[i]=t.test(dayOfYearb,dayOfYeark,paired=T,alternative="t")$p.value
}
##################
summary(p.v)
plot(p.v)
