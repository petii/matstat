
############
###HF a kovetkezo blokk es az eredmeny ertelmezese -es tovabbi hasonlo vizsgalat 
p.v=rep(0,times=365)
for (i in 1:365) {
  jan1b<-homb[365*r+i,1]
  jan1k<-homk[365*r+i,1]
  p.v[i]=t.test(jan1b,jan1k,paired=T,alternative="t")$p.value
}
##################
