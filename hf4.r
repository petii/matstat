
#surusegfv becslesnel: konvergencia vizsgalata 
n=c(10,40,160,640)
est=matrix(0,100,4)
for(j in 1:100) {
  for (i in 1:length(n)) {
    x=rnorm(n[i])
    yy=density(x,adjust=1.2)
    yd=yy$y
    xd=yy$x
    est[j,i]=max(abs(yd-dnorm(xd)))
  }
}

#HF1:
#a/ hogyan tudnánk ábrázolni?
boxplot(est,names=n)
#b/ nézzük meg különböző adjust értékekre és nézzük meg, melyik a legjobb
n=640
adj=c(0.5,1,1.2,1.5,2)
est=matrix(0,100,length(adj))
for(j in 1:100) {
  for (i in 1:length(adj)) {
    x=rnorm(n)
    yy=density(x,adjust=adj[i])
    yd=yy$y
    xd=yy$x
    est[j,i]=max(abs(yd-dnorm(xd)))
  }
}
boxplot(est,names=adj)

