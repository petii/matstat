
########################
#6.gyak
########################


###########
#konfidencia intervallum
###########

a <- 5
sig <- 2
n <- 20
xdat=rnorm(n,a,sig)
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error
hist(xdat)
abline(v=left,col=2,lty=2)
abline(v=right,col=2,lty=2)


a <- 5
sig <- 2
n <- 2000
xdat=rnorm(n,a,sig)
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error
hist(xdat)
abline(v=left,col=2,lty=2)
abline(v=right,col=2,lty=2)

#ellenorizzuk a lefedesi vszget

talal=0
m=1000
for (i in 1:m){
  a <- 5
  sig <- 2
  n <- 20
  xdat=rnorm(n,a,sig)
  error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
  left <- mean(xdat)-error
  right <- mean(xdat)+error
  if (left<a && a<right) talal=talal+1
}
talal

talal=0
m=1000
for (i in 1:m) {
  a <- 5
  sig <- 2
  n <- 2000
  xdat=rnorm(n,a,sig)
  error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
  left <- mean(xdat)-error
  right <- mean(xdat)+error
  if (left<a && a<right) talal=talal+1
}
talal

#mi van, ha nem normalis az elo

talal=0
alul=0
m=1000
for (i in 1:m) {
  a <- 5
  sig <- 2
  n <- 20
  xdat=rexp(n,1/a)
  error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
  left <- mean(xdat)-error
  right <- mean(xdat)+error
  if (left<a && a<right) talal=talal+1
  if (a<left) alul=alul+1
}
talal
alul

###
#pontos
###

talal=0
alul=0
felul=0
m=1000
for (i in 1:m) {
  a <- 5
  #sig <- 2
  n <- 20
  xdat=rexp(n,a)
  #error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
  left <- qgamma(0.025,n)/sum(xdat)
  right <- qgamma(0.975,n)/sum(xdat)
  if (left<a && a<right) talal=talal+1
  if (a<left) alul=alul+1
  if (a>right) felul=felul+1
}
talal
alul
felul



talal=0
m=1000
elt=matrix(0,m,2)
for (i in 1:m) {
  a <- 5
  sig <- 2
  n <- 2000
  xdat=rexp(n,1/a)
  error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
  left <- mean(xdat)-error
  right <- mean(xdat)+error
  if (left<a && a<right) talal=talal+1
  right2 <- 1/(qgamma(0.025,n)/sum(xdat))
  left2 <- 1/(qgamma(0.975,n)/sum(xdat))
  elt[i,1]=left-left2
  elt[i,2]=right-right2
}
talal
#n=20-ra van elteres
#vajon melyik a jobb? A rovidebb a jobb

#sajat adataink

########
#konf.int
#######
dat<-read.table("diak_18a.csv",sep=";",header=T)
n=dim(dat)[1]
xdat=dat[,1]
##testmagasság
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error
sd(xdat)

##súly
xdat=dat[,2]
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error

##cipõméret
xdat=dat[,3]
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error

##tanulási idõ
xdat=dat[,5]
typeof(xdat)
xdat

error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error

##utazási idõ
#gamma would be better instead of normal distribution
xdat=dat[,6]
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error

##valszám jegy
xdat=dat[,7]
error <- qt(0.975,n-1)*sd(xdat)/sqrt(n)
left <- mean(xdat)-error
right <- mean(xdat)+error

left 
right
error
