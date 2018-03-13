
##########
#gyak4
##########

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
boxplot(est)

#b/ nézzük meg különböző adjust értékekre és nézzük meg, melyik a legjobb
n=10
adj=c(0.5,1,1.2,1.5,2)
est=matrix(0,100,length(adj))
for(j in 1:100) {
  #for (i in 1:length(n)) {
  for (i in length(adj)) {
    x=rnorm(n)
    yy=density(x,adjust=adj[i])
    yd=yy$y
    xd=yy$x
    est[j,i]=max(abs(yd-dnorm(xd)))
  }
}
boxplot(est)

###
#Cauchy pelda
####

# neg log likelihood sfvbol
mlogl <- function(mu, x) {
  sum(-dcauchy(x, location = mu, log = TRUE))
}
# es kepletbol
mlogl2 <- function(mu, x) {
  sum(log(1 + (x - mu)^2))
}
#reprodukalhato legyen a szimulacio
n <- 30
set.seed(42)
x <- rcauchy(n)
#µ=0 de mi becsuljuk

mu.start <- median(x)
mu.start
#[1] -0.1955062
out <- nlm(mlogl, mu.start, x = x)
mu.hat <- out$estimate
mu.hat
#[1] -0.1816501
#masikbol
out2 <- nlm(mlogl2, mu.start, x = x)
mu.hat <- out$estimate
mu.hat
#[1] -0.1816501
#ugyanaz
############
# Szimulacio: melyik a jobb: a median vagy az ML becsles
nsim <- 100
mu.hat=rep(0,times=n)
mu.twiddle=mu.hat
mu.atl=mu.hat
mu <- 0
for (i in 1:nsim) {
  xsim <- rcauchy(n, location = mu)
  mu.start <- median(xsim)
  out <- nlm(mlogl, mu.start, x = xsim)
  mu.hat[i] <- out$estimate
  mu.twiddle[i] <- mu.start
  mu.atl[i]=mean(xsim)
}
mean((mu.hat - mu)^2)
#
mean((mu.atl - mu)^2)
mean((mu.twiddle - mu)^2)
#HF2: tehát akkor melyik is a jobb és mennyivel?