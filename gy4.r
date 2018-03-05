##########
#gyak4
##########

#surusegfv becslesnel: konvergencia vizsgalata 
n=c(10,40,160,640)
est=matrix(0,100,4)
for(j in 1:100)
{
  for (i in 1:length(n))
  {x=rnorm(n[i])
  yd=density(x)$y
  xd=density(x)$x
  est[j,i]=max(abs(yd-dnorm(xd)))}
}



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
