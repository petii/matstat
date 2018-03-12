
########################
#5.gyak
########################

#Az MSE-hanyados:
mean((mu.hat - mu)^2)/mean((mu.twiddle - mu)^2)

# asymptotic relative efficiency (ARE) 
# az MLE a pontosabb
################
#ket parameter
################
#neg log likelihood a ketparameteres Cauchy elo-ra
#az eredeti
mlogl3 <- function(theta, x) {
  s=sum(-dcauchy(x, location = theta[1], scale = theta[2], log = TRUE))
}
#a javitott
mlogl3 <- function(theta, x) {
  if (theta[2]>0)
    s=sum(-dcauchy(x, location = theta[1], scale = theta[2], log = TRUE))
  if (theta[2]<=0) s=10^6
  s
}
#MLE 
theta.start <- c(median(x), IQR(x)/2)
theta.start

out <- nlm(mlogl3, theta.start, x = x)
theta.hat <- out$estimate
theta.hat

################
#Fisher Informacio becslese
# nlm kiszamolja az MLE helyen
out <- nlm(mlogl3, theta.start, x = x, hessian = TRUE)
fish <- out$hessian
fish

#az inverz (MLE aszimpt szorasnegyzete)
solve(fish)


#konfidencia intervallum
conf.level <- 0.99
crit <- qnorm((1 + conf.level)/2)
inv.fish <- solve(fish)
theta.hat[1] + c(-1, 1) * crit * sqrt(inv.fish[1, 1])

theta.hat[2] + c(-1, 1) * crit * sqrt(inv.fish[2, 2])

#nem szimultan!!! 

mu=0
talal=0
m=1000
a=mu
n=60
elt=matrix(0,m,2)
for (i in 1:m)
{x <- rcauchy(n, location = mu)
theta.start <- c(median(x), IQR(x)/2)
out <- nlm(mlogl3, theta.start, x = x,hessian=T)
fish <- out$hessian
inv.fish <- solve(fish)
theta.hat <- out$estimate
vec=theta.hat[1] + c(-1, 1) * crit * sqrt(inv.fish[1, 1])

if (vec[1]<a && a<vec[2]) talal=talal+1
}
talal
#n=30 picit kicsi a talalat (kb 940)
#
#HF reprodukalni ezt a Gamma eloszlasra 
#ML becsles, konfidencia intervallum