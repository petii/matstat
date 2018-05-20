setwd('a fájl elérési útvonala')

data <- read.csv('data/Pokemon.csv')
labels(data)[2]

#nem releváns adatok kiszűrése
data <- data[!grepl("Mega",data$Name,fixed=TRUE),]
data <- data[!grepl("Primal",data$Name,fixed=TRUE),]
data <- data[!grepl("Unbound",data$Name,fixed=TRUE),]

for (x in levels(factor(data$X.))) {
  rows <- data[data$X. == x,]
  if (nrow(rows) > 1) {
    data <- data[data$X. != x,]
    name <- rows$Name[1]
    name <- strsplit(gsub("(.)([[:upper:]])", "\\1 \\2", name),' ')[[1]][[1]]
    print(name)
    levels(data$Name) <- c(levels(data$Name),name)
    statlab <- c('Total','HP','Attack','Defense','Sp..Atk','Sp..Def','Speed')
    base <- rows[1,]
    base$Name <- name
    for (l in statlab) {
      base[[l]] <- mean(rows[[l]])
    }
    data[nrow(data)+1,] <- base
  }
}

#normális és legendás pokémonok
legendsplit <- split(data,data$Legendary)
normal <- legendsplit$False
legendary <- legendsplit$True

# Normális és legendás pokémonok közti különbségek

## Tulajdonságok

layout(matrix(c(1,1,1,2,4,6,3,5,7),3,3))
labels <- c('Normal','Legendary')
labels.short <- c('L','N')
boxplot(normal$Total,legendary$Total,main="Total",names = labels)
boxplot(legendary$HP,normal$HP,main="HP",names = labels.short,horizontal = TRUE)
boxplot(legendary$Attack,normal$Attack,main="Attack",names = labels.short,horizontal = TRUE)
boxplot(legendary$Defense,normal$Defense,main="Defense",names = labels.short,horizontal = TRUE)
boxplot(legendary$Speed,normal$Speed,main="Speed",names = labels.short,horizontal = TRUE)
boxplot(legendary$Sp..Atk,normal$Sp..Atk,main="Special Attack",names = labels.short,horizontal = TRUE)
boxplot(legendary$Sp..Def,normal$Sp..Def,main="Special Defense",names = labels.short,horizontal = TRUE)

### Eloszlások

tests.ks <- list()

stats.names <- c('HP','Attack','Defense','Speed','Sp..Atk','Sp..Def')

for (i in stats.names) {
  layout(matrix(c(1:2),1,2,byrow = TRUE))
  tmp.norm <- normal[[i]] - mean(normal[[i]])
  tmp.legend <- legendary[[i]] - mean(legendary[[i]])
  dens.norm <- density(tmp.norm)
  dens.leg <- density(tmp.legend)
  limits.x <- c(min(min(tmp.norm),min(tmp.legend)),max(max(tmp.norm),max(tmp.legend)))
  limits.y <- c(0,max(max(dens.norm$y),max(dens.leg$y)))
  plot(dens.norm,xlim = limits.x, ylim = limits.y,main = i,xlab = '')
  lines(dens.leg,col = 2)
  plot(ecdf(tmp.norm),main='Tapasztalati eloszlásfüggvények')
  plot(ecdf(tmp.legend),add=TRUE,col=2)
  
  tests.ks[[i]] <- ks.test(tmp.norm,tmp.legend)
}

ks.value <- function(alpha,n,m) {
  sqrt(-1/2*log(alpha/2)) * sqrt((n+m)/(n*m))
}

alpha = 0.05
c <- ks.value(alpha,length(normal$Name),length(legendary$Name))
tests.ks
c

  ## Típusok

norm.type1.summary <- summary(normal$Type.1)
norm.type2.summary <- summary(normal$Type.2)[-1]
norm.types.summary <- t(cbind(as.matrix(norm.type1.summary),as.matrix(norm.type2.summary)))
norm.types.summary.normaled <- norm.types.summary/sum(norm.types.summary)
norm.types.summary.normaled.ordered <- norm.types.summary.normaled[,order(colSums(norm.types.summary.normaled),decreasing = TRUE)]
leg.type1.summary <- summary(legendary$Type.1)
leg.type2.summary <- summary(legendary$Type.2)[-1]
leg.types.summary <- t(cbind(as.matrix(leg.type1.summary),as.matrix(leg.type2.summary)))
leg.types.summary.normaled <- leg.types.summary/sum(leg.types.summary)
leg.types.summary.normaled.ordered <- leg.types.summary.normaled[,order(colSums(leg.types.summary.normaled),decreasing = TRUE)]
types.summary <- cbind(colSums(norm.types.summary),colSums(leg.types.summary))
types.summary.normaled <- cbind(types.summary[,1]/sum(types.summary[,1]),types.summary[,2]/sum(types.summary[,2]))

maxval <- max(max(colSums(norm.types.summary.normaled)),max(colSums(leg.types.summary.normaled)))
maxval <- c(0,maxval)

label.x <- 'Density'
legendtext.1 <- c('Type.1','Type.2')

layout(matrix(c(1,2),1,2,byrow = TRUE))
barplot(norm.types.summary.normaled.ordered,las=1,horiz = TRUE,legend.text = legendtext.1,main = 'Types in normal pokemons',xlab = label.x)
barplot(leg.types.summary.normaled.ordered,las=1,horiz = TRUE,xlim = maxval,legend.text = legendtext.1,main = 'Types in legendary pokemons',xlab = label.x)

num.water <- sum(norm.types.summary[,colnames(norm.types.summary)=='Water'])
num.normal <- sum(norm.types.summary[,colnames(norm.types.summary)=='Normal'])
num.sum <- sum(norm.types.summary)

layout(matrix(1,1,1))
barplot(t(types.summary.normaled),beside = TRUE,las = 2,legend.text = labels)

# Generáció szerinti bontás

data.gens <- split(data,data$Generation)
normal.gens <- split(normal,normal$Generation)
legendary.gens <- split(legendary,legendary$Generation)

## Pokémonok száma

poke.num <- rep(0,6)
poke.num.cum <- rep(0,6)
gen.list <- c(1:length(data.gens))
for (i in gen.list) {
  poke.num[i] <- length(data.gens[[i]]$Name)
  poke.num.cum[i] <- sum(poke.num[1:i])
}

regr <- function(coefs,x) coefs[1] + x * coefs[2]

plot(poke.num)
num.model <- lm(poke.num~gen.list)
lines(num.model$fitted.values)
#lines(regr(num.model$coefficients,c(1:6)))

plot(poke.num.cum)
num.cum.model <- lm(poke.num.cum~gen.list)
lines(num.cum.model$fitted.values)

#regr <- function(coefs,x) sum(c(1,x)*coefs) 
gen7.num <- regr(num.model$coefficients,7)
gen7.num.cum <- regr(num.cum.model$coefficients,7)

gen7.num.exact <- 86
gen.list.ext <- c(1:7)
poke.num.ext <- c(poke.num,gen7.num.exact)
plot(poke.num.ext)
legend('topright', legend = c('Eredeti','Kiegészített'), col=c(2,1),lty = 1)
num.ext.model <- lm(poke.num.ext~gen.list.ext)
lines(num.ext.model$fitted.values)
lines(regr(num.model$coefficients,c(1:7)),col=2)

gen8.num <- c(0,0)
gen8.num[1] <- regr(num.model$coefficients,8)
gen8.num[2] <- regr(num.ext.model$coefficients,8)

gen8.num[2]
gen8.num[1]
abs(gen8.num[1]-gen8.num[2])
    
## Tulajdonságok
gen1 <- normal.gens$'1'
gen2 <- normal.gens$'2'
gen3 <- normal.gens$'3'
gen4 <- normal.gens$'4'
gen5 <- normal.gens$'5'
gen6 <- normal.gens$'6'
boxplot(gen1$Total,gen2$Total,gen3$Total,gen4$Total,gen5$Total,gen6$Total,main='Generációk össz tulajdonságai',names = names(data.gens),col = c(2:7))
layout(matrix(c(1:6),2,3,byrow = TRUE))
for (i in stats.names) {
  boxplot(gen1[[i]],gen2[[i]],gen3[[i]],gen4[[i]],gen5[[i]],gen6[[i]],main=i,names = names(data.gens),col = c(2:7))
}

## Típusok

gens.types.summary <- NULL
gens.types.summary.normaled <- NULL

for (gen in normal.gens) {
  gen.type1.summary <- summary(gen$Type.1)
  gen.type2.summary <- summary(gen$Type.2)[-1]
  gen.types.summary <- t(cbind(as.matrix(gen.type1.summary),as.matrix(gen.type2.summary)))
  types.sum <- colSums(gen.types.summary)
  types.summary.normaled <- colSums(gen.types.summary) / nrow(gen) 
  gens.types.summary <- cbind(gens.types.summary,types.sum)
  gens.types.summary.normaled <- cbind(gens.types.summary.normaled,types.summary.normaled)
}
gens.types.summary.normaled <- t(gens.types.summary.normaled)
layout(matrix(1,1,1))
barplot(gens.types.summary.normaled,horiz = FALSE,las=2,beside = TRUE,col=c(2:7))

### Próba

alpha <- 0.05
gens.types.summary
res <- chisq.test(gens.types.summary)
crit <- qchisq(alpha,res$parameter)
res

