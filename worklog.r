n <- 1000
x <- 1:n
S <- sum(x)
onethird <- function(n) sum(3/10^c(1:n))
1/3 - onethird(4)
1/3 - onethird(10)
1/3 - onethird(16)
# integral example
width <- 0.01
x <- seq(2,4,width)
areaofbars <- f(x)*width
f = function(n) n^1.5
areaofbars <- f(x)*width
sum(areaofbars)
head(dat)
view(dat)
View(dat)
library(dplyr)
control <- filter(dat, Diet=="chow")  %>%
select(Bodyweight)  %>%
unlist
treatment <- filter(dat,Diet=="nf")  %>%
select(Bodyweight) %>%
unlist
print(mean(treatment))
treatment <- filter(dat,Diet=="hf")  %>%
select(Bodyweight) %>%
unlist
print( mean(treatment))
print( mean(control))
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)
# Random Variables
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename))
download(url,destfile=filename)
population <- read.csv(filename)
population unlist(population)
population <- unlist(population)
control <- sample(population,12)
mean(control)
control <- sample(population,12)
mean(control)
control <- sample(population,12)
mean(control)
View(population )
# The Null Hypothesis
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename))
download(url,destfile=filename)
download(url,destfile=filename)
population <- read.csv(filename)
population <- unlist(population)
control <- samlpe(population,12)
control <- sample(population,12)
mean(control)
mean(control)
mean(control)
mean(control)
# The Null Hypothesis
control <- sample(population,12)
treatment <- sample(population,12)
print(mean(treatment) - mean(treatment))
print(mean(treatment) - mean(control))
# Now let's do this 10000 times
n <- 10000
null <- vector("numeric",n)
for (i in 1:n){}
for (i in 1:n){
treatment <- sample(population,12)
control <- sample(population,12)
null[i] <- mean(treatment) - mean(control)
}
mean(null >= obsdiff)
View(n)
# Distributions
library(UsingR)
install.packages("UsingR")
library(UsingR)
x <- father.son$fheight
round(sample(x,10),1)
smallest <- floor( min(x))
largest <- ceiling( max(x))
values <- seq(smallest, largest, len=300)
heightecdf <- ecdf(x)
View(x)
plot(values, heightecdf(values), type="l", xlab="a (Height in inches)", ylab="Pr(x <= a)")
?ecdf
plot(values, heightecdf(values), type="l", xlab="a (Height in inches)", ylab="Pr(x <= a)")
# Histograms
hist(x)
bins <- seq(smallest, largest)
hist(x,breaks=bins,xlab="Height (in inches)", main="Adult men heights")
hist(x)
hist(x,breaks=bins,xlab="Height (in inches)", main="Adult men heights")
# Probability Distribution
n <- 500
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
totals <- vector("numeric",11)
for (i in 1:n) {
control <- sample(population, 12)
treatment <- sample(population,12)
nulldiff <- mean(treatment) - mean(control)
j <- pmax(pmin(round(nulldiff)+6,11),1)
totals[j] <- totals[j]+1
text(j-6,totals[j],pch=15,round(nulldiff,1))
if(i <15) Sys.sleep(1)
}
hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)
# Normal Distribution
pnorm(obsdiff,mean(null),sd(null))
# Populations, Samples and Estimates
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
download(url,destfile=filename)
dat <- read.csv(filename)
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow")  %>%
select(Bodyweight)  %>%
unlist
View(dat)
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow")  %>%
select("Bodyweight")  %>%
unlist
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow")  %>%
select("Bodyweight")  %>%
unlist
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow")  %>% select(Bodyweight)  %>% unlist
library(dplyr)
controlPopulation <- filter(dat, Sex=="F" & Diet=="chow")  %>%
select(Bodyweight)
select(Bodyweight) <- controlPopulation unlist
controlPopulation <- controlPopulation unlist
controlPopulation <- unlist controlPopulation
unlist(controlPopulation)
controlPopulation <- unlist(controlPopulation)
hfPopulation <- filter(dat, Sex == "F" & Diet=="hf")  %>%
select(Bodyweight)
hfPopulation <- unlist(hfPopulation)
library(rafalib)
mypar(1,2)
hist(hfPopulation)
hist(controlPopulation)
mypar(1,2)
qqnorm(hfPopulation)
qqline(hfPopulation)
qqnorm(controlPopulation)
qqline(controlPopulation)
# The Central Limit Theorem in Practice
dat <- read.csv("mice_pheno.csv")
head(dat)
library(dplyr)
controlPopulation <- filter(dat,Sex=="F" & Diet == "chow")  %>%
select(Bodyweight)  %>% unlist
hfPopulation <- filter(dat,Sex=="F" & Diet=="hf")  %>%
select(Bodyweight)  %>% unlist
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
x <- controlPopulation
N <- length(x)
populationvar <- mean((x-mean(x))^2)
identical var(x), populationvar)
identical(var(x), populationvar)
var(x)
identical(var(x)*(N-1)/N, populationvar)
library(rafalib)
sd_hf <- popsd(hfPopulation)
sd_control <- popsd(controlPopulation)
N <- 12
hf <- sample(hfPopulation, 12)
control <- sample(controlPopulation, 12)
Ns <- c(3,12,25,50)
B <- 10000 # number of simulations
res <- sapply(Ns,function(n) {
replicate(B,mean(sample(hfPopulation,n)) - mean(sample(controlPopulation,n)))
})
mypar(2,2)
for (i in seq(along=Ns)) {}
for (i in seq(along=Ns)) {
titleavg <- signif(mean(res[,i],3),3)
titlesd <- signif(popsd(res[,i]),3)
title <- paste0("N=",Ns[i]," Avg=",titleavg,"SD=",titlesd)
qqnorm(res[,i],main=title)
qqline(res[,i],col=2)
}
computestat <- function(n) {}
y <- sample(hfPopulation,n)
computestat <- function(n) {
y <- sample(hfPopulation,n)
x <- sample(controlPopulation,n)
(mean(y)-mean(x))/sqrt(var(y)/n+var(x)/n)
}
res < sapply(Ns,function(n) {)
res < sapply(Ns,function(n) {
replicate(B,computestat(n))
})
mypar(2,2)
for (i in seq(along=Ns)) {}
for (i in seq(along=Ns)) {
qqnorm(res[,i],main=Ns[i])
qqline(res[,i],col=2)
}
# t-tests in practice
# Read in and prepare data
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat, Diet=="chow")  %>%
select(Bodyweight)  %>% unlist
treatment <- filter(dat, Diet=="hf")  %>%
select(Bodyweight)  %>% unlist
diff <- mean(treatment) - mean(control)
pritn(diff)
print(diff)
# calculate SE
sd(control)/sqrt(length(control))
se <- sqrt(
var(treatment)/length(treatment) +
var(control)/length(control)
)
tstat <- diff/se
righttail <- 1 - pnorm(abs(tstat))
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)
# The t-distribution in practice
mypar(1,2)
qqnorm(treatment)
qqline(treatment,col=2)
qqnorm(control)
qqline(control,col=2)
t.test(treatment, control)
result <- t.test(treatment, control)
result$p.value
dat <- read.csv("mice_pheno.csv")
control <- filter(dat, Diet=="chow")  %>%
select(Bodyweight)
treatment <- filter(dat, Diet=="hf")
treatment <- filter(dat, Diet=="hf")  %>%
select(Bodyweight)
t.test(treatment, control)
# Confidence Intervals
dat <- read.csv("mice_pheno.csv")
chowPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]
View(chowPopulation)
mu_chow <- mean(chowPopulation)
pritn(mu_chow)
print(mu_chow)
N <- 30
chow <- sample(chowPopulation,N)
print(mean(chow))
se <- sd(chow)/sqrt(N)
print(se)
pnorm(2) - pnorm(-2)
pnorm(1-0.02/2) - pnorm(-2)
pnorm(1-0.05/2) - pnorm(-2)
Q <- qnorm(1- 0.05/2)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se )
interval
interval[1] < mu_chow & interval[2] > mu_chow
B <- 250
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n", xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B) {}
for (i in 1:B) {
chow <- sample(chowPopulation,N)
se <-sd(chow)/sqrt(N)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
color <- ifelse(covered,1,2)
lines(interval, c(i,i),col=color)
}
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n", xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
chow <- sample(chowPopulation,N)
se <- sd(chow)/sqrt(N)
interval <- c(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
color <- ifelse(covered,1,2)
lines(interval, c(i,i),col=color)
}
}
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n", xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
chow <- sample(chowPopulation,N)
se <- sd(chow)/sqrt(N)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
color <- ifelse(covered,1,2)
lines(interval, c(i,i),col=color)
}
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n", xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qt(1- 0.05/2, df=4)
N <- 5
for (i in 1:B) {
chow <- sample(chowPopulation,N)
se <- sd(chow)/sqrt(N)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
color <- ifelse(covered,1,2)
lines(interval, c(i,i),col=color)
}
qt(1- 0.05/2, df=4)
qnorm(1- 0.05/2)
# Power Calculations
dat <- read.csv("mice.pheno.csv") # Previously downloaded
dat <- read.csv("mice_pheno.csv") # Previously downloaded
controlPopulation <- filter(dat,Sex=="F" & Diet=="chow")  %>% select(Bodyweight)  %>% unlist
hfPopulation <- filter(dat,Sex=="F" & Diet=="hf")  %>%
select(Bodyweight)  %>% unlist
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print((mu_hf - mu_control)/mu_control*100) # percent increase
set.seed(1)
N<-5
hf <- sample(controlPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
hf <- sample(controlPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
# Type I error - False Posotive
# Type II error - False Negative
N <- 12
alpha  <- 0.05
B  <- 2000
reject  <- function(N, alpha=0.05){
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
pval <- t.test(hf,control)$p.value
pval <- alpha
}
reject  <- function(N, alpha=0.05){
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
pval <- t.test(hf,control)$p.value
pval < alpha
}
reject(12)
rejections  <- replicate(B,reject(N))
mean(rejections)
Ns  <- seq(5, 50, 5)
power  <-  sapply(NS,function(N){
rejections  <- replicate(B, reject(N))
mean(rejections)
})
power  <-  sapply(Ns,function(N){
rejections  <- replicate(B, reject(N))
mean(rejections)
})
plot(Ns, power, type="b")
N  <- 30
alphas  <- c(0.1,0.05,0.01,0.001,0.0001)
power  <- sapply(alphas,function(alpha){
rejections  <- replicate(B,reject(N,alpha=alpha))
mean(rejections)
})
plot(alphas, power, xlab="alpha", type="b", log="x")
# p-valueas are arbitrary under the Alternative Hypothesis
calculatePvalue <- function(N) {
hf  <- sample(hfPopulation,N)
control <- sample(controlPopualtion,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
}
Ns  <- seq(10,200,by=10)
Ns_rep <- rep(Ns,each=10)
pvalueas  <- sapply(Ns_rep, calculatePvalue)
calculatePvalue <- function(N) {
hf  <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
}
pvalues  <- sapply(Ns_rep, calculatePvalue)
plot(Ns_rep, pvalues, log="y", xlab="sample size", ylab="p-values")
abline(h=c(.01, .05), col="red", lwd=2)
N <- 12
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation, N)
diff <- mean(hf) - mean(control)
diff / mean(control) * 100
control <- sample(controlPopulation, N)
hf <- sample(hfPopulation,N)
diff <- mean(hf) - mean(control)
diff / mean(control) * 100
t.test(hf, control)$conf.int / mean(control) * 100
# Cohen's d
sd_pool  <- sqrt(((N-1)*var(hf) + (N-1)*var(control))/(2*N-2))
diff/sd_pool
## Monte Carlo Simulation
library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation  <- filter(Sex=="F" & Diet=="chow")
controlPopulation  <- filter(dat,Sex=="F" & Diet=="chow")  %>% select(Bodyweight)  %>% unlist
ttestgenerator  <- function(n) {
#note that here we have a false "high fa" group where we actually sample from the nonsmokers. this is because we are modeling the *null*
cases  <- sample(controlPopulation,n)
controls <- sample(controlPopulation,n)
tstat <- (mean(cases)-mean(controls)) / sqrt(var(cases)/n + var(controls)/n)
return(tstat)
}
ttests <-replicate(1000,ttestgenerator(10))
hist(ttests)
qqnorm(ttests)
abline(0,1)
# how about a sample size of three
ttests  <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)
ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
qqnorm(controlPopulation)
qqline(controlPopulation)
# Parametric Simulations for the Observations
controls <- rnorm(5000, mean=24, sd=3.5)
ttestgenerator <- function(n, mean=24, sd=3.5) {
cases <- rnorm(n,mean,sd)
controls <- rnorm(n,mean,sd)
tstat <- (mean(cases)-mean(controls))/sqrt(var(cses)/n + var(controls)/n)
return(tstat)
}
# Permutation tests
dat=read.csv("femaleMiceWeights.csv")
control <- filter(dat,Diet=="chow")  %>%
select(Bodyweight) %>%
unlist
treatment <- filter(dat,Diet=="hf") %>%
select(Bodyweight) %>%
unlist
obsdiff <- mean(treatment)-mean(control)
N <- 12
avgdiff <- replicate(1000, {
all <- sample(c(control,treatment))
newcontrols <- all[1:N]
newtreatments <- all[(N+1):(2*N)]
return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)
# the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff))+1) / (length(avgdiff) + 1)
# and again with a smaller dataset
N <- 5
control <- sample(control,N)
treatment <- sample(treatment,N)
obsdiff<- mean(treatment) - mean(control)
avgdiff <- replicate(1000,{
all <- sample(c(control,treatment))
newcontrols <- all[1:N]
newtreatments <- all[(N+1):(2*N)]
return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)
# Association Tests
tab <- matrix(c(3,1,1,3),2,2)
rownames(tab)<-c("poured Before", "Poured After")
colnames(tab) <- c("Guessed before","Guessed after")
tab
rownames(tab)<-c("Poured Before", "Poured After")
tab
fisher.text(tab,alternative="greater")
fisher.test(tab,alternative="greater")
# Chi-squared test
disease = factor(c(rep(0,180),rep(1,20),rep(1,40),rep(1,10)),labels=c("control","cases"))
genotype=factor(c(rep("AA/Aa",200),rep("aa",50)),levels=c("AA/Aa","aa"))
dat <- data.frame(disease,genotype)
dat <- dat[sample(nrow(dat)),]
head(dat)
dat <- dat[sample(nrow(dat)),] # shuffle them up
head(dat)
table(genotype)
table(disease)
tab <- table(genotype,disease)
tab
# Odds Ratio
(tab[2,2]/tab[2,1]) / (tab[1,2]/tab[1,1])
disease = factor(c(rep(0,180),rep(1,20),rep(1,40),rep(1,10)),labels=c("control","cases")))
disease = factor(c(rep(0,180),rep(1,20),rep(0,40),rep(1,10)),labels=c("control","cases"))
dat <- data.frame(disease, genotype)
dat <- dat[sample(nrow(dat)),] # shuffle it up
head(dat)
tab <- table(genotype,disease)
tab
(tab[2,2]/tab[2,1]) / (tab[1,2]/tab[1,1])
p=mean(disease=="cases")
expected <- rbind(c(1-p,p)*sum(genotype=="AA/Aa"),c(1-p,p)*sum(genotype=="aa"))
dimnames(expected)dimnames(tab)
dimnames(expected) <- dimnames(tab)
expected
chisq.test(tab)$p.value
tab <- tab*10
chisq.test(tab)$p.value
# Large Samples, Small p-values
# Confidence Intervals For The Odd Ratio
fit <- glm(disease~genotype,family="binomial",data=dat)
coeftab <- summary(fit)$coef
coaeftab
coeftab
ci <- coeftab[2,1] + c(-2,2)*coeftab[2,2]
exp(ci)
savehistory("~/repos/RLifeSciences/Chappie1/worklog.r")
