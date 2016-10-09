x <- 2
y <- 3
print(x+y)
install.packages("swirL")
install.packages("swirl")
install.packages("rafalib")
install.packages("downloader")
library(rafalib)
library(downloader)
getwd()
ls
setwd("~/repos/RLifeSciences")
savehistory("~/repos/RLifeSciences/worklog3.r")
ls
library(devtools)
install_github("genomicsclass/GSE5859Subset")
library(GSE5859Subset)
data(GSE5859Subset) # Loads the three tables
dim(geneExpression)
dim(SampleInfo)
dim(sampleInfo)
head(sampleInfo)
sampleInfo$group
match(sampleInfo$filename,colnames(geneExpression))
dim(geneAnnotation)
head(geneAnnotation)
head(match(geneAnnotation$PROBEID,rownames(geneExpression)))
2*(1-pnorm(Z))
read.csv("femaleControlsPopulation.csv"))
read.csv("femaleControlsPopulation.csv"))
read.csv("femaleControlsPopulation.csv"))
read.csv("femaleControlsPopulation.csv")
set.seed(1)
population = unlist(read.csv("femaleControlsPopulation.csv"))
N <- 12
B <- 10000
pvals <- replicate(B,{})
pvals <- replicate(B,{
control = sample(population,N)
treatment = sample(population,N)
t.test(treatment,control)$p.val
})
hist(pvals)
# Thousands of Tests
g <- sampleInfo$group
g
e <- geneExpression[25,]
library(rafalib)
mypar(1,2)
qqnorm(e[g==1])
qqline(e[g==1])
qqnorm(w[g==0])
qqnorm(e[g==0])
qqline(e[g==0])
t.test(e[g==1],e[g==0])$p.value
myttest <- function(x)
t.test(x[g==1],x[g==0],var.equal=TRUE)$p.value
pvals <- apply(geneExpression,1,myttest)
sum(pvals<0.05)
# performing 8000 ttest on data where the null hypothesis is true
set.seed(1)
m <- nrow(geneExpression)
n <- ncol(geneExpression)
randomData <- matrix(rnorm(n*m),m,n)
nullpvals <- aplly(randomData,1,mytest)
nullpvals <- apply(randomData,1,mytest)
nullpvals <- apply(randomData,1,myttest)
sum(nullpvals<0.05)
# More efficient ttest implementation
install_bioc("genefilter")
install_bioc("genefilter")
# Procedures
# response to Multiple Testing Problem
# Define a Procedure then estimate or Control an informative Error Rate
# Guarantee an error rate below a predefined value
# Procedures have flexibility through parameters and cutoffs that let us control specificity and sensitivity
# Error rates
sum(pvals<0.05)
set.seed(1)
population = unlist(read.csv("femaleControlPopulation.csv"))
population = unlist(read.csv("femaleControlsPopulation.csv"))
alpha <- 0.05
N <- 12
m <- 10000
pvals <- replicate(m,{
control = sample(population,N)
treatment = sample(population,N)
t.test(treatment,control)$p.value})
sum(pvals<0.05)
alpha <- 0.05
N <- 12
m <- 10000
p0 <- 0.90 #10% of diests work, 90% don't
m0 <- m*p0
m1 <- m-m0
nullHypothesis <- c(rep(TRUE,m0),rep(FALSE,m1))
delta  <- 3
set.seed(1)
calls <- sapply(1:m, function(i){
control <- sample(population,N)
treatment <- sample(population,N)
if(!nullHypothesis[i]) treatment <- treatment+ delta
ifelse(t.test(treatment,control)$p.value < alpha, "Called Significant", "Not Called Significant")})
null_hypothesis <- factor(nullHypothesis, levels=c("TRUE","FALSE"))
table(null_hypothesis,calls)
B <- 10 ##number of simulations
VandS <- replicate(B,{
calls <- sapply(1:m, function(i){
control <- sample(population,N)
treatment <- sample(population,N)
if(!nullHypothesis[i]) treatment <- treatment + delta
t.test(treatment,control)$p.val < alpha})
cat("V =",sum(nullHypothesis & calls), "S =",sum(!nullHypothesis & calls), "\n")
c(sum(nullHypothesis & calls), sum(!nullHypothesis & calls))
})
## this is used to motivate the Family Wise Error Rate
## Leading to
# The Bonferroni Correction
B <- 10000
minpval <- replicate(B,min(runif(10000,0,1))<0.01)
mean(minpval>=1)
set.seed(1)
pvals <- sapply(1:m,function(i){
control <- sample(population,N)
treatment <- sample(population,N)
if(!nullHypothesis[i]) treatment <- treatment + delta
t.test(treatment,control)$p.value})
sum(pvals < 0.05/10000)
# False Discovery Rate
savehistory("~/repos/RLifeSciences/worklog3.r")
head(population)
population <- unlist(read.csv("femaleControlsPopulation.csv"))
set.seed(1)
pvals <- sapply(1:m, function(i){
control  <- sample(population,6)
treatment <- sample(population,6)
if(!nullHypothesis[i]) treatment <- treatment + delta
t.test(treatment,control)$p.value
})
m <- 10000
N <- 12
p0 <- 0.90
m0 <- m*p0
m1 <- m-m0
delta <- 3
nullHypothesis <- c(rep(TRUE,m0), rep(FALSE,m1))
set.seed(1)
pvals <- sapply ( 1 : m , function ( i ){
control <- sample ( population , 6 )
treatment <- sample ( population , 6 )
if ( ! nullHypothesis [ i ]) treatment <- treatment + delta
t.test ( treatment , control ) $ p.value
})
sum(pvals < 0.05/10000)
library(genefilter) ##rowtests is here
library(rafalib)
install.packages(genefilter)
source("https://bioconductor.org/biocLite.R")
biocLite("genefilter")
library(genefilter)
set.seed(1)
## Define groups to be used with rowttests
g <- factor( c(rep(0,N),rep(1,N)) )
B <- 1000 # number of simulations
Qs <- replicate(B,{
## matrix with control data (rows are tests, columns are mice)
controls <- matrix(sample(population, N*m, replace=TRUE),nrow=m)
## matrix with control data (rows are tests, columns are mice)
treatments <- matrix(sample(population, N*m, replace=TRUE),nrow=m)
## add effect to 10% of them
treatments[which(!nullHypothesis),] <- treatments[which(!nullHypothesis),]+delta
## combine to form one matrix
dat <- cbind(controls,treatments)
calls <- rowttests(dat,g)$p.value < alpha
R=sum(calls)
Q=ifelse(R>0,sum(nullHypothesis & calls)/R,0)
return(Q)
})
savehistory("~/repos/RLifeSciences/worklog3.r")
library(rafalib)
mypar(1,1)
hist(Qs) ## Q is a random variable, this is its distribution
FDR=mean(Qs)
print(FDR)
set.seed(1)
savehistory("~/repos/RLifeSciences/worklog3.r")
controls <- matrix(sample(population, N*m, replace=TRUE), nrow=m)
treatments <- matrix(sample(population,N*m,replace=TRUE),nrow=m)
treatments[which(!nullHypotesis),] <- treatments[which(!nullHypothesis),]+delta
treatments[which(!nullHypothesis),] <- treatments[which(!nullHypothesis),]+delta
dat <- cbind(controls,treatments)
pvals <- rowtests(dat,g)$p.value
h <- hist(pvals,breaks=seq(0,1,0.05))
polygon(c(0,0.05,0.05,0),c(0,0,h$counts[1],h$counts[1]),col="grey")
abline(h=m0/20)
# The grey bar on left represents cases with p-values less than 0.05. The horizontal line is the level of false positives.
h <- hist(pvals,breaks=seq(0,1,0.01))
polygon(c(0,0.01,0.01,0),c(0,0,h$counts[1],h$counts[1]),col="grey")
abline(h=m0/100)
# Benjamini-Hochberg
alpha <- 0.05
1 = seq(along=pvals)
i = seq(along=pvals)
mypar(1,2)
plot(i,sort(pvals))
abline(0,i/m*alpha)
##close-up
plot(i[1:15],sort(pvals)[1:15],main="Close-up")
abline(0,i/m*alpha)
save.image("~/repos/RLifeSciences/worklog3.r.RData")
k <- max(which(sort(pvals) < 1/m*alpha) )
k <- max(which(sort(pvals) < i/m*alpha) )
cutoff <- sort(pvals)[k]
cat("k =", k,"p-value cutoff=",cutoff)
fdr <- p.adjust(pvals, method="fdr")
mypar(1,1)
plot(pvals,fdr,log="xy")
abline(h=alpha,v=cutoff)
# Monte-carlo simulation to confirm FDR is below .05
alpha <- 0.05
B <- 1000 ## number of simulations
res <- replicate(B,{
controls <- matrix(sample(population,N*m,replace=TRUE),nrow=m)
treatments <- matrix(sample(population, N*m, replace=TRUE),nrow=m)
treatments[which(!nullHypothesis),] <- treatments[which(!nullHypothesis),]+delta
dat <- cbind(controls,treatments)
pvals <- rowtests(dat,g)$p.value
##then the FDR
calls <- p.adjust(pvals,method="fdr") < alpha
R=sum(calls)
Q=ifelse(R>0,sum(nullHypothesis & calls)/R,0)
return(c(R,Q))
})
Qs <- res[2,]
mypar(1,1)
hist(Qs)
FDR=mean(Qs)
print(FDR)
Rs <- res[1,]
mean(Rs==0)*100
p.adjust.methods
?p.adjust
# Direct approach to FDR and q-values (Storey)
hist(pvals,breaks=seq(0,1,0.05),freq=FALSE)
lambda = 0.1
pi0=sum(pvals> lambda) / ((1-lambda)*m)
abline(h=pi0)
print(pi0)
library(qvalue)
install.packages(qvalue)
install_bioc(qvalue)
source("https://bioconductor.org/biocLite.R")
biocLite("qvalue")
library(qvalue)
res <- qvalue(pvals)
qvals <- res$qvalues
plot(pvals,qvals)
res$pi0
savehistory("~/repos/RLifeSciences/worklog3.r.RData")
library(genefilter)
## Exploratory Data Analysis with High Throughput Data
source("https://bioconductor.org/biocLite.R")
biocLite("genefilter")
biocLite("genefilter")
library(genefilter)
biocLite()
library(genefilter)
biocLite("genefilter")
biocLite("genefilter")
install.packages("XML")
library(GSE5859Subset)
biocLite("genefilter")
library(genefilter)
data(GSE5859Subset)
g <- facter(sampleInfo$group)
g <- factor(sampleInfo$group)
results <- rowtests(geneExpression,g)
results <- rowttests(geneExpression,g)
pvals <- results$p.value
# then generate p-values from a dataset where we know the null value is true
m <- nrow(geneExpression)
n <- ncol(geneExpression)
randomData <- matrix(rnorm(n*m),m,n)
nullpvals <- rowttests(randomData,g)$p.value
# Volcano plots!
plot(results$dm,-log10(results$p.value), xlab="Effect size", ylab="- log (base 10) p-values")
# P-Value histogram
library(rafalib)
mypar(1,2)
hist(nullpvals,ylim=c(0,1400))
hist(pvals,ylim=c(0,1400))
permg <- sample(g)
permresults <- rowttests(geneExpression,permg)
hist(permresults$p.value)
# Data boxplots and histograms
library(Biobase)
library(GSE5859)
install.packages("GSE5859")
data(GSE5859)
ge <- exprs(e) ##ge for gene expression
ge[,49]/log2(exp(1)) ## immitate error
mypar(1,1)
boxplot(ge,range=0,names=1:ncol(e),col=ifelse(1:ncol(ge)==49,1,2))
qs <- t(apply(ge,2,quantile,prob=c(0.05,0.025,0.5,0.75,0.95)))
matplot(qs,type="l",lty=1)
# kaboxplot!
# also smoothed histograms
mypar(1,1)
shist(ge,unit=0.5)
# MA-plot or Bland-Almond plot
x <- ge[,1]
y <- ge[,2]
mypar(1,2)
plot(x,y)
plot((x+y)/2,x-y)
sd(y-x)
savehistory("~/repos/RLifeSciences/worklog4.r")
