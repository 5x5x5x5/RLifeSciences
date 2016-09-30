# Exploratory Data Analysis
library(UsingR)
library(rafalib)
x <- father.son$fheight
ps <- (seq(0,99) + 0.5)/100
qs <- quantile(x,ps)
normalqs <- qnorm(ps,mean(x),popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ## identity line
qqnorm(x)
qqline(x)
library("rafalib", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("UsingR", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
x
ps
normalqs
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1)
qqnorm(x)
qqline(x)
n <- 1000
x <- rnorm(x)
qqnorm(x)
qqline(x)
dfs <- c(3,6,12,30)
mypar(2,2)
for(df in dfs){
x <- rt(1000,df)
qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
qqline(x)
}
# Boxplots
mypar(1,2)
hist(exec.pay) # in UsingR package
qqnorm(exec.pay)
qqlinr(exec.pay)
qqline(exec.pay)
boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))
# Scatterplots
data("father.son")
x=father.sons$height
x=father.son$height
x=father.son$fheight
y=father.som$sheight
y=father.son$sheight
plot(x,y,xlab="Father's height in inches",ylab="Son's height in inches",main=paste("correlation=",signif(cor(x,y),2)))
# Stratification
groups <- split(y,round(x))
boxplot(groups)
mypar(1)
boxplot(groups)
print(mean(y[round(x) == 72]))
groups <- plit(y,round(x))
groups <- split(y,round(x))
mypar(2,2)
for(i in c(5,8,11,14)){
qqnorm(groups[[i]], main=paste0("X=",names(groups)[i],"strata"), ylim=range(y),xlim=c(-2.5,2.5))
qqline(groups[[i]])
}
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y, round(x*4)/4,mean)
fatherheights=as.numeric(names(means))
mypar(1,1)
plot(fatherheights, means, ylab="average of strata son heights", ylim=range(fatherheights))
abline(0, corr(x,y))
abline(0, cor(x,y))
# Plots To Avoid
pie(browsers,main="Browser Usage (August 2013)")
browser
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig4.RData"
filename <- "fig4.RData"
if (!file.exists(filename))
dwnload(url,filename)
library(rafalib)
library(downloader)
if (!file.exists(filename))
download(url,filename)
load(filename)
mypar(1,2)
plot(x,y,lwd=2,type="n")
fit <- lm(y-x)
fit <- lm(y~x)
abline(fit$coef,lwd=2)
b <- round(fit$coed,4)
b <- round(fit$coef,4)
text(78, 200, paste("y =",b[1], "+", b[2], "x"), adj=c(0,0.5))
rhi <- round(cor(x,y),4)
text(78,187,expression(paste(rhi," = 0.8567")),adj=c(0,0.5))
plot(x,y,lwd=2)
fit <- lm(y~x)
abline(fir$coef,lwd=2)
abline(fit$coef,lwd=2)
# Comparison
set.seed(122011970)
before <- runif(6,5,8)
after <- rnorm(6, before*1.05, 2)
li <- range(c(before,after))
ymx <- max(abs(after-before))
mypar(1,2)
plot(before,after, xlab="Before", ylab="After", ylim=li, xlim=li)
plot(before, after=before, xlab="Before", ylim=c(-ymx, ymx), ylab="Change (After - Before)", lwd=2)
plot(before, after-before, xlab="Before", ylim=c(-ymx, ymx), ylab="Change (After - Before)", lwd=2)
plot(before, after-before, xlab="Before", ylim=c(-ymx, ymx), ylab="Change (After - Before)", lwd=2)
abline(h=0, lty=2, col=1)
z <- rep(c(0,1),rep(6,2))
mypar(1,2)
plot(z, c(before, after), xaxt="n", ylab="Response", xlab="", xlim=c(-0.5,1.5))
axis(side=1, at=c(0,1), c("Before","After"))
segments(rep(0,6), before, rep(1,6), after, col=1)
boxplot(before, after, names=c("Before","After"),ylab="Response")
# Avoid 3-D plots
filename <- "fig8dat.csv"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.RData"
if (!file.exists(filname))
download(url,filename)
if (!file.exists(filename))
download(url,filename)
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.csv"
if (!file.exists(filename))
download(url,filename)
plot(x[,1],x[,2],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1),type="l",lwd=2,col=1)
plot(x[,1],x[,1],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1),type="l",lwd=2,col=1)
x <- read.table(filename)
x <- read.table(filename, sep",", header=TRUE)
x <- read.table(filename, sep=",", header=TRUE)
plot(x[,1],x[,1],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1),type="l",lwd=2,col=1)
lines(x[,1],x[,3],lwd=2,col=2)
lines(x[,1],x[,4],lwd=2,col=3)
legend(1,0.4,c("Drug A","Drug B","Drug C"),lwd=2,col=1:3)
lines(x[,1],x[,2],lwd=2,col=1)
plot(x, y1, ylim=c(0,1),type="n", xlab="Dose", ylab="Response")
plot(x, y1, ylim=c(0,1),type="n", xlab="Dose", ylab="Response")
# Too many significant digits
heights <- cbind(rnorm(8,73,3),rnorm(8,73,3),rnorm(8,30,3), rnorm(8,78,3),rnorm(8,78,3))
colnames(heights) <- c("SG","PG","C"."PF","SF")
colnames(heights) <- c("SG","PG","C","PF","SF")
rownames(heights) <- paste("team".1:8)
rownames(heights) <- paste("team"=1:8)
paste("team",1:8)
heights
round(heights,1)
## Misunderstanding correlation (Advanced)
## Robust summaries
set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ## mistake made in 23rd measurement
boxplot(x)
cat("The average is",mean(x),"and the SD is",sd(x))
median(x)
y=c(rnorm(100,0,1))
mean(x)
mean(y)
## The mean absolute deviation
mad(x)
# Spearman correlation
set.seed(1)
x=c(rnorm(100,0,1)) ## real distribution with a surprise
x[23]
x[23] <- 100
y=c(rnorm(100,0,1)) # real distribution
y[23] <- 84 ## similar mistake in the 23rd measurement
library(rafalib)
mypar()
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)
mypar(1,2)
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(rank(x),rank(y),main=paste0("correlation=",round(corr(x,y,method="spearman"),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(rank(x),rank(y),main=paste0("correlation=",round(cor(x,y,method="spearman"),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(rank(x),rank(y),main=paste0("correlation=",round(cor(x,y,method="spearman"),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)
# Symmetry of log ratios
x <- 2^(rnorm(100))
y <- 2^(rnorm(100))
ratios <- x/y
hist(ratios)
savehistory("~/repos/RLifeSciences/Chappie2/worklog.r")
