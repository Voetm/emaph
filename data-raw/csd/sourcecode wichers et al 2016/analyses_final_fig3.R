#This analysis produces Figure 3 - this is computed on the smoothed data of marieke. The fist measure that after the jump is 31-12-2012 and this corresponds to row 903 in the dataset; this is the location of the abline.

require(mgcv)
library(foreign)
library(ggplot2)
library(psych)
library(qgraph)
library(TTR)

data=read.table(file="SmoothedDataSM.txt",header=T)
data[1:10,]
cor(data) #pa has to be rescored
data$pa=data$pa*-1 #recodepa
cor(data) #now everything correlates positively

#Lag the data
n=dim(data)[1]
tt=1:n
y=data[,1:5]
ydep=y[2:n,]
yL=y[1:(n-1),]

#Average lag-0 correlation over a rolling window of 300 timepoints.

w=300
corlag0=lapply(1:(n-w), function(i)mean(cor(y[i:(i+w),],use="pairwise.complete.obs")))
par(mfrow=c(1,1))
interitem=plot(as.numeric(corlag0),type="l",ylim=c(0,1),xlim=c(0,1473-w), main="Mean interitem correlation (lag-0) over time rises")
abline(v=(825-w))
abline(v=(903-w))
#Obviously the average lag 0 correlation rises over time

#Same for the lag-1 correlations
ydep[1:10,]
yL[650:750,]
i=650
cor(ydep[i:(i+w),],yL[i:(i+w),],use="pairwise.complete.obs")
mean(cor(ydep[i:(i+w),],yL[i:(i+w),],use="pairwise.complete.obs"))
corlag1=lapply(1:(n-w), function(i)mean(cor(ydep[i:(i+w),],yL[i:(i+w),],use="pairwise.complete.obs")))
lag1cors=plot(as.numeric(corlag1),type="l",ylim=c(0,1),xlim=c(0,1473-w), main="Mean lag-1 correlation over time rises")
abline(v=(825-w))
abline(v=(903-w))
#These rise as well

#A regression of t on t-1 on a rolling window of 300 timepoints. At each run we compute the R2 of the total score at t from the vector of individual item scores at t-1. We plot the R2 of this regression; this literally shows the changes in predictability of the system.
r2lag1=lapply(1:(n-w), function(i)summary(lm(rowSums(ydep[i:(i+w),])~yL[i:(i+w),1]+yL[i:(i+w),2]+yL[i:(i+w),3]+yL[i:(i+w),4]+yL[i:(i+w),5]))$r.squared)
lag1r2=plot(as.numeric(r2lag1),type="l",ylim=c(0,1),xlim=c(0,1473-w), main="Mean lag-1 R2 (multivariate predictability) over time rises")
abline(v=(825-w))
abline(v=(903-w))
#This rises as well, but decreases a little after the jump.

#Then everything together makes Figure 3
interitem=plot(as.numeric(corlag0),type="l",ylim=c(0,0.7),xlim=c(0,1473-w),col="red", main="Early Warning Signals",ylab="",xlab="time")
lag1cors=lines(as.numeric(corlag1),type="l",ylim=c(0,1),xlim=c(0,1473-w),col="blue")
lag1r2=lines(as.numeric(r2lag1),type="l",ylim=c(0,1),xlim=c(0,1473-w),col="purple")
abline(v=903-w,col="gray")
legend(-20,0.7,c("Mean lag-0 correlation", "Mean lag-1 correlation","Mean R-squared"),
lty=c(1,1,1),
lwd=c(2.5,2.5,2.5),col=c("red","blue","purple"))
#Obviously everything goes up




