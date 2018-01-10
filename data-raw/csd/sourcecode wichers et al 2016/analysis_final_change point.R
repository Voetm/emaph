getwd()
setwd("C:/Mijn documenten/algemeen/zonmw_doelmatigheid/case peter/EWS_peter")  

x=read.table('week_dep.txt',header=T)
layout(1:2)
plot(x$week,x$dep,type='b')
hist(x$dep)
l1=lm(dep~week,data=x)
summary(l1)
plot(l1)


l2=lm(dep~poly(week,2),data=x)
l3=lm(dep~poly(week,3),data=x)
anova(l1,l2)
anova(l2,l3)
anova(l1,l3)
summary(l3)

install.packages('ecp')
#James N.A., Matteson D.S. (2013). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data.
library(ecp)
e1=e.divisive(matrix(x$dep,,1),sig=.01,min.size=5)
print(e1)


#plot(x$week,x$dep,type='b',col=e1$cluster,pch=e1$cluster,xlab='week',ylab='SLC-90',bty='n')
postscript('changepoint.ps',wi=8,he=5,hor=F)
plot(x$week,x$dep,type='b',pch=(e1$cluster-1)*16+1,xlab='Week',ylab='SLC-90',bty='n')
dev.off()

pdf('changepoint2.pdf',wi=8,he=5)
plot(x$week,x$dep,type='b',pch=(e1$cluster-1)*16+1,xlab='Week',ylab='SLC-90',bty='n')
dev.off()


e1$p.values  # but second not sig.