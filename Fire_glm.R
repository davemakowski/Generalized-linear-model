TAB<-read.table("Fire_site_minus119.25plus48.75.txt", sep="\t", header=T)
Time<-1:nrow(TAB)

X=Time
Y=TAB$NumberFires
DATA<-data.frame(X,Y)
DATA<-na.omit(DATA)

par(mfrow=c(1,1))
plot(X,Y, xlab="Time (month)", ylab="Number of fires", pch=19)
Mod<-glm(Y~X, data=DATA, family="quasipoisson")
summary(Mod)
Pred<-predict(Mod,se.fit=T, type="response")
lines(DATA$X,Pred$fit, col="red", lwd=3)
lines(DATA$X,Pred$fit-1.96*Pred$se.fit, col="red", lwd=1, lty=2)
lines(DATA$X,Pred$fit+1.96*Pred$se.fit, col="red", lwd=1, lty=2)

library(MASS)

Model_nb<-glm.nb(Y~X, data=DATA)
summary(Model_nb)
