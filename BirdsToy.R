Birds<-c(30, 29, 35, 32, 28, 31, 39, 20, 25, 31, 31, 18, 21, 21)
System<-c("o", "o", "o", "o", "o", "o","o","c", "c", "c", "c", "c", "c","c")

data.frame(System, Birds)

Mod<-glm(Birds~System, family=poisson)
summary(Mod)

predict(Mod, type="response", se.fit=T)

par(mfrow=c(1,1))

plot(Birds~as.factor(System), xlab="System", ylab="Number of birds")

points(c(1,2),c(23.65, 32), pch=19, cex=1.5, col="red")

lines(c(1,1),c(23.65-1.96*1.85, 23.65+1.96*1.85), col="red")
lines(c(2,2),c(32-1.96*2.14, 32+1.96*2.14), col="red")