#source("f:\\David\\Projets\\LivreStat\\Programmes\\Exemple3\\ProgExemple3.txt")

TAB<-read.table('Oiseaux.txt',header=T)

print(TAB)

HH<-TAB$HauteurHerbe
Pres<-TAB$Presence

print(summary(HH[Pres==0]))
print(summary(HH[Pres==1]))
print(length(HH[Pres==0]))
print(length(HH[Pres==1]))

par(mfrow=c(2,2))
hist(HH[Pres==0])
hist(HH[Pres==1])
plot(density(HH[Pres==0]))
plot(density(HH[Pres==1]))

par(mfrow=c(1,1))
boxplot(HH~Pres, xlab="Presence/absence",ylab="Hauteur d'herbe (cm)")

Data<-data.frame(HH,Pres)

Mod.Log<-glm(Pres~HH, data=Data, family=binomial)

print(summary(Mod.Log))

print(anova(Mod.Log))
plot(Data$HH,predict(Mod.Log, type="response"))

HH.test<-1:70

Prob.Presence<-exp(Mod.Log$coefficients[1]+Mod.Log$coefficients[2]*HH.test)/(1+exp(Mod.Log$coefficients[1]+Mod.Log$coefficients[2]*HH.test))


par(mfrow=c(1,2))
hist(HH[Pres==0], xlab="Hauteur d'herbe (cm) lorsque y=0", ylab="Frequence", main=" ", cex=1.5)
hist(HH[Pres==1], xlab="Hauteur d'herbe (cm) lorsque y=1", ylab="Frequence", main=" ", cex=1.5)

X11()

plot(HH.test,Prob.Presence,xlim=c(5,60),type="l",xlab="Hauteur herbe (cm)", ylab="Probabilite de presence")

Pred<-predict(Mod.Log, type="response",se.fit=T)
PredSup<-Pred$fit+1.96*Pred$se.fit
PredInf<-Pred$fit-1.96*Pred$se.fit
plot(HH[order(HH)],Pred$fit[order(HH)],  col="red", type="l", lwd=3,ylim=c(0,0.5))
lines(HH[order(HH)],PredSup[order(HH)],col="blue",lty=2,lwd=2)
lines(HH[order(HH)],PredInf[order(HH)],col="blue",lty=2,lwd=2)
