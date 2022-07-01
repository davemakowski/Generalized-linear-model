DATA<-read.table("Worms.txt",header=T, sep="\t")
DATA
summary(DATA)

par(mfrow=c(2,2))

plot(DATA$IFT_Fong,DATA$Allolobophora.chlorotica,xlab="IFT", ylab="#Vers de terre")
plot(DATA$IFT_Herb,DATA$Allolobophora.chlorotica,xlab="IFT", ylab="#Vers de terre")
plot(DATA$IFT_Insec,DATA$Allolobophora.chlorotica,xlab="IFT", ylab="#Vers de terre")
plot(DATA$IFT_tot,DATA$Allolobophora.chlorotica,xlab="IFT", ylab="#Vers de terre")


Mod0<-glm(Allolobophora.chlorotica~1, data=DATA, family="poisson")
summary(Mod0)
Mod1<-glm(Allolobophora.chlorotica~IFT_tot, data=DATA, family="poisson")
summary(Mod1)
Mod2<-glm(Allolobophora.chlorotica~IFT_Fong, data=DATA, family="poisson")
summary(Mod2)
Mod3<-glm(Allolobophora.chlorotica~IFT_Herb, data=DATA, family="poisson")
summary(Mod3)
Mod4<-glm(Allolobophora.chlorotica~IFT_Insec, data=DATA, family="poisson")
summary(Mod4)

AIC(Mod0, Mod1, Mod2, Mod3, Mod4)

par(mfrow=c(1,1))
plot(DATA$IFT_Herb,DATA$Allolobophora.chlorotica,xlab="IFT", ylab="#Vers de terre")

NombreVers<-predict(Mod3, newdata=data.frame(IFT_Herb=0:5),type="response")

lines(0:5, NombreVers, lwd=3, col="red")

predict(Mod3, newdata=data.frame(IFT_Herb=c(3.5, 1.75)), type="response")

exp(3.19-0.35*1.75)-exp(3.19-0.35*3.5)