DATA<-read.table("Oiseaux.txt",header=T)
DATA$Presence
summary(DATA)
table(DATA$Presence)
boxplot(DATA$HauteurHerbe~DATA$Presence)
Mod0<-glm(Presence~1, data=DATA, family="binomial")
summary(Mod0)
Mod<-glm(Presence~HauteurHerbe, data=DATA, family="binomial")
summary(Mod)

Proba<-exp(-1.16-0.031*1:60)/(1+exp(-1.16-0.031*1:60))

Proba<-predict(Mod, newdata=data.frame(HauteurHerbe=1:60),type="response")

plot(1:60, Proba, xlab="Hauteur herbe (cm)", ylab="Probabilité d'avoir un oiseau", type="l", lwd=10, col="red")