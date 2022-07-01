TAB<-read.table("Worms.txt", sep="\t", header=T)
TAB

Y<-TAB[,2]
X1<-TAB$IFT_Herb
X2<-TAB$IFT_Fong
X3<-TAB$IFT_Insec

summary(glm(Y~X1+X2+X3, family="poisson")))
summary(glm(Y~X1+X2, family="poisson")))
summary(glm(Y~X1, family="poisson"))