S=c(1100, 5602, 100551, 99090, 32218, 2401, 7123,66612)
X=c(14, 8, 19, 18, 11, 10, 9, 21)
Y=c(0, 11, 2018, 990, 671, 65, 22, 2387)

par(mfrow=c(1,2))
plot(X,Y, pch=19)
plot(X,Y/S, pch=19)

Mod_1=glm(Y~X, family=poisson)
summary(Mod_1)

Mod_2=glm(Y~X+ offset(log(S)), family=poisson)
summary(Mod_2)

Mod_3=glm(Y~X, family=quasipoisson)
summary(Mod_3)

Mod_4=glm(Y~X+offset(log(S)), family=quasipoisson)
summary(Mod_4)