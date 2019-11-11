# script pour AFDM sur les donn�es de COV
# TP4 - octobre 2019
# a completer selon votre travail 
# M batton-Hubert UP1 Analyse factorielle donn�es mixtes
#------------------------------------------------------------
#chargement du package
library(readxl)
library(FactoMineR)
data <- read_excel("TP4_covC1234_DS19_20.xlsx")
View(data)
#lancement de la procédure FAMD
afdm.cov<-FAMD(data,ncp=2)
#affichage des résultats
print(summary(afdm.cov))
plot(afdm.cov,choix ="ind",habillage = 16)
# tester si vous ne prenez que les 14 premieres variables ...
# que sur les 4 derniere variables ..
# tester ces affichages par exemple 
round(afdm.cov$var$coord[,1:2],2)
round(afdm.cov$eig,2)
round(afdm.cov$ind$dist,2)
round(afdm.cov$ind$contrib[,1:2],2)
round(afdm.cov$var$contrib[,1:2],2)
lapply(dimdesc(afdm.cov),lapply,round,2)
#...




