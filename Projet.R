library(readxl)
library(FactoMineR)
library(factoextra)

data <- read_excel("TP4_covC1234_DS19_20.xlsx")

data <- data[,2:19]

View(data)

#1-a

summary(data)

quantitative <- data[,1:14]

quantitative <- apply(quantitative,2,as.numeric)

moy <- colMeans(quantitative)

ones = rep(1, nrow(quantitative)) 
Mean = ones %*% t(moy)

XC <- (quantitative-Mean)
n <- length(quantitative[,1])

V <- (1/n)*t(XC)%*%XC

sd <- sqrt(diag(V))

#PremiÃ¨re campagne
quantitativeC1 <- data[data$Campagne=="BF2",1:14]

quantitativeC1 <- apply(quantitativeC1,2,as.numeric)

moyC1 <- colMeans(quantitativeC1)

ones = rep(1, nrow(quantitativeC1)) 
MeanC1 = ones %*% t(moyC1)

XCC1 <- (quantitativeC1-MeanC1)
nC1 <- length(quantitativeC1[,1])

VC1 <- (1/nC1)*t(XCC1)%*%XCC1

sdC1 <- sqrt(diag(VC1))

#2Ã©me campagne
quantitativeC2 <- data[data$Campagne=="BF3",1:14]

quantitativeC2 <- apply(quantitativeC2,2,as.numeric)

moyC2 <- colMeans(quantitativeC2)

ones = rep(1, nrow(quantitativeC2)) 
MeanC2 = ones %*% t(moyC2)

XCC2 <- (quantitativeC2-MeanC2)
nC2 <- length(quantitativeC2[,1])

VC2 <- (1/nC2)*t(XCC2)%*%XCC2

sdC2 <- sqrt(diag(VC2))

#3Ã©me campagne
quantitativeC3 <- data[data$Campagne=="CA1",1:14]

quantitativeC3 <- apply(quantitativeC3,2,as.numeric)

moyC3 <- colMeans(quantitativeC3)

ones = rep(1, nrow(quantitativeC3)) 
MeanC3 = ones %*% t(moyC3)

XCC3 <- (quantitativeC3-MeanC3)
nC3 <- length(quantitativeC3[,1])

VC3 <- (1/nC3)*t(XCC3)%*%XCC3

sdC3 <- sqrt(diag(VC3))

#4Ã©me campagne
quantitativeC4 <- data[data$Campagne=="CA2",1:14]

quantitativeC4 <- apply(quantitativeC4,2,as.numeric)

moyC4 <- colMeans(quantitativeC4)

ones = rep(1, nrow(quantitativeC4)) 
MeanC4 = ones %*% t(moyC4)

XCC4 <- (quantitativeC4-MeanC4)
nC4 <- length(quantitativeC4[,1])

VC4 <- (1/nC4)*t(XCC4)%*%XCC4

sdC4 <- sqrt(diag(VC4))

#5Ã©me campagne
quantitativeC5 <- data[data$Campagne=="CA3",1:14]

quantitativeC5 <- apply(quantitativeC5,2,as.numeric)

moyC5 <- colMeans(quantitativeC5)

ones = rep(1, nrow(quantitativeC5)) 
MeanC5 = ones %*% t(moyC5)

XCC5 <- (quantitativeC5-MeanC5)
nC5 <- length(quantitativeC5[,1])

VC5 <- (1/nC5)*t(XCC5)%*%XCC5

sdC5 <- sqrt(diag(VC5))

#6Ã©me campagne
quantitativeC6 <- data[data$Campagne=="CA4",1:14]

quantitativeC6 <- apply(quantitativeC6,2,as.numeric)

moyC6 <- colMeans(quantitativeC6)

ones = rep(1, nrow(quantitativeC6)) 
MeanC6 = ones %*% t(moyC6)

XCC6 <- (quantitativeC6-MeanC6)
nC6 <- length(quantitativeC6[,1])

VC6 <- (1/nC6)*t(XCC6)%*%XCC6

sdC6 <- sqrt(diag(VC6))

#1-b

#en hiver
quantitativehiver <- data[data$SAISON=="hiver",1:14]

quantitativehiver <- apply(quantitativehiver,2,as.numeric)

moyhiver <- colMeans(quantitativehiver)

ones = rep(1, nrow(quantitativehiver)) 
Meanhiver = ones %*% t(moyhiver)

XChiver <- (quantitativehiver-Meanhiver)
nhiver <- length(quantitativehiver[,1])

Vhiver <- (1/nhiver)*t(XChiver)%*%XChiver

sdhiver <- sqrt(diag(Vhiver))

#en Ã©tÃ©
quantitativeete <- data[data$SAISON=="été",1:14]

quantitativeete <- apply(quantitativeete,2,as.numeric)

moyete <- colMeans(quantitativeete)

ones = rep(1, nrow(quantitativeete)) 
Meanete = ones %*% t(moyete)

XCete <- (quantitativeete-Meanete)
nete <- length(quantitativeete[,1])

Vete <- (1/nete)*t(XCete)%*%XCete

sdete <- sqrt(diag(Vete))

#avant ouverture

quantitativeav <- rbind(data[data$Campagne=="BF2",1:14],data[data$Campagne=="BF3",1:14])

quantitativeav <- apply(quantitativeav,2,as.numeric)

moyav <- colMeans(quantitativeav)

ones = rep(1, nrow(quantitativeav)) 
Meanav = ones %*% t(moyav)

XCav <- (quantitativeav-Meanav)
nav <- length(quantitativeav[,1])

Vav <- (1/nav)*t(XCav)%*%XCav

sdav <- sqrt(diag(Vav))

#aprÃ¨s ouverture

quantitativeap <- rbind(data[data$Campagne=="CA1",1:14],data[data$Campagne=="CA2",1:14],data[data$Campagne=="CA3",1:14],data[data$Campagne=="CA4",1:14])

quantitativeap <- apply(quantitativeap,2,as.numeric)

moyap <- colMeans(quantitativeap)

ones = rep(1, nrow(quantitativeap)) 
Meanap = ones %*% t(moyap)

XCap <- (quantitativeap-Meanap)
nap <- length(quantitativeap[,1])

Vap <- (1/nap)*t(XCap)%*%XCap

sdap <- sqrt(diag(Vap))

#2

#moyenne / campagnes
E <- c(moy[3],moyC1[3],moyC2[3],moyC3[3],moyC4[3],moyC5[3],moyC6[3])
ane <- c(moy[8],moyC1[8],moyC2[8],moyC3[8],moyC4[8],moyC5[8],moyC6[8])
BTM <- c(moy[10],moyC1[10],moyC2[10],moyC3[10],moyC4[10],moyC5[10],moyC6[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","BF2","BF3","CA1","CA2","CA3","CA4")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Moyennes",
        xlab = "Campagnes")

#ecarts-type / campagnes
E <- c(sd[3],sdC1[3],sdC2[3],sdC3[3],sdC4[3],sdC5[3],sdC6[3])
ane <- c(sd[8],sdC1[8],sdC2[8],sdC3[8],sdC4[8],sdC5[8],sdC6[8])
BTM <- c(sd[10],sdC1[10],sdC2[10],sdC3[10],sdC4[10],sdC5[10],sdC6[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","BF2","BF3","CA1","CA2","CA3","CA4")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Ã©carts-type",
        xlab = "Campagnes")


#moyenne / saison
E <- c(moy[3],moyhiver[3],moyete[3])
ane <- c(moy[8],moyhiver[8],moyete[8])
BTM <- c(moy[10],moyhiver[10],moyete[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Hiver","été")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "moyennes",
        xlab = "Saisons")


#ecarts-types / saison
E <- c(sd[3],sdhiver[3],sdete[3])
ane <- c(sd[8],sdhiver[8],sdete[8])
BTM <- c(sd[10],sdhiver[10],sdete[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Hiver","été")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Ã©carts-type",
        xlab = "Saisons")

#moyenne / av,ap
E <- c(moy[3],moyav[3],moyap[3])
ane <- c(moy[8],moyav[8],moyap[8])
BTM <- c(moy[10],moyav[10],moyap[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Avant ouverture","AprÃ¨s ouverture")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "moyennes",
        xlab = "Avant/AprÃ¨s ouverture",
        title = "moyennes en fct de avant/aprÃ¨s ouverture")

#ecarts-type / av,ap
E <- c(sd[3],sdav[3],sdap[3])
ane <- c(sd[8],sdav[8],sdap[8])
BTM <- c(sd[10],sdav[10],sdap[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Avant ouverture","AprÃ¨s ouverture")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Ã©carts-type",
        xlab = "Avant/AprÃ¨s ouverture",
        title = "Ecarts-type en fct de avant/aprÃ¨s ouverture")

########################
## PARTIE 2 ##
########################
AnalyseACP<-function(da){
  moy <- colMeans(da)
  ones = rep(1, nrow(da)) 
  Mean = ones %*% t(moy)
  XC <- (da-Mean)
  n <- length(da[,1])
  library(matrixStats)
  SD <-colSds(XC)
  SD <- ones %*% t(SD)
  daCR<-XC/SD
  
  VCR<-var(daCR)
  ACPR<-eigen(VCR)
  VecteursPropresR<-ACPR$vectors
  ValeursPropresR<-ACPR$values
  
  
  InertieR<-ACPR$values
  InertieCumuleeR<-rep(0,14)
  for (i in 1:14) {
    InertieCumuleeR[i]<-sum(ACPR$values[1:i])
  }
  
  barplot(t(as.matrix(cbind(InertieR,InertieCumuleeR))),
          beside = TRUE,
          legend.text = TRUE,
          angle=TRUE,
          ylab = "Inertie",
          xlab = "Vecteurs Propres")
  
  # Projection sur les nouvelles coordonnées
  NewdaCR <- daCR%*%VecteursPropresR
  # print(NewdaCR)
  
  plot(NewdaCR[,1],NewdaCR[,2])
  
  # RIn = Inertie cumulée jusqu'à i / Inertie totale
  RIn<- function(i){
    A<-sum((ValeursPropresR))
    B<-sum((ValeursPropresR[1:i]))
    return(B/A)
  }
  print("Inertie cumulée jusqu'à i / Inertie totale")
  print(RIn(2))
  
  #Definition de la fonction Q(nn,i,k) avc nn la matrice des données
  Qual<- function(nn,i,k){
    A<-sum((nn[i,]^2))
    B<-sum((nn[i,1:k]^2))
    return(B/A)
  }
  
  KR<-0
  for (j in 1:length(da[,1])) {
    KR<-KR+Qual(NewdaCR,j,3)
  }
  KR<-KR/length(da[,1])
  print("qualité des projections")
  print(KR)
        
  coeffR<- function(i,j){
    return(cor(NewdaCR[,i],daCR[,j]))
  }
  R<-matrix(0,14,14)
  
  for (i in 1:14) {
    for (j in 1:14) {
      R[i,j]<-coeffR(i,j)
    }
  }
  # print(R)
  
  colnames(R) <- c("B","T","E","X","9_ane","10_ane","13_ane","14_ane","1_M_2_PA","BTM","FormicAcid","aceticacid","NonaDecanoicAc","Tot_OcNoDecana")
  barplot(sqrt(R[1,]^2+R[2, ]^2),
          axisnames = FALSE,
          col=rainbow(14),
          legend=colnames(R),
          xlim=c(0,27),
          ylim = c(0,1))
  
  # plot(NewdaCR[,1], NewdaCR[,2])
  # library(rgl)
  # open3d()
  # plot3d(NewdaCR[,1], NewdaCR[,2], NewdaCR[,3],col = rainbow(140))
  # for (i in 1:14) {
  #   print(sqrt(sum(R[1:2,i]^2)))
  # }
  
}


AnalyseACP(quantitative)

#Si on choisit deux axes principaux on obtient un coefficient supérieur à 66% 
#On peut voir aussi que c'est cohérent avec la méthode du coude puisqu'on remarque un point
#d'inflexion sur la deuxieme barre

#Q5
# On exécute une analyse ACP sur les données avant, après puis les données hiver/été.

AnalyseACP(quantitativeav)
AnalyseACP(quantitativeap)

# On remarque que Inertie cumulée jusqu'à 2 / Inertie totale est supériere à 66%
# On peut voir à partir du dernier graphe une difference entre les données avant et les
# données après:
# Remarques:
# - Avant: B et 14_ane ont un petit pourcentage alors qu'après elles represente presque toute la totalité
# # des données.
# - Après: aceticacid est très petit et NonaDecanoicAc et FormicAcid depassent à peine la moitié
# alors qu'avant il avaient un pourcentage assez grand.

# On peut dire que les pourcentages de aceticacid ,NonaDecanoicAc ,FormicAcid ,B et 14_ane
# forment une signature des données après et avant.
AnalyseACP(quantitativeete)
AnalyseACP(quantitativehiver)

# Remarques:
# - En été: FormicAcid ,aceticacid  ont un très petit pourcentage
# alors qu'en hiver elles representent presque la moitié données.
# - En hivee: NonaDecanoicAc et Tot_OcNoDecana deviennent plus petit par rapport à l'été

# On peut dire que les pourcentages de FormicAcid ,aceticacid, NonaDecanoicAc
# et Tot_OcNoDecana forment une signature des données en été et en hiver.
