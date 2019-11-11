library(readxl)
library(FactoMineR)
library(factoextra)

data <- read_excel("TP4_covC1234_DS19_20.xlsx")

data <- data[,2:19]


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

#Première campagne
quantitativeC1 <- data[data$Campagne=="BF2",1:14]

quantitativeC1 <- apply(quantitativeC1,2,as.numeric)

moyC1 <- colMeans(quantitativeC1)

ones = rep(1, nrow(quantitativeC1)) 
MeanC1 = ones %*% t(moyC1)

XCC1 <- (quantitativeC1-MeanC1)
nC1 <- length(quantitativeC1[,1])

VC1 <- (1/nC1)*t(XCC1)%*%XCC1

sdC1 <- sqrt(diag(VC1))

#2éme campagne
quantitativeC2 <- data[data$Campagne=="BF3",1:14]

quantitativeC2 <- apply(quantitativeC2,2,as.numeric)

moyC2 <- colMeans(quantitativeC2)

ones = rep(1, nrow(quantitativeC2)) 
MeanC2 = ones %*% t(moyC2)

XCC2 <- (quantitativeC2-MeanC2)
nC2 <- length(quantitativeC2[,1])

VC2 <- (1/nC2)*t(XCC2)%*%XCC2

sdC2 <- sqrt(diag(VC2))

#3éme campagne
quantitativeC3 <- data[data$Campagne=="CA1",1:14]

quantitativeC3 <- apply(quantitativeC3,2,as.numeric)

moyC3 <- colMeans(quantitativeC3)

ones = rep(1, nrow(quantitativeC3)) 
MeanC3 = ones %*% t(moyC3)

XCC3 <- (quantitativeC3-MeanC3)
nC3 <- length(quantitativeC3[,1])

VC3 <- (1/nC3)*t(XCC3)%*%XCC3

sdC3 <- sqrt(diag(VC3))

#4éme campagne
quantitativeC4 <- data[data$Campagne=="CA2",1:14]

quantitativeC4 <- apply(quantitativeC4,2,as.numeric)

moyC4 <- colMeans(quantitativeC4)

ones = rep(1, nrow(quantitativeC4)) 
MeanC4 = ones %*% t(moyC4)

XCC4 <- (quantitativeC4-MeanC4)
nC4 <- length(quantitativeC4[,1])

VC4 <- (1/nC4)*t(XCC4)%*%XCC4

sdC4 <- sqrt(diag(VC4))

#5éme campagne
quantitativeC5 <- data[data$Campagne=="CA3",1:14]

quantitativeC5 <- apply(quantitativeC5,2,as.numeric)

moyC5 <- colMeans(quantitativeC5)

ones = rep(1, nrow(quantitativeC5)) 
MeanC5 = ones %*% t(moyC5)

XCC5 <- (quantitativeC5-MeanC5)
nC5 <- length(quantitativeC5[,1])

VC5 <- (1/nC5)*t(XCC5)%*%XCC5

sdC5 <- sqrt(diag(VC5))

#6éme campagne
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

#en été
quantitativeete <- data[data$SAISON=="?t?",1:14]

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

#après ouverture

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
        ylab = "écarts-type",
        xlab = "Campagnes")


#moyenne / saison
E <- c(moy[3],moyhiver[3],moyete[3])
ane <- c(moy[8],moyhiver[8],moyete[8])
BTM <- c(moy[10],moyhiver[10],moyete[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Hiver","?t?")

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
rownames(M) <- c("tout","Hiver","?t?")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "écarts-type",
        xlab = "Saisons")

#moyenne / av,ap
E <- c(moy[3],moyav[3],moyap[3])
ane <- c(moy[8],moyav[8],moyap[8])
BTM <- c(moy[10],moyav[10],moyap[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Avant ouverture","Après ouverture")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "moyennes",
        xlab = "Avant/Après ouverture",
        title = "moyennes en fct de avant/après ouverture")

#ecarts-type / av,ap
E <- c(sd[3],sdav[3],sdap[3])
ane <- c(sd[8],sdav[8],sdap[8])
BTM <- c(sd[10],sdav[10],sdap[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Avant ouverture","Après ouverture")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "écarts-type",
        xlab = "Avant/Après ouverture",
        title = "Ecarts-type en fct de avant/après ouverture")

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
  print(ValeursPropresR)
  
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
  
  # Projection sur les nouvelles coordonn?es
  NewdaCR <- daCR%*%VecteursPropresR
  # print(NewdaCR)
  
  # RIn = Inertie cumul?e jusqu'? i / Inertie totale
  RIn<- function(i){
    A<-sum((ValeursPropresR))
    B<-sum((ValeursPropresR[1:i]))
    return(B/A)
  }
  print("Inertie cumul?e jusqu'? i / Inertie totale")
  print(RIn(2))
  
  #Definition de la fonction Q(nn,i,k) avc nn la matrice des donn?es
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
  print("qualit? des projections")
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
          col=rainbow(14),
          names.arg=FALSE,
          legend.text=colnames(R),
          xlim=c(0,25),
          ylim=c(0,1)
          
  )
  
  # plot(NewdaCR[,1], NewdaCR[,2])
  # library(rgl)
  # open3d()
  # plot3d(NewdaCR[,1], NewdaCR[,2], NewdaCR[,3],col = rainbow(140))
  # for (i in 1:14) {
  #   print(sqrt(sum(R[1:2,i]^2)))
  # }
  
}
AnalyseACP(quantitative)

#Si on choisit deux axes principaux on obtient un coefficient sup?rieur ? 66% 
#On peut voir aussi que c'est coh?rent avec la m?thode du code puisqu'on remarque un point
#d'inflexion sur la deuxieme barre

#Q5
# On ex?cute une analyse ACP sur les donn?es avant, apr?s puis les donn?es hiver/?t?.

AnalyseACP(quantitativeav)
AnalyseACP(quantitativeap)

# On remarque que Inertie cumul?e jusqu'? 2 / Inertie totale est sup?riere ? 66%
# On peut voir ? partir du dernier graphe une difference entre les donn?es avant et les
# donn?es apr?s:
# Remarques:
# - Avant: B et 14_ane ont un petit pourcentage alors qu'apr?s elles represente presque toute la totalit?
# # des donn?es.
# - Apr?s: aceticacid est tr?s petit et NonaDecanoicAc et FormicAcid depassent ? peine la moiti?
# alors qu'avant il avaient un pourcentage assez grand.

# On peut dire que les pourcentages de aceticacid ,NonaDecanoicAc ,FormicAcid ,B et 14_ane
# forment une signature des donn?es apr?s et avant.
AnalyseACP(quantitativeete)
AnalyseACP(quantitativehiver)

# Remarques:
# - En ?t?: FormicAcid ,aceticacid  ont un tr?s petit pourcentage
# alors qu'en hiver elles representent presque la moiti? donn?es.
# - En hivee: NonaDecanoicAc et Tot_OcNoDecana deviennent plus petit par rapport ? l'?t?

# On peut dire que les pourcentages de FormicAcid ,aceticacid, NonaDecanoicAc
# et Tot_OcNoDecana forment une signature des donn?es en ?t? et en hiver.

#Q6

# par(mfrow=c(1,2))

#moyenne / saison
E <- c(moy[3],moyhiver[3],moyete[3])
ane <- c(moy[8],moyhiver[8],moyete[8])
BTM <- c(moy[10],moyhiver[10],moyete[10])
aceticacid <- c(moy[12],moyhiver[12],moyete[12])
formicacid <- c(moy[11],moyhiver[11],moyete[11])
NonaDecanoicAc <- c(moy[13],moyhiver[13],moyete[13])

M <- as.matrix(cbind(E,ane,BTM,aceticacid,formicacid,NonaDecanoicAc))
colnames(M) <- c("E","14_ane","BTM","Aceticacid","Formicacid","NonaDecanoicAc")
rownames(M) <- c("tout","Hiver","Eté")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "moyennes",
        xlab = "Saisons",
        main = "moyennes en fct de la saison",
        col = c(1,2,3,4,5,7))


#ecarts-types / saison
E <- c(sd[3],sdhiver[3],sdete[3])
ane <- c(sd[8],sdhiver[8],sdete[8])
BTM <- c(sd[10],sdhiver[10],sdete[10])
aceticacid <- c(sd[12],sdhiver[12],sdete[12])
formicacid <- c(sd[11],sdhiver[11],sdete[11])
NonaDecanoicAc <- c(sd[13],sdhiver[13],sdete[13])

M <- as.matrix(cbind(E,ane,BTM,aceticacid,formicacid,NonaDecanoicAc))
colnames(M) <- c("E","14_ane","BTM","Aceticacid","Formicacid","NonaDecanoicAc")
rownames(M) <- c("tout","Hiver","Eté")

barplot(t(M),
        beside = TRUE,
        ylab = "écarts-type",
        xlab = "Saisons",
        main = "écarts-type en fct de la saison",
        col = c(1,2,3,4,5,7))

##7)
#nombre d'observations
n <- 140

# Calcul de B
B <- (1/n)*(83*(moyhiver-moy)%*%t(moyhiver-moy)+57*(moyete-moy)%*%t(moyete-moy))

# Calcul de W
n1 <- 83
Whiver <- (quantitativehiver-Meanhiver)
W1 <- (1/n1)*t(Whiver)%*%Whiver
#variance intraclasse pour la modalité hiver


n2 <- 57
Wete <- (quantitativeete-Meanete)
W2 <- (1/n2)*t(Wete)%*%Wete
#variance intraclasse pour la modalité été


W <- (1/n)*(n1*W1+n2*W2)
# variance intra classe totale

#V-B-W
#vérification de la formule : c bien vérifié

#Décomposition pour assurer la diogonalisibité
C <- matrix(0,nrow=14,ncol=2)

C[,1] <- sqrt(n1/n)*(moyhiver-moy)
C[,2] <- sqrt(n2/n)*(moyete-moy)

#Matrice à diagonaliser
A <- t(C) %*% solve(V) %*% C

decomp1 <- eigen(A)
values1 <- decomp1$values
vectors1 <- decomp1$vectors

vectors1 <- solve(V) %*% C %*% vectors1

#Les nouvelles coordonnées des individus sur le nouveau plan factoriel
Cord <- matrix(0,nrow=140,ncol=4)
colnames(Cord) <- c("C1","C2","SAISON","binary")

Cord <- as.data.frame(Cord)

Cord[,1:2] <- quantitative %*% vectors1

Cord[,3] <- data$SAISON

#La colonne binaire pour différencier la couleur
Cord[which(Cord$SAISON == "hiver"),4] <- 4
Cord[which(Cord$SAISON == "été"),4] <- 2

plot(Cord$C1,Cord$C2,col=Cord$binary)
legend(1, y=-2e-14, legend=c("hiver", "été"),
       col=c(4, 2), lty=1, cex=0.8)

#Inertie
Inertie<-values1
InertieCumulee<-rep(0,length(values1))
for (i in 1:length(values1)) {
  InertieCumulee[i]<-sum(values1[1:i])
}

barplot(t(as.matrix(cbind(Inertie,InertieCumulee))),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Inertie",
        xlab = "Vecteurs Propres")


rapp = rep(1, 2)

for (i in 1:2) {
  v <- vectors1[,i]
  
  SCT <- t(v) %*% V %*% v 
  SCR <- t(v) %*% W %*% v
  SCE <- t(v) %*% B %*% v
  
  rapp[i] <- SCE/SCT
}

recap <- matrix(0,nrow=2,ncol=2)
colnames(recap) <- c("Axe1","Axe2")
rownames(recap) <- c("Valeur propre","critère éta")

recap[1,] <- values1
recap[2,] <- rapp

print(recap)

##8)

# Calcul de la variance interclasse
Bav <- (moyav-moy)%*%t(moyav-moy)
Bap <- (moyap-moy)%*%t(moyap-moy)

# Variance intraclasse deja calculée : Vav et Vap

#Variance totale/inter/intra avant et après ouverture du site
Eav <- c(V[3,3],Bav[3,3],Vav[3,3])
Eap <- c(V[3,3],Bap[3,3],Vap[3,3])
M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("E avant","E après")
rownames(M) <- c("Variance totale","Variance interclasse","Variance intraclasse")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "Variance en fct de avant/après ouverture du site de E",
        col = c(2,4))

#Le reste des variables est calculées de la même façon

par(mfrow=c(2,2))

i=10

Eav <- c(V[i,i],Bav[i,i],Vav[i,i])
Eap <- c(V[i,i],Bap[i,i],Vap[i,i])

M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("BTM avant","BTM après")
rownames(M) <- c("Totale","Inter","Intra")

barplot(t(M),
        beside = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "BTM",
        col = c(2,4))

i=12

Eav <- c(V[i,i],Bav[i,i],Vav[i,i])
Eap <- c(V[i,i],Bap[i,i],Vap[i,i])

M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("aceticacid avant","aceticacid après")
rownames(M) <- c("Totale","Inter","Intra")

barplot(t(M),
        beside = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "aceticacid",
        col = c(2,4))

i=11

Eav <- c(V[i,i],Bav[i,i],Vav[i,i])
Eap <- c(V[i,i],Bap[i,i],Vap[i,i])

M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("formicacid avant","formicacid après")
rownames(M) <- c("Totale","Inter","Intra")

barplot(t(M),
        beside = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "Formicacid",
        col = c(2,4))

i=13

Eav <- c(V[i,i],Bav[i,i],Vav[i,i])
Eap <- c(V[i,i],Bap[i,i],Vap[i,i])

M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("NonaDecanoicAc avant","NonaDecanoicAc après")
rownames(M) <- c("Totale","Inter","Intra")

barplot(t(M),
        beside = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "NonaDecanoicAc",
        col = c(2,4))

g<-colMeans(data[,1:14])
data[,1:14]<-scale(data[,1:14], center=TRUE, scale=TRUE) 

ones = rep(1, nrow(data)) 
Mean<-ones%*%t(g)
View(XX)
XX<-data[,1:14]
XX<-apply(XX,2,as.numeric)
V<-t(XX)%*%XX
C <- matrix(0,nrow=14,ncol=4)
Gk <- matrix(0,nrow=4,ncol=14)
Rural <- data[data$TYPE == "rural",1:14]
Urbain <- data[data$TYPE == "urbain",1:14]
Compostage <- data[data$TYPE == "compostage",1:14]
Sourceindustrie <- data[data$TYPE == "sourceindustrie",1:14]
Gk[1,] <- colMeans(Rural)
Gk[2,] <- colMeans(Urbain)
Gk[3,] <- colMeans(Compostage)
Gk[4,] <- colMeans(Sourceindustrie)
N<-length(data[,1])
n=rep(0,4)
Rural<-apply(Rural,2,as.numeric)
Urbain<-apply(Urbain,2,as.numeric)
Compostage<-apply(Compostage,2,as.numeric)
Sourceindustrie<-apply(Sourceindustrie,2,as.numeric)
n[1]<-length(Rural[,1])
n[2]<-length(Urbain[,1])
n[3]<-length(Compostage[,1])
n[4]<-length(Sourceindustrie[,1])

C[,1] <- sqrt(n[1]/N)*((Gk[1,]))
C[,2] <- sqrt(n[2]/N)*((Gk[2,]))
C[,3] <- sqrt(n[3]/N)*((Gk[3,]))
C[,4] <- sqrt(n[4]/N)*((Gk[4,]))
B<-matrix(0,14,14)
for (i in 1:4) {
  B<-B+n[i]*Gk[i,]%*%t(Gk[i,])
}
B<-(1/N)*B
eigen(B)
# Calcul de W
ones1<-rep(1,n[1])
ones2<-rep(1,n[2])
ones3<-rep(1,n[3])
ones4<-rep(1,n[4])
Mean1 = ones1 %*% t(Gk[1,])
Mean2 = ones2 %*% t(Gk[2,])
Mean3 = ones3 %*% t(Gk[3,])
Mean4 = ones4 %*% t(Gk[4,])
WC1<-(Rural -Mean1)
WC2<-(Urbain-Mean2)
WC3<-(Compostage -Mean3)
WC4<-(Sourceindustrie -Mean4)
W1<-t(WC1)%*%WC1
W2<-t(WC2)%*%WC2
W3<-t(WC3)%*%WC3
W4<-t(WC4)%*%WC4
W<-(1/N)*(W1+W2+W3+W4)


V_1<-solve(V)


# Resolution apres decomposition

d <- solve(V) %*% B

#Decomposition
par(mfrow=c(1,1))
A <- t(C) %*% V_1 %*% C
decomp1 <- eigen(A)
values1 <- decomp1$values
vectors1 <- decomp1$vectors
vectors1 <- V_1 %*% C %*% vectors1
print(values1)
Cord <- matrix(0,nrow=140,ncol=4)
View(vectors1)
Cord[,1:4] <- XX %*% vectors1
plot(Cord[,1],Cord[,2])

View(data)
