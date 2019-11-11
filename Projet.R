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
rownames(M) <- c("tout","Hiver","Eté")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "moyennes",
        xlab = "Saisons",
        title = "moyennes en fct de la saison")


#ecarts-types / saison
E <- c(sd[3],sdhiver[3],sdete[3])
ane <- c(sd[8],sdhiver[8],sdete[8])
BTM <- c(sd[10],sdhiver[10],sdete[10])

M <- as.matrix(cbind(E,ane,BTM))
colnames(M) <- c("E","14_ane","BTM")
rownames(M) <- c("tout","Hiver","Eté")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "écarts-type",
        xlab = "Saisons",
        title = "écarts-type en fct de la saison")

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

#2

#corrélogramme
Gamma <- cor(quantitative)
library(corrplot)
corrplot(Gamma, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "correlogramme des variables quantitatives")

pairs(data[,1:14], pch = 20)


par(mfrow=c(2,2))

#hiver
Gamma <- cor(quantitativehiver)
corrplot(Gamma, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "correlogramme en hiver")

#été
Gamma <- cor(quantitativeete)
corrplot(Gamma, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "correlogramme en été")

#avant ouverture
Gamma <- cor(quantitativeav)
corrplot(Gamma, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "correlogramme avant ouverture")


#après ouverture
Gamma <- cor(quantitativeap)
corrplot(Gamma, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, title = "correlogramme après ouverture")


#Etape 3

# 7

#nombre d'observations
n <- 140

# Calcul de B
B<-(1/n)*(83*(moyhiver-moy)%*%t(moyhiver-moy)+57*(moyete-moy)%*%t(moyete-moy))

# Calcul de W
n1 <- 83
Whiver<-(quantitativehiver-Meanhiver)
W1<-(1/n1)*t(Whiver)%*%Whiver
#variance intraclasse pour la modalité hiver


n2 <- 57
Wete<-(quantitativeete-Meanete)
W2<-(1/n2)*t(Wete)%*%Wete
#variance intraclasse pour la modalité été


W<-(1/n)*(n1*W1+n2*W2)
# variance intra classe totale

#V-B-W
#vérification de la formule : c bien vérifié

#Decomposition
C <- matrix(0,nrow=14,ncol=2)

C[,1] <- sqrt(n1/n)*(moyhiver-moy)
C[,2] <- sqrt(n2/n)*(moyete-moy)

#Pour assurer la diagonalisibité
B - C %*% t(C)

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

#8

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
        main = "Variance en fct de avant/après ouverture du site",
        col = c(2,4))

i=8

Eav <- c(V[i,i],Bav[i,i],Vav[i,i])
Eap <- c(V[i,i],Bap[i,i],Vap[i,i])

M <- as.matrix(cbind(Eav,Eap))
colnames(M) <- c("14_ane avant","14_ane après")
rownames(M) <- c("Variance totale","Variance interclasse","Variance intraclasse")

barplot(t(M),
        beside = TRUE,
        legend.text = TRUE,
        ylab = "Variance",
        xlab = "Type de variance",
        main = "Variance en fct de avant/après ouverture du site de 14_ane",
        col = c(2,4))


BTM <- c(sd[10],sdhiver[10],sdete[10])
aceticacid <- c(sd[12],sdhiver[12],sdete[12])
formicacid <- c(sd[11],sdhiver[11],sdete[11])
NonaDecanoicAc <- c(sd[13],sdhiver[13],sdete[13])


