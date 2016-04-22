
##############Skoomy
#Qui vote pour qui ?
## ------------------------------------------------------------------------
#Charge du jeux de données
gavote <- read.table("gavote.txt")
#on nomme nos variables
atlanta<-gavote$atlanta
ballots<-gavote$ballots
bush<-gavote$bush
econ<-gavote$econ
equip<-gavote$equip
gore<-gavote$gore
other<-gavote$other
perAA<-gavote$perAA
rural<-gavote$rural 
votes<-gavote$votes

## ------------------------------------------------------------------------
str(gavote)

## ------------------------------------------------------------------------
#Variable undercount
gavote$undercount<-(ballots-votes)
gavote<-transform(gavote,undercount)
undercount<-gavote$undercount

#Suppresion des variables inutiles
gavote$ballots<-NULL
gavote$votes<-NULL

## ------------------------------------------------------------------------
#Variable pergore perbush perother
gavote$pergore<-(gore/(other+gore+bush))*100
gavote$perbush<-(bush/(other+gore+bush))*100
gavote$perother<-(other/(other+gore+bush))*100
gavote<-transform(gavote,pergore,perbush,perother)
perother<-gavote$perother
pergore<-gavote$pergore
perbush<-gavote$perbush
#Suppression des variables inutiles
gavote$bush<-NULL
gavote$gore<-NULL
gavote$other<-NULL

## ------------------------------------------------------------------------
str(gavote)

## ------------------------------------------------------------------------
barplot(table(rural))
barplot(table(equip))
barplot(table(atlanta))
barplot(table(econ))

## ------------------------------------------------------------------------
#Tracé du Boxplot
g<-c("pergore","perbush","perother")
boxplot(pergore, perbush,perother,col=c("orange"),ylab="Quantiles",main=paste("pergore", "perbush","perother"),names=g)

boxplot(perAA,col=c("orange"),ylab="Quantiles",main=paste("perAA"))

## ------------------------------------------------------------------------
summary(gavote)

## ------------------------------------------------------------------------
#diagramme de dispersion entre pergore et perAA
plot(pergore,perAA)
abline(lm(perAA~pergore),col="blue")

## ------------------------------------------------------------------------
#Régresser undercount par rapport a perAA
cor(undercount,perAA)
plot(perAA,undercount)
abline(lm(undercount~perAA),col="orange")

## ------------------------------------------------------------------------
#expliquer undercount par rapport a pergore
plot(pergore,undercount)
abline(lm(undercount~pergore),col="orange")


## ------------------------------------------------------------------------
# Création d'une varible contenant les valeurs 
levels(econ)
(econ == "rich")+0->isRich
(econ == "poor")+0->isPoor
(econ == "middle")+0->isMiddle

#distribution de econ
barplot(table(econ))

## ------------------------------------------------------------------------
#Valeur de X 
X<- matrix(c(1,1,1,1,0,0,0,1,0,0,0,1),nrow=3,ncol=4)
X

## ------------------------------------------------------------------------
(rural == "urban")+0->isUrban
(rural == "rural")+0->isRural
summary(lm(undercount~isRich+isMiddle+isPoor-1))


## ------------------------------------------------------------------------
#un comportement pertinent  
#un comportement pertinent  
summary(lm(undercount~isRural+isUrban-1))

## ------------------------------------------------------------------------
model4<-lm(undercount~pergore+perbush+perother+perAA)
summary(model4)

## ------------------------------------------------------------------------
cor(perother,perbush+pergore)

## ------------------------------------------------------------------------
gavote<-gavote[,-which(names(gavote)=="perAA")]
gavote<-gavote[,-which(names(gavote)=="perother")]

## ------------------------------------------------------------------------
model.all<-lm(undercount~.,gavote)
summary(model.all)

## ------------------------------------------------------------------------
model.test<-lm(undercount~econ+equip,gavote)
summary(model.test)

## ------------------------------------------------------------------------
plot(model.test,1)

## ------------------------------------------------------------------------
model.tmp<-lm(undercount~.,gavote)
summary(model.tmp)
model.AICf<-step(model.tmp, direction='forward')
model.AICb<-step(model.tmp, direction='backward')
model.BICf<-step(model.tmp, direction='forward', k=log(159))
model.BICb<-step(model.tmp, direction='backward',k=log(159))
plot(model.AICf,1)
plot(model.AICb,1)
plot(model.BICf,1)
plot(model.BICb,1)

## ------------------------------------------------------------------------
BIC(model.BICb)
BIC(model.BICf)

## ------------------------------------------------------------------------
BIC(model.BICb)
BIC(model.BICf)

## ------------------------------------------------------------------------
library(leaps)
subsets<-regsubsets(undercount~., gavote)
summary(subsets)
summary(subsets)$adjr2
max(summary(subsets)$adjr2)

## ------------------------------------------------------------------------
gavote2 <- gavote[gavote$equip != "PAPER", ]
model.R2<-lm(undercount~gavote2$equip+gavote2$econ+gavote2$rural, gavote2)
summary(model.R2)

## ------------------------------------------------------------------------
plot(model.R2,1)

## ------------------------------------------------------------------------
plot(model.R2,4)
plot(model.R2,5)

