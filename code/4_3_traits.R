library(ggplot2)
library(ggfortify)
library(ggthemes)
library(ade4)
library("FactoMineR")
library(factoextra)

markedplants<-subset(allplants,!is.na(pl_id)&!is.na(n_fl))

cor(markedplants$phen_int,markedplants$n_fl)
cor(markedplants$shoot_h,markedplants$n_fl)
cor(markedplants$phen_int,markedplants$shoot_h)

PCA_traits<-prcomp(~markedplants$phen_int+markedplants$shoot_h+markedplants$n_fl,center=T,scale=T)
summary(PCA_traits)
plot(PCA_traits)
biplot(PCA_traits)

PCA_traits_scores<-as.data.frame(predict(PCA_traits))
markedplants$PC1<-PCA_traits_scores$PC1
hist(markedplants$PC1)

theme_set( theme_base( base_family= "Times"))

autoplot(PCA_traits, loadings = T, loadings.colour = "black", loadings.label = TRUE,loadings.label.colour="black",
         loadings.label.label = c("Phenology","Shoot h","N flowers"), colour="grey",data = markedplants)+
  xlab("PC1 (%)")+ylab("PC2 (%)")+geom_hline(aes(yintercept=0), colour="darkgrey",linetype="dashed")+
  geom_vline(aes(xintercept=0), colour="darkgrey",linetype="dashed")

#Can we estimate number of flowers from phenology?
hist(markedplants$n_fl)
summary(lm(markedplants$n_fl~markedplants$phen_int)) #Adj R2=0.2381
summary(lm(markedplants$n_fl~markedplants$phen_int+I((markedplants$phen_int)^2))) #Adj R2=0.2744
ggplot(markedplants, aes(x=phen_int, y=n_fl))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

hist(log(markedplants$n_fl))
summary(lm(log(markedplants$n_fl)~markedplants$phen_int)) #Adj R2=0.383
summary(lm(log(markedplants$n_fl)~markedplants$phen_int+I((markedplants$phen_int)^2))) #Adj R2=0.4018-->USE
ggplot(markedplants, aes(x=phen_int, y=log(n_fl)))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

#Can we estimate shoot height from phenology?
hist(markedplants$shoot_h)
summary(lm(markedplants$shoot_h~markedplants$phen_int)) #Adj R2=0.2452-->USE
summary(lm(markedplants$shoot_h~markedplants$phen_int+I((markedplants$phen_int)^2))) #Adj R2=0.2494 
ggplot(markedplants, aes(x=phen_int, y=shoot_h))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

hist(log(markedplants$shoot_h))
summary(lm(log(markedplants$shoot_h)~markedplants$phen_int))#Adj R2=0.2821
summary(lm(log(markedplants$shoot_h)~markedplants$phen_int+I((markedplants$phen_int)^2))) ##Adj R2=0.2815
ggplot(markedplants, aes(x=phen_int, y=log(shoot_h)))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

#Number of flowers and shoot height could be estimated from phenology in all plants,
#using equations obtained with marked plants
