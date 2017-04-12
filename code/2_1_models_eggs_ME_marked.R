library(MASS)
library(fmsb)
library(gstat)
library(car)
library(spdep)
library(ncf)
library(nlme)
library(ggthemes)

load(file="allplants.R")  

#Example with only marked plants 
#Two models: binomial model for probability of attack +
#negative binomial model for number of eggs

#binomial model - attack ####
formula1<-attack~scale(as.integer(phen))+scale(Mrub_sum)+
  scale(pldens_2)+scale(phen_n2)+
  scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+
  scale(pldens_2):scale(phen_n2)
model1<-glm(formula1,subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial")
summary(model1)
NagelkerkeR2(model1)

#Get residuals of model
res_model1<-residuals(model1)

spdata <- data.frame(resid = res_model1, x = subset(allplants,!is.na(pl_id)&!is.na(phen))$x, y = subset(allplants,!is.na(pl_id)&!is.na(phen))$y)
coordinates(spdata) <- c("x", "y")
bubble(spdata, "resid", col = c("blue", "orange"), main = "Residuals", xlab = "X-coordinates", 
       ylab = "Y-coordinates")

#Make correlogram of residuals of model
correlog_model1 <- correlog(subset(allplants,!is.na(pl_id)&!is.na(phen))$x, 
                   subset(allplants,!is.na(pl_id)&!is.na(phen))$y, 
                   res_model1,increment=5, resamp=100) #Rerun with 1000
correlog_model1
plot(correlog_model1)
abline(h=0) #Ignore autocorrelation at longer distances!

#With ggplot2
df1<-data.frame(cbind(distance=as.vector(correlog_model1$mean.of.class), 
                      correlation=as.vector(correlog_model1$correlation),
                      p=as.vector(correlog_model1$p)))
ggplot(data=df1,aes(x=distance, y=correlation)) +
  geom_point(colour = ifelse(df1$p < 0.05,"red","black"), size = ifelse(df1$p < 0.05,3,2)) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,80)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.2,1))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

plot(variogram(res_model1~1,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),main="res model 1")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#Create neighbours matrix - dnearneigh (30 m)
markedplants.nb1 <- dnearneigh(subset(allplants,!is.na(pl_id)&!is.na(phen)), 0, 30) 
markedplants.listw1 <- nb2listw(markedplants.nb1) 

#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

moran_model1<- moran.test(res_model1, listw=markedplants.listw1) 
moran_model1 #Significant autocorrelation

geary_model1<-geary.test(res_model1, listw=markedplants.listw1)
geary_model1

#Moran eigenvector GLM filtering
#Moran eigenvectors
ME.model1 <-ME(formula1, listw=markedplants.listw1, 
            data=subset(allplants,!is.na(pl_id)&!is.na(phen)),
            family=binomial,alpha=0.05,verbose=T)

#Repeat model with vectors as predictors
model1_ME<-glm(attack~scale(as.integer(phen))+scale(Mrub_sum)+
           scale(pldens_2)+scale(phen_n2)+
           scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+
           scale(pldens_2):scale(phen_n2)+
           scale(ME.model1$vectors[,1])+scale(ME.model1$vectors[,2]),
           subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial")
summary(model1_ME)
NagelkerkeR2(model1_ME)

res_model1_ME<-residuals(model1_ME)

#Make correlogram of residuals of model
correlog_model1_ME <- correlog(subset(allplants,!is.na(pl_id)&!is.na(phen))$x, 
                      subset(allplants,!is.na(pl_id)&!is.na(phen))$y, 
                      res_model1_ME,increment=5, resamp=100) #Rerun with 1000
correlog_model1_ME
plot(correlog_model1_ME)
abline(h=0)

#With ggplot2
df2<-data.frame(cbind(distance=as.vector(correlog_model1_ME$mean.of.class), 
                      correlation=as.vector(correlog_model1_ME$correlation),
                      p=as.vector(correlog_model1_ME$p)))
ggplot(data=df2,aes(x=distance, y=correlation)) +
  geom_point(colour = ifelse(df2$p < 0.05,"red","black"), size = ifelse(df2$p < 0.05,3,2)) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,80)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.2,1))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

moran_model1_ME<- moran.test(res_model1_ME, listw=markedplants.listw1) 
moran_model1_ME #No significant autocorrelation!!!

geary_model1_ME<-geary.test(res_model1_ME, listw=markedplants.listw1)
geary_model1_ME #No significant autocorrelation!!!

#negative binomial model - number of eggs when present ####
formula2<-n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+
  scale(pldens_2)+scale(phen_n2)+
  scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+
  scale(pldens_2):scale(phen_n2)
model2<-glm.nb(formula2,subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0))
summary(model2)
NagelkerkeR2(model2)

#Get residuals of model
res_model2<-residuals(model2)

#Make correlogram of residuals of model
correlog_model2 <- correlog(subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0)$x, 
                            subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0)$y, 
                            res_model2,increment=5, resamp=100) #Rerun with 1000
correlog_model2
plot(correlog_model2)
abline(h=0) #Ignore autocorrelation at longer distances!

df3<-data.frame(cbind(distance=as.vector(correlog_model2$mean.of.class), 
                      correlation=as.vector(correlog_model2$correlation),
                      p=as.vector(correlog_model2$p)))
ggplot(data=df3,aes(x=distance, y=correlation)) +
  geom_point(colour = ifelse(df3$p < 0.05,"red","black"), size = ifelse(df3$p < 0.05,3,2)) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,80)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.2,1))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

plot(variogram(res_model2~1,data=subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0)),main="res model 2")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#Create neighbours matrix - dnearneigh (30 m)
markedplants.nb2 <- dnearneigh(subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0), 0, 30) 
markedplants.listw2 <- nb2listw(markedplants.nb2) 

#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

moran_model2<- moran.test(res_model2, listw=markedplants.listw2) 
moran_model2 #Significant autocorrelation

geary_model2<-geary.test(res_model2, listw=markedplants.listw2)
geary_model2

#Moran eigenvector GLM filtering
#Moran eigenvectors
ME.model2 <-ME(formula2, listw=markedplants.listw2, 
               data=subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0),
               family=negative.binomial(2.254),alpha=0.23,verbose=T)

#Repeat model with vectors as predictors
model2_ME<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+
                 scale(pldens_2)+scale(phen_n2)+
                 scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+
                 scale(pldens_2):scale(phen_n2)+
                 scale(ME.model2$vectors[,1]),
               subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0))
summary(model2_ME)
NagelkerkeR2(model2_ME)

res_model2_ME<-residuals(model2_ME)

#Make correlogram of residuals of model
correlog_model2_ME <- correlog(subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0)$x, 
                               subset(allplants,!is.na(pl_id)&!is.na(phen)&n_eggs_max>0)$y, 
                               res_model2_ME,increment=5, resamp=100) #Rerun with 1000
correlog_model2_ME
plot(correlog_model2_ME)
abline(h=0)

df4<-data.frame(cbind(distance=as.vector(correlog_model2_ME$mean.of.class), 
                      correlation=as.vector(correlog_model2_ME$correlation),
                      p=as.vector(correlog_model2_ME$p)))
ggplot(data=df4,aes(x=distance, y=correlation)) +
  geom_point(colour = ifelse(df4$p < 0.05,"red","black"), size = ifelse(df4$p < 0.05,3,2)) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,80)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.2,1))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))


#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

moran_model2_ME<- moran.test(res_model2_ME, listw=markedplants.listw2) 
moran_model2_ME #Almost no significant autocorrelation!!!

geary_model2_ME<-geary.test(res_model2_ME, listw=markedplants.listw2)
geary_model2_ME #No significant autocorrelation!!!




