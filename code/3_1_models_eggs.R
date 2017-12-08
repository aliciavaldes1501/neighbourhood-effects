library(fmsb)
library(effects)
library(ggplot2)
library(nlme)
library(spdep)
library(ncf)
library(gstat)
library(ggthemes)
library(RColorBrewer)
library(cowplot)
library(gridExtra)

load(file="allplants.R")  
load(file="allplants.listw1.R")  
load(file="allplants.listw2.R")  

#model with all plants
#binomial model - attack ####
model1<-glm(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
                       scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
                       scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
                     subset(allplants,!is.na(phen)),family="binomial") #MODEL TO USE!
summary(model1)
NagelkerkeR2(model1)

#Get residuals of model
res_model1<-residuals(model1)

plot(variogram(res_model1~1,data=subset(allplants,!is.na(phen))),main="res model 1 all 2 ants")
#cutof= (length of the diagonal of the box spanning the data is divided by three)

#Test for autocorrelation (Moran's I) in residuals of models
moran_model1<- moran.test(res_model1, listw=allplants.listw1) 
moran_model1 #Significant autocorrelation

moran.mc_model1<-moran.mc(res_model1,listw=allplants.listw1,nsim=1000) #Try another function
moran.mc_model1

#Make correlogram of residuals of model
correlog_model1 <- correlog(subset(allplants,!is.na(phen))$x,
                                     subset(allplants,!is.na(phen))$y,
                                     res_model1,increment=5, resamp=100) 
correlog_model1_1 <- correlog(subset(allplants,!is.na(phen))$x,
                            subset(allplants,!is.na(phen))$y,
                            res_model1,increment=1, resamp=100) 
correlog_model1
plot(correlog_model1)
abline(h=0) #Ignore autocorrelation at longer distances!

#Moran eigenvector GLM filtering
#Moran eigenvectors
ME.model1 <-ME(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
                          scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
                          scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
                        listw=allplants.listw1,
                        data=subset(allplants,!is.na(phen)), 
                        family=binomial,alpha=0.05,verbose=T) #USE
ME2.model1 <-ME(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
                 scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
               listw=allplants.listw1,
               data=subset(allplants,!is.na(phen)), 
               family=binomial,alpha=0.20,verbose=T) 

#Repeat model with vectors as predictors
vector1<-ME.model1$vectors[,1]
vector2<-ME.model1$vectors[,2]
model1_ME<-glm(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
           scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
           scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
           scale(vector1)+scale(vector2),
           subset(allplants,!is.na(phen)),family="binomial")
summary(model1_ME) # Model to use ####
NagelkerkeR2(model1_ME)

model1_ME_1v<-glm(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
                 scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
                 scale(ME.model1$vectors[,1]),
               subset(allplants,!is.na(phen)),family="binomial")

res_model1_ME<-residuals(model1_ME)
res_model1_ME_1v<-residuals(model1_ME_1v)

#Make correlogram of residuals of model
correlog_model1_ME <- correlog(subset(allplants,!is.na(phen))$x,
                                        subset(allplants,!is.na(phen))$y,
                                        res_model1_ME,increment=5, resamp=100) #Rerun with 1000
correlog_model1_ME_1v <- correlog(subset(allplants,!is.na(phen))$x,
                               subset(allplants,!is.na(phen))$y,
                               res_model1_ME_1v,increment=5, resamp=100) #Rerun with 1000
correlog_model1_ME
plot(correlog_model1_ME)
abline(h=0)
#Test for autocorrelation (Moran's I) in residuals of models
moran_model1_ME<- moran.test(res_model1_ME, listw=allplants.listw1) 
moran_model1_ME #ONLY LITTLE AUTOCORRELATION LEFT

moran.mc_model1_ME<-moran.mc(res_model1_ME,listw=allplants.listw1,nsim=1000) #Try another function
moran.mc_model1_ME

moran.mc_model1_ME<-moran.mc(res_model1_ME,listw=allplants.listw1,nsim=1000) #Try another function
moran.mc_model1_ME

moran_model1_ME_1v<- moran.test(res_model1_ME_1v, listw=allplants.listw1) 
moran_model1_ME_1v #Better with both vectors


#negative binomial model - number of eggs when present ####
model2<-glm.nb(n_eggs_max ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) + 
        scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s) + 
        scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
        subset(allplants,!is.na(phen)&n_eggs_max>0)) 
summary(model2) # Model to use ####
NagelkerkeR2(model2)

#GRAPHS
plot(effect(term="scale(phen_int)", 
            mod=model2,data=subset(allplants,!is.na(phen))),
     multiline=T,type="response")
plot(effect(term="scale(Mrub_sch_s)", 
            mod=model2,data=subset(allplants,!is.na(phen))),
     multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
            mod=model2,data=subset(allplants,!is.na(phen))),
     x.var="pldens_3",multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
            mod=model2,data=subset(allplants,!is.na(phen))),
     x.var="phen_n3",multiline=T,type="response")


###


#Get residuals of model
res_model2<-residuals(model2)

# #Make correlogram of residuals of model
correlog_model2 <- correlog(subset(allplants,!is.na(phen)&n_eggs_max>0)$x,
                                     subset(allplants,!is.na(phen)&n_eggs_max>0)$y,
                                     res_model2,increment=5, resamp=100) #Rerun with 1000
correlog_model2
plot(correlog_model2)
abline(h=0) #Ignore autocorrelation at longer distances!
# 

plot(variogram(res_model2~1,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),main="res model 2")
#cutof= (length of the diagonal of the box spanning the data is divided by three)

#Test for autocorrelation (Moran's I) in residuals of models

moran_model2<- moran.test(res_model2, listw=allplants.listw2)
moran_model2 #NO Significant autocorrelation!
moran.mc_model2<-moran.mc(res_model2,listw=allplants.listw2,nsim=1000) #Try another function
moran.mc_model2

#Graphs ####
plot(effect(term="scale(phen_int):scale(Mrub_sch_s)", 
            mod=model1_ME,data=subset(allplants,!is.na(phen))),
     x.var="phen_int",multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
            mod=model1_ME,data=subset(allplants,!is.na(phen))),
     x.var="pldens_3",multiline=T,type="response")
plot(effect(term="scale(phen_int)", 
            mod=model2,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),multiline=T,type="response")
plot(effect(term="scale(Mrub_sch_s)", 
            mod=model2,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
            mod=model2,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),
     x.var="pldens_3",multiline=T,type="response")

interaction1<-data.frame(effect(term="scale(phen_int):scale(Mrub_sch_s)", mod=model1_ME,
                            xlevels=list(Mrub_sch_s=seq(0,35,1), phen_int=1:6)))
interaction2<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model1_ME,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))
effect1<-data.frame(effect(term="scale(phen_int)", mod=model2,xlevels=list(phen_int=seq(1,6,0.01))))
effect2<-data.frame(effect(term="scale(Mrub_sch_s)", mod=model2,xlevels=list(Mrub_sch_s=seq(0,35,1))))
interaction3<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model2,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

p1<-ggplot(interaction1, aes(phen_int,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(phen_int,fit,color=Mrub_sch_s))+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Ant abundance")+scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p2<-ggplot(interaction2, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="none")+labs(colour="Neighbor phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p3<-ggplot(effect1, aes(phen_int,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(phen_int,fit))+
  xlab("Shoot phenology")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+scale_y_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p4<-ggplot(effect2, aes(Mrub_sch_s,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(Mrub_sch_s,fit))+
  xlab("Ant abundance")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p5<-ggplot(interaction3, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Number of eggs")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Neigh. phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2.tiff",
     res=600,width=26,height=9,units="cm",family="Times") 
ggdraw()+
  draw_plot(p1,0.02,0.01,0.3,1)+
  draw_plot(p3,1/3+0.04,0.01,0.3,0.81)+
  draw_plot(p4,2/3+0.04,0.01,0.3,0.81)+
  draw_label(label="A)",x=0.01,y=0.8, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.39,y=0.8, fontfamily = "serif", fontface = 1)+
  draw_label(label="C)",x=0.73,y=0.8, fontfamily = "serif", fontface = 1)
dev.off()

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3.tiff",
     res=600,width=22,height=11,units="cm",family="Times") 
ggdraw()+
  draw_plot(p2,0.02,0,0.45,0.83)+
  draw_plot(p5,1/2,0,0.45,1)+
  draw_label(label="A)",x=0.01,y=0.81, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.53,y=0.81, fontfamily = "serif", fontface = 1)
dev.off()

#Raw data

ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max>0)), aes(phen_int,n_eggs_max))+
  geom_point(aes(phen_int,n_eggs_max))+
  stat_smooth(method="glm.nb",  colour="black", mapping=aes(phen_int,n_eggs_max))
  
ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max>0)), aes(Mrub_sch_s,n_eggs_max))+
  geom_point(aes(Mrub_sch_s,n_eggs_max))+
  stat_smooth(method="glm.nb",  colour="black", mapping=aes(Mrub_sch_s,n_eggs_max))

ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max>0)), aes(pldens_3,n_eggs_max))+
  geom_point(aes(pldens_3,n_eggs_max))+
  stat_smooth(method="glm.nb",  colour="black", mapping=aes(pldens_3,n_eggs_max))

ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max>0)), aes(phen_n3,n_eggs_max))+
  geom_point(aes(phen_n3,n_eggs_max))+
  stat_smooth(method="glm.nb",  colour="black", mapping=aes(phen_n3,n_eggs_max))

