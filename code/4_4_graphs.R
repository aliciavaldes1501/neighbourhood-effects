library(ggplot2)
library(ggthemes)
library(effects)
library(RColorBrewer)
library(sjPlot)

#Fr set vs env conds####
#Scatterplots
ggplot(allplants) + 
  geom_point(data=allplants,aes(meanT, fr_in/n_fl))+ 
  geom_smooth(aes(meanT, fr_in/n_fl,weight=n_fl),method=glm,method.args=c(family="binomial"),se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm(cbind(fr_in,n_fl)~meanT,data=allplants,family="binomial"))

ggplot(allplants) + 
  geom_point(data=allplants,aes(moist_per, fr_in/n_fl))+ 
  geom_smooth(aes(moist_per, fr_in/n_fl,weight=n_fl),method=glm,method.args=c(family="binomial"),se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm(cbind(fr_in,n_fl)~moist_per,data=allplants,family="binomial"))

summary(glm(cbind(fr_in,n_fl)~meanT*moist_per,data=allplants,family="binomial")) #None *

#Seeds_per_fl vs env conds####
#Scatterplots
hist(allplants$seeds_per_fl) #Almost normal

ggplot(as.data.frame(allplants)) + 
  geom_point(data=as.data.frame(allplants),aes(meanT, seeds_per_fl))+ 
  geom_smooth(aes(meanT, seeds_per_fl),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(lm(seeds_per_fl~meanT,allplants)) #*

summary(lm(seeds_per_fl~meanT,subset(allplants,n_eggs_max>0))) #NS for plants with eggs
ggplot(subset(as.data.frame(allplants),n_eggs_max>0)) + 
  geom_point(data=subset(as.data.frame(allplants),n_eggs_max>0),aes(meanT, seeds_per_fl))+ 
  geom_smooth(aes(meanT, seeds_per_fl),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")

summary(lm(seeds_per_fl~meanT,subset(allplants,n_eggs_max<1))) #NS for plants without eggs
ggplot(subset(as.data.frame(allplants),n_eggs_max<1)) + 
  geom_point(data=subset(as.data.frame(allplants),n_eggs_max<1),aes(meanT, seeds_per_fl))+ 
  geom_smooth(aes(meanT, seeds_per_fl),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")

ggplot(allplants) + 
  geom_point(data=allplants,aes(moist_per, seeds_per_fl))+ 
  geom_smooth(aes(moist_per, seeds_per_fl),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(lm(seeds_per_fl~moist_per,allplants)) #Almost *

summary(lm(seeds_per_fl~meanT*moist_per,allplants)) #Interaction NS

#Pred vs env conds####

#Scatterplots
ggplot(allplants) + 
  geom_point(data=allplants,aes(meanT, attack))+ 
  geom_smooth(aes(meanT, attack),method=glm,method.args=c(family="binomial"),se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm(attack~meanT,family=binomial,data=allplants))

ggplot(allplants) + 
  geom_point(data=allplants,aes(moist_per, attack))+ 
  geom_smooth(aes(moist_per, attack),method=glm,method.args=c(family="binomial"),se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm(attack~moist_per,family=binomial,data=allplants))

summary(glm(attack~meanT*moist_per,family=binomial,data=allplants)) #Interaction*

hist(subset(allplants,n_eggs_max>0)$n_eggs_max)

ggplot(subset(allplants,n_eggs_max>0)) + 
  geom_point(data=subset(allplants,n_eggs_max>0),aes(meanT, n_eggs_max))+ 
  geom_smooth(aes(meanT, n_eggs_max),method=glm.nb,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm.nb(n_eggs_max~meanT,data=subset(allplants,n_eggs_max>0)))

ggplot(subset(allplants,n_eggs_max>0)) + 
  geom_point(data=subset(allplants,n_eggs_max>0),aes(moist_per, n_eggs_max))+ 
  geom_smooth(aes(moist_per, n_eggs_max),method=glm.nb,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(glm.nb(n_eggs_max~moist_per,data=subset(allplants,n_eggs_max>0)))

summary(glm.nb(n_eggs_max~meanT*moist_per,data=subset(allplants,n_eggs_max>0))) #Interaction NS
summary(glm.nb(n_eggs_max~meanT+moist_per,data=subset(allplants,n_eggs_max>0))) #None *


#Graphs interactive effects for attack

model1<-glm(attack~meanT+moist_per+meanT:moist_per,family=binomial,data=allplants)
interaction1<-data.frame(effect(term="meanT:moist_per", mod=model1,
                                xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
theme_set( theme_base( base_family= "Times"))

ggplot(interaction1, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Probability of having eggs")+theme_base()+
  scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(interaction1, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Probability of having eggs")+theme_base()+
  scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

###

ggplot(allplants) + 
  geom_point(data=allplants,aes(meanT, moist_per, color=attack))+ 
  geom_smooth(aes(meanT, moist_per),method=lm,se=F,color="black")+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("Attack")

ggplot(subset(allplants,n_eggs_max>0)) + 
  geom_point(data=subset(allplants,n_eggs_max>0),aes(meanT, moist_per, size=n_eggs_max, color=n_eggs_max))+ 
  geom_smooth(aes(meanT, moist_per),method=lm,se=F,color="black")+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("N eggs")

#Heatmaps

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(allplants, aes(meanT,moist_per, fill=n_eggs_max)) + 
  geom_raster() +
  coord_fixed(ratio = 1)  + 
  scale_fill_gradientn(colours = rev(rainbow(7))) #Cannot allocate vector of...

ggplot(allplants, aes(meanT,moist_per, fill=n_eggs_max)) +
  geom_raster(aes(fill = n_eggs_max)) #Cannot allocate vector of...

#Seeds_per_fl vs pred####
summary(lm(seeds_per_fl~attack,allplants)) #*

summary_seeds <-summarySE(as.data.frame(allplants), measurevar="seeds_per_fl", 
                          groupvars=c("attack"),na.rm=T)
summary_seeds
ggplot(summary_seeds,aes(as.factor(attack), seeds_per_fl)) + 
  geom_errorbar(aes(ymin=seeds_per_fl-se, ymax=seeds_per_fl+se), width=.1) +
  geom_point()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")


summary(lm(seeds_per_fl~n_eggs_max,subset(allplants,n_eggs_max>0))) #*
ggplot(subset(allplants,n_eggs_max>0)) + 
  geom_point(data=subset(allplants,n_eggs_max>0),aes(n_eggs_max, seeds_per_fl))+ 
  geom_smooth(aes(n_eggs_max, seeds_per_fl),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")

#Pl phen(+other traits) vs env conds####
hist(allplants$phen_int) #Almost normal
hist(allplants$shoot_h) #Almost normal
hist(allplants$n_fl) 
hist(log(allplants$n_fl)) #Almost normal

summary(lm(phen_int~meanT,allplants))#NS
summary(lm(phen_int~moist_per,allplants))#*
summary(lm(phen_int~meanT*moist_per,allplants))#Interaction*

summary(lm(shoot_h~meanT+moist_per,allplants))#Interaction NS

summary(lm(n_fl~meanT+moist_per,allplants))#Interaction NS, none*

summary(lm(PC1~meanT+moist_per,markedplants))#Interaction NS, none*

model3<-lm(phen_int~meanT+moist_per+meanT:moist_per,data=allplants)
interaction3<-data.frame(effect(term="meanT:moist_per", mod=model3,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
theme_set( theme_base( base_family= "Times"))

ggplot(interaction3, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Plant phenology")+theme_base()+
  scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_y_continuous(limit=c(NA,6))

ggplot(interaction3, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Plant phenology")+theme_base()+
  scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_y_continuous(limit=c(NA,6))

#Ant abundance vs env conds####
hist(allplants$Mrub_sch_s) 
hist(subset(allplants,Mrub_sch_s>0)$Mrub_sch_s) 

summary(glm.nb(Mrub_sch_s~meanT*moist_per,allplants))#Interaction*
summary(glm.nb(Mrub_sch_s~meanT*moist_per,subset(allplants,Mrub_sch_s>0)))#Interaction*

model4<-glm.nb(Mrub_sch_s~meanT+moist_per+meanT:moist_per,data=allplants)
interaction4<-data.frame(effect(term="meanT:moist_per", mod=model4,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
theme_set( theme_base( base_family= "Times"))

ggplot(interaction4, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Number of host ants (M. rubra + M. schencki)")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(interaction4, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Number of host ants (M. rubra + M. schencki)")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

#Neigh suit vs env conds####
hist(allplants$suit_neigh) #Almost normal

summary(lm(suit_neigh~meanT*moist_per,allplants))#Interaction*

model5<-lm(suit_neigh~meanT+moist_per+meanT:moist_per,data=allplants)
interaction5<-data.frame(effect(term="meanT:moist_per", mod=model5,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

ggplot(interaction5, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Neighbor suitability (density x phenology)")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(interaction5, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Neighbor suitability (density x phenology)")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

#Neigh density vs. env conds#### 
hist(allplants$pldens_3) #Almost? normal

summary(lm(pldens_3~meanT*moist_per,allplants))#Interaction*

model6<-lm(pldens_3~meanT+moist_per+meanT:moist_per,data=allplants)
interaction6<-data.frame(effect(term="meanT:moist_per", mod=model6,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

ggplot(interaction6, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Neighbor density")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

ggplot(interaction6, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Neighbor density")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

#Neigh phen vs. env conds#### 
hist(allplants$phen_n3) #Almost? normal

summary(lm(phen_n3~meanT*moist_per,allplants))#Interaction*

model7<-lm(phen_n3~meanT+moist_per+meanT:moist_per,data=allplants)
interaction7<-data.frame(effect(term="meanT:moist_per", mod=model7,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))

ggplot(interaction7, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Neighbor phenology")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_y_continuous(limit=c(NA,6))

ggplot(interaction7, aes(moist_per,fit, group = as.factor(meanT)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(moist_per,fit,color=meanT))+
  xlab("Moisture percentage")+ylab("Neighbor phenology")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Soil temperature")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_y_continuous(limit=c(NA,6))

#Pred vs. phen, context, sutiabilit and env conds#### 
summary(glm(attack~phen_int,allplants,family="binomial"))
summary(glm(attack~Mrub_sch_s,allplants,family="binomial"))
summary(glm(attack~phen_int*Mrub_sch_s,allplants,family="binomial"))#Interaction*
summary(glm(attack~phen_int*Mrub_sch_s+pldens_3*phen_n3,allplants,family="binomial"))#Interaction*

model8<-glm(attack~phen_int+Mrub_sch_s+pldens_3+phen_n3+
        phen_int:Mrub_sch_s+pldens_3:phen_n3,allplants,family="binomial")
interaction8a<-data.frame(effect(term="phen_int:Mrub_sch_s", mod=model8,
               xlevels=list(phen_int=seq(1:6), Mrub_sch_s=seq(0,34,1))))
interaction8b<-data.frame(effect(term="pldens_3:phen_n3", mod=model8,
               xlevels=list(pldens_3=seq(0,50,1), phen_n3=seq(2.8,6,0.05))))

ggplot(interaction8a, aes(phen_int,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(phen_int,fit,color=Mrub_sch_s))+
  xlab("Phenology")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Ant abundance")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))

ggplot(interaction8b, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Neighbor phenology")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

model9<-glm(attack~phen_int+Mrub_sch_s+pldens_3+phen_n3+meanT+moist_per+
        phen_int:Mrub_sch_s+pldens_3:phen_n3,allplants,family="binomial")
interaction9a<-data.frame(effect(term="phen_int:Mrub_sch_s", mod=model9,
                                 xlevels=list(phen_int=seq(1:6), Mrub_sch_s=seq(0,34,1))))
interaction9b<-data.frame(effect(term="pldens_3:phen_n3", mod=model9,
                                 xlevels=list(pldens_3=seq(0,50,1), phen_n3=seq(2.8,6,0.05))))

ggplot(interaction9a, aes(phen_int,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(phen_int,fit,color=Mrub_sch_s))+
  xlab("Phenology")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Ant abundance")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))

ggplot(interaction9b, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Neighbor phenology")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

summary(glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial"))#Interaction*
model10<-glm(attack~phen_int+Mrub_sch_s+phen_int:Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial")

interaction10<-data.frame(effect(term="phen_int:Mrub_sch_s", mod=model9,
               xlevels=list(phen_int=seq(1:6), Mrub_sch_s=seq(0,34,1))))

ggplot(interaction10, aes(phen_int,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(phen_int,fit,color=Mrub_sch_s))+
  xlab("Phenology")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Ant abundance")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))

plot_model(model10, type = "pred", terms = c("suit_neigh"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Neighbor suitability (density x phenology)")+ylab("Probability of having eggs")+
  scale_y_continuous(breaks=c(0.1,0.2,0.3))

plot_model(model10, type = "pred", terms = c("meanT"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil temperature")+ylab("Probability of having eggs")+scale_y_continuous()

plot_model(model10, type = "pred", terms = c("moist_per"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil moisture percentage")+ylab("Probability of having eggs")+scale_y_continuous()

model11<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s+phen_int:Mrub_sch_s+suit_neigh+meanT+moist_per,
         data=subset(allplants,n_eggs_max>0)) #Interaction NS
model11<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s+suit_neigh+meanT+moist_per,
         data=subset(allplants,n_eggs_max>0)) #All*

plot_model(model11, type = "pred", terms = c("phen_int"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Plant phenology")+ylab("Number of eggs")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+scale_y_continuous(breaks=c(0,1,2,3,4,5,6))

plot_model(model11, type = "pred", terms = c("Mrub_sch_s"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Ant abundance")+ylab("Number of eggs")+scale_y_continuous(breaks=c(0,4,8,12,16))

plot_model(model11, type = "pred", terms = c("suit_neigh"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Neighbor suitability (density x phenology)")+ylab("Number of eggs")+
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7))

plot_model(model11, type = "pred", terms = c("meanT"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil temperature")+ylab("Number of eggs")

plot_model(model11, type = "pred", terms = c("moist_per"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil moisture percentage")+ylab("Number of eggs")




