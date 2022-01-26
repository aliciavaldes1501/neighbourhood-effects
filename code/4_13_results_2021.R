#### Load libraries ####

library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(ggpubr)
library(piecewiseSEM)
library(MASS)
library(semEff)
library(RPushbullet)
library(DHARMa)
library(data.table)
library(ncf)
library(spdep)
library(spatialreg)
library(ggeffects)
library(beepr)
library(tidyverse)
library(car)

#### Define ggplot themes ####
my_theme <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(legend.position="none")+theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}

my_theme_legend <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}

### Load data and make fixes ####

load(file="allplants.R")  
head(allplants)

# Changes needed in allplants after inspection of comments on seed data
# Some fruits noted wrong P_UP
allplants$fr_in[4486]=3
allplants$fr_inC[4486]=0
allplants$fr_pr[4486]=1
allplants$fr_prC[4486]=1
allplants$fr_in[7626]=4
allplants$fr_inC[7626]=0
allplants$fr_pr[7626]=1
allplants$fr_prC[7626]=1

allplants$fruitset<-allplants$fr_in/allplants$n_fl
subset(allplants,fruitset>1) #5 cases
# Change n_fl when fruiset results to be >1
allplants$n_fl<-ifelse(is.na(allplants$fr_in),allplants$n_fl,
                       ifelse(allplants$fr_in>allplants$n_fl,allplants$fr_in,
                              allplants$n_fl))
allplants$fruitset<-ifelse(allplants$fruitset>1,1,allplants$fruitset)

seeds<-read.table("C:/Users/User/Dropbox/SU/projects/neighbourhood_effects/data/raw/tanga2016_seeds.txt",header=T,sep="\t",dec=".")
head(seeds)
seeds<-subset(seeds,!is.na(n_dev_seeds))
seeds <- dcast(seeds, pl_id_ALL ~ inmature+P_UP, value.var="n_dev_seeds")

head(seeds)
names(seeds)<-c("pl_id","P_m","UP_m","P_i","UP_i")
seeds$status<-ifelse(!is.na(seeds$P_i)&!is.na(seeds$UP_i),"Both_i",
                     ifelse(!is.na(seeds$P_i),"P_i",
                            ifelse(!is.na(seeds$UP_i),"UP_i","Both_m")))

head(as.data.frame(allplants)[c(4,6,18,20)])
seeds<-merge(seeds,as.data.frame(allplants)[c(4,6,18,20)])
seeds$seeds_per_fl<-(((rowSums(seeds[,c("UP_m", "UP_i")], na.rm=T))*seeds$fr_in)+
                       ((rowSums(seeds[,c("P_m", "P_i")], na.rm=T))*seeds$fr_pr))/seeds$n_fl

allplants<-merge(allplants,seeds[c(1:6,10)],all.x=T)
hist(allplants$seeds_per_fl)

allplants$attack_f<-as.factor(allplants$attack)

#### Effect of microclimate on plant performance ####

#Seeds per fl against temp and moist
# (w marked pls which are the only ones w seed data)
# UNIVARIATE linear regressions of n seeds per flower against temp and moist

model1<-lm(seeds_per_fl~meanT,data=allplants)
model2<-lm(seeds_per_fl~moist_per,data=allplants)
summary(model1)
summary(model2)

#### Model diagnostics ####

simulationOutput_model1<-simulateResiduals(fittedModel=model1,n=5000)
simulationOutput_model2<-simulateResiduals(fittedModel=model2,n=5000)

plot(simulationOutput_model1) # OK
plot(simulationOutput_model2) # OK

#### Test residuals for spatial autocorrelation ####

# Checking for spatial autocorrelation in the residuals of model1 and model2

allplants$X<-print(allplants@coords[,1],digits=22)
allplants$Y<-print(allplants@coords[,2],digits=22)

# Spatial correlogram model1 and model2
res_model1<-residuals(model1) #Get residuals of model
res_model2<-residuals(model2) #Get residuals of model
correlog_model1 <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                            y=subset(allplants,!is.na(seeds_per_fl))$Y,
                            res_model1,increment=1, resamp=100,quiet=F) 
correlog_model2 <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                            y=subset(allplants,!is.na(seeds_per_fl))$Y,
                            res_model2,increment=1, resamp=100,quiet=F) 
plot(correlog_model1)
plot(correlog_model2)

spline.correlog_model1 <- spline.correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                            y=subset(allplants,!is.na(seeds_per_fl))$Y,
                            res_model1,resamp=100,quiet=F,type="boot") 
spline.correlog_model2 <- spline.correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                            y=subset(allplants,!is.na(seeds_per_fl))$Y,
                            res_model2,resamp=100,quiet=F,type="boot") 
plot(spline.correlog_model1)
plot(spline.correlog_model2)

# Calculation of global Moran's I with a permutation test 
# (1000 random permutations) for model 1, based on a connectivity matrix of 
# pairwise Euclidean distances among the shoots up to a distance of 30 m. 

# Create neighbours matrix (30 m)
allplants.listw_seeds <- nb2listw(dnearneigh(subset(allplants,
                                               !is.na(seeds_per_fl)),0,30)) 

moran_model1<- moran.mc(res_model1, listw=allplants.listw_seeds,nsim=999)
moran_model2<- moran.mc(res_model2, listw=allplants.listw_seeds,nsim=999) 
moran_model1 #Significant autocorrelation in the residuals of model1
moran_model2 #Significant autocorrelation in the residuals of model2

# Moran's eigenvector mapping of model1 and model2
ME.model1 <-ME(seeds_per_fl~meanT,listw=allplants.listw1,
               data=subset(allplants,!is.na(seeds_per_fl)),
               alpha=0.05,verbose=T) 
ME.model2 <-ME(seeds_per_fl~moist_per,listw=allplants.listw1,
               data=subset(allplants,!is.na(seeds_per_fl)),
               alpha=0.05,verbose=T) 

# Repeat model1 with Moran's eigenvector as predictor
vector1<-ME.model1$vectors[,1]
vector2<-ME.model2$vectors[,1]

model1_ME<-lm(seeds_per_fl~meanT+vector1,
              data=subset(allplants,!is.na(seeds_per_fl)))
model2_ME<-lm(seeds_per_fl~moist_per+vector2,
              data=subset(allplants,!is.na(seeds_per_fl)))
summary(model1_ME) # Temp NS
summary(model2_ME) # Moisture *

### Figure 1 #### (not needed)

fig1a<-ggplot(data.frame(allplants),aes(x=meanT,y=seeds_per_fl))+
  xlab("Soil temperature (ºC)")+ylab(NULL)+ggtitle("A)")+my_theme()+
  geom_ribbon(data=ggpredict(model1_ME,terms = "meanT [all]"),
              aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
              fill="grey",alpha=0.7)+
  geom_line(data=ggpredict(model1_ME,terms = "meanT [all]"),
            aes(x=x,y=predicted),size=1,color="black")+
  geom_point(size=2,alpha=0.2)+
  ggtitle("A)")
fig1b<-ggplot(data.frame(allplants),aes(x=moist_per,y=seeds_per_fl))+
  xlab("Soil moisture (%)")+ylab(NULL)+ggtitle("B)")+my_theme()+
  geom_ribbon(data=ggpredict(model2_ME,terms = "moist_per [all]"),
              aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high),
              fill="grey",alpha=0.7)+
  geom_line(data=ggpredict(model2_ME,terms = "moist_per [all]"),
            aes(x=x,y=predicted),size=1,color="black")+
  geom_point(size=2,alpha=0.2)+
  ggtitle("B)")

fig1<-grid.arrange(fig1a,fig1b,ncol=2,
                   left=textGrob("Number of seeds per flower",just="center",
                                 hjust=0.42,
                                 gp=gpar(fontsize=16,fontfamily="serif"),
                                 rot = 90))

ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/plant/figures/fig1.tiff",
       plot=fig1,device="tiff",width=22,height=11,units="cm",dpi=300,
       compression="lzw")

# Checking for spatial autocorrelation 
# in the residuals of model1_ME and model2_ME 

# Spatial correlogram model1_ME and model2_ME
res_model1_ME<-residuals(model1_ME) #Get residuals of model
res_model2_ME<-residuals(model2_ME) #Get residuals of model
correlog_model1_ME <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                               y=subset(allplants,!is.na(seeds_per_fl))$Y,
                               res_model1_ME,increment=1, resamp=100,quiet=F) 
correlog_model2_ME <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                            y=subset(allplants,!is.na(seeds_per_fl))$Y,
                            res_model2_ME,increment=1, resamp=100,quiet=F) 
plot(correlog_model1_ME)
plot(correlog_model2_ME)

spline.correlog_model1_ME <- spline.correlog(x=subset(allplants,
                                                      !is.na(seeds_per_fl))$X,
                                          y=subset(allplants,
                                                   !is.na(seeds_per_fl))$Y,
                                          res_model1_ME,resamp=100,quiet=F,
                                          type="boot") 
spline.correlog_model2_ME <- spline.correlog(x=subset(allplants,
                                                      !is.na(seeds_per_fl))$X,
                                          y=subset(allplants,
                                                   !is.na(seeds_per_fl))$Y,
                                          res_model2_ME,resamp=100,quiet=F,
                                          type="boot") 
plot(spline.correlog_model1_ME)
plot(spline.correlog_model2_ME)

# Still some significant autocorrelation in both cases

# Moran's I

moran_model1_ME<- moran.mc(res_model1_ME, listw=allplants.listw_seeds,nsim=999)
moran_model2_ME<- moran.mc(res_model2_ME, listw=allplants.listw_seeds,nsim=999) 
moran_model1_ME # No spatial autocorrelation
moran_model2_ME # No spatial autocorrelation

# Simultaneous autoregressive error (SAR) models

model1_SAR<-errorsarlm(formula=seeds_per_fl~meanT,
                       data=subset(allplants,!is.na(seeds_per_fl)),
                       listw=allplants.listw_seeds,interval = NULL)
model2_SAR<-errorsarlm(formula=seeds_per_fl~moist_per,
                       data=subset(allplants,!is.na(seeds_per_fl)),
                       listw=allplants.listw_seeds,interval = NULL)
summary(model1_SAR,Nagelkerke=T)
summary(model2_SAR,Nagelkerke=T)

# Spatial correlograms
res_model1_SAR<-residuals(model1_SAR) #Get residuals of model
res_model2_SAR<-residuals(model2_SAR) #Get residuals of model
correlog_model1_SAR <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                               y=subset(allplants,!is.na(seeds_per_fl))$Y,
                               res_model1_SAR,increment=1, resamp=100,quiet=F) 
correlog_model2_SAR <- correlog(x=subset(allplants,!is.na(seeds_per_fl))$X,
                                y=subset(allplants,!is.na(seeds_per_fl))$Y,
                                res_model2_SAR,increment=1, resamp=100,quiet=F) 
plot(correlog_model1_SAR)
plot(correlog_model2_SAR)

spline.correlog_model1_SAR <- spline.correlog(x=subset(allplants,
                                                      !is.na(seeds_per_fl))$X,
                                             y=subset(allplants,
                                                      !is.na(seeds_per_fl))$Y,
                                             res_model1_SAR,resamp=100,quiet=F,
                                             type="boot") 
spline.correlog_model2_SAR <- spline.correlog(x=subset(allplants,
                                                      !is.na(seeds_per_fl))$X,
                                             y=subset(allplants,
                                                      !is.na(seeds_per_fl))$Y,
                                             res_model2_SAR,resamp=100,quiet=F,
                                             type="boot") 
plot(spline.correlog_model1_SAR)
plot(spline.correlog_model2_SAR)

# Moran's I
moran_model1_SAR<- moran.mc(res_model1_SAR,listw=allplants.listw_seeds,nsim=999) 
moran_model2_SAR<- moran.mc(res_model2_SAR,listw=allplants.listw_seeds,nsim=999) 
moran_model1_SAR # No spatial autocorrelation
moran_model2_SAR # No spatial autocorrelation

# Correlograms comparing the three models

# model1
corr_model1<-data.frame(cbind(distance=as.vector(correlog_model1$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model1$correlation[1:31]),
                      p=as.vector(correlog_model1$p[1:31])))
corr_model1_ME<-data.frame(cbind(distance=as.vector(correlog_model1_ME$mean.of.class[1:31]), 
                                  correlation=as.vector(correlog_model1_ME$correlation[1:31]),
                                  p=as.vector(correlog_model1_ME$p[1:31])))
corr_model1_SAR<-data.frame(cbind(distance=as.vector(correlog_model1_SAR$mean.of.class[1:31]), 
                              correlation=as.vector(correlog_model1_SAR$correlation[1:31]),
                              p=as.vector(correlog_model1_SAR$p[1:31])))

corr_model1$type<-"model1"
corr_model1_ME$type<-"model1_ME"
corr_model1_SAR$type<-"model1_SAR"
corr_1<-rbind(corr_model1,corr_model1_ME,corr_model1_SAR)
corr_1$sig<-as.factor(ifelse(corr_1$p<0.05,1,0))

# model2
corr_model2<-data.frame(cbind(distance=as.vector(correlog_model2$mean.of.class[1:31]), 
                              correlation=as.vector(correlog_model2$correlation[1:31]),
                              p=as.vector(correlog_model2$p[1:31])))
corr_model2_ME<-data.frame(cbind(distance=as.vector(correlog_model2_ME$mean.of.class[1:31]), 
                                 correlation=as.vector(correlog_model2_ME$correlation[1:31]),
                                 p=as.vector(correlog_model2_ME$p[1:31])))
corr_model2_SAR<-data.frame(cbind(distance=as.vector(correlog_model2_SAR$mean.of.class[1:31]), 
                                  correlation=as.vector(correlog_model2_SAR$correlation[1:31]),
                                  p=as.vector(correlog_model2_SAR$p[1:31])))

corr_model2$type<-"model2"
corr_model2_ME$type<-"model2_ME"
corr_model2_SAR$type<-"model2_SAR"
corr_2<-rbind(corr_model2,corr_model2_ME,corr_model2_SAR)
corr_2$sig<-as.factor(ifelse(corr_2$p<0.05,1,0))

### Figure A1 ####
figA1<-grid.arrange(
  ggplot(subset(corr_1,type!="model1_SAR"),aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + ylab(NULL)+
    scale_x_continuous("Distance (mean of class)",limits=c(0,30),
                       breaks=c(0,5,10,15,20,25,30)) + 
    scale_y_continuous(limits=c(-0.05,0.2),
                       breaks = c(-0.05,0,0.05,0.1,0.15,0.20))+
    scale_shape_manual(values=c(1,19))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("A)"),
  ggplot(subset(corr_2,type!="model2_SAR"),aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + ylab(NULL)+
    scale_x_continuous("Distance (mean of class)",limits=c(0,30),
                       breaks=c(0,5,10,15,20,25,30)) + 
    scale_y_continuous(limits=c(-0.05,0.2),
                       breaks = c(-0.05,0,0.05,0.1,0.15,0.20))+
    scale_shape_manual(values=c(1,19))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("B)"),
  ncol=2,left=textGrob("Correlation (Moran's I)",just="center",
                      hjust=0.42,
                      gp=gpar(fontsize=16,fontfamily="serif"),
                      rot = 90))

ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/plant/figures/figA1.tiff",
       plot=figA1,device="tiff",width=22,height=9,units="cm",dpi=300,
       compression="lzw")

ggplot(corr_2,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),
                     breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),
                     breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="top")+theme(text=element_text(family="serif"))

# Compare the three models

AIC(model1,model1_ME,model1_SAR)
AIC(model2,model2_ME,model2_SAR)

#### Correlation among temp and moist ####

cor(allplants$meanT,allplants$moist_per)

# Checked VIFs for all models below (without interaction meanT*moist_per)
# and they are all OK

#### How much of the effect of microclimate on plant performance is due to 
#### direct / indirect / indirect2 effects 
#### SEMs ####

# Component models of the SEM

subset1<-subset(allplants,!is.na(phen_int))
subset2<-subset(allplants,!is.na(phen_int)&!is.na(seeds_per_fl))

mod1<-lm(phen_int~meanT*moist_per,subset1,na.action=na.fail)
mod2<-glm.nb(round(Mrub_sum)~meanT*moist_per,allplants,na.action=na.fail)
mod3<-lm(pldens_3~meanT*moist_per,allplants,na.action=na.fail)
mod3_nb<-glm.nb(round(pldens_3)~meanT*moist_per,allplants,
                na.action=na.fail) # USE
mod4<-lm(phen_n3~meanT*moist_per,allplants,na.action=na.fail)
mod5<-glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+
            pldens_3+phen_n3+pldens_3:phen_n3+meanT*moist_per,
          subset1,family="binomial",na.action=na.fail)
mod5_1<-glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+
            pldens_3+phen_n3+pldens_3:phen_n3+meanT+moist_per,
          subset1,family="binomial",na.action=na.fail)
# Removed interaction meanT*moist_per because NS
mod6<-lm(seeds_per_fl~phen_int+as.integer(attack)+meanT*moist_per,subset2,
         na.action=na.fail)
mod6_1<-lm(seeds_per_fl~phen_int+as.integer(attack)+meanT+moist_per,subset2,
           na.action=na.fail) # Removed interaction meanT*moist_per because NS

AIC(mod3,mod3_nb)
AIC(mod5,mod5_1)
AIC(mod6,mod6_1)

# Model diagnostics of the component models

simulationOutput_mod1<-simulateResiduals(fittedModel=mod1,n=5000)
simulationOutput_mod2<-simulateResiduals(fittedModel=mod2,n=5000)
simulationOutput_mod3<-simulateResiduals(fittedModel=mod3,n=5000)
simulationOutput_mod3_nb<-simulateResiduals(fittedModel=mod3_nb,n=5000)
simulationOutput_mod4<-simulateResiduals(fittedModel=mod4,n=5000)
simulationOutput_mod5<-simulateResiduals(fittedModel=mod5,n=5000)
simulationOutput_mod5_1<-simulateResiduals(fittedModel=mod5_1,n=5000)
simulationOutput_mod6<-simulateResiduals(fittedModel=mod6,n=5000)
simulationOutput_mod6_1<-simulateResiduals(fittedModel=mod6_1,n=5000)

plot(simulationOutput_mod1)
plot(simulationOutput_mod2)
plot(simulationOutput_mod3)
plot(simulationOutput_mod3_nb)
plot(simulationOutput_mod4)
plot(simulationOutput_mod5)
plot(simulationOutput_mod5_1)
plot(simulationOutput_mod6)
plot(simulationOutput_mod6_1)

# VIFs of the component models without interactions

vif(lm(phen_int~meanT+moist_per,subset1,na.action=na.fail))
vif(glm.nb(round(Mrub_sum)~meanT+moist_per,allplants,na.action=na.fail))
vif(glm.nb(round(pldens_3)~meanT+moist_per,allplants,na.action=na.fail))
vif(lm(phen_n3~meanT+moist_per,allplants,na.action=na.fail))
vif(glm(as.integer(attack)~phen_int+round(Mrub_sum)+pldens_3+phen_n3+
          meanT+moist_per,subset1,family="binomial",na.action=na.fail))
vif(lm(seeds_per_fl~phen_int+as.integer(attack)+meanT+moist_per,subset2,
       na.action=na.fail))

# Test residuals for spatial autocorrelation



# Moran's eigenvector mapping of mod1-5
# We cannot use SAR because some of the responses (mod2, mod3 and mod5)
# are not normal
ME.mod1 <-ME(phen_int~meanT*moist_per,listw=allplants.listw1,
               data=subset(allplants,!is.na(phen_int)),
             family="gaussian",alpha=0.05,verbose=T) 
ME.mod2 <-ME(mod2,listw=allplants.listw_ants,alpha=0.05,verbose=T) 
ME.mod3_nb <-ME(mod3_nb,listw=allplants.listw_ants,alpha=0.05,verbose=T)
ME.mod4 <-ME(phen_n3~meanT*moist_per,listw=allplants.listw_ants,
             data=allplants,
             family="gaussian",alpha=0.05,verbose=T) 
ME.mod5_1 <-ME(as.integer(attack)~phen_int*round(Mrub_sum)+
               round(pldens_3)*phen_n3+meanT+moist_per,
             listw=allplants.listw1,
             data=subset(allplants,!is.na(phen_int)),
             family="binomial",alpha=0.05,verbose=T)
# Warning messages:
#   1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 4: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# Component models of the SEM with ME vectors

mod1_ME<-lm(phen_int~meanT*moist_per+ME.mod1$vectors[,1]+ME.mod1$vectors[,2],
            subset1,na.action=na.fail)
mod2_ME<-glm.nb(round(Mrub_sum)~meanT*moist_per+ME.mod2$vectors[,1],allplants,
                na.action=na.fail)
mod3_nb_ME<-glm.nb(round(pldens_3)~meanT*moist_per+
                     ME.mod3_nb$vectors[,1]+ME.mod3_nb$vectors[,2], 
                   allplants,
                  na.action=na.fail)
mod4_ME<-lm(phen_n3~meanT*moist_per+ME.mod4$vectors[,1]+ME.mod4$vectors[,2]+
            ME.mod4$vectors[,3]+ME.mod4$vectors[,4]+
            ME.mod4$vectors[,5],allplants,na.action=na.fail)
mod5_1_ME<-glm(as.integer(attack)~phen_int+round(Mrub_sum)+
                 phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+
                 meanT+moist_per+ME.mod5_1$vectors[,1],
               subset1,family="binomial",na.action=na.fail)

summary(mod1_ME)
summary(mod2_ME)
summary(mod3_nb_ME)
summary(mod4_ME)
summary(mod5_ME_1)
summary(mod6_1)

# Model diagnostics of the component models with ME vectors

simulationOutput_mod1_ME<-simulateResiduals(fittedModel=mod1_ME,n=5000)
simulationOutput_mod2_ME<-simulateResiduals(fittedModel=mod2_ME,n=5000)
simulationOutput_mod3_nb_ME<-simulateResiduals(fittedModel=mod3_nb_ME,n=5000)
simulationOutput_mod4_ME<-simulateResiduals(fittedModel=mod4_ME,n=5000)
simulationOutput_mod5_ME_1<-simulateResiduals(fittedModel=mod5_ME_1,n=5000)

plot(simulationOutput_mod1_ME)
plot(simulationOutput_mod2_ME)
plot(simulationOutput_mod3_nb_ME)
plot(simulationOutput_mod4_ME)
plot(simulationOutput_mod5_ME_1)

# Test residuals for spatial autocorrelation

# Correlograms - takes super long! Do not run again!!!

correlog_mod1_ME <- correlog(x=subset(allplants,!is.na(phen_int))$X,
                          y=subset(allplants,!is.na(phen_int))$Y,
                          residuals(mod1_ME),increment=1,resamp=100,quiet=F) 
correlog_mod2_ME <- correlog(x=allplants$X,
                          y=allplants$Y,
                          residuals(mod2_ME),increment=1, resamp=100,quiet=F) 
correlog_mod3_nb_ME <- correlog(x=allplants$X,
                          y=allplants$Y,
                          residuals(mod3_nb_ME),increment=1, resamp=100,quiet=F)
correlog_mod4_ME <- correlog(x=allplants$X,
                          y=allplants$Y,
                          residuals(mod4_ME),increment=1, resamp=100,quiet=F)
correlog_mod5_1_ME <- correlog(x=subset(allplants,!is.na(phen_int))$X,
                             y=subset(allplants,!is.na(phen_int))$Y,
                             residuals(mod5_1_ME),increment=1, resamp=100,
                             quiet=F)
plot(correlog_mod1_ME)
plot(correlog_mod2_ME)
plot(correlog_mod3_nb_ME)
plot(correlog_mod4_ME)
plot(correlog_mod5_1_ME)

# Moran's I - takes super long! Do not run again!!!

moran_mod1_ME<- moran.mc(residuals(mod1_ME),listw=allplants.listw1,nsim=999)
moran_mod2_ME<- moran.mc(residuals(mod2_ME),listw=allplants.listw_ants,nsim=999)
moran_mod3_nb_ME<- moran.mc(residuals(mod3_nb_ME),listw=allplants.listw_ants,nsim=999)
moran_mod4_ME<- moran.mc(residuals(mod4_ME),listw=allplants.listw_ants,nsim=999)
moran_mod5_1_ME<- moran.mc(residuals(mod5_1_ME),listw=allplants.listw1,nsim=999)

moran_mod1_ME # No significant autocorrelation in the residuals of mod1_ME
moran_mod2_ME # No significant autocorrelation in the residuals of mod2_ME
moran_mod3_nb_ME # No significant autocorrelation in the residuals of mod3_nb_ME
moran_mod4_ME # No significant autocorrelation in the residuals of mod4_ME
moran_mod5_1_ME # Significant autocorrelation in the residuals of mod5_1_ME

# Spatial autocorrelation is mostly removed by including ME vectors
# (except in moran_mod5_1_ME, p-value went from 0.001 to 0.024)

# Correlograms comparing models with and without ME vectors

# mod1
corr_mod1<-data.frame(cbind(distance=as.vector(correlog_mod1$mean.of.class[1:31]), 
                              correlation=as.vector(correlog_mod1$correlation[1:31]),
                              p=as.vector(correlog_mod1$p[1:31])))
corr_mod1_ME<-data.frame(cbind(distance=as.vector(correlog_mod1_ME$mean.of.class[1:31]), 
                                 correlation=as.vector(correlog_mod1_ME$correlation[1:31]),
                                 p=as.vector(correlog_mod1_ME$p[1:31])))

corr_mod1$type<-"mod1"
corr_mod1_ME$type<-"mod1_ME"
corr_sem_1<-rbind(corr_mod1,corr_mod1_ME)
corr_sem_1$sig<-as.factor(ifelse(corr_sem_1$p<0.05,1,0))

# mod2
corr_mod2<-data.frame(cbind(distance=as.vector(correlog_mod2$mean.of.class[1:31]), 
                            correlation=as.vector(correlog_mod2$correlation[1:31]),
                            p=as.vector(correlog_mod2$p[1:31])))
corr_mod2_ME<-data.frame(cbind(distance=as.vector(correlog_mod2_ME$mean.of.class[1:31]), 
                               correlation=as.vector(correlog_mod2_ME$correlation[1:31]),
                               p=as.vector(correlog_mod2_ME$p[1:31])))

corr_mod2$type<-"mod2"
corr_mod2_ME$type<-"mod2_ME"
corr_sem_2<-rbind(corr_mod2,corr_mod2_ME)
corr_sem_2$sig<-as.factor(ifelse(corr_sem_2$p<0.05,1,0))

# mod3
corr_mod3<-data.frame(cbind(distance=as.vector(correlog_mod3_nb$mean.of.class[1:31]), 
                            correlation=as.vector(correlog_mod3_nb$correlation[1:31]),
                            p=as.vector(correlog_mod3_nb$p[1:31])))
corr_mod3_ME<-data.frame(cbind(distance=as.vector(correlog_mod3_nb_ME$mean.of.class[1:31]), 
                               correlation=as.vector(correlog_mod3_nb_ME$correlation[1:31]),
                               p=as.vector(correlog_mod3_nb_ME$p[1:31])))

corr_mod3$type<-"mod3"
corr_mod3_ME$type<-"mod3_ME"
corr_sem_3<-rbind(corr_mod3,corr_mod3_ME)
corr_sem_3$sig<-as.factor(ifelse(corr_sem_3$p<0.05,1,0))

# mod4
corr_mod4<-data.frame(cbind(distance=as.vector(correlog_mod4$mean.of.class[1:31]), 
                            correlation=as.vector(correlog_mod4$correlation[1:31]),
                            p=as.vector(correlog_mod4$p[1:31])))
corr_mod4_ME<-data.frame(cbind(distance=as.vector(correlog_mod4_ME$mean.of.class[1:31]), 
                               correlation=as.vector(correlog_mod4_ME$correlation[1:31]),
                               p=as.vector(correlog_mod4_ME$p[1:31])))

corr_mod4$type<-"mod4"
corr_mod4_ME$type<-"mod4_ME"
corr_sem_4<-rbind(corr_mod4,corr_mod4_ME)
corr_sem_4$sig<-as.factor(ifelse(corr_sem_4$p<0.05,1,0))

# mod5
corr_mod5<-data.frame(cbind(distance=as.vector(correlog_mod5_1$mean.of.class[1:31]), 
                            correlation=as.vector(correlog_mod5_1$correlation[1:31]),
                            p=as.vector(correlog_mod5_1$p[1:31])))
corr_mod5_ME<-data.frame(cbind(distance=as.vector(correlog_mod5_1_ME$mean.of.class[1:31]), 
                               correlation=as.vector(correlog_mod5_1_ME$correlation[1:31]),
                               p=as.vector(correlog_mod5_1_ME$p[1:31])))

corr_mod5$type<-"mod5"
corr_mod5_ME$type<-"mod5_ME"
corr_sem_5<-rbind(corr_mod5,corr_mod5_ME)
corr_sem_5$sig<-as.factor(ifelse(corr_sem_5$p<0.05,1,0))

# mod6
corr_sem_6<-data.frame(cbind(distance=as.vector(correlog_mod6_1$mean.of.class[1:31]), 
                            correlation=as.vector(correlog_mod6_1$correlation[1:31]),
                            p=as.vector(correlog_mod6_1$p[1:31])))
corr_sem_6$sig<-as.factor(ifelse(corr_sem_6$p<0.05,1,0))

### Figure A2 ####
figA2<-grid.arrange(
  ggplot(corr_sem_1,aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.1,0.4),
                       breaks = c(-0.1,0,0.1,0.20,0.30,0.40))+
    scale_shape_manual(values=c(1,19))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 1")+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.006, p = 0.001",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif"))))+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.000, p = 0.113",
                                        x=0.18,y=0.85,hjust=0,
                                        gp=gpar(col="darkgrey",fontsize=16,
                                                fontfamily="serif")))),
  ggplot(corr_sem_2,aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.4,1.2),
                       breaks = c(-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,1.2))+
    scale_shape_manual(values=c(19,1))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 2")+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.004, p = 0.001",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif"))))+
    annotation_custom(grobTree(textGrob("Global Moran's I = -0.002, p = 0.999",
                                        x=0.18,y=0.85,hjust=0,
                                        gp=gpar(col="darkgrey",fontsize=16,
                                                fontfamily="serif")))),
  ggplot(corr_sem_3,aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.5,1.25),
                        breaks = seq(-0.5,1.25,0.25))+
    scale_shape_manual(values=c(1,19))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 3")+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.085, p = 0.001",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif"))))+
    annotation_custom(grobTree(textGrob("Global Moran's I = -0.010, p = 0.999",
                                        x=0.18,y=0.85,hjust=0,
                                        gp=gpar(col="darkgrey",fontsize=16,
                                                fontfamily="serif")))),
  ggplot(corr_sem_4,aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.25,1),
                        breaks =seq(-0.25,1,0.25))+
    scale_shape_manual(values=c(19,1))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 4")+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.101, p = 0.001",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif"))))+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.000, p = 0.651",
                                        x=0.18,y=0.85,hjust=0,
                                        gp=gpar(col="darkgrey",fontsize=16,
                                                fontfamily="serif")))),
  ggplot(corr_sem_5,aes(x=distance, y=correlation)) +
    geom_point(aes(colour=type,shape=sig),size=2) +
    geom_line(aes(colour=type)) + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.05,0.25),
                        breaks =seq(-0.05,0.25,0.05))+
    scale_shape_manual(values=c(1,19))+
    scale_color_manual(values=c("black","darkgrey"))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 5")+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.007, p = 0.001",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif"))))+
    annotation_custom(grobTree(textGrob("Global Moran's I = 0.000, p = 0.024",
                                        x=0.18,y=0.85,hjust=0,
                                        gp=gpar(col="darkgrey",fontsize=16,
                                                fontfamily="serif")))),
  ggplot(corr_sem_6,aes(x=distance, y=correlation)) +
    geom_point(aes(shape=sig),size=2,colour="black") +
    geom_line(colour="black") + xlab(NULL) + ylab(NULL)+
    scale_x_continuous(limits=c(0,30),
                       breaks=c(0,10,20,30)) + 
    scale_y_continuous(limits=c(-0.05,0.15),
                        breaks =seq(-0.05,0.15,0.05))+
    scale_shape_manual(values=c(1,19))+
    geom_hline(aes(yintercept=0), colour="grey",linetype=3)+
    my_theme()+ggtitle("Component model 6")+
    annotation_custom(grobTree(textGrob("Global Moran's I = -0.004, p = 0.591",
                                        x=0.18,y=0.95,hjust=0,
                                        gp=gpar(col="black",fontsize=16,
                                                fontfamily="serif")))),
  ncol=2,
  left=textGrob("Correlation (Moran's I)",just="center",
                hjust=0.42,gp=gpar(fontsize=16,fontfamily="serif"),rot=90),
  bottom=textGrob("Distance (mean of class)",just="center",
                  hjust=0.42,gp=gpar(fontsize=16,fontfamily="serif")))

ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/plant/figures/figA2.tiff",
       plot=figA2,device="tiff",width=25,height=30,units="cm",dpi=300,
       compression="lzw") 

# Fitting the SEM

vector1_1<-data.frame(FID=subset(allplants,!is.na(phen_int))$FID,
                      vector1_1=ME.mod1$vectors[,1])
vector1_2<-data.frame(FID=subset(allplants,!is.na(phen_int))$FID,
                      vector1_2=ME.mod1$vectors[,2])
vector2_1<-data.frame(FID=allplants$FID,vector2_1=ME.mod2$vectors[,1])
vector3_1<-data.frame(FID=allplants$FID,vector3_1=ME.mod3_nb$vectors[,1])
vector3_2<-data.frame(FID=allplants$FID,vector3_2=ME.mod3_nb$vectors[,2])
vector4_1<-data.frame(FID=allplants$FID,vector4_1=ME.mod4$vectors[,1])
vector4_2<-data.frame(FID=allplants$FID,vector4_2=ME.mod4$vectors[,2])
vector4_3<-data.frame(FID=allplants$FID,vector4_3=ME.mod4$vectors[,3])
vector4_4<-data.frame(FID=allplants$FID,vector4_4=ME.mod4$vectors[,4])
vector4_5<-data.frame(FID=allplants$FID,vector4_5=ME.mod4$vectors[,5])
vector5_1<-data.frame(FID=subset(allplants,!is.na(phen_int))$FID,
                      vector5_1=ME.mod5_1$vectors[,1])

allplants_df<-data.frame(allplants)%>%
  full_join(vector1_1)%>%full_join(vector1_2)%>%full_join(vector2_1)%>%
  full_join(vector3_1)%>%full_join(vector3_2)%>%full_join(vector4_1)%>%
  full_join(vector4_2)%>%full_join(vector4_3)%>%full_join(vector4_4)%>%
  full_join(vector4_5)%>%full_join(vector5_1) 

allplants_df$Mrub_sum_round<-round(allplants_df$Mrub_sum)
allplants_df$pldens_3_round<-round(allplants_df$pldens_3)

#### SEM with no ME vectors ####

sem1<-psem(
  # Model 1)
  lm(phen_int~meanT*moist_per,subset(allplants_df,!is.na(phen_int))),
  # Model 2)
  glm.nb(Mrub_sum_round~meanT*moist_per,allplants_df),
  # Model 3)
  glm.nb(pldens_3_round~meanT*moist_per,allplants_df),
  # Model 4)
  lm(phen_n3~meanT*moist_per,allplants_df),
  # Model 5)
  glm(as.integer(attack)~phen_int*Mrub_sum_round+pldens_3_round*phen_n3+
        meanT+moist_per,subset(allplants_df,!is.na(phen_int)),
      family="binomial"),
  # Model 6)
  lm(seeds_per_fl~phen_int+as.integer(attack)+meanT+moist_per,
     subset(allplants_df,!is.na(phen_int)))
)

summary(sem1,conserve=TRUE) # p=0, include correlated errors

sem1<-psem(
  # Model 1)
  lm(phen_int~meanT*moist_per,subset(allplants_df,!is.na(phen_int))),
  # Model 2)
  glm.nb(Mrub_sum_round~meanT*moist_per,allplants_df),
  # Model 3)
  glm.nb(pldens_3_round~meanT*moist_per,allplants_df),
  # Model 4)
  lm(phen_n3~meanT*moist_per,allplants_df),
  # Model 5)
  glm(as.integer(attack)~phen_int*Mrub_sum_round+pldens_3_round*phen_n3+
        meanT+moist_per,subset(allplants_df,!is.na(phen_int)),
      family="binomial"),
  # Model 6)
  lm(seeds_per_fl~phen_int+as.integer(attack)+meanT+moist_per,
     subset(allplants_df,!is.na(phen_int))),
  # Correlated errors
  phen_n3 %~~% phen_int,phen_n3 %~~% pldens_3_round,phen_n3 %~~% Mrub_sum_round,
  pldens_3_round %~~% Mrub_sum_round,Mrub_sum_round %~~% phen_int,
  pldens_3_round %~~% phen_int
)

summary(sem1) # p=0.089, OK!!!

#### HERE: SEM with ME vectors ####

# Problems when including many vectors
# Also the test of direct separation suggested several missing paths 
# including vectors, which we don't want to include

# Show SEM without ME vectors and then (in Appendix) 
# results of models wwith ME vectors

summary(mod1_ME)
summary(mod2_ME)
summary(mod3_nb_ME)
summary(mod4_ME)
summary(mod5_ME_1)
summary(mod6_1)

# moist_per --> attack: * without ME vectors, but NS with ME vectors

# Refit mod5 without most_per and check for autocorrelation in residuals

mod5_2<-glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+
              pldens_3+phen_n3+pldens_3:phen_n3+meanT,
            subset1,family="binomial",na.action=na.fail)

moran_mod5_2<- moran.mc(residuals(mod5_2),listw=allplants.listw1,nsim=99)
moran_mod5_2

ME.mod5_2 <-ME(as.integer(attack)~phen_int*round(Mrub_sum)+
                 round(pldens_3)*phen_n3+meanT,
               listw=allplants.listw1,
               data=subset(allplants,!is.na(phen_int)),
               family="binomial",alpha=0.05,verbose=T) #RUN

### HERE, try this ####

# mod1 and 4 as SAR models

mod1_SAR<-errorsarlm(formula=phen_int~meanT*moist_per,
            data=subset1,listw=allplants.listw1,interval = NULL)
mod4_SAR<-errorsarlm(formula=phen_n3~meanT*moist_per,
                     data=allplants,listw=allplants.listw_ants,interval = NULL)

# SEM with SAR models (mod1 and 4) ME vectors (mod2, 3, 5)

sem1_spatial<-psem(
  # Model 1)
  errorsarlm(formula=phen_int~meanT*moist_per,
             data=subset1,listw=allplants.listw1,interval = NULL),
  # Model 2)
  glm.nb(Mrub_sum_round~meanT*moist_per+vector2_1,allplants_df),
  # Model 3)
  glm.nb(pldens_3_round~meanT*moist_per+vector3_1+vector3_2,allplants_df),
  # Model 4)
  errorsarlm(formula=phen_n3~meanT*moist_per,
             data=allplants,listw=allplants.listw_ants,interval = NULL),
  # Model 5)
  glm(as.integer(attack)~phen_int*Mrub_sum_round+pldens_3_round*phen_n3+
        meanT*moist_per+vector5_1,subset(allplants_df,!is.na(phen_int)),
      family="binomial"),
  # Model 6)
  lm(seeds_per_fl~phen_int+as.integer(attack)+meanT*moist_per,
     subset(allplants_df,!is.na(phen_int))),
  # Correlated errors
  phen_n3 %~~% phen_int,phen_n3 %~~% pldens_3_round,phen_n3 %~~% Mrub_sum_round,
  pldens_3_round %~~% Mrub_sum_round,Mrub_sum_round %~~% phen_int,
  pldens_3_round %~~% phen_int
)
# Does not work!

summary(sem1_spatial,conserve=T) # p=

#### Testing if preyed fruits have fewer seeds than intact #####

seeds_P_UP<-seeds%>%
  mutate(seeds_UP=rowSums(.[c(3,5)],na.rm=T),
         seeds_P=rowSums(.[c(2,4)],na.rm=T))%>%
  select(c("pl_id","seeds_UP","seeds_P"))%>%
  pivot_longer(cols=seeds_UP:seeds_P,names_to="P_UP",values_to="seeds")

ggplot(seeds_P_UP,aes(x=P_UP,y=seeds))+geom_boxplot()

summary(lm(seeds~P_UP,seeds_P_UP))
summary(glm(seeds~P_UP,seeds_P_UP,family=poisson))
summary(glm.nb(seeds~P_UP,seeds_P_UP))
  
seeds_P_UP%>%
  group_by(P_UP)%>%
  summarise(mean=mean(seeds),sd=sd(seeds))



#### OLD below ####


#### Model selection ####

#From Oikos paper: For each of the two piecewise SEMs, we constructed a global model containing all 
#possible paths. These models were then simplified by backwards stepwise removal of paths based on 
#Akaike information criterion corrected for small sample sizes (AICc). For this, we constructed 
#alternative models by removing one path at a time, and considered that alternative models improved 
#the model fit to the data if the AICc was more than two units lower than the AICc of the global model
#(models with ΔAICc > 2 are considered to fit the data equally well, Burnham and Anderson 2002).




# Model selection using this model

#Starting model

# #Step1 - removed PC1-->seeds_per_fl
# sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
#             glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
#             lm(pldens_3~PC1+PC2,allplants_clean_rubra),
#             lm(phen_n3~PC1+PC2,allplants_clean_rubra),
#             glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
#                 subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~phen_int+as.integer(attack)+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
#             phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
# summary(sem14) # p=0.127, interaction *!
# AIC(sem14,aicc=T) # AICc=80.635->no more than 2 units lower
# 
# #Step2 - removed PC2-->seeds_per_fl
# sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
#             glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
#             lm(pldens_3~PC1+PC2,allplants_clean_rubra),
#             lm(phen_n3~PC1+PC2,allplants_clean_rubra),
#             glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
#                 subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~phen_int+as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
#             phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
# summary(sem14) # p=0.163, interaction *!
# AIC(sem14,aicc=T) # AICc=79.931->no more than 2 units lower

# This could be a final model (using neigh_dens*phen)

# #Step3 - removed phen_int-->seeds_per_fl
# sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
#             glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
#             lm(pldens_3~PC1+PC2,allplants_clean_rubra),
#             lm(phen_n3~PC1+PC2,allplants_clean_rubra),
#             glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
#                 subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
#             phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
# summary(sem14) # p=0.118, interaction *!
# AIC(sem14,aicc=T) # AICc=81.616->higher

# Repeat model using another distribution for ants
# in order to be able to get standardized coefs
sem15<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            lm(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
summary(sem15) # p=0.096, interaction *!
AIC(sem15,aicc=T) # AICc=81.199

# This model (sem15) will be used ONLY to get the values of the
# standardized coefficients from PC1->ants and PC2->ants
# But the significances for those will still be taken from sem14

#### Effects (using sem14) ####

coefs1<-coefs(sem14)
coefs1

coefs2<-coefs(sem15)
coefs2

#PC1

#Direct effect of PC1 on seed sper fl: -0.0322
as.numeric(coefs1[19,8])

#Total indirect effect of PC1 on seeds per fl: -0.000726414 (prev -1.176888)
as.numeric(coefs1[13,8])*as.numeric(coefs1[18,8])+                                  #PC1->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*as.numeric(coefs1[17,8])+                                 #PC1->phen->seeds_per_fl
  as.numeric(coefs1[1,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+         #PC1->phen->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC1->ants->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC1->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC1->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC1->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC1->phen_n3->int->attack->seeds_per_fl

#Indirect effects of PC1 on seeds per fl acting through attack: -0.05200965 (prev -0.2051301)
as.numeric(coefs1[13,8])*as.numeric(coefs1[18,8])                                   #PC1->attack->seeds_per_fl

#Indirect effects of PC1 on seeds per fl acting through phen: 0.02514024 (prev 0.02162098)
as.numeric(coefs1[1,8])*as.numeric(coefs1[17,8])                                    #PC1->phen->seeds_per_fl

#(Very) indirect effects of PC1 on seeds per fl acting through effects of phen, ants and suit on attack: 0.026143 (prev -0.9933788)
as.numeric(coefs1[1,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+           #PC1->phen->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC1->ants->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC1->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC1->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC1->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC1->phen_n3->int->attack->seeds_per_fl

#(Very) indirect effects of PC1 on seeds per fl acting through effects of phen, ants on attack: -0.0496366 
as.numeric(coefs1[1,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+           #PC1->phen->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC1->ants->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])      #PC1->ants->int->attack->seeds_per_fl

#(Very) indirect effects of PC1 on seeds per fl acting through effects of suit on attack: 0.0757796
as.numeric(coefs1[5,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC1->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC1->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC1->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC1->phen_n3->int->attack->seeds_per_fl

#PC2

#Direct effect of PC2 on seeds per fl: -0.0401
as.numeric(coefs1[20,8])

#Total indirect effect of PC2 on seeds per fl: -0.1286283 (prev 0.6608619)
as.numeric(coefs1[14,8])*as.numeric(coefs1[18,8])+                                  #PC2->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*as.numeric(coefs1[17,8])+                                 #PC2->phen->seeds_per_fl
  as.numeric(coefs1[2,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+         #PC2->phen->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC2->ants->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC2->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC2->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC2->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC2->phen_n3->int->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through attack: -0.07330015 (prev -0.5916516)
as.numeric(coefs1[14,8])*as.numeric(coefs1[18,8])                                   #PC2->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through phen: -0.02611656 (prev -0.02943603)
as.numeric(coefs1[2,8])*as.numeric(coefs1[17,8])                                    #PC2->phen->seeds_per_fl

#(Very) indirect effects of PC2 on seeds per fl acting through effects of phen, ants and suit on attack: -0.0292116 (prev 1.28195)
as.numeric(coefs1[2,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+           #PC2->phen->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC2->ants->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC2->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC2->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC2->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC2->phen_n3->int->attack->seeds_per_fl

#(Very) indirect effects of PC2 on seeds per fl acting through effects of phen, ants on attack: 0.05478016
as.numeric(coefs1[2,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+           #PC2->phen->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+        #PC2->ants->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*1*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])      #PC2->ants->int->attack->seeds_per_fl

#(Very) indirect effects of PC2 on seeds per fl acting through effects of suit on attack: -0.08399176
as.numeric(coefs1[6,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+        #PC2->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+      #PC2->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+        #PC2->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*1*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])       #PC2->phen_n3->int->attack->seeds_per_fl

# Did not make it work from here!

# semEff

# Convert model to list (with easier model for a start)

easy_model<-list(lm(phen_int~PC1,subset(allplants_clean_rubra,!is.na(phen_int))),
                 glm(as.integer(attack)~phen_int+PC1,subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
                 lm(seeds_per_fl~phen_int+as.integer(attack)+PC1,subset(allplants_clean_rubra,!is.na(seeds_per_fl))))

effects1<-semEff(easy_model) # Error in sort.int(x, na.last = na.last, decreasing = decreasing, ...) : 
# 'x' must be atomic


# Bootstrap model effects (takes a while...)
system.time(easy_model_boot <- bootEff(easy_model, seed = 53908))
eff <- semEff(easy_model_boot)



effects1<-semEff()

effects1<-semEff(list(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
                      glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
                      lm(pldens_3~PC1+PC2,allplants_clean_rubra),
                      lm(phen_n3~PC1+PC2,allplants_clean_rubra),
                      glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                          subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
                      lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))))

pbPost("note", "R has finished!")

