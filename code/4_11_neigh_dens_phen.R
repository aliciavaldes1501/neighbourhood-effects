library(ggplot2)
library(ggthemes)
library(gridExtra)
library(FactoMineR)
library(piecewiseSEM)
library(MASS)
library(cowplot)
library(effects)

allplants<-as.data.frame(allplants)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
theme_set( theme_base( base_family= "Times"))

### Repeating analyses with neighbor density * phenology instead of suit_neigh ###

cor(allplants$suit_neigh,allplants$pldens_3)
cor(allplants$suit_neigh,allplants$phen_n3)

# Before scaling, suit_neigh is basically a measure of neighbor density

#### How much of the effect of microclimate on plant performance is due to direct / indirect / indirect2 effects 
#### Path analyses ####

allplants$Mrub_sch_s_pres_num<-ifelse(allplants$Mrub_sch_s_pres==0,0,1) 
#Ant presence instead of abundance, to use binomial model
#All models are then lm or binomial GLM --> OK to use scale

#### Model selection ####

#From Oikos paper: For each of the two piecewise SEMs, we constructed a global model containing all 
#possible paths. These models were then simplified by backwards stepwise removal of paths based on 
#Akaike information criterion corrected for small sample sizes (AICc). For this, we constructed 
#alternative models by removing one path at a time, and considered that alternative models improved 
#the model fit to the data if the AICc was more than two units lower than the AICc of the global model
#(models with ΔAICc  2 are considered to fit the data equally well, Burnham and Anderson 2002).

#### Models egg occurrence ####

#Starting model
model10_1<-lm(phen_int~PC1+PC2,subset(allplants,!is.na(phen_int)))
model10_2<-glm(Mrub_sch_s_pres_num~PC1+PC2,subset(allplants,!is.na(phen_int)),family="binomial")
model10_3<-lm(pldens_3~PC1+PC2,subset(allplants,!is.na(phen_int)))
model10_4<-lm(phen_n3~PC1+PC2,subset(allplants,!is.na(phen_int)))
model10_5<-glm(attack~phen_int+Mrub_sch_s_pres_num+phen_int:Mrub_sch_s_pres_num+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
               subset(allplants,!is.na(phen_int)),family="binomial")
model10_6<-lm(seeds_per_fl~phen_int+attack+PC1+PC2,subset(allplants,!is.na(phen_int))) 
sem10<-psem(
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5,
  model10_6,
  pldens_3%~~%phen_int,
  pldens_3%~~%Mrub_sch_s_pres_num,
  phen_n3%~~%phen_int,
  phen_n3%~~%pldens_3
)
summary(sem10, data=allplants) #p=0.157, AICc=79.81

# No path can be removed to get ΔAICc > 2 --> KEEP 
  


