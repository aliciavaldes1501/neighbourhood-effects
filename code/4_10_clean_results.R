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

#### Effect of the presence of different ant species on egg occurrence ####

allplants$allM_p<-ifelse(allplants$allM_sum>=1,1,0)
allplants$Mrub_sch_p<-ifelse(allplants$Mrub_p==1|allplants$Msch_p==1,1,0)

# Univariate GLMs relating the probability of having eggs (binomial model) and the number of eggs in plants with 
# at least one egg (negative binomial model) to the presence of different ant species. 

summary(glm(attack_f~allM_p,allplants,family="binomial")) #-0.5924
summary(glm(attack_f~Mrub_p,allplants,family="binomial")) #0.22487+
summary(glm(attack_f~Msca_p,allplants,family="binomial")) #-1.58440
summary(glm(attack_f~Mrug_p,allplants,family="binomial")) #-0.25954
summary(glm(attack_f~Msch_p,allplants,family="binomial")) #1.46293+
summary(glm(attack_f~oth_p,allplants,family="binomial")) #-0.85653
summary(glm(attack_f~Mrub_sch_s_pres,allplants,family="binomial")) #0.88833
summary(glm(attack_f~Mrub_sch_p,allplants,family="binomial")) #1.08644

with(allplants,cor(Mrub_sch_p,as.numeric(Mrub_sch_s_pres))) #0.76
#Is Mrub_sch_p a better variable??? But no effect in  models, keep using Mrub_sch_s_pres

summary(glm.nb(n_eggs_max~allM_p,subset(allplants,n_eggs_max>0))) #-0.2663 NS
summary(glm.nb(n_eggs_max~Mrub_p,subset(allplants,n_eggs_max>0))) #0.08668 NS
summary(glm.nb(n_eggs_max~Msca_p,subset(allplants,n_eggs_max>0))) #-0.09730 NS
summary(glm.nb(n_eggs_max~Mrug_p,subset(allplants,n_eggs_max>0))) #-0.04657 NS
summary(glm.nb(n_eggs_max~Msch_p,subset(allplants,n_eggs_max>0))) #0.07376 NS
summary(glm.nb(n_eggs_max~oth_p,subset(allplants,n_eggs_max>0))) #-0.05900 NS
summary(glm.nb(n_eggs_max~Mrub_sch_s_pres,subset(allplants,n_eggs_max>0))) #0.10116 NS
summary(glm.nb(n_eggs_max~Mrub_sch_p,subset(allplants,n_eggs_max>0))) #0.14917*

#Not very conclusive, maybe use results with abundances (supp. material in paper 3.1)
#Keep using Mrub_sch_s_pres

#### Effect of microclimate on plant performance ####

#Seeds per fl against temp and moist (w marked pls which are the only ones w seed data)

summary(lm(seeds_per_fl~meanT+moist_per,data=allplants)) #Interaction was NS, here temp* 

p1<-ggplot(allplants,aes(x=meanT,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil temperature (ºC)")+ylab("Number of seeds per flower")+    
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))
p2<-ggplot(allplants,aes(x=moist_per,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil moisture (%)")+ylab("Number of seeds per flower")+   
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))+
  theme(axis.title.y = element_text(colour = "white"),axis.text.y=element_text(colour="white"))

ggdraw()+
  draw_plot(p2,1/2-0.03,0.01,0.52,0.98)+
  draw_plot(p1,0.01,0.01,0.52,0.98)+
  draw_label(label="A)",x=0.02,y=0.97, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.545,y=0.97, fontfamily = "serif", fontface = 1)  #Fig seeds / temp - moist
ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/plant/figures/fig1.tiff",
       device="tiff",width=20,height=10,units="cm",dpi=600)

summary(lm(seeds_per_fl~meanT,allplants)) #*
summary(lm(seeds_per_fl~moist_per,allplants)) #P=0.0589

#Correlation among temp and moist (sampling points)

head(data_pts)

cor(data_pts$meanT,data_pts$MOIST_PER_M,use="complete.obs") # -0.4048787
cor.test(data_pts$meanT,data_pts$MOIST_PER_M,use="complete.obs") # -0.4048787

ggplot(data_pts,aes(x=meanT,y=MOIST_PER_M))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil temperature (ºC)")+ylab("Soil moisture (%)")+   
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

with(data_pts,summary(lm(MOIST_PER_M~meanT))) #*

#As they are correlated --> PCA

#PCA with temp and moist
PCA_env2<-prcomp(~allplants$meanT+allplants$moist_per,center=T,scale=T)
summary(PCA_env2)
plot(PCA_env2)
biplot(PCA_env2)

PCA_env2_scores<-as.data.frame(predict(PCA_env2))
allplants$PC1<-PCA_env2_scores$PC1
allplants$PC2<-PCA_env2_scores$PC2
hist(allplants$PC1)
hist(allplants$PC2)

PCA_env2 <- PCA(allplants[c(32,42)], graph = T)
PCA_env2
summary(PCA_env2)

#loadings
sweep(PCA_env2$var$coord,2,sqrt(PCA_env2$eig[1:ncol(PCA_env2$var$coord),1]),FUN="/")

PC1 <- PCA_env2$ind$coord[,1]
PC2 <- PCA_env2$ind$coord[,2]
labs <- rownames(PCA_env2$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- labs

vPC1 <- PCA_env2$var$coord[,1]
vPC2 <- PCA_env2$var$coord[,2]
vlabs <- rownames(PCA_env2$var$coord)
vPCs <- data.frame(cbind(vPC1,vPC2))
rownames(vPCs) <- vlabs
colnames(vPCs) <- colnames(PCs)

angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
ggplot() + theme_base(base_size = 20) + geom_path(aes(x, y), data = df, colour="grey70") + 
  geom_text(data=vPCs, aes(x=vPC1,y=vPC2,label=c("Soil temperature","Soil moisture")), size=5,family="serif") + xlab("PC1 (68.46%)") + ylab("PC2 (31.54%)")+
  geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(1.5, 'picas'),angle=20,type="open"), color = "black",size=1.5)+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_hline(yintercept = 0,lty=3,size=1)+geom_vline(xintercept = 0,lty=3,size=1) #Biplot of PCA

# PC1 = high temp, low moist
# PC2 = high temp & moist
 
#Seeds per fl against PC1 and PC2 (w marked pls which are the only ones w seed data)

summary(lm(seeds_per_fl~PC1+PC2,data=allplants)) #PC1* 

p3<-ggplot(allplants,aes(x=PC1,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("PC1 (high temperature, low moisture)")+ylab("Number of seeds per flower")+    
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))
p4<-ggplot(allplants,aes(x=PC2,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("PC1 (high temperature, high moisture)")+ylab("Number of seeds per flower")+   
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))+
  theme(axis.title.y = element_text(colour = "white"),axis.text.y=element_text(colour="white"))

ggdraw()+
  draw_plot(p4,1/2-0.03,0.01,0.52,0.98)+
  draw_plot(p3,0.01,0.01,0.52,0.98)+
  draw_label(label="A)",x=0.02,y=0.97, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.55,y=0.97, fontfamily = "serif", fontface = 1)  #Fig seeds / PC1 - PC2

summary(lm(seeds_per_fl~PC1,allplants)) #*
summary(lm(seeds_per_fl~PC2,allplants)) #NS

#### How much of the effect of microclimate on plant performance is due to direct / indirect / indirect2 effects 
#### Path analyses ####

allplants$Mrub_sch_s_pres<-as.factor(allplants$Mrub_sch_s_pres) 
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
model10_1<-lm(phen_int~PC1+PC2,allplants)
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,allplants)
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,allplants) 
sem10<-list(
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=allplants) #p=0.038, AICc=75.467
sem.coefs(sem10, data=allplants,standardize="scale")

#Step1 - removed PC1-->seeds_per_fl
model10_1<-lm(phen_int~PC1+PC2,allplants)
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,allplants)
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f+PC2,allplants) 
sem10<-list(
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=allplants) #p=0.051, AICc=74.797
sem.coefs(sem10, data=allplants,standardize="scale")

#Step2 - removed PC2-->seeds_per_fl
model10_1<-lm(phen_int~PC1+PC2,allplants)
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,allplants)
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f,allplants) 
sem10<-list(
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=allplants) #p=0.065, AICc=74.168 -->lowest, KEEP
sem.coefs(sem10, data=allplants,standardize="scale")

#made-up interaction variable
allplants$phen_ants<-allplants$phen_int*as.numeric(as.character(allplants$Mrub_sch_s_pres))

summary(glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial"))
summary(glm(attack_f~phen_int+Mrub_sch_s_pres+phen_ants+suit_neigh+PC1+PC2,allplants,family="binomial"))

model10_1<-lm(phen_int~PC1+PC2,allplants)
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,allplants)
model10_4<-lm(phen_ants~phen_int+Mrub_sch_s_pres,allplants)
model10_5<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_ants+suit_neigh+PC1+PC2,allplants,family="binomial")
model10_6<-lm(seeds_per_fl~phen_int+attack_f,allplants) 
sem10<-list(
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5,
  model10_6
)
sem.fit(sem10, data=allplants) #p=0
sem.coefs(sem10, data=allplants,standardize="scale")

phen_to_int<-sem.coefs(sem10, data=allplants,standardize="scale")[7,3]
ants_to_int<-sem.coefs(sem10, data=allplants,standardize="scale")[8,3]

#Check if coefs match with models-->YES!
summary(lm(scale(phen_int)~scale(PC1)+scale(PC2),allplants))
summary(glm(Mrub_sch_s_pres~scale(PC1)+scale(PC2),allplants,family="binomial"))
summary(lm(scale(suit_neigh)~scale(PC1)+scale(PC2),allplants))
summary(glm(attack_f~scale(phen_int)+Mrub_sch_s_pres+scale(phen_int):Mrub_sch_s_pres+scale(suit_neigh)+scale(PC1)+scale(PC2),allplants,family="binomial"))
summary(lm(scale(seeds_per_fl)~scale(phen_int)+attack_f,allplants))

# #Step3 - removed phen_int-->seeds_per_fl
# model10_1<-lm(phen_int~PC1+PC2,allplants)
# model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
# model10_3<-lm(suit_neigh~PC1+PC2,allplants)
# model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial")
# model10_5<-lm(seeds_per_fl~attack_f,allplants) 
# sem10<-list(
#   model10_1,
#   model10_2,
#   model10_3,
#   model10_4,
#   model10_5
# )
# sem.fit(sem10, data=allplants) #p=0.055, AICc=75.179 --> higher
# sem.coefs(sem10, data=allplants,standardize="scale")

#### Models egg number ####

#Starting model

model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC1+PC2+scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.454, AICc=83.365
sem.coefs(sem11)

#Step1 - removed PC1-->seeds_per_fl
model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC2+scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.044, AICc=83.202
sem.coefs(sem11)

#Step2 - removed phen_int:Mrub_sch_s_pres-->n_eggs_max
model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC2+scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.414, AICc=66.067
sem.coefs(sem11)

#Step3 - removed PC2-->seeds_per_fl
model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.44, AICc=65.119
sem.coefs(sem11)

#Step4 - removed phen_int-->seeds_per_fl
model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.444, AICc=64.469
sem.coefs(sem11)

#Step5 - removed Mrub_sch_s_pres-->n_eggs_max
model11_1<-lm(phen_int~PC1+PC2,allplants)
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,allplants)
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(allplants,n_eggs_max>0))
model11_5<-lm(seeds_per_fl~scale(n_eggs_max),subset(allplants,n_eggs_max>0))
sem11<-list(
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=allplants) #p=0.444, AICc=63.894 -->lowest, KEEP
sem.coefs(sem11, data=allplants,standardize="scale") #Take coefs for model11_1-2-3
sem.coefs(sem11, data=subset(allplants,n_eggs_max>0),standardize="scale") #Take coefs for model11_4-5

#Check if coefs match with models-->YES!
summary(lm(scale(phen_int)~scale(PC1)+scale(PC2),allplants))
summary(glm(Mrub_sch_s_pres~scale(PC1)+scale(PC2),allplants,family="binomial"))
summary(lm(scale(suit_neigh)~scale(PC1)+scale(PC2),allplants))
summary(glm.nb(n_eggs_max~scale(phen_int)+scale(suit_neigh)+scale(PC1)+scale(PC2),subset(allplants,n_eggs_max>0)))
summary(lm(scale(seeds_per_fl)~scale(n_eggs_max),subset(allplants,n_eggs_max>0)))

# #Step5 - removed PC1-->n_eggs_max
# model11_1<-lm(phen_int~PC1+PC2,allplants)
# model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,allplants,family="binomial")
# model11_3<-lm(suit_neigh~PC1+PC2,allplants)
# model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC2,subset(allplants,n_eggs_max>0))
# model11_5<-lm(seeds_per_fl~scale(n_eggs_max),subset(allplants,n_eggs_max>0))
# sem11<-list(
#   model11_1,
#   model11_2,
#   model11_3,
#   model11_4,
#   model11_5
# )
# sem.fit(sem11, data=allplants) #p=0.014, AICc=79.545 --> higher
# sem.coefs(sem11)

#### Selected models #### 
sem.fit(sem10,data=allplants) #p=0.065, AICc=74.168 
coefs1<-sem.coefs(sem10,data=allplants,standardize="scale")
sem.model.fits(sem10)

#Effects

#PC1
#Total indirect effect of PC1 on seeds per fl: -1.176888
coefs1[10,3]*coefs1[13,3]+                          #PC1->attack->seeds_per_fl
coefs1[2,3]*coefs1[8,3]*coefs1[13,3]+               #PC1->phen->attack->seeds_per_fl
coefs1[2,3]*coefs1[14,3]+                           #PC1->phen->seeds_per_fl
coefs1[3,3]*coefs1[12,3]*coefs1[13,3]+              #PC1->ants->attack->seeds_per_fl
coefs1[2,3]*phen_to_int*coefs1[11,3]*coefs1[13,3]+  #PC1->phen->int->attack->seeds_per_fl
coefs1[3,3]*ants_to_int*coefs1[11,3]*coefs1[13,3]+  #PC1->ants->int->attack->seeds_per_fl
coefs1[6,3]*coefs1[7,3]*coefs1[13,3]                #PC1->suit->attack->seeds_per_fl

#Indirect effects of PC1 on seeds per fl acting through attack: -0.2051301
coefs1[10,3]*coefs1[13,3]                           #PC1->attack->seeds_per_fl

#Indirect effects of PC1 on seeds per fl acting through phen: 0.02162098
coefs1[2,3]*coefs1[14,3]                           #PC1->phen->seeds_per_fl
  
#(Very) indirect effects of PC1 on seeds per fl acting through effects of phen, ants and suit on attack: -0.9933788
coefs1[2,3]*coefs1[8,3]*coefs1[13,3]+               #PC1->phen->attack->seeds_per_fl
coefs1[3,3]*coefs1[12,3]*coefs1[13,3]+              #PC1->ants->attack->seeds_per_fl
coefs1[2,3]*phen_to_int*coefs1[11,3]*coefs1[13,3]+  #PC1->phen->int->attack->seeds_per_fl
coefs1[3,3]*ants_to_int*coefs1[11,3]*coefs1[13,3]+  #PC1->ants->int->attack->seeds_per_fl
coefs1[6,3]*coefs1[7,3]*coefs1[13,3]                #PC1->suit->attack->seeds_per_fl

#PC2
#Total indirect effect of PC2 on seeds per fl: 0.6608619
coefs1[9,3]*coefs1[13,3]+                           #PC2->attack->seeds_per_fl
coefs1[1,3]*coefs1[8,3]*coefs1[13,3]+               #PC2->phen->attack->seeds_per_fl
coefs1[1,3]*coefs1[14,3]+                           #PC2->phen->seeds_per_fl
coefs1[4,3]*coefs1[12,3]*coefs1[13,3]+              #PC2->ants->attack->seeds_per_fl
coefs1[1,3]*phen_to_int*coefs1[11,3]*coefs1[13,3]+  #PC2->phen->int->attack->seeds_per_fl
coefs1[4,3]*ants_to_int*coefs1[11,3]*coefs1[13,3]+  #PC2->ants->int->attack->seeds_per_fl
coefs1[5,3]*coefs1[7,3]*coefs1[13,3]                #PC2->suit->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through attack: -0.5916516
coefs1[9,3]*coefs1[13,3]                            #PC2->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through phen: -0.02943603
coefs1[1,3]*coefs1[14,3]                            #PC2->phen->seeds_per_fl

#(Very) indirect effects of PC2 on seeds per fl acting through effects of phen, ants and suit on attack: 1.28195
coefs1[1,3]*coefs1[8,3]*coefs1[13,3]+               #PC2->phen->attack->seeds_per_fl
coefs1[4,3]*coefs1[12,3]*coefs1[13,3]+              #PC2->ants->attack->seeds_per_fl
coefs1[1,3]*phen_to_int*coefs1[11,3]*coefs1[13,3]+  #PC2->phen->int->attack->seeds_per_fl
coefs1[4,3]*ants_to_int*coefs1[11,3]*coefs1[13,3]+  #PC2->ants->int->attack->seeds_per_fl
coefs1[5,3]*coefs1[7,3]*coefs1[13,3]                #PC2->suit->attack->seeds_per_fl

#Effect summary

#Total indirect effect of PC1 on seeds per fl: -1.176888
#Indirect effects of PC1 on seeds per fl acting through attack: -0.2051301
#Indirect effects of PC1 on seeds per fl acting through phen: 0.02162098 (NS)
#(Very) indirect effects of PC1 on seeds per fl acting through effects of phen, ants and suit on attack: -0.9933788

#Total indirect effect of PC2 on seeds per fl: 0.6608619
#Indirect effects of PC2 on seeds per fl acting through attack: -0.5916516
#Indirect effects of PC2 on seeds per fl acting through phen: -0.02943603 (NS)
#(Very) indirect effects of PC2 on seeds per fl acting through effects of phen, ants and suit on attack: 1.28195

sem.fit(sem11, data=allplants) #p=0.444, AICc=63.894 -->lowest, KEEP
sem.coefs(sem11, data=allplants,standardize="scale") #Take coefs for model11_1-2-3
sem.coefs(sem11, data=subset(allplants,n_eggs_max>0),standardize="scale") #Take coefs for model11_4-5
sem.model.fits(sem11)

# The effect of microclimate on suit_neigh seems to be mainly due to an effect of neigh density

summary(lm(pldens_3~PC1+PC2,allplants))
summary(lm(phen_n3~PC1+PC2,allplants))

#### STUFF FROM HERE NOT USED IN PAPER ####

#### Selected graphs #### 

#Not sure if needed, only done for egg occurrence by now

ggplot(allplants)+geom_smooth(aes(x=PC1,y=phen_int),method="lm",color="black")+
  geom_smooth(aes(x=PC2,y=phen_int),method="lm",color="black",lty="dashed")+
  xlab("Microclimate")+ylab(expression(paste(italic("G. pneumonanthe"),"phenology")))+ 
  annotate("text", x = 3.7, y = 4.8, label = "PC1",family = "serif",size=6)+
  annotate("text", x = 2.5, y = 2.9, label = "PC2",family = "serif",size=6)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

ggplot(allplants)+geom_smooth(aes(x=PC1,y=as.numeric(ifelse(allplants$Mrub_sch_s_pres==0,0,1))),method="glm",method.args=list(family="binomial"),color="black")+
  geom_smooth(aes(x=PC2,y=as.numeric(ifelse(allplants$Mrub_sch_s_pres==0,0,1))),method="glm",method.args=list(family="binomial"),color="black",lty="dashed")+
  xlab("Microclimate")+ylab("Ant presence")+ 
  annotate("text", x = 3.7, y = 0.8, label = "PC1",family = "serif",size=6)+
  annotate("text", x = 2.5, y = 0.1, label = "PC2",family = "serif",size=6)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif")) #Error

ggplot(allplants)+geom_smooth(aes(x=PC1,y=suit_neigh),method="lm",color="black")+
  geom_smooth(aes(x=PC2,y=suit_neigh),method="lm",color="black",lty="dashed")+
  xlab("Microclimate")+ylab("Neighbor suitability")+ 
  annotate("text", x = 3.7, y = 40, label = "PC1",family = "serif",size=6)+
  annotate("text", x = 2.5, y = 115, label = "PC2",family = "serif",size=6)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

ggplot(allplants)+geom_smooth(aes(x=PC1,y=pldens_3),method="lm",color="black")+
  geom_smooth(aes(x=PC2,y=pldens_3),method="lm",color="black",lty="dashed")+
  xlab("Microclimate")+ylab("Neighbor density")+ 
  annotate("text", x = 3.7, y = 10, label = "PC1",family = "serif",size=6)+
  annotate("text", x = 2.5, y = 35, label = "PC2",family = "serif",size=6)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

ggplot(allplants)+geom_smooth(aes(x=PC1,y=phen_n3),method="lm",color="black")+
  geom_smooth(aes(x=PC2,y=phen_n3),method="lm",color="black",lty="dashed")+
  xlab("Microclimate")+ylab("Neighbor phenology")+ 
  annotate("text", x = 3.7, y = 4.7, label = "PC1",family = "serif",size=6)+
  annotate("text", x = 2.5, y = 3, label = "PC2",family = "serif",size=6)+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

interaction1<-data.frame(effect(term="phen_int:Mrub_sch_s_pres", mod=model10_4,
              xlevels=list(phen_int=seq(1:6), Mrub_sch_s=seq(1:0))))
ggplot(interaction1, aes(phen_int,fit, group = Mrub_sch_s_pres))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=F,size=0.5,aes(phen_int,fit,color=Mrub_sch_s_pres))+
  xlab("Phenology")+ylab("Probability of having eggs")+
  theme_base()+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Ant presence")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))

effect1<-data.frame(effect(term="attack_f", mod=model10_5,
                                xlevels=list(attack_f=seq(1:0))))
ggplot(effect1,aes(x=attack_f,y=fit))+geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+geom_point(size=4)+
  xlab("Egg occurrence")+ylab("Number of seeds per flower")+
  theme_base()+theme(text=element_text(family="serif"))+scale_y_continuous(limits=c(200,500))+
  theme(plot.background=element_rect(fill="white", colour=NA))


#### Variation partitioning #### 
#--> USE? Does not say very different things from std coefs!

#On egg occurrence
model10_4

model10_4_res_nei_env<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,allplants,family="binomial")
model10_4_res<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres,allplants,family="binomial")
model10_4_nei<-glm(attack_f~suit_neigh,allplants,family="binomial")
model10_4_env<-glm(attack_f~PC1+PC2,allplants,family="binomial")
model10_4_res_nei<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh,allplants,family="binomial")
model10_4_res_env<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+PC1+PC2,allplants,family="binomial")
model10_4_nei_env<-glm(attack_f~suit_neigh+PC1+PC2,allplants,family="binomial")

R_res_nei_env<-sem.model.fits(model10_4_res_nei_env)$R.squared
R_res<-sem.model.fits(model10_4_res)$R.squared
R_nei<-sem.model.fits(model10_4_nei)$R.squared
R_env<-sem.model.fits(model10_4_env)$R.squared
R_res_nei<-sem.model.fits(model10_4_res_nei)$R.squared
R_res_env<-sem.model.fits(model10_4_res_env)$R.squared
R_nei_env<-sem.model.fits(model10_4_nei_env)$R.squared

res<-R_res_nei_env-R_nei_env
nei<-R_res_nei_env-R_res_env
env<-R_res_nei_env-R_res_nei
res_nei<-R_res_nei_env-R_env-res-nei
res_env<-R_res_nei_env-R_nei-res-env
nei_env<-R_res_nei_env-R_res-nei-env
res_nei_env<-R_res_nei_env-res-nei-env-res_nei-res_env-nei_env

(res/R_res_nei_env)*100
(nei/R_res_nei_env)*100
(env/R_res_nei_env)*100
(res_nei/R_res_nei_env)*100
(res_env/R_res_nei_env)*100
(nei_env/R_res_nei_env)*100
(res_nei_env/R_res_nei_env)*100

res+nei+env+res_nei+res_env+nei_env+res_nei_env

#On egg number?

#On n seeds per fl (including effects of microclimate although they were removed from the selected SEM)
model10_5

model10_5_alt<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,allplants)

model10_5_alt_phe_att_env<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,allplants)
model10_5_alt_phe<-lm(seeds_per_fl~phen_int,allplants)
model10_5_alt_att<-lm(seeds_per_fl~attack_f,allplants)
model10_5_alt_env<-lm(seeds_per_fl~PC1+PC2,allplants)
model10_5_alt_phe_att<-lm(seeds_per_fl~phen_int+attack_f,allplants)
model10_5_alt_phe_env<-lm(seeds_per_fl~phen_int+PC1+PC2,allplants)
model10_5_alt_att_env<-lm(seeds_per_fl~attack_f+PC1+PC2,allplants)

R_phe_att_env<-sem.model.fits(model10_5_alt_phe_att_env)$R.squared
R_phe<-sem.model.fits(model10_5_alt_phe)$R.squared
R_att<-sem.model.fits(model10_5_alt_att)$R.squared
R_env<-sem.model.fits(model10_5_alt_env)$R.squared
R_phe_att<-sem.model.fits(model10_5_alt_phe_att)$R.squared
R_phe_env<-sem.model.fits(model10_5_alt_phe_env)$R.squared
R_att_env<-sem.model.fits(model10_5_alt_att_env)$R.squared

phe<-R_phe_att_env-R_att_env
att<-R_phe_att_env-R_phe_env
env<-R_phe_att_env-R_phe_att
phe_att<-R_phe_att_env-R_env-phe-att
phe_env<-R_phe_att_env-R_att-phe-env
att_env<-R_phe_att_env-R_phe-att-env
phe_att_env<-R_phe_att_env-phe-att-env-phe_att-phe_env-att_env


(phe/R_phe_att_env)*100
(att/R_phe_att_env)*100
(env/R_phe_att_env)*100
(phe_att/R_phe_att_env)*100
(phe_env/R_phe_att_env)*100
(att_env/R_phe_att_env)*100
(phe_att_env/R_phe_att_env)*100

phe+att+env+phe_att+phe_env+att_env+phe_att_env





