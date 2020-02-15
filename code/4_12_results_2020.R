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

allplants<-as.data.frame(allplants)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
my_theme <- function(){
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
    theme(legend.position="none")+theme(text=element_text(family="serif"))+
    theme(plot.title = element_text(hjust =-0.06))
}

#### Effect of microclimate on plant performance ####

#Seeds per fl against temp and moist (w marked pls which are the only ones w seed data)
# UNIVARIATE linear regressions of n seeds per flower against temp and moist

summary(lm(seeds_per_fl~meanT,data=allplants)) 
summary(lm(seeds_per_fl~moist_per,data=allplants))

# Figure 1
p1<-ggplot(allplants,aes(x=meanT,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil temperature (ºC)")+ylab(NULL)+ggtitle("A)")+    
  my_theme()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))+
  theme(plot.title=element_text(hjust=-0.25,vjust=0))
p2<-ggplot(allplants,aes(x=moist_per,y=seeds_per_fl))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil moisture (%)")+ylab(NULL)+ggtitle("B)")+   
  my_theme()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))+
  theme(axis.text.y=element_text(colour="white"))+
  theme(plot.title=element_text(hjust=-0.13,vjust=0))
fig1<-grid.arrange(p1,p2,ncol=2,left=textGrob("Number of seeds per flower",just="center",hjust=0.42,
                     gp=gpar(fontsize=16,fontfamily="serif"),rot = 90))

ggsave(filename="C:/Users/avald/Dropbox/SU/Projects/neighbourhood_effects/results/plant/figures/fig1.tiff",
       plot=fig1,device="tiff",width=22,height=11,units="cm",dpi=300,compression="lzw")

#Correlation among temp and moist (sampling points)

head(data_pts)

cor(data_pts$meanT,data_pts$MOIST_PER_M,use="complete.obs") # -0.4048787
cor.test(data_pts$meanT,data_pts$MOIST_PER_M,use="complete.obs") # -0.4048787

ggplot(as.data.frame(data_pts),aes(x=meanT,y=MOIST_PER_M))+geom_point()+geom_smooth(method="lm",color="black")+
  xlab("Soil temperature (ºC)")+ylab("Soil moisture (%)")+   
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+theme(text=element_text(family="serif"))

with(as.data.frame(data_pts),summary(lm(MOIST_PER_M~meanT))) #*

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

# Code not working now...
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

# ... till here

# PC1 = high temp, low moist
# PC2 = high temp & moist

#Seeds per fl against PC1 and PC2 (w marked pls which are the only ones w seed data)

summary(lm(seeds_per_fl~PC1+PC2,data=allplants)) #PC1* 

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
#(models with ΔAICc > 2 are considered to fit the data equally well, Burnham and Anderson 2002).

#### Models rubra & schencki (old version) #### 

# clean dataset with only the variables that we will use

# In new version of piecewiseSEM, response variables for binomial glms need to be integers/numeric

allplants$Mrub_sch_s_pres_numeric<-as.numeric(as.character(allplants$Mrub_sch_s_pres))

allplants_clean<-allplants[c(77,85:86,93,96:97,101)]

#Starting model
sem10<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean,!is.na(phen_int))),
             glm(Mrub_sch_s_pres_numeric~PC1+PC2,allplants_clean,family="binomial"),
             lm(suit_neigh~PC1+PC2,allplants_clean),
             glm(as.integer(attack)~phen_int+Mrub_sch_s_pres_numeric+phen_int:Mrub_sch_s_pres_numeric+suit_neigh+PC1+PC2,
                 subset(allplants_clean,!is.na(phen_int)),family="binomial"),
             lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean,!is.na(seeds_per_fl)))) 
summary(sem10) # p=0.043
AIC(sem10,aicc=T) # AICc=70.657

#Step1 - removed PC1-->seeds_per_fl
sem10<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean,!is.na(phen_int))),
            glm(Mrub_sch_s_pres_numeric~PC1+PC2,allplants_clean,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean),
            glm(as.integer(attack)~phen_int+Mrub_sch_s_pres_numeric+phen_int:Mrub_sch_s_pres_numeric+suit_neigh+PC1+PC2,
                subset(allplants_clean,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC2,subset(allplants_clean,!is.na(seeds_per_fl)))) #p=0.038, AICc=75.467
summary(sem10) # p=0.062
AIC(sem10,aicc=T) # AICc=69.985 -> KEEP (but not >2 units lower! but p>0.05!)

#Step2 - removed PC2-->seeds_per_fl
sem10<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean,!is.na(phen_int))),
            glm(Mrub_sch_s_pres_numeric~PC1+PC2,allplants_clean,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean),
            glm(as.integer(attack)~phen_int+Mrub_sch_s_pres_numeric+phen_int:Mrub_sch_s_pres_numeric+suit_neigh+PC1+PC2,
                subset(allplants_clean,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack),subset(allplants_clean,!is.na(seeds_per_fl)))) #p=0.038, AICc=75.467
summary(sem10) # p=0.083
AIC(sem10,aicc=T) # AICc=69.317 -> KEEP

# #Step3 - removed phen_int-->seeds_per_fl
# sem10<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean,!is.na(phen_int))),
#             glm(Mrub_sch_s_pres_numeric~PC1+PC2,allplants_clean,family="binomial"),
#             lm(suit_neigh~PC1+PC2,allplants_clean),
#             glm(as.integer(attack)~phen_int+Mrub_sch_s_pres_numeric+phen_int:Mrub_sch_s_pres_numeric+suit_neigh+PC1+PC2,
#                 subset(allplants_clean,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~as.integer(attack),subset(allplants_clean,!is.na(seeds_per_fl)))) #p=0.038, AICc=75.467
# summary(sem10) # p=0.07
# AIC(sem10,aicc=T) # AICc=70.431 -> higher

#Check if coefs match with models-->YES!
summary(lm(phen_int~PC1+PC2,subset(allplants_clean,!is.na(phen_int))))
summary(glm(Mrub_sch_s_pres_numeric~PC1+PC2,allplants_clean,family="binomial"))
summary(lm(suit_neigh~PC1+PC2,allplants_clean))
summary(glm(as.integer(attack)~phen_int+Mrub_sch_s_pres_numeric+phen_int:Mrub_sch_s_pres_numeric+suit_neigh+PC1+PC2,
    subset(allplants_clean,!is.na(phen_int)),family="binomial"))
summary(lm(seeds_per_fl~phen_int+as.integer(attack),subset(allplants_clean,!is.na(seeds_per_fl))))

#### Models rubra with ant presence, suit_neigh ####

# clean dataset with only the variables that we will use

allplants_clean_rubra<-allplants[c(44,70,76:78,85:86,93,96:97)]

#Starting model
sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+Mrub_p+phen_int:Mrub_p+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem11) # p=0.025, interaction NS!
AIC(sem11,aicc=T) # AICc=72.473

#Step1 - removed phen_int:Mrub_p-->attack
sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+Mrub_p+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem11) # p=0.025
AIC(sem11,aicc=T) # AICc=70.193

#Step2 - removed Mrub_p-->attack
sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem11) # p=0.046
AIC(sem11,aicc=T) # AICc=68.794

#Step3 - removed PC1-->seeds_per_fl
sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem11) # p=0.063
AIC(sem11,aicc=T) # AICc=68.129

#Step4 - removed PC2-->seeds_per_fl
sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem11) # p=0.082
AIC(sem11,aicc=T) # AICc=67.469

# #Step5 - removed phen_int-->seeds_per_fl
# sem11<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
#             glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
#             lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
#             glm(as.integer(attack)~phen_int+suit_neigh+PC1+PC2,
#                 subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
# summary(sem11) # p=0.067
# AIC(sem11,aicc=T) # AICc=68.69 -> higher

#### Models rubra with ant presence, neigh_dens*phen ####

#Starting model
sem12<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+Mrub_p+phen_int:Mrub_p+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem12,conserve=T) #p=0

sem12<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+Mrub_p+phen_int:Mrub_p+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            pldens_3%~~%phen_int,pldens_3%~~%Mrub_p) 
summary(sem12) #p=0

sem12<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm(Mrub_p~PC1+PC2,allplants_clean_rubra,family="binomial"),
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+Mrub_p+phen_int:Mrub_p+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem12,direction=c("phen_int<-pldens_3")) #p=0

# In none of the cases is the interaction significant!

#### Models rubra with ant abundance, suit_neigh - OK ####

#Starting model
sem13<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem13) # p=0.016, interaction *!
AIC(sem13,aicc=T) # AICc=76.148

# Model selection suggests to keep all paths - but the model does not fit the data!

# Try to include correlated errors based on independence claims shown in summary
sem13<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            suit_neigh%~~%round(Mrub_sum)) 
summary(sem13) # p=0.06, interaction *!
AIC(sem13,aicc=T) # AICc=68.907

#Step1 - removed PC1-->seeds_per_fl
sem13<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            suit_neigh%~~%round(Mrub_sum)) 
summary(sem13) # p=0.087, interaction *!
AIC(sem13,aicc=T) # AICc=68.238

#Step2 - removed PC2-->seeds_per_fl
sem13<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+suit_neigh+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            suit_neigh%~~%round(Mrub_sum)) 
summary(sem13) # p=0.116, interaction *!
AIC(sem13,aicc=T) # AICc=67.572

# This could be a final model (using suit_neigh)

# #Step3 - removed phen_int-->seeds_per_fl
# sem13<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
#             glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
#             lm(suit_neigh~PC1+PC2,allplants_clean_rubra),
#             glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+suit_neigh+PC1+PC2,
#                 subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
#             lm(seeds_per_fl~as.integer(attack),subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
#             suit_neigh%~~%round(Mrub_sum)) 
# summary(sem13) # p=0.088, interaction *!
# AIC(sem13,aicc=T) # AICc=69.071->higher

#### Models rubra with ant abundance, neigh_dens*phen - OK, FINAL ####

#Starting model
sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem14,conserve=T) # p=0, interaction *!
summary(sem14,direction=c("phen_int<-pldens_3")) #p=0, interaction NS

sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            pldens_3%~~%phen_int) 
summary(sem14,conserve=T) # p=0, interaction *!

# Model selection using this model

#Starting model
sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl)))) 
summary(sem14,conserve=T) # p=0, interaction *!

# Model selection suggests to keep all paths - but the model does not fit the data!

# Try to include correlated errors based on independence claims shown in summary
sem14<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
summary(sem14) # p=0.093, interaction *!
AIC(sem14,aicc=T) # AICc=81.343

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

# This model (sem15) will be used ONLY to get the values of the standardized coefficients from PC1->ants and PC2->ants
# But the significances for those will still be taken from sem14

#### Effects (using sem14) ####

coefs1<-coefs(sem14)
coefs1

coefs2<-coefs(sem15)
coefs2

#Make-up interaction variables
allplants_clean_rubra$phen_ants<-allplants_clean_rubra$phen_int*round(allplants_clean_rubra$Mrub_sum)
allplants_clean_rubra$dens_phen<-allplants_clean_rubra$pldens_3*allplants_clean_rubra$phen_n3

glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+PC1+PC2,
    subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial")
glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_ants+pldens_3+phen_n3+dens_phen+PC1+PC2,
    subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial")

sem14_int<-psem(lm(phen_int~PC1+PC2,subset(allplants_clean_rubra,!is.na(phen_int))),
            glm.nb(round(Mrub_sum)~PC1+PC2,allplants_clean_rubra), # Round to avoid non-integers
            lm(pldens_3~PC1+PC2,allplants_clean_rubra),
            lm(phen_n3~PC1+PC2,allplants_clean_rubra),
            lm(phen_ants~phen_int+round(Mrub_sum),allplants_clean_rubra),
            lm(dens_phen~pldens_3+phen_n3,allplants_clean_rubra),
            glm(as.integer(attack)~phen_int+round(Mrub_sum)+phen_ants+pldens_3+phen_n3+dens_phen+PC1+PC2,
                subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
            lm(seeds_per_fl~phen_int+as.integer(attack)+PC1+PC2,subset(allplants_clean_rubra,!is.na(seeds_per_fl))),
            phen_n3%~~%pldens_3,phen_int%~~%phen_n3,phen_n3%~~%round(Mrub_sum),pldens_3%~~%round(Mrub_sum),phen_int%~~%pldens_3) 
summary(sem14_int) # p=0.002 

phen_to_int<-as.numeric(coefs(sem14_int)[9,8])
ants_to_int<-as.numeric(coefs(sem14_int)[10,8])
pldens_3_to_int<-as.numeric(coefs(sem14_int)[11,8])
phen_n3_to_int<-as.numeric(coefs(sem14_int)[12,8])

#PC1

#Direct effect of PC1 on seed sper fl: 
as.numeric(coefs1[19,8])

#Total indirect effect of PC1 on seeds per fl: -0.04206903 (prev -1.176888)
as.numeric(coefs1[13,8])*as.numeric(coefs1[18,8])+                                            #PC1->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*as.numeric(coefs1[17,8])+                                           #PC1->phen->seeds_per_fl
  as.numeric(coefs1[1,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+                   #PC1->phen->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*phen_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+                  #PC1->ants->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*ants_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+                  #PC1->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*pldens_3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+  #PC1->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+                  #PC1->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*phen_n3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])    #PC1->phen_n3->int->attack->seeds_per_fl
  
#Indirect effects of PC1 on seeds per fl acting through attack: -0.05200965 (prev -0.2051301)
as.numeric(coefs1[13,8])*as.numeric(coefs1[18,8])                                             #PC1->attack->seeds_per_fl

#Indirect effects of PC1 on seeds per fl acting through phen: 0.02514024 (prev 0.02162098)
as.numeric(coefs1[1,8])*as.numeric(coefs1[17,8])                                              #PC1->phen->seeds_per_fl

#(Very) indirect effects of PC1 on seeds per fl acting through effects of phen, ants and suit on attack: -0.01519962 (prev -0.9933788)
as.numeric(coefs1[1,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+                     #PC1->phen->attack->seeds_per_fl
  as.numeric(coefs1[1,8])*phen_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+                  #PC1->ants->attack->seeds_per_fl
  as.numeric(coefs2[3,8])*ants_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC1->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+                  #PC1->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[5,8])*pldens_3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+  #PC1->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+                  #PC1->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[7,8])*phen_n3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])    #PC1->phen_n3->int->attack->seeds_per_fl

#PC2

#Direct effect of PC2 on seeds per fl: -0.0401
as.numeric(coefs1[20,8])

#Total indirect effect of PC2 on seeds per fl: -0.08355863 (prev 0.6608619)
as.numeric(coefs1[14,8])*as.numeric(coefs1[18,8])+                                            #PC2->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*as.numeric(coefs1[17,8])+                                           #PC2->phen->seeds_per_fl
  as.numeric(coefs1[2,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+                   #PC2->phen->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*phen_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+                  #PC2->ants->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*ants_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+                  #PC2->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*pldens_3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+  #PC2->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+                  #PC2->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*phen_n3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])    #PC2->phen_n3->int->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through attack: -0.07330015 (prev -0.5916516)
as.numeric(coefs1[14,8])*as.numeric(coefs1[18,8])                                             #PC2->attack->seeds_per_fl

#Indirect effects of PC2 on seeds per fl acting through phen: -0.02611656 (prev -0.02943603)
as.numeric(coefs1[2,8])*as.numeric(coefs1[17,8])                                              #PC2->phen->seeds_per_fl

#(Very) indirect effects of PC2 on seeds per fl acting through effects of phen, ants and suit on attack: 0.01585808 (prev 1.28195)
as.numeric(coefs1[2,8])*as.numeric(coefs1[9,8])*as.numeric(coefs1[18,8])+                     #PC2->phen->attack->seeds_per_fl
  as.numeric(coefs1[2,8])*phen_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->phen->int->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*as.numeric(coefs1[10,8])*as.numeric(coefs1[18,8])+                  #PC2->ants->attack->seeds_per_fl
  as.numeric(coefs2[4,8])*ants_to_int*as.numeric(coefs1[15,8])*as.numeric(coefs1[18,8])+      #PC2->ants->int->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*as.numeric(coefs1[11,8])*as.numeric(coefs1[18,8])+                  #PC2->pldens_3->attack->seeds_per_fl
  as.numeric(coefs1[6,8])*pldens_3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])+  #PC2->pldens_3->int->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*as.numeric(coefs1[12,8])*as.numeric(coefs1[18,8])+                  #PC2->phen_n3->attack->seeds_per_fl
  as.numeric(coefs2[8,8])*phen_n3_to_int*as.numeric(coefs1[16,8])*as.numeric(coefs1[18,8])    #PC2->phen_n3->int->attack->seeds_per_fl

# semEff

# Convert model to list (with easier model for a start)

easy_model<-list(lm(phen_int~PC1,subset(allplants_clean_rubra,!is.na(phen_int))),
                 glm(as.integer(attack)~phen_int+PC1,subset(allplants_clean_rubra,!is.na(phen_int)),family="binomial"),
                 lm(seeds_per_fl~phen_int+as.integer(attack)+PC1,subset(allplants_clean_rubra,!is.na(seeds_per_fl))))

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

