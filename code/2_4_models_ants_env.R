library(effects)
library(gridExtra)
library(lattice)

#Ants in 3 m ##########
ants_presabs<-read.table("D:/SU/projects/neighbourhood_effects/gis/tables/ants_in3m.txt",header=T,sep=",",dec=".")
head(ants_presabs)
ants_presabs$TARGET_FID<-NULL
ants_presabs$Mrub_p<-ifelse(ants_presabs$SUM_Mrub_s>0,1,0)
ants_presabs$Msca_p<-ifelse(ants_presabs$SUM_Msca_s>0,1,0)
ants_presabs$Mrug_p<-ifelse(ants_presabs$SUM_Mrug_s>0,1,0)
ants_presabs$Msch_p<-ifelse(ants_presabs$SUM_Msch_s>0,1,0)
ants_presabs$oth_p<-ifelse(ants_presabs$SUM_oth_su>0,1,0)
ants_presabs$SUM_Mrub_s<-NULL
ants_presabs$SUM_Msca_s<-NULL
ants_presabs$SUM_Mrug_s<-NULL
ants_presabs$SUM_Msch_s<-NULL
ants_presabs$SUM_oth_su<-NULL

allplants<-merge(allplants,ants_presabs,by="FID")

names(allplants)

hist(allplants$Mrub_sum)
hist(allplants$Msca_sum)
hist(allplants$Mrug_sum)
hist(allplants$Msch_sum)
hist(allplants$oth_sum)

hist(allplants$Mrub_p)
hist(allplants$Msca_p)
hist(allplants$Mrug_p)
hist(allplants$Msch_p)
hist(allplants$oth_p)

#Ant pres ~ temp + moist w. points ##########

data_pts<-read.table("D:/SU/projects/neighbourhood_effects/data/clean/points_all_data.txt",header=T,sep="\t",dec=".")
head(data_pts)
str(data_pts)
names(data_pts)

summary(glm(Mrub_pres~scale(meanT)*scale(MOIST_PER_M),data_pts,family="binomial"))
summary(glm(Msca_pres~scale(meanT)*scale(MOIST_PER_M),data_pts,family="binomial"))
summary(glm(Mrug_pres~scale(meanT)*scale(MOIST_PER_M),data_pts,family="binomial"))
summary(glm(Msch_pres~scale(meanT)*scale(MOIST_PER_M),data_pts,family="binomial")) 
summary(glm(oth_pres~scale(meanT)*scale(MOIST_PER_M),data_pts,family="binomial"))

#Interaction was never significant --> models without interaction
summary(glm(Mrub_pres~scale(meanT)+scale(MOIST_PER_M),data_pts,family="binomial")) #-temp
summary(glm(Msca_pres~scale(meanT)+scale(MOIST_PER_M),data_pts,family="binomial")) #+temp, +moist
summary(glm(Mrug_pres~scale(meanT)+scale(MOIST_PER_M),data_pts,family="binomial")) #-temp, (+moist)
summary(glm(Msch_pres~scale(meanT)+scale(MOIST_PER_M),data_pts,family="binomial")) #none
summary(glm(oth_pres~scale(meanT)+scale(MOIST_PER_M),data_pts,family="binomial")) #none

plot(effect(term="meanT", 
            mod=glm(Mrub_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm(Mrub_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm(Mrub_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),
            xlevels=list(moist_per=35:81, meanT=seq(15,18,1))),multiline=T,type="response")

plot(effect(term="meanT", 
            mod=glm(Msca_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm(Msca_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm(Msca_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),
            xlevels=list(moist_per=35:81, meanT=seq(15,18,1))),multiline=T,type="response")

plot(effect(term="meanT", 
            mod=glm(Mrug_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm(Mrug_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm(Mrug_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),
            xlevels=list(moist_per=35:81, meanT=seq(15,18,1))),multiline=T,type="response")

plot(effect(term="meanT", 
            mod=glm(Msch_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm(Msch_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm(Msch_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),
            xlevels=list(moist_per=35:81, meanT=seq(15,18,1))),multiline=T,type="response")

plot(effect(term="meanT", 
            mod=glm(oth_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm(oth_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm(oth_pres~meanT+MOIST_PER_M,data_pts,family="binomial"),
            xlevels=list(moist_per=35:81, meanT=seq(15,18,1))),multiline=T,type="response")

#Ant ab ~ temp + moist w. points ##########

summary(glm.nb(Mrub_sum~scale(meanT)+scale(MOIST_PER_M),data_pts))
summary(glm.nb(Msca_sum~scale(meanT)+scale(MOIST_PER_M),data_pts))
summary(glm.nb(Mrug_sum~scale(meanT)+scale(MOIST_PER_M),data_pts))
summary(glm.nb(Msch_sum~scale(meanT)+scale(MOIST_PER_M),data_pts))
summary(glm.nb(oth_sum~scale(meanT)*scale(MOIST_PER_M),data_pts))

plot(effect(term="MOIST_PER_M", 
            mod=glm.nb(Mrub_sum~meanT+MOIST_PER_M,data_pts),type="response"))
plot(effect(term="MOIST_PER_M", 
            mod=glm.nb(Msca_sum~meanT+MOIST_PER_M,data_pts),type="response"))
plot(effect(term="meanT", 
            mod=glm.nb(Mrug_sum~meanT+MOIST_PER_M,data_pts),type="response"))
plot(effect(term="meanT:MOIST_PER_M", 
            mod=glm.nb(oth_sum~meanT*MOIST_PER_M,data_pts)),multiline=T,type="response")
plot(effect(term="MOIST_PER_M:meanT", 
            mod=glm.nb(oth_sum~MOIST_PER_M*meanT,data_pts)),multiline=T,type="response")


#dbRDA w. points ##########
#community:ants_pres
#environmental: env
ants_pres<-data_pts[c(1,15:19)]
env<-data_pts[c(1,26:35,39,43)]
row.names(ants_pres)<-sub("^", "p", ants_pres$point_id )
row.names(env)<-sub("^", "p", env$point_id )
ants_pres$point_id<-NULL
env$point_id<-NULL
env<-env[complete.cases(env), ]
ants_pres<-ants_pres[(row.names(ants_pres) %in% row.names(env)), ]
ants_pres<-ants_pres[rowSums(ants_pres[, -1])>0, ] #Keep rows with at least one sp
env<-env[(row.names(env) %in% row.names(ants_pres)), ]

dbRDA1<-capscale(ants_pres~meanT+maxT+minT+MOIST_PER_M+MOIST_MV_M,data=env,distance="bray")
dbRDA1
head(summary(dbRDA1, scaling=1))
ordiplot(dbRDA1,type="text",display=c("species","bp"))
#Permutation tests to access significance of constraints
anova(dbRDA1) ## overall test of the significance of the analysis
anova(dbRDA1, by="axis", perm.max=1000) ## test axes for significance
anova(dbRDA1, by="terms", permu=1000) ## test for sig. environ. variables
#With by=terms, the terms are assessed sequentially from first to last, 
#and the order of the terms will influence their significances
anova(dbRDA1, by="margin", perm.max=1000) ## test for sig. environ. variables
# Setting by = "margin" will perform separate significance test for 
#each marginal term in a model with all other terms

dbRDA2<-capscale(ants_pres~meanT+MOIST_PER_M,data=env,distance="bray")
anova(dbRDA2) ## overall test of the significance of the analysis
anova(dbRDA2, by="margin", permu=1000) ## test for sig. environ. variables
ordiplot(dbRDA2,type="text",display=c("species","bp"))

ants_sum<-data_pts[c(1,5:9)]
row.names(ants_sum)<-sub("^", "p", ants_sum$point_id )
ants_sum$point_id<-NULL
ants_sum<-ants_sum[(row.names(ants_sum) %in% row.names(env)), ]

dbRDA3<-capscale(ants_sum~meanT+MOIST_PER_M,data=env,distance="bray")
anova(dbRDA3) ## overall test of the significance of the analysis
anova(dbRDA3, by="margin", permu=1000) ## test for sig. environ. variables
ordiplot(dbRDA3,type="text",display=c("species","bp"))

ants_max<-data_pts[c(1,10:14)]
row.names(ants_max)<-sub("^", "p", ants_max$point_id )
ants_max$point_id<-NULL
ants_max<-ants_max[(row.names(ants_max) %in% row.names(env)), ]

dbRDA4<-capscale(ants_max~meanT+MOIST_PER_M,data=env,distance="bray")
anova(dbRDA4) ## overall test of the significance of the analysis
anova(dbRDA4, by="margin", permu=1000) ## test for sig. environ. variables
ordiplot(dbRDA4,type="text",display=c("species","bp"))

par(mfrow=(c(1,3)))
ordiplot(dbRDA2,type="text",display=c("species","bp"))
ordiplot(dbRDA3,type="text",display=c("species","bp"))
ordiplot(dbRDA4,type="text",display=c("species","bp"))
par(mfrow=c(1,1))

#Only with Myrmica
myrmica_pres<-ants_pres[1:4]
myrmica_pres<-myrmica_pres[rowSums(myrmica_pres[, -1])>0, ] #Keep rows with at least one sp
env_myrmica<-env[(row.names(env) %in% row.names(myrmica_pres)), ]
dbRDA5<-capscale(myrmica_pres~meanT+MOIST_PER_M,data=env_myrmica,distance="bray")
anova(dbRDA5) ## overall test of the significance of the analysis
anova(dbRDA5, by="margin", permu=1000) ## test for sig. environ. variables
ordiplot(dbRDA5,type="text",display=c("species","bp"))

#On graph lm moist~temp ##########
ggplot(data_pts,aes(meanT, MOIST_PER_M))+ geom_point(color="red")+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(lm(MOIST_PER_M~meanT,data=data_pts))

p1<-ggplot(data_pts) + 
  geom_point(data=subset(data_pts,Mrub_pres==1),aes(meanT, MOIST_PER_M, size=Mrub_sum,color=Mrub_sum))+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+scale_y_continuous(limits=c(20,100))+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("M. rubra")
p2<-ggplot(data_pts) + 
  geom_point(data=subset(data_pts,Msca_pres==1),aes(meanT, MOIST_PER_M, size=Msca_sum,color=Msca_sum))+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+scale_y_continuous(limits=c(20,100))+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("M. scabrinodis")
p3<-ggplot(data_pts) + 
  geom_point(data=subset(data_pts,Mrug_pres==1),aes(meanT, MOIST_PER_M, size=Mrug_sum,color=Mrug_sum))+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+scale_y_continuous(limits=c(20,100))+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("M. ruginodis")
p4<-ggplot(data_pts) + 
  geom_point(data=subset(data_pts,Msch_pres==1),aes(meanT, MOIST_PER_M, size=Msch_sum,color=Msch_sum))+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+scale_y_continuous(limits=c(20,100))+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("M. schenki")
p5<-ggplot(data_pts) + 
  geom_point(data=subset(data_pts,oth_pres==1),aes(meanT, MOIST_PER_M, size=oth_sum,color=oth_sum))+ 
  geom_smooth(aes(meanT, MOIST_PER_M),method=lm,se=F,color="black")+scale_y_continuous(limits=c(20,100))+
  scale_colour_gradient(low = "#FFCCCC", high = "red")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("Other ants")
pdf("D:/SU/Projects/neighbourhood_effects//results/figures/ants_temp_moist.pdf", 
    width=8,height=8,family="Times")
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()
tiff("D:/SU/Projects/neighbourhood_effects//results/figures/ants_temp_moist.tif", 
    width=700,height=700,family="Times")
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()


#dbRDA w. data on plants ##########
ants_pres1<-allplants[c(1,74:78)]
env1<-allplants[c(1,30:41)]
row.names(ants_pres1)<-sub("^", "p", ants_pres1$FID )
row.names(env1)<-sub("^", "p", env1$FID )
ants_pres1$FID<-NULL
env1$FID<-NULL
ants_pres1<-as.data.frame(ants_pres1)
env1<-as.data.frame(env1)
ants_pres1$x<-NULL
ants_pres1$y<-NULL
env1$x<-NULL
env1$y<-NULL
env1<-env1[complete.cases(env1),]
ants_pres1<-ants_pres1[(row.names(ants_pres1) %in% row.names(env1)), ]
ants_pres1<-ants_pres1[rowSums(ants_pres1[, -1])>0, ] #Keep rows with at least one sp
env1<-env1[(row.names(env1) %in% row.names(ants_pres1)), ]

dbRDA6<-capscale(ants_pres1~meanT+moist_per,data=env1,distance="bray") #Not run
anova(dbRDA6) ## overall test of the significance of the analysis
anova(dbRDA6, by="margin", permu=1000) ## test for sig. environ. variables
ordiplot(dbRDA6,type="text",display=c("species","bp"))


