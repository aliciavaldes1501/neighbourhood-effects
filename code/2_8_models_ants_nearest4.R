ants_nearest4<-read.csv("D:/SU/projects/neighbourhood_effects/gis/tables/ants_nearest4.csv",header=T,sep=",",dec=".")
names(ants_nearest4)
ants_nearest4[1:2]<-NULL
ants_nearest4[3:9]<-NULL
ants_nearest4[4:5]<-NULL
ants_nearest4[25:59]<-NULL
ants_nearest4[9:24]<-NULL
ants_nearest4_agg<-aggregate(ants_nearest4,by=list(id=ants_nearest4$IN_FID),FUN=sum)
head(ants_nearest4_agg)
ants_nearest4_agg$IN_FID<-NULL
ants_nearest4_agg$NEAR_FID<-NULL
ants_nearest4_agg$point_id<-NULL
ants_nearest4_agg$Mrub_4<-as.factor(ifelse(ants_nearest4_agg$Mrub_sum>0,1,0))
ants_nearest4_agg$Msca_4<-as.factor(ifelse(ants_nearest4_agg$Msca_sum>0,1,0))
ants_nearest4_agg$Mrug_4<-as.factor(ifelse(ants_nearest4_agg$Mrug_sum>0,1,0))
ants_nearest4_agg$Msch_4<-as.factor(ifelse(ants_nearest4_agg$Msch_sum>0,1,0))
ants_nearest4_agg$oth_4<-as.factor(ifelse(ants_nearest4_agg$oth_sum>0,1,0))
ants_nearest4_agg[2:6]<-NULL
ants_nearest4_agg$FID<-ants_nearest4_agg$id
ants_nearest4_agg$id<-NULL
head(ants_nearest4_agg)
str(ants_nearest4_agg)

allplants<-merge(allplants,ants_nearest4_agg,by="FID")
head(allplants)


#binomial model - attack ####
model5<-glm(attack ~ scale(as.integer(phen)) + Mrub_4 + scale(pldens_3) +
                       scale(phen_n3) + scale(as.integer(phen)):Mrub_4+
                       scale(as.integer(phen)):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
                     subset(allplants,!is.na(phen)),family="binomial")
summary(model5)
NagelkerkeR2(model5)
