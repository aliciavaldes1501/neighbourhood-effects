library(data.table)

seeds<-read.table("C:/Users/User/Dropbox/SU/projects/neighbourhood_effects/data/raw/tanga2016_seeds.txt",header=T,sep="\t",dec=".")
head(seeds)
seeds<-subset(seeds,!is.na(n_dev_seeds))
seeds <- dcast(seeds, pl_id_ALL ~ inmature+P_UP, value.var="n_dev_seeds")

head(seeds)
names(seeds)<-c("pl_id","P_m","UP_m","P_i","UP_i")
seeds$status<-ifelse(!is.na(seeds$P_i)&!is.na(seeds$UP_i),"Both_i",
    ifelse(!is.na(seeds$P_i),"P_i",ifelse(!is.na(seeds$UP_i),"UP_i","Both_m")))
  
head(as.data.frame(allplants)[c(4,6,18,20)])
seeds<-merge(seeds,as.data.frame(allplants)[c(4,6,18,20)])
seeds$seeds_per_fl<-(((rowSums(seeds[,c("UP_m", "UP_i")], na.rm=T))*seeds$fr_in)+
                       ((rowSums(seeds[,c("P_m", "P_i")], na.rm=T))*seeds$fr_pr))/seeds$n_fl

allplants<-merge(allplants,seeds[c(1:6,10)],all.x=T)
hist(allplants$seeds_per_fl)

#Models
#Model attack####
allplants$attack_f<-as.factor(allplants$attack)
model6_1<-lm(phen_int~meanT+moist_per+meanT:moist_per,allplants)
model6_2<-glm.nb(Mrub_sch_s~meanT+moist_per+meanT:moist_per,allplants)
model6_3<-lm(suit_neigh~meanT+moist_per+meanT:moist_per,allplants)
model6_4<-glm(attack_f~phen_int+Mrub_sch_s+phen_int:Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial")
model6_5<-lm(seeds_per_fl~phen_int+meanT+moist_per+attack_f,allplants)

sem6<-list(#with fitness, attack
      model6_1,
      model6_2,
      model6_3,
      model6_4,
      model6_5
      )
sem.fit(sem6, data=allplants) #p=0
sem.fit(sem6, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem6)
sem.coefs(sem6, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

# sem6a<-list(#with fitness, attack
#   lm(phen_int~meanT*moist_per,allplants),
#   glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
#   lm(suit_neigh~meanT*moist_per,allplants),
#   glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT*moist_per,allplants,family="binomial"),
#   lm(seeds_per_fl~phen_int+meanT+moist_per+attack,allplants)
# )
# sem.fit(sem6a, data=allplants) #p=0
# sem.fit(sem6a, data=allplants,
#         corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
# sem.model.fits(sem6a)
# sem.coefs(sem6a, data=allplants,
#           corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
# 
# sem6b<-list(#with fitness, attack
#   lm(phen_int~meanT*moist_per,allplants),
#   glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
#   lm(suit_neigh~meanT*moist_per,allplants),
#   glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial"),
#   lm(seeds_per_fl~phen_int+meanT*moist_per+attack,allplants)
# )
# 
# sem.fit(sem6b, data=allplants) #p=0
# sem.fit(sem6b, data=allplants,
#         corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
# sem.model.fits(sem6b)
# sem.coefs(sem6b, data=allplants,
#           corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

#Graphs attack####
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
theme_set( theme_base( base_family= "Times"))

interaction6_1<-data.frame(effect(term="meanT:moist_per", mod=model6_1,
                xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))
ggplot(interaction6_1, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Plant phenology")+theme_base()+
  scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_y_continuous(limit=c(NA,6))

interaction6_2<-data.frame(effect(term="meanT:moist_per", mod=model6_2,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))
ggplot(interaction6_2, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Number of host ants (M. rubra + M. schencki)")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

interaction6_3<-data.frame(effect(term="meanT:moist_per", mod=model6_3,
              xlevels=list(meanT=seq(14.7,17.3,0.05), moist_per=seq(35,81,1))))
ggplot(interaction6_3, aes(meanT,fit, group = as.factor(moist_per)))+
  geom_smooth(method=lm,se=F,size=0.5,aes(meanT,fit,color=moist_per))+
  xlab("Soil temperature")+ylab("Neighbor suitability (density x phenology)")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Moisture percentage")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))

interaction6_4<-data.frame(effect(term="phen_int:Mrub_sch_s", mod=model6_4,
                xlevels=list(phen_int=seq(1:6), Mrub_sch_s=seq(0,34,1))))
ggplot(interaction6_4, aes(phen_int,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.5,aes(phen_int,fit,color=Mrub_sch_s))+
  xlab("Phenology")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+theme(legend.key.width=unit(1,"cm"))+
  labs(colour="Ant abundance")+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))

plot_model(model6_4, type = "pred", terms = c("suit_neigh"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Neighbor suitability (density x phenology)")+ylab("Probability of having eggs")+
  scale_y_continuous(breaks=c(0.1,0.2,0.3))

plot_model(model6_4, type = "pred", terms = c("meanT"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil temperature")+ylab("Probability of having eggs")+scale_y_continuous()

plot_model(model6_4, type = "pred", terms = c("moist_per"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil moisture percentage")+ylab("Probability of having eggs")+scale_y_continuous()

# plot_model(model6_5, type = "pred", terms = c("phen_int"))+theme(text=element_text(family="serif"))+
#   theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
#   xlab("Plant phenology")+ylab("Seeds per flower")
# 
# plot_model(model6_5, type = "pred", terms = c("meanT"))+theme(text=element_text(family="serif"))+
#   theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
#   xlab("Soil temperature")+ylab("Seeds per flower")
# 
# plot_model(model6_5, type = "pred", terms = c("moist_per"))+theme(text=element_text(family="serif"))+
#   theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
#   xlab("Soil moisture percentage")+ylab("Seeds per flower")

plot_model(model6_5, type = "pred", terms = c("attack_f"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Attack")+ylab("Seeds per flower")


#Model n eggs####

model7_1<-lm(phen_int~meanT+moist_per+meanT:moist_per,allplants)
model7_2<-glm.nb(Mrub_sch_s~meanT+moist_per+meanT:moist_per,allplants)
model7_3<-lm(suit_neigh~meanT+moist_per+meanT:moist_per,allplants)
model7_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s+suit_neigh+meanT+moist_per,subset(allplants,n_eggs_max>0))
model7_5<-lm(seeds_per_fl~phen_int+meanT+moist_per+n_eggs_max,subset(allplants,n_eggs_max>0))

sem7<-list(#with fitness, number of eggs (all plants)
  model7_1,
  model7_2,
  model7_3,
  model7_4,
  model7_5
)

sem.fit(sem7, data=allplants) #p=0
sem.fit(sem7, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem7)
sem.coefs(sem7, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

#Graphs n eggs####

plot_model(model7_4, type = "pred", terms = c("phen_int"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Plant phenology")+ylab("Number of eggs")

plot_model(model7_4, type = "pred", terms = c("Mrub_sch_s"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Ant abundance")+ylab("Number of eggs")

plot_model(model7_4, type = "pred", terms = c("suit_neigh"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Neighbor suitability (density x phenology)")+ylab("Number of eggs")

plot_model(model7_4, type = "pred", terms = c("meanT"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil temperature")+ylab("Number of eggs")+scale_y_continuous()

plot_model(model7_4, type = "pred", terms = c("moist_per"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Soil moisture percentage")+ylab("Number of eggs")+scale_y_continuous()

plot_model(model7_5, type = "pred", terms = c("phen_int"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Plant phenology")+ylab("Seeds per flower")

plot_model(model7_5, type = "pred", terms = c("n_eggs_max"))+theme(text=element_text(family="serif"))+
  theme(plot.background=element_rect(fill="white", colour=NA))+ggtitle(NULL)+
  xlab("Number of eggs")+ylab("Seeds per flower")


