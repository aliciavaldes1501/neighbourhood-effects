#Abundance of M. rubra and M. schenki
allplants$Mrub_sch_s<-allplants$Mrub_sum+allplants$Msch_sum

#model with all plants
#binomial model - attack ####
model1all_2ants<-glm(attack ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) +
          scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s)+
          scale(phen_int):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
          subset(allplants,!is.na(phen)),family="binomial") #MODEL TO USE!
summary(model1all_2ants)
NagelkerkeR2(model1all_2ants)

#GRAPHS TO USE
allplants$phen_num<-as.numeric(allplants$phen)

model1all_2ants_noscale<-glm(attack ~ phen_num + Mrub_sch_s + pldens_3 +
                               phen_n3 + phen_num:Mrub_sch_s+
                               phen_num:phen_n3 + pldens_3:phen_n3,
                             subset(allplants,!is.na(phen)),family="binomial") 

plot(effect(term="phen_num:Mrub_sch_s", 
            mod=model1all_2ants_noscale,data=subset(allplants,!is.na(phen))),
     x.var="phen_num",multiline=T,type="response")
plot(effect(term="phen_num:Mrub_sch_s", 
            mod=model1all_2ants_noscale,data=subset(allplants,!is.na(phen))),
     x.var="Mrub_sch_s",multiline=T,type="response")
plot(effect(term="pldens_3:phen_n3", 
            mod=model1all_2ants_noscale,data=subset(allplants,!is.na(phen))),
     x.var="pldens_3",multiline=T,type="response")
plot(effect(term="pldens_3:phen_n3", 
            mod=model1all_2ants_noscale,data=subset(allplants,!is.na(phen))),
     x.var="phen_n3",multiline=T,type="response")



#Get into this later...
int_ants<-data.frame(effect(term="Mrub_sch_s:phen_num", mod=model1all_2ants_noscale,
                            xlevels=list(Mrub_sch_s=seq(0,35,5), phen_num=1:6)))

ggplot(int_ants, aes(phen_num,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.6,aes(phen_num,fit,color=as.factor(Mrub_sch_s)))+
  xlab("phen")+ylab("attack")+theme_base()

ggplot(int_ants, aes(phen_num,fit, group = as.factor(Mrub_sch_s)))+
  geom_line(size=0.6,aes(phen_num,fit,color=as.factor(Mrub_sch_s)))
  xlab("phen")+ylab("attack")+theme_base()


    
    
    

###

model1all_2ants_sep<-glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sum) + scale(Msch_sum)+
          scale(pldens_2) +scale(phen_n2) + 
          scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(Msch_sum)+
          scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
          subset(allplants,!is.na(phen)),family="binomial")
summary(model1all_2ants_sep)
NagelkerkeR2(model1all_2ants_sep)

model1all_2ants_pres<-glm(attack ~ scale(as.integer(phen)) + as.factor(Mrub_sch_p) + scale(pldens_2) +
                       scale(phen_n2) + scale(as.integer(phen)):as.factor(Mrub_sch_p)+
                       scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
                     subset(allplants,!is.na(phen)),family="binomial")
summary(model1all_2ants_pres)
NagelkerkeR2(model1all_2ants_pres)

#Get residuals of model
res_model1all_2ants<-residuals(model1all_2ants)

plot(variogram(res_model1all_2ants~1,data=subset(allplants,!is.na(phen))),main="res model 1 all 2 ants")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#START HERE - things that take A LONG TIME

#Test for autocorrelation (Moran's I) in residuals of models
moran_model1all_2ants<- moran.test(res_model1all_2ants, listw=allplants.listw1) 
moran_model1all_2ants #Significant autocorrelation

#Make correlogram of residuals of model
correlog_model1all_2ants <- correlog(subset(allplants,!is.na(phen))$x,
subset(allplants,!is.na(phen))$y,
res_model1all_2ants,increment=5, resamp=100) #Rerun with 1000
correlog_model1all_2ants
plot(correlog_model1all_2ants)
abline(h=0) #Ignore autocorrelation at longer distances!

#Moran eigenvector GLM filtering
#Moran eigenvectors
ME.model1all_2ants <-ME(attack ~ scale(as.integer(phen)) + scale(Mrub_sch_s) + scale(pldens_2) +
                          scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sch_s)+
                          scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
                  listw=allplants.listw1,
                  data=subset(allplants,!is.na(phen)), #allplants_df?
                  family=binomial,alpha=0.05,verbose=T)

#Repeat model with vectors as predictors
model1all_2ants_ME<-glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sch_s) + scale(pldens_2) +
                    scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sch_s)+
                    scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),#+vectors
                    subset(allplants,!is.na(phen)),family="binomial")
summary(model1all_2ants_ME)
NagelkerkeR2(model1all_2ants_ME)

res_model1all_2ants_ME<-residuals(model1all_2ants_ME)

#Make correlogram of residuals of model
correlog_model1all_2ants_ME <- correlog(subset(allplants,!is.na(phen))$x,
                                  subset(allplants,!is.na(phen))$y,
                                  res_model1all_2ants_ME,increment=5, resamp=100) #Rerun with 1000
correlog_model1all_2ants_ME
plot(correlog_model1all_2ants_ME)
abline(h=0)
#Test for autocorrelation (Moran's I) in residuals of models
moran_model1all_2ants_ME<- moran.test(res_model1all_2ants_ME, listw=allplants.listw1) 
moran_model1all_2ants_ME #NO SIGNIFICANT AUTOCORRELATION LEFT!!! :) :) :)

#negative binomial model - number of eggs when present ####
model2all_2ants<-glm.nb(n_eggs_max ~ scale(phen_int) + scale(Mrub_sch_s) + scale(pldens_3) + 
                          scale(phen_n3) + scale(phen_int):scale(Mrub_sch_s) + 
                          scale(phen_int):scale(phen_n3) + ,
                        subset(allplants,!is.na(phen)&n_eggs_max>0)) #MODEL TO USE!
summary(model2all_2ants)
NagelkerkeR2(model2all_2ants)

#GRAPHS TO USE
plot(effect(term="scale(phen_int)", 
     mod=model2all_2ants,data=subset(allplants,!is.na(phen))),
     multiline=T,type="response")
plot(effect(term="scale(Mrub_sch_s)", 
     mod=model2all_2ants,data=subset(allplants,!is.na(phen))),
     multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
     mod=model2all_2ants,data=subset(allplants,!is.na(phen))),
     x.var="pldens_3",multiline=T,type="response")
plot(effect(term="scale(pldens_3):scale(phen_n3)", 
     mod=model2all_2ants,data=subset(allplants,!is.na(phen))),
     x.var="phen_n3",multiline=T,type="response")


###

model2all_2ants_sep<-glm.nb(n_eggs_max ~ scale(as.integer(phen)) + 
       scale(Mrub_sum) + scale(Msch_sum) + scale(pldens_2) + scale(phen_n2) + 
       scale(as.integer(phen)):scale(Mrub_sum) + scale(as.integer(phen)):scale(Msch_sum)+
       scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
       subset(allplants,!is.na(phen)&n_eggs_max>0))
summary(model2all_2ants_sep)
NagelkerkeR2(model2all_2ants_sep)

#Get residuals of model
res_model2all_2ants<-residuals(model2all_2ants)

# #Make correlogram of residuals of model
correlog_model2all_2ants <- correlog(subset(allplants,!is.na(phen)&n_eggs_max>0)$x,
                               subset(allplants,!is.na(phen)&n_eggs_max>0)$y,
                               res_model2all_2ants,increment=5, resamp=100) #Rerun with 1000
correlog_model2all_2ants
plot(correlog_model2all_2ants)
abline(h=0) #Ignore autocorrelation at longer distances!
# 

plot(variogram(res_model2all_2ants~1,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),main="res model 2 all 2 ants")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#Test for autocorrelation (Moran's I) in residuals of models

moran_model2all_2ants<- moran.test(res_model2all, listw=allplants.listw2)
moran_model2all_2ants #NO Significant autocorrelation!

# Plots ####

plot(effect(term="phen_int:Mrub_sch_s", 
            mod=model1all_2ants,multiline=T,type="response"))
plot(effect(term="pldens_2:phen_n2", 
            mod=model1all_2ants,multiline=T,type="response"))
#Errors phen_int etc.
#...More graphs like these




