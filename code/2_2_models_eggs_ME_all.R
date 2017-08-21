allplants$attack<-ifelse(allplants$n_eggs_max>0,1,0)

#Example with all plants

#model with all plants
#binomial model - attack ####
# model1all<-glm(formula1,subset(allplants,!is.na(phen)),family="binomial")
# summary(model1all)
# NagelkerkeR2(model1all)

#Get residuals of model
# res_model1all<-residuals(model1all)

#Make correlogram of residuals of model
#correlog_model1all <- correlog(subset(allplants,!is.na(phen))$x,
                               # subset(allplants,!is.na(phen))$y,
                               # res_model1all,increment=5, resamp=100) #Rerun with 1000
# correlog_model1all
# plot(correlog_model1all)
# abline(h=0) #Ignore autocorrelation at longer distances!
# 
# plot(variogram(res_model1all~1,data=subset(allplants,!is.na(phen))),main="res model 1 all")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#Create neighbours matrix - dnearneigh (30 m)
#allplants.nb1 <- dnearneigh(subset(allplants,!is.na(phen)), 0, 30) 
#allplants.listw1 <- nb2listw(allplants.nb1) 

#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

# moran_model1all<- moran.test(res_model1all, listw=allplants.listw1) 
# moran_model1all #Significant autocorrelation

#Moran eigenvector GLM filtering
#Moran eigenvectors
# ME.model1all <-ME(formula1, listw=allplants.listw1, 
#                   data=subset(allplants_df,!is.na(phen)),
#                   family=binomial,alpha=0.05,verbose=T)
load(file="vector1.R")  
load(file="vector2.R")  

#Repeat model with vectors as predictors
#model1all_ME<-glm(attack~scale(as.integer(phen))+scale(Mrub_sum)+
              # scale(pldens_2)+scale(phen_n2)+
              # scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+
              # scale(pldens_2):scale(phen_n2)+scale(vector1)+scale(vector2),
              # subset(allplants,!is.na(phen)),family="binomial")
summary(model1all_ME)
NagelkerkeR2(model1all_ME)

res_model1all_ME<-residuals(model1all_ME)

#Make correlogram of residuals of model
# correlog_model1all_ME <- correlog(subset(allplants,!is.na(phen))$x, 
#                                   subset(allplants,!is.na(phen))$y, 
#                                   res_model1all_ME,increment=5, resamp=100) #Rerun with 1000
correlog_model1all_ME
plot(correlog_model1all_ME)
abline(h=0)
#Test for autocorrelation (Moran's I) in residuals of models
load(file="moran2.R")  
# moran_model1all_ME<-moran2
# moran_model1all_ME<- moran.test(res_model1all_ME, listw=allplants.listw1) 
moran_model1all_ME #NO SIGNIFICANT AUTOCORRELATION LEFT!!! :) :) :)

#negative binomial model - number of eggs when present ####
# model2all<-glm.nb(formula2,subset(allplants,!is.na(phen)&n_eggs_max>0))
summary(model2all)
NagelkerkeR2(model2all)

#Get residuals of model
# res_model2all<-residuals(model2all)

# #Make correlogram of residuals of model
# correlog_model2all <- correlog(subset(allplants,!is.na(phen)&n_eggs_max>0)$x, 
#                                subset(allplants,!is.na(phen)&n_eggs_max>0)$y, 
#                                res_model2all,increment=5, resamp=100) #Rerun with 1000
#correlog_model2all
plot(correlog_model2all)
abline(h=0) #Ignore autocorrelation at longer distances!
# 
# plot(variogram(res_model2all~1,data=subset(allplants,!is.na(phen)&n_eggs_max>0)),main="res model 2 all")
#cutof=26.546924 (length of the diagonal of the box spanning the data is divided by three)

#Create neighbours matrix - dnearneigh (30 m)
# allplants.nb2 <- dnearneigh(subset(allplants,!is.na(phen)&n_eggs_max>0), 0, 30) 
# allplants.listw2 <- nb2listw(allplants.nb2) 

#Test for autocorrelation (Moran's I and Geary's C) in residuals of models

# moran_model2all<- moran.test(res_model2all, listw=allplants.listw2) 
# moran_model2all #NO Significant autocorrelation!
# 
# geary_model2all<-geary.test(res_model2all, listw=allplants.listw2)
# geary_model2all #NO Significant autocorrelation!





                    