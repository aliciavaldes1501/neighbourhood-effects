library(spdep)
load(file="formula1.R")  
load(file="allplants.R")  
load(file="allplants.listw1.R")  

#allplants.nb1 <- dnearneigh(subset(allplants,!is.na(phen)), 0, 30) 
#allplants.listw1 <- nb2listw(allplants.nb1) 

ME.model1all <-ME(formula1, listw=allplants.listw1,
                  data=subset(allplants,!is.na(phen)),
                  family=binomial,alpha=0.05,verbose=T)

save(ME.model1all,file="ME.model1all_vectors.R")

modelito1<-glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sum) + scale(pldens_2) + 
              scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sum) + 
              scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2)+
              ME.model1all$vectors[,1],family=binomial,data=subset(allplants,!is.na(phen)))
modelito2<-glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sum) + scale(pldens_2) + 
                 scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sum) + 
                 scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2)+
                 ME.model1all$vectors[,1]+ME.model1all$vectors[,2],family=binomial,data=subset(allplants,!is.na(phen)))
summary(modelito1)
summary(modelito2)
res_modelito1<-residuals(modelito1)
res_modelito2<-residuals(modelito2)

moran1<-moran.test(res_modelito1,listw=allplants.listw1)
moran1

moran2<-moran.test(res_modelito2,listw=allplants.listw1)
moran2

NagelkerkeR2(modelito2)
