set.seed(1234)
moran.mc_model1<-moran.mc(res_model1,listw=allplants.listw1,nsim=1000) #Try another function
moran.mc_model1_ME<-moran.mc(res_model1_ME,listw=allplants.listw1,nsim=1000) #Try another function

#RUN from here
moran.mc_model1_ME<-moran.mc(res_model1_ME,listw=allplants.listw1,nsim=10000)


correlog_model1_1 <- correlog(subset(allplants,!is.na(phen))$x,
                              subset(allplants,!is.na(phen))$y,
                              res_model1,increment=1, resamp=100) 
correlog_model1_ME_1 <- correlog(subset(allplants,!is.na(phen))$x,
                              subset(allplants,!is.na(phen))$y,
                              res_model1_ME,increment=1, resamp=100) 
correlog_model2_1 <- correlog(subset(allplants,!is.na(phen)&n_eggs_max>0)$x,
                              subset(allplants,!is.na(phen)&n_eggs_max>0)$y,
                              res_model2,increment=1, resamp=100) 
