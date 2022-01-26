# Alternative way to select MEM vectors
# (different from that on function ME, used previously)

prueba<-mem.select(
  x=residuals(mod5_1),
  listw=allplants.listw1,
  MEM.autocor = "all",
  method = "MIR",
  MEM.all = TRUE,
  nperm = 99,
  nperm.global = 999,
  alpha = 0.05,
  verbose = TRUE)

# prueba$MEM.select$MEM2 equal to ME.mod5_1$vectors[,1]

save.image("C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/4_models_plant/.RData")

prueba2<-mem.select(
  x=residuals(mod5_1),
  listw=allplants.listw1,
  MEM.autocor = "all",
  method = "FWD",
  MEM.all = TRUE,
  nperm = 99,
  nperm.global = 999,
  alpha = 0.05,
  verbose = TRUE)

# If the objective is to optimize the detection of the spatial patterns 
# in the residuals of a model of the response variable(s) against a set 
# of environmental predictors, for instance, then x should be the model 
# residuals, and method = "FWD". This allows optimizing the detection of
# residual spatial patterns once the effect of the environmental 
# predictors has been removed.

save.image("C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/4_models_plant/.RData")

summary(mod5_ME_1)
model_prueba2<-glm(as.integer(attack)~phen_int+round(Mrub_sum)+
              phen_int:round(Mrub_sum)+pldens_3+phen_n3+pldens_3:phen_n3+
              meanT+moist_per+prueba2$MEM.select$MEM2+prueba2$MEM.select$MEM9+
                prueba2$MEM.select$MEM56+prueba2$MEM.select$MEM5,
            subset1,family="binomial",na.action=na.fail)
summary(model_prueba2)

moran_model_prueba2<- moran.mc(residuals(model_prueba2),listw=allplants.listw1,nsim=999)
moran_model_prueba2


