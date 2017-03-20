library(MASS)
library(fmsb)
library(gstat)
library(car)
library(spdep)
library(ncf)
library(nlme)

hist(allplants$n_eggs_max,breaks=50) #Highly skewed, probably zero-inflated

# All + temp, moist ####
mod1<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2),allplants)
summary(mod1)
NagelkerkeR2(mod1)

# Marked + temp, moist ####
mod2<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2),subset(allplants,!is.na(pl_id)))
summary(mod2)
NagelkerkeR2(mod2)

# Autocorrelation ####
#Variogram of dependent variable (eggs)
plot(variogram(n_eggs_max~1,data=allplants),main="Eggs",ylim=c(0,7))
plot(variogram(n_eggs_max~1,data=subset(allplants,!is.na(pl_id))),main="Eggs",ylim=c(0,30))

# correlog1 <- correlog(allplants$x, allplants$y, allplants$n_eggs_max,increment=1, resamp=0) #Slow
correlog2 <- correlog(subset(allplants,!is.na(pl_id))$x, subset(allplants,!is.na(pl_id))$y,
                      subset(allplants,!is.na(pl_id))$n_eggs_max,increment=1, resamp=100) #Rerun with 1000

#Variogram of residuals
res1 <- residuals(mod1)
res2 <- residuals(mod2)
plot(variogram(res1~1,data=allplants),main="Negative binomial model",ylim=c(0,7))
plot(variogram(res2~1,data=subset(allplants,!is.na(pl_id))),main="Negative binomial model",ylim=c(0,30))
# correlog3 <- correlog(allplants$x, allplants$y, res1,increment=1, resamp=0) #Slow
correlog4 <- correlog(subset(allplants,!is.na(pl_id))$x, subset(allplants,!is.na(pl_id))$y, 
                      res2,increment=1, resamp=100) #Rerun with 1000

plot(correlog2)
plot(correlog4)
plot(correlog2$correlation[1:20], type="b", pch=16, cex=1.5, lwd=1.5,
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)
plot(correlog4$correlation[1:20], type="b", pch=16, cex=1.5, lwd=1.5,
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)

#Calculate and test Moran's I (slow)
# allplants.nb <- dnearneigh(allplants, 0, 20) 
# allplants.listw <- nb2listw(allplants.nb) 
# GlobMT1<- moran.test(n_eggs_max, listw=allplants.listw)
# GlobMT2<- moran.test(res1, listw=allplants.listw)


#Calculate and test Moran's I (marked plants)
markedplants.nb <- dnearneigh(subset(allplants,!is.na(pl_id)), 0, 20) 
markedplants.listw <- nb2listw(markedplants.nb) 
GlobMT3<- moran.test(subset(allplants,!is.na(pl_id))$n_eggs_max, listw=markedplants.listw)
GlobMT4<- moran.test(res2, listw=markedplants.listw) #Still significant autocorrelation
GlobMT3
GlobMT4

# Autocov. marked ####
coords_mark<-as.matrix(cbind(subset(allplants,!is.na(pl_id))$x,subset(allplants,!is.na(pl_id))$y))
ac_mark <- autocov_dist(subset(allplants,!is.na(pl_id))$n_eggs_max, coords_mark, nbs = 20, type = "inverse",style="B")
ac_mark_sq <- autocov_dist(subset(allplants,!is.na(pl_id))$n_eggs_max, coords_mark, nbs = 20, type = "inverse.squared",style="B")

#Example autocovariate model (marked plants) - ac_mark
mod2aut<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                  scale(ac_mark),subset(allplants,!is.na(pl_id)))
summary(mod2aut)
NagelkerkeR2(mod2aut)

res2aut<-residuals(mod2aut)
correlog4aut <- correlog(subset(allplants,!is.na(pl_id))$x, subset(allplants,!is.na(pl_id))$y, 
                         res2aut,increment=1, resamp=100) 
plot(correlog4aut)
GlobMT4aut<- moran.test(res2aut, listw=markedplants.listw) 
GlobMT4aut#No significant autocorrelation after including autocovariate!

mod2aut_int<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
             scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2)+
             scale(ac_mark),subset(allplants,!is.na(pl_id)))
summary(mod2aut_int)
NagelkerkeR2(mod2aut_int)

res2aut_int<-residuals(mod2aut_int)
correlog4aut_int <- correlog(subset(allplants,!is.na(pl_id))$x, subset(allplants,!is.na(pl_id))$y, 
                         res2aut_int,increment=1, resamp=100) 
plot(correlog4aut_int)
GlobMT4aut_int<- moran.test(res2aut_int, listw=markedplants.listw) #Error different length!
GlobMT4aut_int#No significant autocorrelation after including autocovariate!

#Example autocovariate model (marked plants) - ac_mark_sq
mod2aut_sq<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                  scale(ac_mark_sq),subset(allplants,!is.na(pl_id)))
summary(mod2aut_sq)
NagelkerkeR2(mod2aut_sq)
AIC(mod2aut,mod2aut_sq) #Model with ac_mark is better

# Autocov. all ####
coords<-as.matrix(cbind(allplants$x,allplants$y))
ac <- autocov_dist(allplants$n_eggs_max, coords, nbs = 20, type = "inverse",style="B") #Slow

#Example autocovariate model (all plants) - slow
mod1aut<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                  scale(ac),allplants)
summary(mod1aut)
NagelkerkeR2(mod1aut)
res1aut<-residuals(mod1aut)

# correlog2aut <- correlog(allplants$x, allplants$y, 
#                          res1aut,increment=1, resamp=100) 
# plot(correlog2aut)
# GlobMT2aut<- moran.test(res1aut, listw=allplants.listw) 
# GlobMT2aut#No significant autocorrelation after including autocovariate!

# All - temp, moist + interactions ####
#Negative binomial
mod1_int<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                      scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2),allplants)
summary(mod1_int)
NagelkerkeR2(mod1_int)
res1_int<-residuals(mod1_int)
plot(variogram(res1_int~1,data=allplants))

#Hurdle
mod1_int_h<-hurdle(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
            scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2),
            dist="negbin",zero.dist="binomial",allplants)
summary(mod1_int_h)
res1_int_h<-residuals(mod1_int_h)
plot(variogram(res1_int_h~1,data=allplants))

AIC(mod1_int,mod1_int_h) #Hurdle better than negative binomial
nullmodel_eggs<-hurdle(n_eggs_max~1,data=allplants,dist="negbin",zero.dist="binomial")
#. Percent deviance explained, defined as [D(null model) - D(fitted model)] / D(null model)
2*logLik(mod1_int_h)     # Deviance model_eggs
2*logLik(nullmodel_eggs)  # Deviance null model
(-8695.87-(-6849.654))/-8695.87 # 21% Only!?

#Two models
allplants$attack<-as.factor(ifelse(allplants$n_eggs_max>0,1,0))
mod1_int_bin<-glm(attack~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
              scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2),
              family="binomial",allplants)
summary(mod1_int_bin)
NagelkerkeR2(mod1_int_bin)
res1_int_bin<-residuals(mod1_int_bin)
plot(variogram(res1_int_bin~1,data=allplants))

mod1_int_count<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2),
                subset(allplants,attack==1))
summary(mod1_int_count)
NagelkerkeR2(mod1_int_count)
res1_int_count<-residuals(mod1_int_count)
plot(variogram(res1_int_count~1,data=subset(allplants,attack==1)))

# Autocov. all - temp, moist + interactions ####
# TO USE IN PPT ####
#Negative binomial
mod1aut_int<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                      scale(as.integer(phen)):scale(Mrub_sum)+scale(as.integer(phen)):scale(phen_n2)+scale(pldens_2):scale(phen_n2)+
                      scale(ac),allplants)
summary(mod1aut_int)
NagelkerkeR2(mod1aut_int)
res1aut_int<-residuals(mod1aut_int)

#Hurdle
#Two models


#------------------------------------------------------------------------------------------------#

#Temp, moist on phen, ants, dens ####
mod_phen<-lm(as.integer(phen)~scale(meanTday)*scale(moist_per),allplants, na.action="na.fail")
summary(mod_phen)

mod_ants<-glm.nb(Mrub_sum~scale(meanTday)*scale(moist_per),allplants, na.action="na.fail")
summary(mod_ants)
plot(allplants$meanTday,allplants$Mrub_sum)
plot(allplants$moist_per,allplants$Mrub_sum)

mod_dens<-lm(pldens_2~scale(meanTday)*scale(moist_per),allplants, na.action="na.fail")
summary(mod_dens)
plot(allplants$meanTday,allplants$pldens_2)
plot(allplants$moist_per,allplants$pldens_2)

mod_phen_neigh<-lm(phen_n2~scale(meanTday)*scale(moist_per),allplants, na.action="na.fail")
summary(mod_phen_neigh)
plot(allplants$meanTday,allplants$phen_n2)
plot(allplants$moist_per,allplants$phen_n2)

#Ant models ####
summary(glm.nb(n_eggs_max~scale(Mrub_rug_s),allplants))
summary(glm.nb(n_eggs_max~scale(Mrub_rug_p),allplants))
summary(glm.nb(n_eggs_max~scale(Mrub_sum),allplants))
summary(glm(n_eggs_max~scale(Msca_sum),allplants,family="poisson")) #Strange error with glm.nb
summary(glm.nb(n_eggs_max~scale(Mrug_sum),allplants))
summary(glm.nb(n_eggs_max~scale(Msch_sum),allplants))
summary(glm.nb(n_eggs_max~scale(oth_sum),allplants))
summary(glm.nb(n_eggs_max~scale(allM_sum),allplants))
summary(glm.nb(n_eggs_max~scale(allM_max),allplants))
summary(glm.nb(n_eggs_max~scale(allM_pres),allplants))

summary(glm.nb(n_eggs_max~scale(Mrub_pres),allplants))
summary(glm.nb(n_eggs_max~scale(Msca_pres),allplants))
summary(glm.nb(n_eggs_max~scale(Mrug_pres),allplants))
summary(glm.nb(n_eggs_max~scale(Msch_pres),allplants))

#Ants - marked plants + autocovariate
summary(glm.nb(n_eggs_max~scale(Mrub_rug_s)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Mrub_rug_p)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Mrub_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Msca_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id)))) #Strange error with glm.nb
summary(glm.nb(n_eggs_max~scale(Mrug_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Msch_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(oth_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(allM_sum)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(allM_max)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(allM_pres)+scale(ac_mark),subset(allplants,!is.na(pl_id))))

summary(glm.nb(n_eggs_max~scale(Mrub_pres)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Msca_pres)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Mrug_pres)+scale(ac_mark),subset(allplants,!is.na(pl_id))))
summary(glm.nb(n_eggs_max~scale(Msch_pres)+scale(ac_mark),subset(allplants,!is.na(pl_id))))

#Correlations ants
cor(allplants$Mrub_rug_p,allplants$n_eggs_max)
cor(allplants$Mrub_sum,allplants$n_eggs_max)
cor(allplants$Msca_sum,allplants$n_eggs_max)
cor(allplants$Mrug_sum,allplants$n_eggs_max)
cor(allplants$Msch_sum,allplants$n_eggs_max)
cor(allplants$oth_sum,allplants$n_eggs_max)

summary(glm.nb(Mrub_sum~scale(meanTday)+scale(moist_per),allplants))
summary(glm.nb(Msch_sum~scale(meanTday)+scale(moist_per),allplants))
summary(lm(pldens_1~scale(meanTday)+scale(moist_per),allplants))

