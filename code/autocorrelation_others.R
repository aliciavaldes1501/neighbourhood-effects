#Try spatial generalized linear mixed model
allplants_df<-allplants
allplants_df$X<-allplants@coords[,1]
allplants_df$Y<-allplants@coords[,2]
allplants_df<-as.data.frame(allplants_df)
str(allplants_df)

group <- factor(rep("a",nrow(allplants_df)))
allplants_df <- cbind(allplants_df, group)
markedplants_df<-subset(allplants_df,!is.na(pl_id))
attach(markedplants_df)
model.e <- glmmPQL(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2),
                   random=~1|group,data=markedplants_df,correlation=corExp(form=~x+y), family=quasipoisson,niter=10,verbose=T)
model.g <- glmmPQL(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2),
                   random=~1|group,data=markedplants_df,correlation=corGaus(form=~x+y), family=quasipoisson,niter=10,verbose=T)
model.s <- glmmPQL(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2),
                   random=~1|group,data=markedplants_df,correlation=corSpher(form=~x+y), family=quasipoisson,niter=10,verbose=T)

#PCNM
MEmod2 <- ME(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+
               scale(pldens_2)+scale(phen_n2), data=subset(allplants,!is.na(pl_id)),family = quasipoisson, 
             listw = markedplants.listw, alpha = 0.05, nsim = 99, stdev = FALSE, verbose = TRUE)
#Only one vector
MEmod2$vectors[,1]

#Try model with PCNM vector
mod2PCNM<-glm.nb(n_eggs_max~scale(as.integer(phen))+scale(meanTday)+scale(moist_per)+scale(Mrub_sum)+scale(pldens_2)+scale(phen_n2)+
                   MEmod2$vectors[,1],subset(allplants,!is.na(pl_id)))
summary(mod2PCNM)
NagelkerkeR2(mod2PCNM)

res2PCNM<-residuals(mod2PCNM)
correlog4PCNM <- correlog(subset(allplants,!is.na(pl_id))$x, subset(allplants,!is.na(pl_id))$y, 
                          res2PCNM,increment=1, resamp=100) 
plot(correlog4PCNM)
GlobMT4PCNM<- moran.test(res2PCNM, listw=markedplants.listw) 
GlobMT4PCNM #No significant autocorrelation after including PCNM axis!
