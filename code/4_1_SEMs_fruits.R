library(car)
library(MASS)
library(fmsb)
library(piecewiseSEM)

load(file="allplants.R")  
load(file="allplants.listw1.R")  
load(file="allplants.listw2.R")  
head(allplants)

#Changes needed in allplants after inspection of comments on seed data
#Some fruits noted wrong P_UP
allplants$fr_in[4486]=3
allplants$fr_inC[4486]=0
allplants$fr_pr[4486]=1
allplants$fr_prC[4486]=1
allplants$fr_in[7626]=4
allplants$fr_inC[7626]=0
allplants$fr_pr[7626]=1
allplants$fr_prC[7626]=1

sem1<-list(#density and phenology of neighbours
    lm(phen_int~meanT*moist_per,subset(allplants,!is.na(phen))),
    glm.nb(Mrub_sch_s~meanT*moist_per,subset(allplants,!is.na(phen))),
    lm(pldens_3~meanT*moist_per,subset(allplants,!is.na(phen))),
    lm(phen_n3~meanT*moist_per,subset(allplants,!is.na(phen))),
    glm(attack~phen_int*Mrub_sch_s+pldens_3*phen_n3+meanT+moist_per,subset(allplants,!is.na(phen)),family="binomial")
      ) 
sem.fit(sem1, data=subset(allplants,!is.na(phen)),
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~pldens_3","pldens_3~~Mrub_sch_s",
        "phen_n3~~phen_int","phen_n3~~Mrub_sch_s","phen_n3~~pldens_3"))
sem.model.fits(sem1)
sem.coefs(sem1, data=subset(allplants,!is.na(phen)),
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~pldens_3","pldens_3~~Mrub_sch_s",
        "phen_n3~~phen_int","phen_n3~~Mrub_sch_s","phen_n3~~pldens_3"))

sem2<-list(#only density of neighbours
  lm(phen_int~meanT*moist_per,subset(allplants,!is.na(phen))),
  glm.nb(Mrub_sch_s~meanT*moist_per,subset(allplants,!is.na(phen))),
  lm(pldens_3~meanT*moist_per,subset(allplants,!is.na(phen))),
  glm(attack~phen_int*Mrub_sch_s+pldens_3+meanT+moist_per,subset(allplants,!is.na(phen)),family="binomial")
) 
sem.fit(sem2, data=subset(allplants,!is.na(phen)),
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~pldens_3","pldens_3~~Mrub_sch_s"))
sem.model.fits(sem2)
sem.coefs(sem2, data=subset(allplants,!is.na(phen)),
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~pldens_3","pldens_3~~Mrub_sch_s"))

allplants$suit_neigh<-allplants$pldens_3*allplants$phen_n3
hist(allplants$suit_neigh)

sem3<-list(#suitability of neighbours
  lm(phen_int~meanT*moist_per,subset(allplants,!is.na(phen))),
  glm.nb(Mrub_sch_s~meanT*moist_per,subset(allplants,!is.na(phen))),
  lm(suit_neigh~meanT*moist_per,subset(allplants,!is.na(phen))),
  glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,subset(allplants,!is.na(phen)),family="binomial")
) 
sem.fit(sem3, data=subset(allplants,!is.na(phen)),
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem3)
sem.coefs(sem3, data=subset(allplants,!is.na(phen)),
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

allplants$fruitset<-allplants$fr_in/allplants$n_fl
subset(allplants,fruitset>1) #5 cases
#Change n_fl when fruiset results to be >1
allplants$n_fl<-ifelse(is.na(allplants$fr_in),allplants$n_fl,ifelse(allplants$fr_in>allplants$n_fl,allplants$fr_in,allplants$n_fl))
allplants$fruitset<-ifelse(allplants$fruitset>1,1,allplants$fruitset)

sem4<-list(#with fitness, attack
  lm(phen_int~meanT*moist_per,allplants),
  glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
  lm(suit_neigh~meanT*moist_per,allplants),
  glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial"),
  glm(cbind(fr_in,n_fl)~phen_int+meanT+moist_per+attack,allplants,family="binomial")
  )

sem.fit(sem4, data=allplants) #p=0
sem.fit(sem4, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem4)
sem.coefs(sem4, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

#Plot component models
par(mfrow=c(2,2))
plot(lm(phen_int~meanT*moist_per,allplants))
plot(glm.nb(Mrub_sch_s~meanT*moist_per,allplants))
plot(lm(suit_neigh~meanT*moist_per,allplants))
plot(glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial"))
plot(glm(cbind(fr_in,n_fl)~phen_int+meanT+moist_per+attack,allplants,family="binomial"))

sem5<-list(#with fitness, number of eggs (only pls w eggs)
  lm(phen_int~meanT*moist_per,allplants),
  glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
  lm(suit_neigh~meanT*moist_per,allplants),
  glm.nb(n_eggs_max~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,subset(allplants,n_eggs_max>0)),
  glm(cbind(fr_in,n_fl)~phen_int+meanT+moist_per+n_eggs_max,subset(allplants,n_eggs_max>0),family="binomial")
)
sem.fit(sem5, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem5)
sem.coefs(sem5, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

#Plot component models
par(mfrow=c(2,2))
plot(glm.nb(n_eggs_max~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,subset(allplants,n_eggs_max>0)))
plot(glm(cbind(fr_in,n_fl)~phen_int+meanT+moist_per+n_eggs_max,subset(allplants,n_eggs_max>0),family="binomial"))
   
   