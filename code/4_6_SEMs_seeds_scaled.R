#Presence-absence of ants: presence when >1
allplants$Mrub_sch_s_pres<-ifelse(allplants$Mrub_sch_s>=1,1,0)
hist(allplants$Mrub_sch_s)
hist(allplants$Mrub_sch_s_pres)

#Models
#Model attack####
model8_1<-lm(phen_int~meanT+moist_per+meanT:moist_per,as.data.frame(allplants))
model8_2<-glm(Mrub_sch_s_pres~meanT+moist_per+meanT:moist_per,as.data.frame(allplants),family="binomial")
model8_3<-lm(suit_neigh~meanT+moist_per+meanT:moist_per,as.data.frame(allplants))
model8_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+meanT+moist_per,as.data.frame(allplants),family="binomial")
model8_5<-lm(seeds_per_fl~phen_int+meanT+moist_per+attack_f,as.data.frame(allplants))
#All standardized coefs

sem8<-list(#with fitness, attack
  model8_1,
  model8_2,
  model8_3,
  model8_4,
  model8_5
)
sem.fit(sem8, data=as.data.frame(allplants)) #p=0
sem.fit(sem8, data=as.data.frame(allplants),
        corr.errors=c("phen_int~~Mrub_sch_s_pres","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s_pres"))
sem.model.fits(sem8)
sem.coefs(sem8, data=as.data.frame(allplants),standardize="scale",
          corr.errors=c("phen_int~~Mrub_sch_s_pres","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s_pres"))

#Model n eggs####

model9_1<-lm(phen_int~meanT+moist_per+meanT:moist_per,as.data.frame(allplants))
model9_2<-glm(Mrub_sch_s_pres~meanT+moist_per+meanT:moist_per,as.data.frame(allplants),family="binomial")
model9_3<-lm(suit_neigh~meanT+moist_per+meanT:moist_per,as.data.frame(allplants))
model9_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+meanT+moist_per,subset(as.data.frame(allplants),n_eggs_max>0))
model9_5<-lm(seeds_per_fl~phen_int+meanT+moist_per+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
#All standardized coefs

sem9<-list(#with fitness, number of eggs (all plants)
  model9_1,
  model9_2,
  model9_3,
  model9_4,
  model9_5
)

sem.fit(sem9, data=as.data.frame(allplants)) #p=0
sem.fit(sem9, data=as.data.frame(allplants),
        corr.errors=c("phen_int~~Mrub_sch_s_pres","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s_pres"))
sem.model.fits(sem9)
sem.coefs(sem9, data=as.data.frame(allplants),standardize="scale",
          corr.errors=c("phen_int~~Mrub_sch_s_pres","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s_pres"))



