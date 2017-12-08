#Models
#Model attack####
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,as.data.frame(allplants)) 
sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.038, AICc=75.467
sem.coefs(sem10, data=as.data.frame(allplants),standardize="scale")

#Step1
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,as.data.frame(allplants)) 
sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.018, AICc=75.857

#Step2
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~attack_f+PC1+PC2,as.data.frame(allplants)) 
sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.035, AICc=76.217

#Step3
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f+PC2,as.data.frame(allplants)) 
sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.051, AICc=74.797 --> Lower

#Step4
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f,as.data.frame(allplants)) 
sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.065, AICc=74.168 --> Lower --> SELECTED
sem.model.fits(sem10)
sem.coefs(sem10, data=as.data.frame(allplants),standardize="scale")

#Model n eggs####

model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC1+PC2+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.454, AICc=66.232
sem.coefs(sem11, data=as.data.frame(allplants),standardize="scale")

#Step1
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC1+PC2+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.455, AICc=65.557--> Lower

#Step2
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC2+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.414, AICc=66.067

#Step3
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+Mrub_sch_s_pres+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC1+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.412, AICc=66.097

#Step4
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+PC2+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.417, AICc=65.429--> Lower

#Step5
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~phen_int+scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.44, AICc=64.509--> Lower

#Step6
model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)
sem.fit(sem11, data=as.data.frame(allplants)) #p=0.444, AICc=63.894--> Lower --> SELECTED
sem.model.fits(sem11)
sem.coefs(sem11, data=as.data.frame(allplants),standardize="scale")

