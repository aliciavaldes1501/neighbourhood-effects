#Models
#after model selction (in 4_7)
#Model attack####
model10_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model10_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_5<-lm(seeds_per_fl~phen_int+attack_f,as.data.frame(allplants)) #Removed PC1+PC2
#All standardized coefs

sem10<-list(#with fitness, attack
  model10_1,
  model10_2,
  model10_3,
  model10_4,
  model10_5
)
sem.fit(sem10, data=as.data.frame(allplants)) #p=0.065
sem.model.fits(sem10)
sem.coefs(sem10, data=as.data.frame(allplants),standardize="scale")

#Model n eggs####

model11_1<-lm(phen_int~PC1+PC2,as.data.frame(allplants))
model11_2<-glm(Mrub_sch_s_pres~PC1+PC2,as.data.frame(allplants),family="binomial")
model11_3<-lm(suit_neigh~PC1+PC2,as.data.frame(allplants))
model11_4<-glm.nb(n_eggs_max~phen_int+suit_neigh+PC1+PC2,subset(as.data.frame(allplants),n_eggs_max>0))
model11_5<-lm(seeds_per_fl~scale(n_eggs_max),subset(as.data.frame(allplants),n_eggs_max>0))
#All standardized coefs

sem11<-list(#with fitness, number of eggs (all plants)
  model11_1,
  model11_2,
  model11_3,
  model11_4,
  model11_5
)

sem.fit(sem11, data=as.data.frame(allplants)) #p=0.454
sem.model.fits(sem11)
sem.coefs(sem11, data=as.data.frame(allplants),standardize="scale")

