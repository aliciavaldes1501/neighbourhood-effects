library(data.table)

seeds<-read.table("D:/SU/projects/neighbourhood_effects/data/raw/tanga2016_seeds.txt",header=T,sep="\t",dec=".")
head(seeds)
seeds<-subset(seeds,!is.na(n_dev_seeds))
seeds <- dcast(seeds, pl_id_ALL ~ inmature+P_UP, value.var="n_dev_seeds")

head(seeds)
names(seeds)<-c("pl_id","P_m","UP_m","P_i","UP_i")
seeds$status<-ifelse(!is.na(seeds$P_i)&!is.na(seeds$UP_i),"Both_i",
    ifelse(!is.na(seeds$P_i),"P_i",ifelse(!is.na(seeds$UP_i),"UP_i","Both_m")))
  
head(as.data.frame(allplants)[c(4,6,18,20)])
seeds<-merge(seeds,as.data.frame(allplants)[c(4,6,18,20)])
seeds$seeds_per_fl<-(((rowSums(seeds[,c("UP_m", "UP_i")], na.rm=T))*seeds$fr_in)+
                       ((rowSums(seeds[,c("P_m", "P_i")], na.rm=T))*seeds$fr_pr))/seeds$n_fl

allplants<-merge(allplants,seeds[c(1:6,10)],all.x=T)
hist(allplants$seeds_per_fl)

#Models
sem6<-list(#with fitness, attack
  lm(phen_int~meanT*moist_per,allplants),
  glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
  lm(suit_neigh~meanT*moist_per,allplants),
  glm(attack~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants,family="binomial"),
  lm(seeds_per_fl~phen_int+meanT+moist_per+attack,allplants)
)

sem.fit(sem6, data=allplants) #p=0
sem.fit(sem6, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem6)
sem.coefs(sem6, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))

sem7<-list(#with fitness, number of eggs (only pls w eggs)
  lm(phen_int~meanT*moist_per,allplants),
  glm.nb(Mrub_sch_s~meanT*moist_per,allplants),
  lm(suit_neigh~meanT*moist_per,allplants),
  glm.nb(n_eggs_max~phen_int*Mrub_sch_s+suit_neigh+meanT+moist_per,allplants),
  lm(seeds_per_fl~phen_int+meanT+moist_per+n_eggs_max,allplants)
)

sem.fit(sem7, data=allplants) #p=0
sem.fit(sem7, data=allplants,
        corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))
sem.model.fits(sem7)
sem.coefs(sem7, data=allplants,
          corr.errors=c("phen_int~~Mrub_sch_s","phen_int~~suit_neigh","suit_neigh~~Mrub_sch_s"))


