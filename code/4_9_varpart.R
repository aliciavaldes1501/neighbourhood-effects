#Variation partitioning
#Model attack
model10_4

model10_4<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")

model10_4_res_nei_env<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_4_res<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres,as.data.frame(allplants),family="binomial")
model10_4_nei<-glm(attack_f~suit_neigh,as.data.frame(allplants),family="binomial")
model10_4_env<-glm(attack_f~PC1+PC2,as.data.frame(allplants),family="binomial")
model10_4_res_nei<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+suit_neigh,as.data.frame(allplants),family="binomial")
model10_4_res_env<-glm(attack_f~phen_int+Mrub_sch_s_pres+phen_int:Mrub_sch_s_pres+PC1+PC2,as.data.frame(allplants),family="binomial")
model10_4_nei_env<-glm(attack_f~suit_neigh+PC1+PC2,as.data.frame(allplants),family="binomial")

R_res_nei_env<-sem.model.fits(model10_4_res_nei_env)$R.squared
R_res<-sem.model.fits(model10_4_res)$R.squared
R_nei<-sem.model.fits(model10_4_nei)$R.squared
R_env<-sem.model.fits(model10_4_env)$R.squared
R_res_nei<-sem.model.fits(model10_4_res_nei)$R.squared
R_res_env<-sem.model.fits(model10_4_res_env)$R.squared
R_nei_env<-sem.model.fits(model10_4_nei_env)$R.squared

res<-R_res_nei_env-R_nei_env
nei<-R_res_nei_env-R_res_env
env<-R_res_nei_env-R_res_nei
res_nei<-R_res_nei_env-R_env-res-nei
res_env<-R_res_nei_env-R_nei-res-env
nei_env<-R_res_nei_env-R_res-nei-env
res_nei_env<-R_res_nei_env-res-nei-env-res_nei-res_env-nei_env

(res/R_res_nei_env)*100
(nei/R_res_nei_env)*100
(env/R_res_nei_env)*100
(res_nei/R_res_nei_env)*100
(res_env/R_res_nei_env)*100
(nei_env/R_res_nei_env)*100
(res_nei_env/R_res_nei_env)*100

res+nei+env+res_nei+res_env+nei_env+res_nei_env

#model seeds
model10_5

model10_5<-lm(seeds_per_fl~phen_int+attack_f,as.data.frame(allplants))

model10_5_phe_att<-lm(seeds_per_fl~phen_int+attack_f,as.data.frame(allplants))
model10_5_phe<-lm(seeds_per_fl~phen_int,as.data.frame(allplants))
model10_5_att<-lm(seeds_per_fl~attack_f,as.data.frame(allplants))

R_phe_att<-sem.model.fits(model10_5_phe_att)$R.squared
R_phe<-sem.model.fits(model10_5_phe)$R.squared
R_att<-sem.model.fits(model10_5_att)$R.squared

phe<-R_phe_att-R_att
att<-R_phe_att-R_phe
phe_att<-R_phe_att-phe-att

(phe/R_phe_att)*100
(att/R_phe_att)*100
(phe_att/R_phe_att)*100

phe+att+phe_att

#model seeds with env
model10_5_alt<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,as.data.frame(allplants))

model10_5_alt_phe_att_env<-lm(seeds_per_fl~phen_int+attack_f+PC1+PC2,as.data.frame(allplants))
model10_5_alt_phe<-lm(seeds_per_fl~phen_int,as.data.frame(allplants))
model10_5_alt_att<-lm(seeds_per_fl~attack_f,as.data.frame(allplants))
model10_5_alt_env<-lm(seeds_per_fl~PC1+PC2,as.data.frame(allplants))
model10_5_alt_phe_att<-lm(seeds_per_fl~phen_int+attack_f,as.data.frame(allplants))
model10_5_alt_phe_env<-lm(seeds_per_fl~phen_int+PC1+PC2,as.data.frame(allplants))
model10_5_alt_att_env<-lm(seeds_per_fl~attack_f+PC1+PC2,as.data.frame(allplants))

R_phe_att_env<-sem.model.fits(model10_5_alt_phe_att_env)$R.squared
R_phe<-sem.model.fits(model10_5_alt_phe)$R.squared
R_att<-sem.model.fits(model10_5_alt_att)$R.squared
R_env<-sem.model.fits(model10_5_alt_env)$R.squared
R_phe_att<-sem.model.fits(model10_5_alt_phe_att)$R.squared
R_phe_env<-sem.model.fits(model10_5_alt_phe_env)$R.squared
R_att_env<-sem.model.fits(model10_5_alt_att_env)$R.squared

phe<-R_phe_att_env-R_att_env
att<-R_phe_att_env-R_phe_env
env<-R_phe_att_env-R_phe_att
phe_att<-R_phe_att_env-R_env-phe-att
phe_env<-R_phe_att_env-R_att-phe-env
att_env<-R_phe_att_env-R_phe-att-env
phe_att_env<-R_phe_att_env-phe-att-env-phe_att-phe_env-att_env

(phe/R_phe_att_env)*100
(att/R_phe_att_env)*100
(env/R_phe_att_env)*100
(phe_att/R_phe_att_env)*100
(phe_env/R_phe_att_env)*100
(att_env/R_phe_att_env)*100
(phe_att_env/R_phe_att_env)*100

phe+att+env+phe_att+phe_env+att_env+phe_att_env










