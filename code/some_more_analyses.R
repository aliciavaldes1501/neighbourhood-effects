




########################################################################

#Ants - all plants
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

####################################################
cor(allplants$Mrub_rug_p,allplants$n_eggs_max)
cor(allplants$Mrub_sum,allplants$n_eggs_max)
cor(allplants$Msca_sum,allplants$n_eggs_max)
cor(allplants$Mrug_sum,allplants$n_eggs_max)
cor(allplants$Msch_sum,allplants$n_eggs_max)
cor(allplants$oth_sum,allplants$n_eggs_max)

summary(glm.nb(Mrub_sum~scale(meanTday)+scale(moist_per),allplants))
summary(glm.nb(Msch_sum~scale(meanTday)+scale(moist_per),allplants))
summary(lm(pldens_1~scale(meanTday)+scale(moist_per),allplants))

#Try hurdle models for eggs, or binomial (attack) + other (n_eggs on attacked plants)


##################################



