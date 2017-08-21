library(ggbiplot)
library(vegan)

#pca ants
pca_ants<-prcomp(as.data.frame(subset(allplants,!is.na(phen)))[44:48],scale=T)
plot(pca_ants)
biplot(pca_ants,col=c("grey","black"))

allplants$Mrub_sch_p<-ifelse(allplants$Mrub_p==1|allplants$Msch_p==1,"1","0")


#model with all plants
#binomial model - attack ####
summary(glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sum+Msca_sum) + scale(pldens_2) + 
                 scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sum+Msca_sum) + 
                 scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
               subset(allplants,!is.na(phen)),family="binomial"))

summary(glm(attack ~scale(allM_sum), subset(allplants,!is.na(phen)),family="binomial")) #-0.52232
summary(glm(attack ~scale(Mrub_sum), subset(allplants,!is.na(phen)),family="binomial")) #0.17423
summary(glm(attack ~scale(Msca_sum), subset(allplants,!is.na(phen)),family="binomial")) #-1.36392
summary(glm(attack ~scale(Mrug_sum), subset(allplants,!is.na(phen)),family="binomial")) #-0.21499
summary(glm(attack ~scale(Msch_sum), subset(allplants,!is.na(phen)),family="binomial")) #0.35843
summary(glm(attack ~scale(oth_sum), subset(allplants,!is.na(phen)),family="binomial")) #-0.40260
summary(glm(attack ~scale(Mrub_sch_s), subset(allplants,!is.na(phen)),family="binomial")) #0.36009

summary(glm(attack ~scale(Mrub_sum)+scale(Msca_sum)+scale(Mrug_sum)+scale(Msch_sum)+
              scale(oth_sum), subset(allplants,!is.na(phen)),family="binomial")) #

summary(glm(attack ~scale(Mrub_sum)*scale(Mrug_sum)+scale(Msca_sum)*scale(Mrug_sum)+
              scale(Msch_sum)*scale(Mrug_sum)+scale(oth_sum)*scale(Mrug_sum), 
            subset(allplants,!is.na(phen)),family="binomial")) #Mrug interactions w all others


summary(glm(attack ~as.factor(Mrub_p), subset(allplants,!is.na(phen)),family="binomial")) #0.22471
summary(glm(attack ~as.factor(Msca_p), subset(allplants,!is.na(phen)),family="binomial")) #-1.58382
summary(glm(attack ~as.factor(Mrug_p), subset(allplants,!is.na(phen)),family="binomial")) #-0.25981
summary(glm(attack ~as.factor(Msch_p), subset(allplants,!is.na(phen)),family="binomial")) #1.46238
summary(glm(attack ~as.factor(oth_p), subset(allplants,!is.na(phen)),family="binomial")) #-0.85729
summary(glm(attack ~as.factor(Mrub_sch_p), subset(allplants,!is.na(phen)),family="binomial")) #1.08614

#model with all plants
#negative binomial model - eggs on attacked pls ####

summary(glm.nb(n_eggs_max~scale(allM_sum), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(Mrub_sum), subset(allplants,!is.na(phen)&n_eggs_max>0))) #0.13664
summary(glm.nb(n_eggs_max~scale(Msca_sum), subset(allplants,!is.na(phen)&n_eggs_max>0))) #-0.11955
summary(glm.nb(n_eggs_max~scale(Mrug_sum), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(Msch_sum), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(oth_sum), subset(allplants,!is.na(phen)&n_eggs_max>0)))  #NS
summary(glm.nb(n_eggs_max~scale(Mrub_sch_s), subset(allplants,!is.na(phen)&n_eggs_max>0))) #0.11490

summary(glm.nb(n_eggs_max~as.factor(Mrub_p), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~as.factor(Msca_p), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~as.factor(Mrug_p), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~as.factor(Msch_p), subset(allplants,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~as.factor(oth_p), subset(allplants,!is.na(phen)&n_eggs_max>0)))  #NS
summary(glm.nb(n_eggs_max~as.factor(Mrub_sch_p), subset(allplants,!is.na(phen)&n_eggs_max>0)))  #0.14917*



