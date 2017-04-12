library(ggbiplot)
library(vegan)

#pca ants
pca_ants<-prcomp(as.data.frame(subset(allplants,!is.na(phen)))[44:47],scale=T)
plot(pca_ants)
biplot(pca_ants,col=c("grey","black"))

#model with all plants
#binomial model - attack ####
summary(glm(attack ~ scale(as.integer(phen)) + scale(Mrub_sum+Msca_sum) + scale(pldens_2) + 
                 scale(phen_n2) + scale(as.integer(phen)):scale(Mrub_sum+Msca_sum) + 
                 scale(as.integer(phen)):scale(phen_n2) + scale(pldens_2):scale(phen_n2),
               subset(allplants,!is.na(phen)),family="binomial"))



summary(glm(attack ~scale(allM_sum), subset(allplants,!is.na(phen)),family="binomial")) #-0.52232
summary(glm(attack ~scale(Mrub_sum), subset(allplants,!is.na(phen)),family="binomial")) #0.17423
summary(glm(attack ~scale(Msca_sum), subset(allplants,!is.na(phen)),family="binomial")) #-1.36392
summary(glm(attack ~scale(Mrug_sum), subset(allplants,!is.na(phen)),family="binomial")) #0.21499
summary(glm(attack ~scale(Msch_sum), subset(allplants,!is.na(phen)),family="binomial")) #0.35843
summary(glm(attack ~scale(oth_sum), subset(allplants,!is.na(phen)),family="binomial")) #-0.40260

summary(glm(attack ~scale(Mrub_sum)+scale(Msca_sum)+scale(Mrug_sum)+scale(Msch_sum)+
              scale(oth_sum), subset(allplants,!is.na(phen)),family="binomial")) #

summary(glm(attack ~scale(Mrub_sum)*scale(Mrug_sum)+scale(Msca_sum)*scale(Mrug_sum)+
              scale(Msch_sum)*scale(Mrug_sum)+scale(oth_sum)*scale(Mrug_sum), 
            subset(allplants,!is.na(phen)),family="binomial")) #Mrug interactions w all others







