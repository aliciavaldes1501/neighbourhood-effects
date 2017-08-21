library(MuMIn)

allplants$phen_int<-as.integer(allplants$phen)

#Models for eggs with other vars
#(number of flowers, shoot height, vegetation height)
#with marked plants

#binomial model - attack ####
model3<-glm(attack ~ scale(phen_int)+scale(Mrub_sch_s)+#As before w. only 3 vars added
            scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+
            scale(phen_int):scale(Mrub_sch_s)+
            scale(phen_int):scale(phen_n2)+
            scale(pldens_2):scale(phen_n2),
            subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial")
model3<-glm(attack ~ scale(phen_int)+scale(Mrub_sch_s)+#All with p<0.2
              scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+
              scale(phen_int):scale(pldens_2)+
              scale(phen_int):scale(phen_n2)+
              scale(phen_int):scale(n_fl)+
              scale(phen_int):scale(shoot_h)+
              scale(phen_int):scale(veg_h_mean)+
              scale(pldens_2):scale(phen_n2)+
              scale(pldens_2):scale(shoot_h)+
              scale(pldens_2):scale(veg_h_mean)+
              scale(n_fl):scale(shoot_h),
            subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial")
model3<-glm(attack ~ scale(phen_int)+scale(Mrub_sch_s)+#Reduction till all *
              scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+
              scale(phen_int):scale(n_fl)+
              scale(pldens_2):scale(phen_n2)+
              scale(pldens_2):scale(shoot_h)+
              scale(pldens_2):scale(veg_h_mean),
            subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial")
summary(model3)
NagelkerkeR2(model3)
vif(model3)

plot(effect(term="scale(phen_int):scale(n_fl)", 
            mod=model3,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
            multiline=T,type="response")
plot(effect(term="scale(pldens_2):scale(phen_n2)", 
            mod=model3,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
            multiline=T,type="response")
plot(effect(term="scale(pldens_2):scale(shoot_h)", 
            mod=model3,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
            multiline=T,type="response")
plot(effect(term="scale(pldens_2):scale(veg_h_mean)", 
            mod=model3,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
            multiline=T,type="response")

model3<-glm(attack ~ (scale(phen_int)+scale(Mrub_sch_s)+#Full
            scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean))^2,
            subset(allplants,!is.na(pl_id)&!is.na(phen)),family="binomial",na.action="na.fail")
models3<-dredge(model3)
summary(model.avg(models3, subset = delta < 2)) 

cor(subset(allplants,!is.na(pl_id)&!is.na(phen))$n_fl,subset(allplants,!is.na(pl_id)&!is.na(phen))$shoot_h)

model3_step<-stepAIC(model3,direction="both")
summary(model3_step)
NagelkerkeR2(model3_step)

#negative binomial model - number of eggs when present ####
model4<-glm.nb(n_eggs_max ~ (scale(phen_int)+scale(Mrub_sch_s)+
        scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean))^2,
        subset(allplants,!is.na(phen)&n_eggs_max>0))#Full
model4<-glm.nb(n_eggs_max ~ scale(phen_int)+scale(Mrub_sch_s)+#All with p<0.2
        scale(pldens_2)+scale(phen_n2)+scale(n_fl)+scale(shoot_h)+scale(veg_h_mean)+
        scale(pldens_2):scale(n_fl)+
        scale(phen_n2):scale(veg_h_mean),
        subset(allplants,!is.na(phen)&n_eggs_max>0))
summary(model4)
NagelkerkeR2(model4)
vif(model4)

models4<-pdredge(model4,cluster=clust)
summary(model.avg(models4, subset = delta < 2)) 

plot(effect(term="scale(pldens_2):scale(n_fl)", 
     mod=model4,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
     multiline=T,type="response")
plot(effect(term="scale(phen_n2):scale(veg_h_mean)", 
     mod=model4,data=subset(allplants,!is.na(pl_id)&!is.na(phen))),
     multiline=T,type="response")

model4_step<-stepAIC(model4,direction="both")
summary(model4_step)
NagelkerkeR2(model4_step)
