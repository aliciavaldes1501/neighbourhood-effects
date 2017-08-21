#Pl phen ~ temp + moist ##########
hist(as.integer(subset(allplants,!is.na(phen))$phen))

summary(lm(as.integer(phen)~scale(meanT)*scale(moist_per),
            data=subset(allplants,!is.na(phen))))

plot(glm(as.integer(phen)~scale(meanT)*scale(moist_per),
         data=subset(allplants,!is.na(phen))))

plot(effect(term="meanT:moist_per", 
            mod=lm(as.integer(phen)~meanT*moist_per,data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))

#Plants flower earlier with lower temperatures (!)
#and the effect is stronger when moisture is low
#At high moisture levels, almost no effect of temperature

plot(effect(term="meanT", 
            mod=lm(as.integer(phen)~meanT*moist_per,
            data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))

plot(effect(term="moist_per", 
            mod=lm(as.integer(phen)~meanT*moist_per,
            data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))

#On graph lm moist~temp ##########
ggplot(as.data.frame(subset(allplants,!is.na(phen))),
  aes(meanT, moist_per))+ geom_point(color="red")+ 
  geom_smooth(aes(meanT, moist_per),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")
summary(lm(moist_per~meanT,data=subset(allplants,!is.na(phen))))

ggplot(as.data.frame(subset(allplants,!is.na(phen)))) + 
  geom_point(data=as.data.frame(subset(allplants,!is.na(phen)&phen=="f")),
  aes(meanT, moist_per))+ 
  geom_smooth(aes(meanT, moist_per),method=lm,se=F,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+ ggtitle("Phen")
#Don't see very well

#Pl dens ~ temp + moist ##########
hist(as.integer(subset(allplants,!is.na(phen))$pldens_2))
hist(log(as.integer(subset(allplants,!is.na(phen))$pldens_2)))

summary(lm(pldens_2~scale(meanT)*scale(moist_per),
            data=subset(allplants,!is.na(phen))))

plot(lm(pldens_2~scale(meanT)*scale(moist_per),
         data=subset(allplants,!is.na(phen))))

plot(effect(term="meanT:moist_per", 
            mod=lm(pldens_2~meanT*moist_per,
            data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))

#Plant density decreases with temperature when moisture is low
#but increases with temperature when moisture is high

plot(effect(term="meanT", 
            mod=lm(pldens_2~meanT*moist_per,
                   data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))

plot(effect(term="moist_per", 
            mod=lm(pldens_2~meanT*moist_per,
                   data=subset(allplants,!is.na(phen))),
            multiline=T,type="response"))


