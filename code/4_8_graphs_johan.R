library(ggplot2)
library(ggthemes)

theme_set( theme_base( base_family= "Times"))

png(filename = "attack-phen.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_point(data=as.data.frame(allplants),aes(phen_int, attack))+
  geom_smooth(aes(phen_int, attack),method=glm,method.args=c(family="binomial"),se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()

png(filename = "attack-phen_nopoints.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_smooth(aes(phen_int, attack),method=glm,method.args=c(family="binomial"),se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()

png(filename = "eggs-phen.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(subset(as.data.frame(allplants),n_eggs_max>0)) +
  geom_point(data=subset(as.data.frame(allplants),n_eggs_max>0),aes(phen_int, n_eggs_max))+
  geom_smooth(aes(phen_int, n_eggs_max),method=glm,method.args=c(family="poisson"),se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Number of eggs")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()

png(filename = "eggs-phen_nopoints.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(subset(as.data.frame(allplants),n_eggs_max>0)) +
  geom_smooth(aes(phen_int, n_eggs_max),method=glm,method.args=c(family="poisson"),se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Number of eggs")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()

png(filename = "seeds-attack.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(summary_seeds,aes(as.factor(attack), seeds_per_fl)) +
  geom_errorbar(aes(ymin=seeds_per_fl-se, ymax=seeds_per_fl+se), width=.1,size=1) +
  geom_point(size=4)+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Attack")+ylab("Seeds per flower")
dev.off()

png(filename = "seeds-eggs_allplants.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_point(data=as.data.frame(allplants),aes(n_eggs_max, seeds_per_fl))+
  geom_smooth(aes(n_eggs_max, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Number of eggs")+ylab("Number of seeds per flower")
dev.off()

png(filename = "seeds-eggs_allplants_nopoints.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_smooth(aes(n_eggs_max, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Number of eggs")+ylab("Number of seeds per flower")+coord_cartesian(ylim=c(0,500))
dev.off()

png(filename = "seeds-eggs_plantswitheggs.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(subset(as.data.frame(allplants),n_eggs_max>0)) +
  geom_point(data=subset(as.data.frame(allplants),n_eggs_max>0),aes(n_eggs_max, seeds_per_fl))+
  geom_smooth(aes(n_eggs_max, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Number of eggs")+ylab("Number of seeds per flower")
dev.off()

png(filename = "seeds-eggs_plantswitheggs_nopoints.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(subset(as.data.frame(allplants),n_eggs_max>0)) +
  geom_smooth(aes(n_eggs_max, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Number of eggs")+ylab("Number of seeds per flower")+coord_cartesian(ylim=c(0,400))
dev.off()

png(filename = "seeds-phen.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_point(data=as.data.frame(allplants),aes(phen_int, seeds_per_fl))+
  geom_smooth(aes(phen_int, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Number of seeds per flower")
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()

png(filename = "seeds-phen_nopoints.png",
    width = 10, height = 10, units = "cm", pointsize = 20,
    bg = "white", res = 600, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
ggplot(as.data.frame(allplants)) +
  geom_smooth(aes(phen_int, seeds_per_fl),method=lm,se=T,color="black")+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  theme_base()+ theme(legend.position="none")+
  xlab("Shoot phenology")+ylab("Number of seeds per flower")
  scale_x_continuous(breaks=c(1,2,3,4,5,6))
dev.off()











