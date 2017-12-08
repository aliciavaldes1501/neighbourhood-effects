tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2_A.tiff",
     res=600,width=10,height=11,units="cm",family="Times") 
p1
dev.off()

tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2_B.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
p3
dev.off()

tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2_C.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
p4
dev.off()



tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3_A.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
ggplot(interaction2, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Neighbor phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3_B.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
p5
dev.off()


tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2_A_mean.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
ggplot(interaction1, aes(phen_int,fit))+
  geom_smooth(method=loess,se=F,size=1,aes(phen_int,fit))+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+theme_base()+
  theme(legend.position="top")+scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3_A_mean.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
ggplot(interaction2, aes(pldens_3,fit))+
  geom_smooth(method=loess,se=F,size=1,aes(pldens_3,fit))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

tiff("C:/Users/Alicia/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3_B_mean.tiff",
     res=600,width=9,height=9,units="cm",family="Times") 
ggplot(interaction3, aes(pldens_3,fit))+
  geom_smooth(method=loess,se=F,size=1,aes(pldens_3,fit))+
  xlab("Neighbor density")+ylab("Number of eggs")+
  theme_base()+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()
