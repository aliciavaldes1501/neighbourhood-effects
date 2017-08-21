#increment=5 m####
df1<-data.frame(cbind(distance=as.vector(correlog_model1$mean.of.class[1:7]), 
                      correlation=as.vector(correlog_model1$correlation[1:7]),
                      p=as.vector(correlog_model1$p[1:7])))
ggplot(data=df1,aes(x=distance, y=correlation)) +
  geom_point(colour = "black", size = 2) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

df2<-data.frame(cbind(distance=as.vector(correlog_model1_ME$mean.of.class[1:7]), 
                      correlation=as.vector(correlog_model1_ME$correlation[1:7]),
                      p=as.vector(correlog_model1_ME$p[1:7])))
ggplot(data=df2,aes(x=distance, y=correlation)) +
  geom_point(colour = "red", size = 2) +
  geom_line(colour = "red") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) +  
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

df1$type<-"model1"
df2$type<-"model1_ME"
df3<-rbind(df1,df2)

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS6.tiff",
     res=600,width=13,height=11,units="cm",family="Times") 
ggplot(df3,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
dev.off()

df4<-data.frame(cbind(distance=as.vector(correlog_model2$mean.of.class[1:7]), 
                      correlation=as.vector(correlog_model2$correlation[1:7]),
                      p=as.vector(correlog_model2$p[1:7])))

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS7.tiff",
     res=600,width=13,height=11,units="cm",family="Times") 
ggplot(df4,aes(x=distance, y=correlation)) +
  geom_point(size=2) +
  geom_line() +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.1,0.25),breaks = c(-0.1,-0.05,0,0.05,0.1,0.15,0.20,0.25))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
dev.off()

#increment=1 m####
df5<-data.frame(cbind(distance=as.vector(correlog_model1_1$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model1_1$correlation[1:31]),
                      p=as.vector(correlog_model1_1$p[1:31])))
ggplot(data=df5,aes(x=distance, y=correlation)) +
  geom_point(colour = "black", size = 2) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

df6<-data.frame(cbind(distance=as.vector(correlog_model1_ME_1$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model1_ME_1$correlation[1:31]),
                      p=as.vector(correlog_model1_ME_1$p[1:31])))
ggplot(data=df6,aes(x=distance, y=correlation)) +
  geom_point(colour = "red", size = 2) +
  geom_line(colour = "red") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) +  
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

df5$type<-"model1_1"
df6$type<-"model1_ME_1"
df7<-rbind(df5,df6)

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS6_1.tiff",
     res=600,width=13,height=11,units="cm",family="Times") 
ggplot(df7,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
dev.off()

df8<-data.frame(cbind(distance=as.vector(correlog_model2_1$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model2_1$correlation[1:31]),
                      p=as.vector(correlog_model2_1$p[1:31])))

tiff("C:/Users/Ali/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS7_1.tiff",
     res=600,width=13,height=11,units="cm",family="Times") 
ggplot(df8,aes(x=distance, y=correlation)) +
  geom_point(size=2) +
  geom_line() +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.15,0.25),breaks = c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.20,0.25))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
dev.off()

