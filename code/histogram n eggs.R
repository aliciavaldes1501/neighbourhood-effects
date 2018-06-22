hist1<-ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max==0)),aes(n_eggs_max))+
  geom_histogram(binwidth=0.12,bins=50,color="black",fill="white")+
  ylab("Proportion")+xlab(NULL)+ggtitle("N eggs = 0")+
  scale_x_continuous(breaks=c(0))+
  theme(text=element_text(family="serif"))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.background=element_rect(fill="white", colour=NA))

hist2<-ggplot(as.data.frame(subset(allplants,!is.na(phen)&n_eggs_max>0)),aes(n_eggs_max))+
  geom_histogram(bins=50,color="black",fill="white")+
  ylab(NULL)+xlab(NULL)+ggtitle("N eggs > 0")+
  theme(text=element_text(family="serif"))+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.background=element_rect(fill="white", colour=NA))+
  scale_x_continuous(breaks=c(1,5,10,15,20,25,30,35,40,45,50))

grid.arrange(hist1,hist2,ncol=2,widths=c(1,3),bottom="Number of eggs")
