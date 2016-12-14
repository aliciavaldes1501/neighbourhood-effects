#####################################################################################
######################### Data exploration ants Tanga Hed ###########################
#####################################################################################

library(ggplot2)
library(tidyr)
library(reshape2)
library(ggthemes)

ants_T<-read.table("./data/raw/tanga2016_ants.txt",header=T,sep="\t",dec=".")
head(ants_T)

ants_T_long<-gather(ants_T,species,number,Mrubra,Mscabrinodis,Mruginodis,Mschencki,others,factor_key=TRUE)
ants_T_long$point_id<-as.factor(ants_T_long$point_id)
head(ants_T_long)

ants_T_long$date_id<-as.factor(ifelse(ants_T_long$date=="03/09/2016","date2","date1"))

ants_T_long_wide <-dcast(ants_T_long, point_id + species ~ date_id, value.var="number",fun.aggregate=sum)
ants_T_long_wide$sumdates<-ants_T_long_wide$date1+ants_T_long_wide$date2
ants_T_long_wide$pres<-ifelse(ants_T_long_wide$sumdates>0,1,0)
head(ants_T_long_wide)


pdf("./results/figures/ant_counts_T.pdf", family="Times")
#total number of individuals of each species
ggplot(ants_T_long,aes(x=species,y=number))+geom_bar(stat="identity")+guides(fill=FALSE)+
  xlab("Species")+ylab("Total number of individuals")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))
#number of points with presence of each species
ggplot(ants_T_long_wide,aes(x=species,y=pres))+geom_bar(stat="identity")+guides(fill=FALSE)+
  xlab("Species")+ylab("Number of points with presence")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))
dev.off()

