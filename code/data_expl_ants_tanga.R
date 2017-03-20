#####################################################################################
######################### Data exploration ants Tanga Hed ###########################
#####################################################################################

library(ggplot2)
library(tidyr)
library(reshape2)
library(ggthemes)
library(foreign)

ants_T<-read.table("./data/raw/tanga2016_ants.txt",header=T,sep="\t",dec=".")
head(ants_T)

ants_T_long<-gather(ants_T,species,number,Mrubra,Mscabrinodis,Mruginodis,Mschencki,others,factor_key=TRUE)
head(ants_T_long)

ants_T_long$date_id<-as.factor(ifelse(ants_T_long$date=="03/09/2016","date2","date1"))

ants_T_long_wide <-dcast(ants_T_long, point_id + species ~ date_id, value.var="number",fun.aggregate=sum)
ants_T_long_wide$sumdates<-ants_T_long_wide$date1+ants_T_long_wide$date2
ants_T_long_wide$meandates<-(ants_T_long_wide$date1+ants_T_long_wide$date2)/2
ants_T_long_wide$maxdates<-pmax(ants_T_long_wide$date1,ants_T_long_wide$date2)
ants_T_long_wide$pres<-ifelse(ants_T_long_wide$sumdates>0,1,0)
head(ants_T_long_wide)

cor(ants_T_long_wide[5:7])

#Write table to use in GIS
ants_T_GIS_sum <-dcast(ants_T_long_wide, point_id ~ species, value.var="sumdates",fun.aggregate=sum)
ants_T_GIS_max <-dcast(ants_T_long_wide, point_id ~ species, value.var="maxdates",fun.aggregate=sum)
ants_T_GIS_pres <-dcast(ants_T_long_wide, point_id ~ species, value.var="pres",fun.aggregate=sum)
names(ants_T_GIS_sum)<-c("point_id","Mrub_sum","Msca_sum","Mrug_sum","Msch_sum","oth_sum")
names(ants_T_GIS_max)<-c("point_id","Mrub_max","Msca_max","Mrug_max","Msch_max","oth_max")
names(ants_T_GIS_pres)<-c("point_id","Mrub_pres","Msca_pres","Mrug_pres","Msch_pres","oth_pres")
head(ants_T_GIS_sum)
head(ants_T_GIS_max)
head(ants_T_GIS_pres)
ants_T_GIS<-merge(ants_T_GIS_sum,ants_T_GIS_max,by="point_id")
ants_T_GIS<-merge(ants_T_GIS,ants_T_GIS_pres,by="point_id")
ants_T_GIS$allM_sum<-ants_T_GIS$Mrub_sum+ants_T_GIS$Msca_sum+ants_T_GIS$Mrug_sum+ants_T_GIS$Msch_sum
ants_T_GIS$allM_max<-ants_T_GIS$Mrub_max+ants_T_GIS$Msca_max+ants_T_GIS$Mrug_max+ants_T_GIS$Msch_max
ants_T_GIS$allM_pres<-ifelse(ants_T_GIS$allM_sum>0,1,0)
ants_T_GIS$Mrub_rug_sum<-ants_T_GIS$Mrub_sum+ants_T_GIS$Mrug_sum
ants_T_GIS$Mrub_rug_max<-ants_T_GIS$Mrub_max+ants_T_GIS$Mrug_max
ants_T_GIS$Mrub_rug_pres<-ifelse(ants_T_GIS$Mrub_rug_sum>0,1,0)
head(ants_T_GIS)
write.dbf(ants_T_GIS, "./gis/tables/ants_T_GIS.dbf")


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

