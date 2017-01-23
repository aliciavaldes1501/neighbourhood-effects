#####################################################################################
########################### Preparing data for analysis #############################
#####################################################################################

library(dplyr)
library(foreign)

#Join data for marked plants on visits 1, 2, 3
mplants1<-read.table("./data/raw/tanga2016_plants1.txt",header=T,sep="\t",dec=".")
mplants2<-read.table("./data/raw/tanga2016_plants2.txt",header=T,sep="\t",dec=".")
mplants3<-read.table("./data/raw/tanga2016_plants3.txt",header=T,sep="\t",dec=".")

mplants1$plot_id<-as.factor(mplants1$plot_id)
mplants2$comments2<-as.character(mplants2$comments2)
mplants2$new_mark<-as.factor(mplants2$new_mark)
mplants2$plot_id<-as.factor(mplants2$plot_id)
mplants3$comments3<-as.character(mplants3$comments3)

head(mplants1)
tail(mplants1)
head(mplants2)
tail(mplants2)
head(mplants3)
tail(mplants3)

str(mplants1) #621 plants
str(mplants2) #708 plants
str(mplants3) #715 plants

#Join data visit 1 and data from already marked plants on visit 2
head(subset(mplants2,new_mark==0)[1:7]) #Data from already marked plants on visit 2
str(subset(mplants2,new_mark==0)[1:7]) #614 plants

mplants<-merge(mplants1,subset(mplants2,new_mark==0)[1:7],all.x=T,all.y=T)
head(mplants)
str(mplants) #621
subset(mplants,is.na(new_mark)) #7 plants marked on visit 1 that had no data on visit 2

mplants[c("new_mark")][is.na(mplants[c("new_mark")])] <- 0
mplants[c("comments2")][is.na(mplants[c("comments2")])] <- "Not included in mplants2. This comment was manually typed in R"

subset(mplants,is.na(n_fl))
head(mplants)
str(mplants)

#Join data from newly marked plants on visit 2

head(subset(mplants2,new_mark==1)) #Data from newly marked plants on visit 2
str(subset(mplants2,new_mark==1)) # 94 plants

mplants<-merge(mplants,subset(mplants2,new_mark==1),all.x=T,all.y=T)
head(mplants)
str(mplants) # 715 plants

#Join data visit 3
mplants<-merge(mplants,mplants3,all.x=T,all.y=T)
head(mplants)
str(mplants) # 715 plants

#Reorder variables
names(mplants)
mplants <- select(mplants, 
                  pl_id,plot_id,n_shoots,n_fl,shoot_h,phen,n_eggs_1,n_eggs_2,veg_h_1,veg_h_2,
                  bud_in,bud_pr,bud_ab,fl_in,fl_pr,fl_ab,fr_in,fr_inC,fr_pr,fr_prC,fr_ab,
                  new_mark,state_after_2,state_after_3,comments2,comments3) 
head(mplants)
write.table(mplants, "./data/clean/mplants.txt", sep="\t",dec=".",row.names=F)

#Save as .dbf to use in ArcGIS
#Change NA values to -9999
mplants_dbf<-mplants #USE ONLY FOR CONVERTING TO DBF!
mplants_dbf[is.na(mplants_dbf[3:21])]<--9999

mplants_dbf[, 3:5][is.na(mplants_dbf[, 3:5])] <- -9999
mplants_dbf[, 7:21][is.na(mplants_dbf[, 7:21])] <- -9999

write.dbf(mplants_dbf, "./gis/tables/mplants.dbf", factor2char = TRUE, max_nchar = 250)

