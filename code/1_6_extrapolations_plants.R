#Extrapolate data temperature, moisture, ants to every plant
#Read table exported from GIS with data on all plants (marked or not)
allplants<-read.table("D:/SU/projects/neighbourhood_effects/gis/tables/plants_wmdata.txt",header=T,sep="\t",dec=".")
head(allplants)
str(allplants)

#Prepare data
allplants$idMark<-NULL
allplants$date_map<-NULL
allplants$OID_<-NULL
allplants$plot_id<-NULL
allplants$phen_1<-NULL
allplants$eggs<-ifelse(allplants$pl_id>0,NA,allplants$eggs)
allplants$pl_id<-ifelse(allplants$pl_id>0,allplants$pl_id,NA)
allplants$n_shoots<-ifelse(is.na(allplants$pl_id),NA,allplants$n_shoots)
allplants$n_fl<-ifelse(is.na(allplants$pl_id),NA,allplants$n_fl)
allplants$shoot_h<-ifelse(is.na(allplants$pl_id),NA,allplants$shoot_h)
allplants$n_eggs_1<-ifelse(is.na(allplants$pl_id),NA,allplants$n_eggs_1)
allplants$n_eggs_2<-ifelse(is.na(allplants$pl_id),NA,allplants$n_eggs_2)
allplants[allplants == -9999] <- NA
allplants$veg_h_1<-ifelse(is.na(allplants$pl_id),NA,allplants$veg_h_1)
allplants$veg_h_2<-ifelse(is.na(allplants$pl_id),NA,allplants$veg_h_2)
allplants$bud_in<-ifelse(is.na(allplants$pl_id),NA,allplants$bud_in)
allplants$bud_pr<-ifelse(is.na(allplants$pl_id),NA,allplants$bud_pr)
allplants$bud_ab<-ifelse(is.na(allplants$pl_id),NA,allplants$bud_ab)
allplants$fl_in<-ifelse(is.na(allplants$pl_id),NA,allplants$fl_in)
allplants$fl_pr<-ifelse(is.na(allplants$pl_id),NA,allplants$fl_pr)
allplants$fl_ab<-ifelse(is.na(allplants$pl_id),NA,allplants$fl_ab)
allplants$fr_in<-ifelse(is.na(allplants$pl_id),NA,allplants$fr_in)
allplants$fr_inC<-ifelse(is.na(allplants$pl_id),NA,allplants$fr_inC)
allplants$fr_pr<-ifelse(is.na(allplants$pl_id),NA,allplants$fr_pr)
allplants$fr_prC<-ifelse(is.na(allplants$pl_id),NA,allplants$fr_prC)
allplants$fr_ab<-ifelse(is.na(allplants$pl_id),NA,allplants$fr_ab)
allplants$state_afte<-ifelse(is.na(allplants$pl_id),NA,allplants$state_afte)
allplants$state_af_1<-ifelse(is.na(allplants$pl_id),NA,allplants$state_af_1)
allplants$n_eggs_max<-with(allplants,pmax(eggs,n_eggs_1,n_eggs_2,na.rm=T))
allplants$veg_h_mean<-rowMeans(allplants[,10:11],na.rm=T)
allplants$veg_h_mean[is.nan(allplants$veg_h_mean)==TRUE] <- NA

#Defining coordinates and coordinate system####
coordinates(allplants) <- c("x", "y")
proj4string(allplants) = CRS(project1) #assign CRS with projected coordinates
plot(allplants)

head(allplants)
tail(allplants)
str(allplants)

#Extrapolations
allplants$meanT<-over(allplants,krig1_meanT$krige_output)$var1.pred
allplants$maxT<-over(allplants,krig1_maxT$krige_output)$var1.pred
allplants$minT<-over(allplants,krig1_minT$krige_output)$var1.pred
allplants$sdT<-over(allplants,krig1_sdT$krige_output)$var1.pred
allplants$rangeT<-over(allplants,krig1_rangeT$krige_output)$var1.pred
allplants$meanTday<-over(allplants,krig1_meanTday$krige_output)$var1.pred
allplants$maxTday<-over(allplants,krig1_maxTday$krige_output)$var1.pred
allplants$minTday<-over(allplants,krig1_minTday$krige_output)$var1.pred
allplants$sdTday<-over(allplants,krig1_sdTday$krige_output)$var1.pred
allplants$rangeTday<-over(allplants,krig1_rangeTday$krige_output)$var1.pred
allplants$moist_per<-over(allplants,krig1_MOIST_PER$krige_output)$var1.pred
allplants$moist_mv<-over(allplants,krig1_MOIST_MV$krige_output)$var1.pred

allplants$Mrub_sum<-over(allplants,idw1_Mrub_sum)$var1.pred
allplants$Msca_sum<-over(allplants,idw1_Msca_sum)$var1.pred
allplants$Mrug_sum<-over(allplants,idw1_Mrug_sum)$var1.pred
allplants$Msch_sum<-over(allplants,idw1_Msch_sum)$var1.pred
allplants$oth_sum<-over(allplants,idw1_oth_sum)$var1.pred
allplants$Mrub_max<-over(allplants,idw1_Mrub_max)$var1.pred
allplants$Msca_max<-over(allplants,idw1_Msca_max)$var1.pred
allplants$Mrug_max<-over(allplants,idw1_Mrug_max)$var1.pred
allplants$Msch_max<-over(allplants,idw1_Msch_max)$var1.pred
allplants$oth_max<-over(allplants,idw1_oth_max)$var1.pred
allplants$Mrub_pres<-over(allplants,idw1_Mrub_pres)$var1.pred
allplants$Msca_pres<-over(allplants,idw1_Msca_pres)$var1.pred
allplants$Mrug_pres<-over(allplants,idw1_Mrug_pres)$var1.pred
allplants$Msch_pres<-over(allplants,idw1_Msch_pres)$var1.pred
allplants$oth_pres<-over(allplants,idw1_oth_pres)$var1.pred
allplants$allM_sum<-over(allplants,idw1_allM_sum)$var1.pred
allplants$allM_max<-over(allplants,idw1_allM_max)$var1.pred
allplants$allM_pres<-over(allplants,idw1_allM_pres)$var1.pred
allplants$Mrub_rug_s<-over(allplants,idw1_Mrub_rug_s)$var1.pred
allplants$Mrub_rug_m<-over(allplants,idw1_Mrub_rug_m)$var1.pred
allplants$Mrub_rug_p<-over(allplants,idw1_Mrub_rug_p)$var1.pred

#Export results of interpolations as rasters to use in ArcGIS (only two examples exported by now)
gridded(krig1_meanT$krige_output)<-TRUE
gridded(idw1_Mrub_sum)<-TRUE
writeRaster(raster(krig1_meanT$krige_output), "D:/SU/projects/neighbourhood_effects/gis/raster/krig1_meanT", format = "GTiff")
writeRaster(raster(idw1_Mrub_sum), "D:/SU/projects/neighbourhood_effects/gis/raster/idw1_Mrub_sum", format = "GTiff")
# and edit Spatial References in ArcCatalog --> RT90

#Divide marked plants between those that have GIS data and those that have not
mplants_GIS<-subset(allplants,!is.na(pl_id))
mplants_noGIS<-mplants[!mplants$pl_id %in% mplants_GIS$pl_id,]
str(mplants_GIS)
str(mplants_noGIS)

#allplants does not include plants in mplants_noGIS
