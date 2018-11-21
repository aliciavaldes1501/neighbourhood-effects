#Load packages
library(MASS)
library(fmsb)
library(spdep)
library(rasterVis)
library(ncf)
library(maptools)
library(gstat)
library(ggplot2)
library(ggthemes)
library(effects)
library(cowplot)
library(car)
library(beepr)

#Read data ants
data_ants_paper<-read.table("C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/data/clean/data_ants_paper.txt",header=T,sep="\t",dec=".")
head(data_ants_paper)
str(data_ants_paper)

#Defining coordinates and coordinate system
coordinates(data_ants_paper) <- c("x", "y")
project1<-"+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #RT90 0 gon (spatialreference.org)
proj4string(data_ants_paper) = CRS(project1) #assign CRS with projected coordinates

#Preparing a prediction grid
plot(data_ants_paper)
locator(4)

min_x =1499923 #minimun x coordinate
min_y =-51.043612 #minimun y coordinate
x_length = 1499995 - min_x #easting amplitude
y_length = -2.330070 - min_y #northing amplitude
cellsize = 0.18 #pixel size
ncol = round(x_length/cellsize,0) #number of columns in grid
nrow = round(y_length/cellsize,0) #number of rows in grid

grd1<-GridTopology(cellcentre.offset=c(min_x,min_y),cellsize=c(cellsize,cellsize),cells.dim=c(ncol,nrow))

#Convert GridTopolgy object to SpatialPixelsDataFrame object.
grd1<-SpatialPixelsDataFrame(grd1,
                             data=data.frame(id=1:prod(ncol,nrow)),
                             proj4string=CRS(project1))
plot(grd1)

#Cut grid with shapefile "mask" (20 cm out of the borders of the occupied plots)
mask<-readShapePoly("C:/Users/User/Dropbox/SU/projects/neighbourhood_effects/gis/shapefiles/mask_20cm.shp",IDvar=NULL,proj4string=CRS(project1))
plot(mask)
mask<-mask@polygons
mask<-SpatialPolygons(mask, proj4string=CRS(project1))
plot(mask)
grd1<-grd1[!is.na(over(grd1, mask)),]
plot(grd1)

#Inverse distance weighting (IDW) interpolation based on the values at the sampling points 
#to generate values of the abundance/presence of ant species over the surface of all occupied subplots. 
#A maximum distance of 3 m was used because it represents the ground foraging distance 
#of most investigated Myrmica species (Elmes et al. 1998).
#From these interpolated surfaces (see electronic supplementary material figures S1-5), 
#we extracted values of abundance of the different ant species for each of the mapped shoots. 

idw1_Mrub_sum<-idw(Mrub_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msca_sum<-idw(Msca_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrug_sum<-idw(Mrug_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msch_sum<-idw(Msch_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_oth_sum<-idw(oth_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)

data_ants_paper$allM_sum<-data_ants_paper$Mrub_sum+data_ants_paper$Msca_sum+
  data_ants_paper$Mrug_sum+data_ants_paper$Msch_sum
data_ants_paper$Mrub_rug_sum<-data_ants_paper$Mrub_sum+data_ants_paper$Mrug_sum
data_ants_paper$Mrub_sch_sum<-data_ants_paper$Mrub_sum+data_ants_paper$Msch_sum
data_ants_paper$Mrub_sch_rug_sum<-data_ants_paper$Mrub_sum+data_ants_paper$Msch_sum+
  data_ants_paper$Mrug_sum

idw1_allM_sum<-idw(allM_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_rug_sum<-idw(Mrub_rug_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_sch_sum<-idw(Mrub_sch_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_sch_rug_sum<-idw(Mrub_sch_rug_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)

data_ants_paper$allM_pres<-ifelse(data_ants_paper$Mrub_sum>0|data_ants_paper$Msca_sum>0|
                                    data_ants_paper$Mrug_sum>0|data_ants_paper$Msch_sum>0,
                                  1,0) #Presence of any Myrmica
data_ants_paper$Mrub_pres<-ifelse(data_ants_paper$Mrub_sum>0,1,0)
data_ants_paper$Msca_pres<-ifelse(data_ants_paper$Msca_sum>0,1,0)
data_ants_paper$Mrug_pres<-ifelse(data_ants_paper$Mrug_sum>0,1,0)
data_ants_paper$Msch_pres<-ifelse(data_ants_paper$Msch_sum>0,1,0)
data_ants_paper$oth_pres<-ifelse(data_ants_paper$oth_sum>0,1,0)

data_ants_paper$Mrub_rug_pres<-ifelse(data_ants_paper$Mrub_sum>0|data_ants_paper$Mrug_sum>0,1,0) #Presence of Mrub OR Mrug
data_ants_paper$Mrub_sch_pres<-ifelse(data_ants_paper$Mrub_sum>0|data_ants_paper$Msch_sum>0,1,0) #Presence of Mrub OR Msch
data_ants_paper$Mrub_sch_rug_pres<-ifelse(data_ants_paper$Mrub_sum>0|data_ants_paper$Msch_sum|data_ants_paper$Mrug_sum>0,1,0) #Presence of Mrub OR Msch OR Mrug

idw1_allM_pres<-idw(allM_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_pres<-idw(Mrub_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msca_pres<-idw(Msca_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrug_pres<-idw(Mrug_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msch_pres<-idw(Msch_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_oth_pres<-idw(oth_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_rug_pres<-idw(Mrub_rug_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_sch_pres<-idw(Mrub_sch_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrub_sch_rug_pres<-idw(Mrub_sch_rug_pres ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)

#Figures inverse distance weighted interpolation for ant abundances (Figures S1-5)
levelplot(raster(idw1_Mrub_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msca_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrug_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msch_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_oth_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_allM_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrub_rug_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrub_sch_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrub_sch_rug_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))

levelplot(raster(idw1_Mrub_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msca_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrug_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msch_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_oth_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_allM_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE)) #Presence of any Myrmica
levelplot(raster(idw1_Mrub_rug_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE)) #Presence of Mrub OR Mrug
levelplot(raster(idw1_Mrub_sch_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE)) #Presence of Mrub OR Msch
levelplot(raster(idw1_Mrub_sch_rug_pres), par.settings=myTheme, margin=F,scales=list(draw=FALSE)) #Presence of Mrub OR Msch OR Mrug

#Read data shoot phenology, neighbor density and phenology
data_plants_paper<-read.table("C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/data/clean/data_plants_paper.txt",header=T,sep="\t",dec=".")
head(data_plants_paper)
str(data_plants_paper)

#Defining coordinates and coordinate system
coordinates(data_plants_paper) <- c("x", "y")
proj4string(data_plants_paper) = CRS(project1) #assign CRS with projected coordinates
plot(data_plants_paper)

head(data_plants_paper)
tail(data_plants_paper)
str(data_plants_paper)

#Extrapolate ant data to every plant
data_plants_paper$Mrub_sum<-over(data_plants_paper,idw1_Mrub_sum)$var1.pred
data_plants_paper$Msca_sum<-over(data_plants_paper,idw1_Msca_sum)$var1.pred
data_plants_paper$Mrug_sum<-over(data_plants_paper,idw1_Mrug_sum)$var1.pred
data_plants_paper$Msch_sum<-over(data_plants_paper,idw1_Msch_sum)$var1.pred
data_plants_paper$oth_sum<-over(data_plants_paper,idw1_oth_sum)$var1.pred
data_plants_paper$allM_sum<-over(data_plants_paper,idw1_allM_sum)$var1.pred
data_plants_paper$Mrub_rug_sum<-over(data_plants_paper,idw1_Mrub_rug_sum)$var1.pred
data_plants_paper$Mrub_sch_sum<-over(data_plants_paper,idw1_Mrub_sch_sum)$var1.pred
data_plants_paper$Mrub_sch_rug_sum<-over(data_plants_paper,idw1_Mrub_sch_rug_sum)$var1.pred
data_plants_paper$Mrub_pres<-over(data_plants_paper,idw1_Mrub_pres)$var1.pred
data_plants_paper$Msca_pres<-over(data_plants_paper,idw1_Msca_pres)$var1.pred
data_plants_paper$Mrug_pres<-over(data_plants_paper,idw1_Mrug_pres)$var1.pred
data_plants_paper$Msch_pres<-over(data_plants_paper,idw1_Msch_pres)$var1.pred
data_plants_paper$oth_pres<-over(data_plants_paper,idw1_oth_pres)$var1.pred
data_plants_paper$allM_pres<-over(data_plants_paper,idw1_allM_pres)$var1.pred
data_plants_paper$Mrub_rug_pres<-over(data_plants_paper,idw1_Mrub_rug_pres)$var1.pred
data_plants_paper$Mrub_sch_pres<-over(data_plants_paper,idw1_Mrub_sch_pres)$var1.pred
data_plants_paper$Mrub_sch_rug_pres<-over(data_plants_paper,idw1_Mrub_sch_rug_pres)$var1.pred

dev.off()
par(mfrow=c(3,2))
hist(data_plants_paper$Mrub_sum)
hist(data_plants_paper$Mrub_rug_sum)
hist(data_plants_paper$allM_sum)
hist(data_plants_paper$Mrub_pres)
hist(data_plants_paper$Mrub_rug_pres)
hist(data_plants_paper$allM_pres)
par(mfrow=c(1,1))
#Mrub_pres,Mrug_pres,Msch_pres are PROBABILITIES OF PRESENCE

head(data_plants_paper)
str(data_plants_paper)

#Calculation of the probability  of a shoot having at least one egg  (0/1)
data_plants_paper$attack<-ifelse(data_plants_paper$n_eggs_max>0,1,0)

#Univariate GLMs relating the probability of having eggs to the abundance/presence of different ant species (Table S1A)
summary(glm(attack ~scale(Mrub_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #0.17423
summary(glm(attack ~scale(Mrug_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-0.21499
summary(glm(attack ~scale(Mrub_rug_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-0.10148

#Univariate GLMs relating the number of eggs in plants with at least one egg to the abundance/presence of different ant species (Table S1B)
summary(glm.nb(n_eggs_max~scale(Mrub_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #0.13664
summary(glm.nb(n_eggs_max~scale(Mrug_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(Mrub_rug_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #NS

#Binomial GLM relating the probability of a shoot of G. pneumonanthe having eggs of P. alcon to shoot phenology, ant abundance, 
#neighbor density and neighbor phenology, and to three different two-way interactions of these predictors (Table S3)

# With Mrub_sum
model1<-glm(attack ~ scale(phen) + scale(Mrub_sum) + scale(pldens_3) +
              scale(phen_n3) + scale(phen):scale(Mrub_sum)+
              scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
            subset(data_plants_paper,!is.na(phen)),family="binomial") 
summary(model1)
NagelkerkeR2(model1)

# With other species
# With Mrug_sum
model2<-glm(attack ~ scale(phen) + scale(Mrug_sum) + scale(pldens_3) +
              scale(phen_n3) + scale(phen):scale(Mrug_sum)+
              scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
            subset(data_plants_paper,!is.na(phen)),family="binomial") # Interaction NS
summary(model2)
NagelkerkeR2(model2)

# With Mrub_rug_sum
model3<-glm(attack ~ scale(phen) + scale(Mrub_rug_sum) + scale(pldens_3) +
              scale(phen_n3) + scale(phen):scale(Mrub_rug_sum)+
              scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
            subset(data_plants_paper,!is.na(phen)),family="binomial") # Interaction NS
summary(model3)
NagelkerkeR2(model3) 

#Checking for spatial autocorrelation in the residuals of model1
res_model1<-residuals(model1) #Get residuals of model

#Spatial correlogram model 1
correlog_model1_1 <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                              subset(data_plants_paper,!is.na(phen))$y,
                              res_model1,increment=1, resamp=100) 

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

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model 1, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
# data_plants_paper.nb1 <- dnearneigh(subset(data_plants_paper,!is.na(phen)), 0, 30) #Create neighbours matrix (30 m)
# data_plants_paper.listw1 <- nb2listw(data_plants_paper.nb1) 

load(file="allplants.listw1.R")  
load(file="allplants.listw2.R")  

data_plants_paper.listw1<-allplants.listw1
data_plants_paper.listw2<-allplants.listw2

moran_model1<- moran.test(res_model1, listw=data_plants_paper.listw1) 
moran_model1 #Significant autocorrelation in the residuals of model1

#Moran's eigenvector mapping of model1
ME.model1 <-ME(attack ~ scale(phen) + scale(Mrub_sum) + scale(pldens_3)+scale(phen_n3) + scale(phen):scale(Mrub_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),listw=data_plants_paper.listw1,
               data=subset(data_plants_paper,!is.na(phen)),family=binomial,alpha=0.05,verbose=T) 

#Repeat model1 with Moran's eigenvectors as predictors (Table 1A)
vector1<-ME.model1$vectors[,1]
vector2<-ME.model1$vectors[,2]
model1_MEa<-glm(attack ~ scale(phen) + scale(Mrub_sum) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen):scale(Mrub_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
                 scale(vector1)+scale(vector2),
               subset(data_plants_paper,!is.na(phen)),family="binomial")
summary(model1_MEa) # Interaction NS, moran OK
NagelkerkeR2(model1_MEa)

#Checking for spatial autocorrelation in the residuals of model1_MEa
res_model1_MEa<-residuals(model1_MEa) #Get residuals of model

#Spatial correlogram model1_ME
correlog_model1_MEa <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                               subset(data_plants_paper,!is.na(phen))$y,
                               res_model1_MEa,increment=1, resamp=100) 

df6<-data.frame(cbind(distance=as.vector(correlog_model1_MEa$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model1_MEa$correlation[1:31]),
                      p=as.vector(correlog_model1_MEa$p[1:31])))

df5$type<-"model1_1"
df6$type<-"model1_MEa_1"
df7<-rbind(df5,df6)

#Figure S6
ggplot(df7,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS6_1.tiff",
       device="tiff",width=12,height=10,units="cm",dpi=600)

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model1_ME, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model1_MEa<- moran.test(res_model1_MEa, listw=data_plants_paper.listw1) 
moran_model1_MEa #Significant autocorrelation in the residuals of model1_MEa, but much reduced p-value = 0.003691

#Checking for spatial autocorrelation in the residuals of model2
res_model2<-residuals(model2) #Get residuals of model

#Spatial correlogram model 2
correlog_model2_1 <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                              subset(data_plants_paper,!is.na(phen))$y,
                              res_model2,increment=1, resamp=100) 

df8<-data.frame(cbind(distance=as.vector(correlog_model2_1$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model2_1$correlation[1:31]),
                      p=as.vector(correlog_model2_1$p[1:31])))
ggplot(data=df8,aes(x=distance, y=correlation)) +
  geom_point(colour = "black", size = 2) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model 2, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model2<- moran.test(res_model2, listw=data_plants_paper.listw1) 
moran_model2 #Significant autocorrelation in the residuals of model2

#Moran's eigenvector mapping of model2
ME.model2 <-ME(attack ~ scale(phen) + scale(Mrug_sum) + scale(pldens_3)+scale(phen_n3) + scale(phen):scale(Mrug_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),listw=data_plants_paper.listw1,
               data=subset(data_plants_paper,!is.na(phen)),family=binomial,alpha=0.05,stdev=T,verbose=T) 

#Repeat model2 with Moran's eigenvectors as predictors (Table S2)
vector3<-ME.model2$vectors[,1]
vector4<-ME.model2$vectors[,2]
model2_ME<-glm(attack ~ scale(phen) + scale(Mrug_sum) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen):scale(Mrug_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
                 scale(vector1)+scale(vector2),
               subset(data_plants_paper,!is.na(phen)),family="binomial") 
summary(model2_ME) 
NagelkerkeR2(model2_ME)

#Checking for spatial autocorrelation in the residuals of model2_ME
res_model2_ME<-residuals(model2_ME) #Get residuals of model

#Spatial correlogram model2_ME
correlog_model2_ME <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                               subset(data_plants_paper,!is.na(phen))$y,
                               res_model2_ME,increment=1, resamp=100) 

df9<-data.frame(cbind(distance=as.vector(correlog_model2_ME$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model2_ME$correlation[1:31]),
                      p=as.vector(correlog_model2_ME$p[1:31])))

df8$type<-"model2_1"
df9$type<-"model2_ME_1"
df10<-rbind(df8,df9)

#Correlogram
ggplot(df10,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model2_ME, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model2_ME<- moran.test(res_model2_ME, listw=data_plants_paper.listw1) 
moran_model2_ME

#Checking for spatial autocorrelation in the residuals of model3
res_model3<-residuals(model3) #Get residuals of model

#Spatial correlogram model 3
correlog_model3_1 <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                              subset(data_plants_paper,!is.na(phen))$y,
                              res_model3,increment=1, resamp=100) 

df11<-data.frame(cbind(distance=as.vector(correlog_model3_1$mean.of.class[1:31]), 
                       correlation=as.vector(correlog_model3_1$correlation[1:31]),
                       p=as.vector(correlog_model2_1$p[1:31])))
ggplot(data=df11,aes(x=distance, y=correlation)) +
  geom_point(colour = "black", size = 2) +
  geom_line(colour = "black") +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model 3, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model3<- moran.test(res_model3, listw=data_plants_paper.listw1) 
moran_model3 #Significant autocorrelation in the residuals of model3

#Moran's eigenvector mapping of model3
ME.model3 <-ME(attack ~ scale(phen) + scale(Mrub_rug_sum) + scale(pldens_3)+scale(phen_n3) + scale(phen):scale(Mrub_rug_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),listw=data_plants_paper.listw1,
               data=subset(data_plants_paper,!is.na(phen)),family=binomial,alpha=0.05,stdev=T,verbose=T) 

#Repeat model3 with Moran's eigenvectors as predictors (Table S2)
vector5<-ME.model3$vectors[,1]
vector6<-ME.model3$vectors[,2]
model3_ME<-glm(attack ~ scale(phen) + scale(Mrub_rug_sum) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen):scale(Mrub_rug_sum)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
                 scale(vector5)+scale(vector6),
               subset(data_plants_paper,!is.na(phen)),family="binomial")  
summary(model3_ME) 
NagelkerkeR2(model3_ME)

#Checking for spatial autocorrelation in the residuals of model3_ME
res_model3_ME<-residuals(model3_ME) #Get residuals of model

#Spatial correlogram model3_ME
correlog_model3_ME <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                               subset(data_plants_paper,!is.na(phen))$y,
                               res_model3_ME,increment=1, resamp=100) 

df12<-data.frame(cbind(distance=as.vector(correlog_model3_ME$mean.of.class[1:31]), 
                       correlation=as.vector(correlog_model3_ME$correlation[1:31]),
                       p=as.vector(correlog_model3_ME$p[1:31])))

df11$type<-"model3_1"
df12$type<-"model3_ME_1"
df13<-rbind(df11,df12)

#Correlogram
ggplot(df13,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))

#Calculation of global Moran's I with a permutation test (1000 random permutations) for model3_ME, 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model3_ME<- moran.test(res_model3_ME, listw=data_plants_paper.listw1) 
moran_model3_ME

#Negative binomial GLM relating the number of eggs in shoots with at least one egg to shoot phenology, ant abundance, 
#neighbor density and neighbor phenology, and to three different two-way interactions of these predictors (Table 1B)
model4<-glm.nb(n_eggs_max ~ scale(phen) + scale(Mrub_sum) + scale(pldens_3) + 
                 scale(phen_n3) + scale(phen):scale(Mrub_sum) + 
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
               subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)) 
summary(model4) 
NagelkerkeR2(model4)

#Checking for spatial autocorrelation in the residuals of model4
res_model4<-residuals(model4) #Get residuals of model

#Spatial correlogram
correlog_model4 <- correlog(subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)$x,
                            subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)$y,
                            res_model4,increment=1, resamp=100) 

df14<-data.frame(cbind(distance=as.vector(correlog_model4$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model4$correlation[1:31]),
                      p=as.vector(correlog_model4$p[1:31])))

#Figure S7
ggplot(df14,aes(x=distance, y=correlation)) +
  geom_point(size=2) +
  geom_line() +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.15,0.25),breaks = c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.20,0.25))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))
ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/figS7_1.tiff",
       device="tiff",width=12,height=10,units="cm",dpi=600)

#Calculation of global Moran's I with a permutation test (1000 random permutations), 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
# data_plants_paper.nb2 <- dnearneigh(subset(data_plants_paper,!is.na(phen)&n_eggs_max>0), 0, 30) #Create neighbours matrix (30 m)
# data_plants_paper.listw2 <- nb2listw(data_plants_paper.nb2) 

moran_model4<- moran.test(res_model4, listw=data_plants_paper.listw2)
moran_model4 #NO Significant autocorrelation!

# With other species
# With Mrug_sum
model5<-glm.nb(n_eggs_max ~ scale(phen) + scale(Mrug_sum) + scale(pldens_3) + 
                 scale(phen_n3) + scale(phen):scale(Mrug_sum) + 
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
               subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)) 
summary(model5) 
NagelkerkeR2(model5)

res_model5<-residuals(model5) #Get residuals of model
moran_model5<- moran.test(res_model5, listw=data_plants_paper.listw2)
moran_model5 #NO Significant autocorrelation!

# With Mrug_rub_sum
model6<-glm.nb(n_eggs_max ~ scale(phen) + scale(Mrub_rug_sum) + scale(pldens_3) + 
                 scale(phen_n3) + scale(phen):scale(Mrub_rug_sum) + 
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
               subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)) 
summary(model6) 
NagelkerkeR2(model6)

res_model6<-residuals(model6) #Get residuals of model
moran_model6<- moran.test(res_model6, listw=data_plants_paper.listw2)
moran_model6 #NO Significant autocorrelation!

#Figures paper
interaction1<-data.frame(effect(term="scale(phen):scale(Mrub_sum)", mod=model1_MEa,
                                xlevels=list(Mrub_sum=seq(0,34,1), phen=1:6)))
interaction2<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model1_MEa,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))
effect1<-data.frame(effect(term="scale(phen)", mod=model4,xlevels=list(phen=seq(1,6,0.01))))
effect2<-data.frame(effect(term="scale(Mrub_sum)", mod=model4,xlevels=list(Mrub_sum=seq(0,34,1))))
interaction3<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model4,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))

p1<-ggplot(interaction1, aes(phen,fit, group = as.factor(Mrub_sum)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(phen,fit,color=Mrub_sum))+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+theme_base()+scale_colour_distiller(type = "seq", palette=8,direction = 1)+
  theme(legend.position="top")+labs(colour="Ant abundance")+scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p2<-ggplot(interaction2, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+scale_colour_distiller(type = "seq", palette=8,direction = 1)+
  theme(legend.position="none")+labs(colour="Neighbor phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p3<-ggplot(effect1, aes(phen,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(phen,fit))+
  xlab("Shoot phenology")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+scale_y_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p4<-ggplot(effect2, aes(Mrub_sum,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(Mrub_sum,fit))+
  xlab("Ant abundance")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35))+
  scale_y_continuous(breaks=c(0,5,10,15,20,25,30))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p5<-ggplot(interaction3, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Number of eggs")+
  theme_base()+scale_colour_distiller(type = "seq", palette=8,direction = 1)+
  theme(legend.position="top")+labs(colour="Neigh. phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

#Figure 2
ggdraw()+
  draw_plot(p1,0.02,0.01,0.32,1)+
  draw_plot(p3,1/3+0.02,0.01,0.32,0.78)+
  draw_plot(p4,2/3+0.015,0.01,0.32,0.78)+
  draw_label(label="A)",x=0.015,y=0.81, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.37,y=0.81, fontfamily = "serif", fontface = 1)+
  draw_label(label="C)",x=0.70,y=0.81, fontfamily = "serif", fontface = 1)
ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig2.tiff",
       device="tiff",width=26,height=10,units="cm",dpi=600)

#Figure 3
ggdraw()+
  draw_plot(p2,0.03,0,0.45,0.79)+
  draw_plot(p5,1/2,0,0.45,1)+
  draw_label(label="A)",x=0.04,y=0.81, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.52,y=0.81, fontfamily = "serif", fontface = 1)
ggsave(filename="C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/results/butterfly/figures/fig3.tiff",
       device="tiff",width=20,height=10,units="cm",dpi=600)


interaction4<-data.frame(effect(term="scale(phen):scale(Mrub_sum)", mod=model4,
                                xlevels=list(Mrub_sum=seq(0,34,1),phen=1:6)))
ggplot(interaction4, aes(phen,fit, group = as.factor(Mrub_sum)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(phen,fit,color=Mrub_sum))+
  xlab("Shoot phenology")+ylab("Number of eggs")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Ant abundance")+scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

