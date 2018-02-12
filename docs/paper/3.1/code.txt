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


#Inverse distance weighting (IDW) interpolation based on the values at the sampling points to generate values of the abundance of ant species 
#over the surface of all occupied subplots. 
#A maximum distance of 3 m was used because it represents the ground foraging distance of most investigated Myrmica species (Elmes et al. 1998).
#From these interpolated surfaces (see electronic supplementary material figures S1-5), we extracted values of abundance of the different ant 
#species for each of the mapped shoots. 

idw1_Mrub_sum<-idw(Mrub_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msca_sum<-idw(Msca_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Mrug_sum<-idw(Mrug_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_Msch_sum<-idw(Msch_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)
idw1_oth_sum<-idw(oth_sum ~ 1, data_ants_paper, grd1, idp = 1, maxdist=3)

#Figures inverse distance weighted interpolation for ant abundances (Figures S1-5)
myTheme=rasterTheme(region=c("#B3B3B3",brewer.pal('RdBu', n=11)))
levelplot(raster(idw1_Mrub_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msca_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Mrug_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_Msch_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))
levelplot(raster(idw1_oth_sum), par.settings=myTheme, margin=F,scales=list(draw=FALSE))

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

head(data_plants_paper)
str(data_plants_paper)

#Summed abundance of all Myrmica species
data_plants_paper$allM_sum<-data_plants_paper$Mrub_sum+data_plants_paper$Msca_sum+data_plants_paper$Mrug_sum+data_plants_paper$Msch_sum

#Summed abundance of M. rubra and M. schencki 
data_plants_paper$Mrub_sch_s<-data_plants_paper$Mrub_sum+data_plants_paper$Msch_sum

#Calculation of the probability  of a shoot having at least one egg  (0/1)
data_plants_paper$attack<-ifelse(data_plants_paper$n_eggs_max>0,1,0)

#Univariate GLMs relating the probability of having eggs to the abundance of different ant species (Table S1A)
summary(glm(attack ~scale(allM_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-0.52232
summary(glm(attack ~scale(Mrub_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #0.17423
summary(glm(attack ~scale(Msca_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-1.36392
summary(glm(attack ~scale(Mrug_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-0.21499
summary(glm(attack ~scale(Msch_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #0.35843
summary(glm(attack ~scale(oth_sum), subset(data_plants_paper,!is.na(phen)),family="binomial")) #-0.40260
summary(glm(attack ~scale(Mrub_sch_s), subset(data_plants_paper,!is.na(phen)),family="binomial")) #0.36009

#Univariate GLMs relating the number of eggs in plants with at least one egg to the abundance of different ant species (Table S1B)
summary(glm.nb(n_eggs_max~scale(allM_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(Mrub_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #0.13664
summary(glm.nb(n_eggs_max~scale(Msca_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #-0.11955
summary(glm.nb(n_eggs_max~scale(Mrug_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(Msch_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #NS
summary(glm.nb(n_eggs_max~scale(oth_sum), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)))  #NS
summary(glm.nb(n_eggs_max~scale(Mrub_sch_s), subset(data_plants_paper,!is.na(phen)&n_eggs_max>0))) #0.11490

#Binomial GLM relating the probability of a shoot of G. pneumonanthe having eggs of P. alcon to shoot phenology, ant abundance, 
#neighbor density and neighbor phenology, and to three different two-way interactions of these predictors (Table S2)
model1<-glm(attack ~ scale(phen) + scale(Mrub_sch_s) + scale(pldens_3) +
              scale(phen_n3) + scale(phen):scale(Mrub_sch_s)+
              scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
            subset(data_plants_paper,!is.na(phen)),family="binomial") 
summary(model1)
NagelkerkeR2(model1)

#Checking for spatial autocorrelation in the residuals of model1
res_model1<-residuals(model1) #Get residuals of model

#Spatial correlogram
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

#Calculation of global Moran's I with a permutation test (1000 random permutations), 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
data_plants_paper.nb1 <- dnearneigh(subset(data_plants_paper,!is.na(phen)), 0, 30) #Create neighbours matrix (30 m)
data_plants_paper.listw1 <- nb2listw(data_plants_paper.nb1) 

moran_model1<- moran.test(res_model1, listw=data_plants_paper.listw1) 
moran_model1 #Significant autocorrelation in the residuals of model1

#Moran's eigenvector mapping of model1
ME.model1 <-ME(attack ~ scale(phen) + scale(Mrub_sch_s) + scale(pldens_3)+scale(phen_n3) + scale(phen):scale(Mrub_sch_s)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),listw=data_plants_paper.listw1,
               data=subset(data_plants_paper,!is.na(phen)),family=binomial,alpha=0.05,verbose=T) 

#Repeat model1 with Moran's eigenvectors as predictors (Table 1A)
vector1<-ME.model1$vectors[,1]
vector2<-ME.model1$vectors[,2]
model1_ME<-glm(attack ~ scale(phen) + scale(Mrub_sch_s) + scale(pldens_3) +
                 scale(phen_n3) + scale(phen):scale(Mrub_sch_s)+
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3)+
                 scale(vector1)+scale(vector2),
               subset(data_plants_paper,!is.na(phen)),family="binomial")
summary(model1_ME) 
NagelkerkeR2(model1_ME)

#Checking for spatial autocorrelation in the residuals of model1_ME
res_model1_ME<-residuals(model1_ME) #Get residuals of model

#Spatial correlogram
correlog_model1_ME <- correlog(subset(data_plants_paper,!is.na(phen))$x,
                               subset(data_plants_paper,!is.na(phen))$y,
                               res_model1_ME,increment=1, resamp=100) 

df6<-data.frame(cbind(distance=as.vector(correlog_model1_ME$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model1_ME$correlation[1:31]),
                      p=as.vector(correlog_model1_ME$p[1:31])))

df5$type<-"model1_1"
df6$type<-"model1_ME_1"
df7<-rbind(df5,df6)

#Figure S8
ggplot(df7,aes(x=distance, y=correlation)) +
  geom_point(aes(colour=type),size=2) +
  geom_line(aes(colour=type)) +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.05,0.3),breaks = c(-0.05,0,0.05,0.1,0.15,0.20,0.25,0.30))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))

#Calculation of global Moran's I with a permutation test (1000 random permutations), 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
moran_model1_ME<- moran.test(res_model1_ME, listw=data_plants_paper.listw1) 
moran_model1_ME

#Negative binomial GLM relating the number of eggs in shoots with at least one egg to shoot phenology, ant abundance, 
#neighbor density and neighbor phenology, and to three different two-way interactions of these predictors (Table 1B)
model2<-glm.nb(n_eggs_max ~ scale(phen) + scale(Mrub_sch_s) + scale(pldens_3) + 
                 scale(phen_n3) + scale(phen):scale(Mrub_sch_s) + 
                 scale(phen):scale(phen_n3) + scale(pldens_3):scale(phen_n3),
               subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)) 
summary(model2) 
NagelkerkeR2(model2)

#Checking for spatial autocorrelation in the residuals of model1_ME
res_model2<-residuals(model2) #Get residuals of model

#Spatial correlogram
correlog_model2 <- correlog(subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)$x,
                            subset(data_plants_paper,!is.na(phen)&n_eggs_max>0)$y,
                            res_model2,increment=1, resamp=100) 

df8<-data.frame(cbind(distance=as.vector(correlog_model2$mean.of.class[1:31]), 
                      correlation=as.vector(correlog_model2$correlation[1:31]),
                      p=as.vector(correlog_model2$p[1:31])))

#Figure S7
ggplot(df8,aes(x=distance, y=correlation)) +
  geom_point(size=2) +
  geom_line() +
  scale_x_continuous("Distance (mean of class)",limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) + 
  scale_y_continuous("Correlation (Moran's I)",limits=c(-0.15,0.25),breaks = c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.20,0.25))+
  geom_hline(aes(yintercept=0), colour="darkgrey")+
  theme_base()+theme(plot.background=element_rect(fill="white", colour=NA))+
  theme(legend.position="none")+theme(text=element_text(family="serif"))

#Calculation of global Moran's I with a permutation test (1000 random permutations), 
#based on a connectivity matrix of pairwise Euclidean distances among the shoots up to a distance of 30 m. 
data_plants_paper.nb2 <- dnearneigh(subset(data_plants_paper,!is.na(phen)&n_eggs_max>0), 0, 30) #Create neighbours matrix (30 m)
data_plants_paper.listw2 <- nb2listw(data_plants_paper.nb2) 

moran_model2<- moran.test(res_model2, listw=data_plants_paper.listw2)
moran_model2 #NO Significant autocorrelation!

#Figures paper
interaction1<-data.frame(effect(term="scale(phen):scale(Mrub_sch_s)", mod=model1_ME,
                                xlevels=list(Mrub_sch_s=seq(0,35,1), phen=1:6)))
interaction2<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model1_ME,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))
effect1<-data.frame(effect(term="scale(phen)", mod=model2,xlevels=list(phen=seq(1,6,0.01))))
effect2<-data.frame(effect(term="scale(Mrub_sch_s)", mod=model2,xlevels=list(Mrub_sch_s=seq(0,35,1))))
interaction3<-data.frame(effect(term="scale(pldens_3):scale(phen_n3)", mod=model2,
                                xlevels=list(phen_n3=seq(2.8,6,0.05),pldens_3=0:50)))

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

p1<-ggplot(interaction1, aes(phen,fit, group = as.factor(Mrub_sch_s)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(phen,fit,color=Mrub_sch_s))+
  xlab("Shoot phenology")+ylab("Probability of having eggs")+theme_base()+scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Ant abundance")+scale_x_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p2<-ggplot(interaction2, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Probability of having eggs")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="none")+labs(colour="Neighbor phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))
p3<-ggplot(effect1, aes(phen,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(phen,fit))+
  xlab("Shoot phenology")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6))+scale_y_continuous(breaks=c(1,2,3,4,5,6))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p4<-ggplot(effect2, aes(Mrub_sch_s,fit))+
  geom_smooth(method=loess,se=T,size=1,color="black",aes(Mrub_sch_s,fit))+
  xlab("Ant abundance")+ylab("Number of eggs")+theme_base()+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5,fill="grey")
p5<-ggplot(interaction3, aes(pldens_3,fit, group = as.factor(phen_n3)))+
  geom_smooth(method=loess,se=F,size=0.3,aes(pldens_3,fit,color=phen_n3))+
  xlab("Neighbor density")+ylab("Number of eggs")+
  theme_base()+ scale_colour_gradientn(colours = myPalette(100))+
  theme(legend.position="top")+labs(colour="Neigh. phenology")+
  theme(text=element_text(family="serif"))+theme(plot.background=element_rect(fill="white", colour=NA))

#Figure 2
ggdraw()+
  draw_plot(p1,0.02,0.01,0.3,1)+
  draw_plot(p3,1/3+0.04,0.01,0.3,0.81)+
  draw_plot(p4,2/3+0.04,0.01,0.3,0.81)+
  draw_label(label="A)",x=0.01,y=0.8, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.39,y=0.8, fontfamily = "serif", fontface = 1)+
  draw_label(label="C)",x=0.73,y=0.8, fontfamily = "serif", fontface = 1)

#Figure 3
ggdraw()+
  draw_plot(p2,0.02,0,0.45,0.83)+
  draw_plot(p5,1/2,0,0.45,1)+
  draw_label(label="A)",x=0.01,y=0.81, fontfamily = "serif", fontface = 1)+
  draw_label(label="B)",x=0.53,y=0.81, fontfamily = "serif", fontface = 1)

