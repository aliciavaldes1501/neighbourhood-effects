library(sp)
library(raster)
library(rgdal)
library(maptools)
library(gstat)
library(lattice)
library(ggplot2)
library(automap)
library(intamap)
library(gridExtra)

#Reading data####
data_pts<-read.table("./data/clean/points_all_data.txt",header=T,sep="\t",dec=".")
head(data_pts)
str(data_pts)
#Defining coordinates and coordinate system####
coordinates(data_pts) <- c("x", "y")
project1<-"+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #RT90 0 gon (spatialreference.org)
proj4string(data_pts) = CRS(project1) #assign CRS with projected coordinates

#Some plots
spplot(data_pts, "meanTday", do.log=T, colorkey = TRUE)
bubble(data_pts_t, "meanTday", do.log = T, key.space = "bottom")

#Preparing a prediction grid####
plot(data_pts)
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
mask<-readShapePoly("./gis/shapefiles/mask_20cm.shp",IDvar=NULL,proj4string=CRS(project1))
plot(mask)
mask<-mask@polygons
mask<-SpatialPolygons(mask, proj4string=CRS(project1))
plot(mask)
grd1<-grd1[!is.na(over(grd1, mask)),]
plot(grd1)

#Example IDW with temperature####
data_pts_t<-data_pts[!is.na(data_pts$meanTday),]
idw.meanTday <- idw(meanTday ~ 1, data_pts_t, grd1, idp = 2.5)
idw.meanTday <- idw(meanTday ~ 1, data_pts_t, grd1, idp = 1, maxdist=20)
plot(idw.meanTday)
as.data.frame(idw.meanTday)[1:5, ]

# Leave-one-out validation routine
IDW.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  IDW.out[i] <- idw(meanTday ~ 1, data_pts_t[-i,], data_pts_t[i,], idp=1.0,maxdist=20)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ data_pts_t$meanTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ data_pts_t$meanTday), col="red", lw=2,lty=2)
abline(0,1)
par(OP)

# Compute RMSE
sqrt( sum((IDW.out - data_pts_t$meanTday)^2) / length(data_pts_t))


#Variography####
hscat(meanTday ~ 1, data_pts_t, 
      c(3,5,8,10,20,30,40,50,60,70,80)) #20-30 m, r goes down, but little up again at 70 m
hscat(MOIST_PER_M ~ 1, data_pts, 
      c(3,5,8,10,20,30,40,50,60,70,80)) #r goes down after 40 m
hscat(Mrub_sum ~ 1, data_pts, 
      c(3,4,5,6,7,8,10,20,30,40,50,60,70,80)) #Only correlated at small distances

cld_meanTday <- variogram(meanTday ~ 1, data_pts_t, cloud = TRUE)
svgm_meanTday <- variogram(meanTday ~ 1, data_pts_t,width=1,tol.hor=5,cutoff=30)

theme_set(theme_bw())
ggplot(cld_meanTday,aes(x=dist,y=gamma)) + geom_point() #Variogram cloud
ggplot(svgm_meanTday,aes(x=dist,y=gamma,size=np)) + geom_point() #Variogram

plot(variogram(meanTday~1,width=3,cutoff=30,map=TRUE,data_pts_t)) #Variogram map

svgm_meanTday <- autofitVariogram(meanTday ~ 1, data_pts_t,verbose=T) #Auto-fit variogram
summary(svgm_meanTday)
plot(svgm_meanTday) #Good! But does not take anisotropy into account-->Start with this
#Fit "by hand" with the same parameters and boundaries
svgmb_meanTday <- variogram(meanTday~1, data_pts_t,boundaries = c(3,4,6,9,12,16,21,26))
eye1 <- vgm(psill=1.02080788, model="Ste",range=74.85307,nugget=0.08008408,kappa=0.2)
plot(svgmb_meanTday,eye1)
fit.eye1<-fit.variogram(svgmb_meanTday, eye1)
fit.eye1
plot(svgmb_meanTday, fit.eye1)

#Ordinary Kriging - no anisotropy####
krig1_meanTday <-autoKrige(meanTday~1,data_pts_t,grd1,verbose=T)
plot(krig1_meanTday)
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig1_meanTday$krige_output)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal()
krig1_meanTday
krig1_meanTday.cv<-autoKrige.cv(meanTday~1,data_pts_t,model=c("Ste")) #Cross-validation
summary(krig1_meanTday.cv)
# # mean error, ideally 0:
# mean(out$residual)
# # MSPE, ideally small
# # Mean square normalized error (MSNE), ideally close to 1
# # correlation observed and predicted, ideally 1
# # correlation predicted and residual, ideally 0

#OR
krig1b_meanTday <- krige(meanTday ~ 1,  data_pts_t, grd1, model = fit.eye1)
summary(krig1b_meanTday)
krig1b_meanTday
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig1b_meanTday)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal() #Plot predictions
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig1b_meanTday)) + geom_tile(aes(fill = var1.var))+
  scale_fill_gradient(low = "blue", high = "yellow") + coord_equal() #Plot standard error


#Anisotropy####
#Inspect anisotropy
estimateAnisotropy(data_pts_t,formulaString=meanTday~1) #No anisotropy

#Fitting models for different directions anyway
svgm_a_meanTday <- variogram(meanTday~1,alpha=c(0,45,90,135),tol.hor=5,data_pts_t,width=1)
#ggplot(svgm_meanTday,aes(x=dist,y=gamma,size=np,col=factor(dir.hor)))+geom_point() 
ggplot(aes(x = dist, y = gamma), data = svgm_a_meanTday)+geom_point()+facet_wrap(~dir.hor)
#Variogram in four different directions

#Spatial correlation strongest in the 90 (and 135) degree direction
#and weakest in the 45 (and 0) degress direction 
#(things stay similar for longer if you move in the 90 (and 135) degree direction)

#Fitting the model in the 90 degree direction
#Variogram Model Fitting
vgm.90_meanTday <- subset(svgm_a_meanTday, svgm_a_meanTday$dir.hor == 90)
plot(vgm.90_meanTday)
#Eyeball the nugget, sill and range parameters from the empirical variogram
eye2 <- vgm(0.50, "Ste",6,0.08,kappa=0.5)
# eye2 <- vgm(0.23, "Wav",5,0.26)
plot(vgm.90_meanTday, eye2)
fit.eye2<-fit.variogram(vgm.90_meanTday, vgm(psill=0.50,model="Ste",nugger=0.08))
fit.eye2
plot(vgm.90_meanTday, fit.eye2)

#Ordinary kriging with model in the 90 degree direction####
krig2_meanTday <- krige(meanTday ~ 1,  data_pts_t, grd1, model = fit.eye2)
summary(krig2_meanTday)
krig2_meanTday
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig2_meanTday)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal() #Plot predictions
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig2_meanTday)) + geom_tile(aes(fill = var1.var))+
  scale_fill_gradient(low = "blue", high = "yellow") + coord_equal() #Plot standard error

#Ordinary kriging with anisotropy####
svgm_a_meanTday <- variogram(meanTday~1,alpha=c(0,45,90,135),tol.hor=5,data_pts_t,width=1)
eye3 <- vgm(psill=0.50, model="Ste", range=6, nugget=0.08, anis = c(90, 0.8),kappa=0.5)  #Eyeball a model
plot(svgm_a_meanTday, eye3)
fit.eye3 <- fit.variogram(svgm_a_meanTday, model = eye3)
plot(svgm_a_meanTday, fit.eye3)
krig3_meanTday <- krige(meanTday ~ 1,  data_pts_t, grd1, model = fit.eye3)
summary(krig3_meanTday)
krig3_meanTday
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig3_meanTday)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal() #Plot predictions
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig3_meanTday)) + geom_tile(aes(fill = var1.var))+
  scale_fill_gradient(low = "blue", high = "yellow") + coord_equal() #Plot standard error

#Compare Kriging 1-2-3####
k1<-ggplot(aes(x = s1, y = s2), data = as.data.frame(krig1_meanTday$krige_output)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal()+ggtitle("Kriging 1")
k2<-ggplot(aes(x = s1, y = s2), data = as.data.frame(krig2_meanTday)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal()+ggtitle("Kriging 2 - 90 degrees")
k3<-ggplot(aes(x = s1, y = s2), data = as.data.frame(krig3_meanTday)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal()+ggtitle("Kriging 3 - anisotropy")
grid.arrange(k1, k2, k3)

k1<-spplot(krig1b_meanTday["var1.pred"],at = seq(14,20,1))
k2<-spplot(krig2_meanTday["var1.pred"],at = seq(14,20,1))
k3<-spplot(krig3_meanTday["var1.pred"],at = seq(14,20,1))
grid.arrange(k1, k2, k3)

k1<-spplot(krig1b_meanTday["var1.var"],at=seq(0.10,0.50,0.05))
k2<-spplot(krig2_meanTday["var1.var"],at=seq(0.10,0.50,0.05))
k3<-spplot(krig3_meanTday["var1.var"],at=seq(0.10,0.50,0.05))
grid.arrange(k1, k2, k3)

# Leave-one-out validation routines
k1.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  k1.out[i] <- krige(meanTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = fit.eye1)$var1.pred
}

k2.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  k2.out[i] <- krige(meanTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = fit.eye2)$var1.pred
}

k3.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  k3.out[i] <- krige(meanTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = fit.eye3)$var1.pred
}

# Plot the differences
par(mfrow=c(1,3))
plot(k1.out ~ data_pts_t$meanTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(k1.out ~ data_pts_t$meanTday), col="red", lw=2,lty=2)
abline(0,1)
plot(k2.out ~ data_pts_t$meanTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(k2.out ~ data_pts_t$meanTday), col="red", lw=2,lty=2)
abline(0,1)
plot(k3.out ~ data_pts_t$meanTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(k3.out ~ data_pts_t$meanTday), col="red", lw=2,lty=2)
abline(0,1)

