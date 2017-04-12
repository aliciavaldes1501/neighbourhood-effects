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
data_pts<-read.table("D:/SU/projects/neighbourhood_effects/data/clean/points_all_data.txt",header=T,sep="\t",dec=".")
head(data_pts)
str(data_pts)
#Defining coordinates and coordinate system####
coordinates(data_pts) <- c("x", "y")
project1<-"+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" #RT90 0 gon (spatialreference.org)
proj4string(data_pts) = CRS(project1) #assign CRS with projected coordinates

#Some plots
spplot(data_pts, "meanTday", do.log=T, colorkey = TRUE)

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
mask<-readShapePoly("D:/SU/projects/neighbourhood_effects/gis/shapefiles/mask_20cm.shp",IDvar=NULL,proj4string=CRS(project1))
plot(mask)
mask<-mask@polygons
mask<-SpatialPolygons(mask, proj4string=CRS(project1))
plot(mask)
grd1<-grd1[!is.na(over(grd1, mask)),]
plot(grd1)

#Leave only data points which have temperature values (remove NAs)
data_pts_t<-data_pts[!is.na(data_pts$meanTday),]

#Inspect anisotropy####
estimateAnisotropy(data_pts_t,formulaString=meanT~1)
estimateAnisotropy(data_pts_t,formulaString=maxT~1)
estimateAnisotropy(data_pts_t,formulaString=minT~1)
estimateAnisotropy(data_pts_t,formulaString=sdT~1)
estimateAnisotropy(data_pts_t,formulaString=rangeT~1)
estimateAnisotropy(data_pts_t,formulaString=meanTday~1)
estimateAnisotropy(data_pts_t,formulaString=maxTday~1)
estimateAnisotropy(data_pts_t,formulaString=minTday~1)
estimateAnisotropy(data_pts_t,formulaString=sdTday~1)
estimateAnisotropy(data_pts_t,formulaString=rangeTday~1)
estimateAnisotropy(data_pts,formulaString=MOIST_PER_M~1)#anisotropy
estimateAnisotropy(data_pts,formulaString=MOIST_MV_M~1)#anisotropy

#Ordinary Kriging - no anisotropy####
krig1_meanT <-autoKrige(meanT~1,data_pts_t,grd1,verbose=T)
krig1_maxT <-autoKrige(maxT~1,data_pts_t,grd1,verbose=T)
krig1_minT <-autoKrige(minT~1,data_pts_t,grd1,verbose=T)
krig1_sdT <-autoKrige(sdT~1,data_pts_t,grd1,verbose=T)
krig1_rangeT <-autoKrige(rangeT~1,data_pts_t,grd1,verbose=T)
krig1_meanTday <-autoKrige(meanTday~1,data_pts_t,grd1,verbose=T)
krig1_maxTday <-autoKrige(maxTday~1,data_pts_t,grd1,verbose=T)
krig1_minTday <-autoKrige(minTday~1,data_pts_t,grd1,verbose=T)
krig1_sdTday <-autoKrige(sdTday~1,data_pts_t,grd1,verbose=T)
krig1_rangeTday <-autoKrige(rangeTday~1,data_pts_t,grd1,verbose=T)
krig1_MOIST_PER <-autoKrige(MOIST_PER_M~1,data_pts,grd1,verbose=T)
krig1_MOIST_MV <-autoKrige(MOIST_MV_M~1,data_pts,grd1,verbose=T)

pdf("D:/SU/projects/neighbourhood_effects/results/figures/kriging_temp_moist.pdf", family="Times",width=8,height=8)
plot(krig1_meanT)
plot(krig1_maxT)
plot(krig1_minT)
plot(krig1_sdT)
plot(krig1_rangeT)
plot(krig1_meanTday)
plot(krig1_maxTday)
plot(krig1_minTday)
plot(krig1_sdTday)
plot(krig1_rangeTday)
plot(krig1_MOIST_PER)
plot(krig1_MOIST_MV)
dev.off()

#Cross-validation####
krig1_meanT.cv<-autoKrige.cv(meanT~1,data_pts_t,model=c("Ste")) 
krig1_maxT.cv<-autoKrige.cv(maxT~1,data_pts_t,model=c("Ste")) 
krig1_minT.cv<-autoKrige.cv(minT~1,data_pts_t,model=c("Ste")) 
krig1_sdT.cv<-autoKrige.cv(sdT~1,data_pts_t,model=c("Ste")) 
krig1_rangeT.cv<-autoKrige.cv(rangeT~1,data_pts_t,model=c("Ste")) 
krig1_meanTday.cv<-autoKrige.cv(meanTday~1,data_pts_t,model=c("Ste")) 
krig1_maxTday.cv<-autoKrige.cv(maxTday~1,data_pts_t,model=c("Ste")) 
krig1_minTday.cv<-autoKrige.cv(minTday~1,data_pts_t,model=c("Ste")) 
krig1_sdTday.cv<-autoKrige.cv(sdTday~1,data_pts_t,model=c("Ste")) 
krig1_rangeTday.cv<-autoKrige.cv(rangeTday~1,data_pts_t,model=c("Ste")) 
krig1_MOIST_PER.cv<-autoKrige.cv(MOIST_PER_M~1,data_pts,model=c("Ste"))
krig1_MOIST_MV.cv<-autoKrige.cv(MOIST_MV_M~1,data_pts,model=c("Ste")) 

summary(krig1_meanT.cv)
summary(krig1_maxT.cv)
summary(krig1_minT.cv)
summary(krig1_sdT.cv)
summary(krig1_rangeT.cv)
summary(krig1_meanTday.cv)
summary(krig1_maxTday.cv)
summary(krig1_minTday.cv)
summary(krig1_sdTday.cv)
summary(krig1_rangeTday.cv)
summary(krig1_MOIST_PER.cv)
summary(krig1_MOIST_MV.cv)
# # mean error, ideally 0:
# mean(out$residual)
# # MSPE, ideally small
# # Mean square normalized error (MSNE), ideally close to 1
# # correlation observed and predicted, ideally 1
# # correlation predicted and residual, ideally 0

#Leave-one-out validation####

krig1_meanT.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_meanT.out[i] <- krige(meanT ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_meanT$var_model)$var1.pred
}
krig1_maxT.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_maxT.out[i] <- krige(maxT ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_maxT$var_model)$var1.pred
}
krig1_minT.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_minT.out[i] <- krige(minT ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_minT$var_model)$var1.pred
}
krig1_sdT.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_sdT.out[i] <- krige(sdT ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_sdT$var_model)$var1.pred
}
krig1_rangeT.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_rangeT.out[i] <- krige(rangeT ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_rangeT$var_model)$var1.pred
}
krig1_meanTday.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_meanTday.out[i] <- krige(meanTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_meanTday$var_model)$var1.pred
}
krig1_maxTday.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_maxTday.out[i] <- krige(maxTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_maxTday$var_model)$var1.pred
}
krig1_minTday.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_minTday.out[i] <- krige(minTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_minTday$var_model)$var1.pred
}
krig1_sdTday.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_sdTday.out[i] <- krige(sdTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_sdTday$var_model)$var1.pred
}
krig1_rangeTday.out <- vector(length = length(data_pts_t))
for (i in 1:length(data_pts_t)) {
  krig1_rangeTday.out[i] <- krige(rangeTday ~ 1, data_pts_t[-i,], data_pts_t[i,], model = krig1_rangeTday$var_model)$var1.pred
}
krig1_MOIST_PER.out <- vector(length = length(data_pts))
for (i in 1:length(data_pts)) {
  krig1_MOIST_PER.out[i] <- krige(MOIST_PER_M ~ 1, data_pts[-i,], data_pts[i,], model = krig1_MOIST_PER$var_model)$var1.pred
}
krig1_MOIST_MV.out <- vector(length = length(data_pts))
for (i in 1:length(data_pts)) {
  krig1_MOIST_MV.out[i] <- krige(MOIST_MV_M ~ 1, data_pts[-i,], data_pts[i,], model = krig1_MOIST_MV$var_model)$var1.pred
}

# Compute RMSE
sqrt( sum((krig1_meanT.out - data_pts_t$meanT)^2) / length(data_pts_t))
sqrt( sum((krig1_maxT.out - data_pts_t$maxT)^2) / length(data_pts_t))
sqrt( sum((krig1_minT.out - data_pts_t$minT)^2) / length(data_pts_t))
sqrt( sum((krig1_sdT.out - data_pts_t$sdT)^2) / length(data_pts_t))
sqrt( sum((krig1_rangeT.out - data_pts_t$rangeT)^2) / length(data_pts_t))
sqrt( sum((krig1_meanTday.out - data_pts_t$meanTday)^2) / length(data_pts_t))
sqrt( sum((krig1_maxTday.out - data_pts_t$maxTday)^2) / length(data_pts_t))
sqrt( sum((krig1_minTday.out - data_pts_t$minTday)^2) / length(data_pts_t))
sqrt( sum((krig1_sdTday.out - data_pts_t$sdTday)^2) / length(data_pts_t))
sqrt( sum((krig1_rangeTday.out - data_pts_t$rangeTday)^2) / length(data_pts_t))
sqrt( sum((krig1_MOIST_PER.out - data_pts$MOIST_PER_M)^2) / length(data_pts))
sqrt( sum((krig1_MOIST_MV.out - data_pts$MOIST_MV_M)^2) / length(data_pts))

# Plot the differences
pdf("D:/SU/projects/neighbourhood_effects/results/figures/kriging_temp_moist_validation.pdf", family="Times",width=12,height=8)
par(mfrow=c(2,3))
plot(krig1_meanT.out ~ data_pts_t$meanT, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation meanT")
abline(lm(krig1_meanT.out ~ data_pts_t$meanT), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_maxT.out ~ data_pts_t$maxT, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation maxT")
abline(lm(krig1_maxT.out ~ data_pts_t$maxT), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_minT.out ~ data_pts_t$minT, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation minT")
abline(lm(krig1_minT.out ~ data_pts_t$minT), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_sdT.out ~ data_pts_t$sdT, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation sdT")
abline(lm(krig1_sdT.out ~ data_pts_t$sdT), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_rangeT.out ~ data_pts_t$rangeT, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation rangeT")
abline(lm(krig1_rangeT.out ~ data_pts_t$rangeT), col="red", lw=2,lty=2)
abline(0,1)
par(mfrow=c(2,3))
plot(krig1_meanTday.out ~ data_pts_t$meanTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation meanTday")
abline(lm(krig1_meanTday.out ~ data_pts_t$meanTday), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_maxTday.out ~ data_pts_t$maxTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation maxTday")
abline(lm(krig1_maxTday.out ~ data_pts_t$maxTday), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_minTday.out ~ data_pts_t$minTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation minTday")
abline(lm(krig1_minTday.out ~ data_pts_t$minTday), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_sdTday.out ~ data_pts_t$sdTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation sdTday")
abline(lm(krig1_sdTday.out ~ data_pts_t$sdTday), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_rangeTday.out ~ data_pts_t$rangeTday, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation rangeTday")
abline(lm(krig1_rangeTday.out ~ data_pts_t$rangeTday), col="red", lw=2,lty=2)
abline(0,1)
par(mfrow=c(2,3))
plot(krig1_MOIST_PER.out ~ data_pts$MOIST_PER_M, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation MOIST_PER")
abline(lm(krig1_MOIST_PER.out ~ data_pts$MOIST_PER_M), col="red", lw=2,lty=2)
abline(0,1)
plot(krig1_MOIST_MV.out ~ data_pts$MOIST_MV_M, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation MOIST_MV")
abline(lm(krig1_MOIST_MV.out ~ data_pts$MOIST_MV_M), col="red", lw=2,lty=2)
abline(0,1)
dev.off()
