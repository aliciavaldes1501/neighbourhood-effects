#Ordinary kriging with anisotropy####

krig1_MOIST_PER$var_model
svgm_MOIST_PER <- variogram(MOIST_PER_M~1,alpha=c(0,45,90,135),tol.hor=5,data_pts,width=1)
plot(svgm_MOIST_PER)
eye1 <- vgm(psill=100, model="Ste", range=17, nugget=143, anis = c(0, 0.5),kappa=10)  #Eyeball a model
plot(svgm_MOIST_PER, eye1)
fit.eye1 <- fit.variogram(svgm_MOIST_PER, model = eye1)
plot(svgm_MOIST_PER, fit.eye1)
krig2_MOIST_PER <- krige(MOIST_PER_M ~ 1,  data_pts, grd1, model = fit.eye1)
summary(krig2_MOIST_PER)
plot(krig2_MOIST_PER)
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig2_MOIST_PER)) + geom_tile(aes(fill = var1.pred))+
  scale_fill_gradient(low = "yellow", high = "blue") + coord_equal() #Plot predictions
ggplot(aes(x = s1, y = s2), data = as.data.frame(krig2_MOIST_PER)) + geom_tile(aes(fill = var1.var))+
  scale_fill_gradient(low = "blue", high = "yellow") + coord_equal() #Plot standard error

krig2_MOIST_PER.out <- vector(length = length(data_pts))
for (i in 1:length(data_pts)) {
  krig2_MOIST_PER.out[i] <- krige(MOIST_PER_M ~ 1, data_pts[-i,], data_pts[i,], model = fit.eye1)$var1.pred
}
sqrt( sum((krig2_MOIST_PER.out - data_pts$MOIST_PER_M)^2) / length(data_pts)) #RMSE

plot(krig2_MOIST_PER.out ~ data_pts$MOIST_PER_M, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5),main="Validation MOIST_PER")
abline(lm(krig2_MOIST_PER.out ~ data_pts$MOIST_PER_M), col="red", lw=2,lty=2)
abline(0,1)

#Very similar to without anisotropy, not repeating for MOIST_MV