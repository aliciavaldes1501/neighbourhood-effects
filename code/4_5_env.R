PCA_env<-prcomp(~markedplants$meanT+markedplants$moist_per+markedplants$veg_h_mean,center=T,scale=T)
summary(PCA_env)
plot(PCA_env)
biplot(PCA_env)

#Can we estimate veg h from temp and moist?
hist(markedplants$veg_h_mean)
summary(lm(markedplants$veg_h_mean~markedplants$meanT)) 
summary(lm(markedplants$veg_h_mean~markedplants$moist_per)) 
summary(lm(markedplants$veg_h_mean~markedplants$meanT*markedplants$moist_per)) 

#R2 always <0.1-->not really useful for estimating

ggplot(as.data.frame(markedplants), aes(x=meanT, y=veg_h_mean))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

ggplot(as.data.frame(markedplants), aes(x=moist_per, y=veg_h_mean))+ geom_point(shape=1)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,color="red")+
  stat_smooth(method = "lm", size = 1)

#PCA with temp and moist
PCA_env2<-prcomp(~allplants$meanT+allplants$moist_per,center=T,scale=T)
summary(PCA_env2)
plot(PCA_env2)
biplot(PCA_env2)

PCA_env2_scores<-as.data.frame(predict(PCA_env2))
allplants$PC1<-PCA_env2_scores$PC1
allplants$PC2<-PCA_env2_scores$PC2
hist(allplants$PC1)
hist(allplants$PC2)



