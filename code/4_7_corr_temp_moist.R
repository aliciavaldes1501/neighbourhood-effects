data_pts<-read.table("C:/Users/User/Dropbox/SU/Projects/neighbourhood_effects/data/clean/data_pts.txt",
                        sep="\t",header=T)
head(data_pts)

with(data_pts,cor(meanT,MOIST_PER_M,use="complete.obs"))

with(data_pts,plot(meanT,MOIST_PER_M))
with(data_pts,abline(lm(MOIST_PER_M~meanT)))
with(data_pts,summary(lm(MOIST_PER_M~meanT)))
