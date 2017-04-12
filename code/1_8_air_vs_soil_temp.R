library(reshape2)

head(loggers[3:8])
pts_air<-as.data.frame(data_pts)[c(1,3:4)]
pts_air<-subset(pts_air,loggerA_id>0)

pts_air_long <- melt(pts_air, id.vars=c("point_id"))
head(pts_air_long)
names(pts_air_long)<-c("point_id","A_S","Location")
pts_air_long<-subset(pts_air_long,!point_id==247)

loggers_select<-loggers[3:8]

loggers_select<-loggers_select[loggers_select$Location %in% pts_air_long$Location,]
loggers_select$Location<-as.factor(loggers_select$Location)
loggers_select$hour<-as.factor(loggers_select$hour)
loggers_select$timeday<-as.factor(loggers_select$timeday)

pts_air_long$point_id<-as.factor(pts_air_long$point_id)
pts_air_long$Location<-as.factor(pts_air_long$Location)

loggers_select$Location<-droplevels(loggers_select$Location)
pts_air_long$Location<-droplevels(pts_air_long$Location)


pts_air_long<-merge(pts_air_long,loggers_select,by="Location")
head(pts_air_long)

pts_air_wide <- dcast(pts_air_long,date+hour+point_id~A_S,value.var="LogData")
head(pts_air_wide)
str(pts_air_wide)


with(pts_air_wide,plot(loggerA_id,loggerS_id,xlab="Air temperature",ylab="Soil temperature",
                       main="R square = 0.28, P<0.001"))
abline(with(pts_air_wide,lm(loggerS_id~loggerA_id)))
summary(with(pts_air_wide,lm(loggerS_id~loggerA_id)))

with(pts_air_wide,hist(loggerA_id,xlim=c(-5,45),xlab=NULL,main="Air temperature from 24 loggers"))
with(pts_air_wide,hist(loggerS_id,xlim=c(-5,45),xlab=NULL,main="Soil temperature from 24 loggers"))
