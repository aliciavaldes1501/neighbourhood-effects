library(anytime)
library(plyr)
library(foreign)

#Reading data
loggers<-read.table("./data/raw/tanga2016_loggers.txt",header=T,sep="\t",dec=".")
head(loggers)
str(loggers)

#Put the date in the correct format
loggers$date<-as.Date(as.character(loggers$date),format="%d/%m/%Y")

#Dates from 14/07-01/09
#Logger with location 381 is wrong, temperature was -40 in all the measurements --> Remove
loggers<-subset(loggers,!Location==381)

#Categorize day and night
loggers$timeday<-ifelse(loggers$hour==1|loggers$hour==4|loggers$hour==22,yes="night",no="day")

#Subset with only daily temperatures
loggers_day<-subset(loggers,timeday=="day")

#Calculate mean, maximum, minimum, standard deviation and range
#For each Location
loggers_agg<-ddply(loggers,c("Location","date"), summarise,
                       meanT = mean(LogData),
                       maxT = max(LogData),
                       minT = min(LogData),
                       sdT  = sd(LogData))
loggers_agg$rangeT<-loggers_agg$maxT-loggers_agg$minT
head(loggers_agg)
loggers_agg<-aggregate(cbind(meanT, maxT, minT, sdT, rangeT) ~ Location, data=loggers_agg, FUN=mean)
head(loggers_agg)
str(loggers_agg)

#Calculate daily mean, maximum, minimum, standard deviation and range
#For each Location
loggers_day_agg<-ddply(loggers_day,c("Location","date"), summarise,
                   meanT = mean(LogData),
                   maxT = max(LogData),
                   minT = min(LogData),
                   sdT  = sd(LogData))
loggers_day_agg$rangeT<-loggers_day_agg$maxT-loggers_day_agg$minT
head(loggers_day_agg)
loggers_day_agg<-aggregate(cbind(meanT, maxT, minT, sdT, rangeT) ~ Location, data=loggers_day_agg, FUN=mean)
head(loggers_day_agg)
str(loggers_day_agg)

#Correlation matrix for the different temperature measures
cor(loggers_agg[2:6])
cor(loggers_day_agg[2:6])

write.dbf(loggers_agg, "./gis/tables/temp_GIS.dbf")
write.dbf(loggers_day_agg, "./gis/tables/temp_day_GIS.dbf")

