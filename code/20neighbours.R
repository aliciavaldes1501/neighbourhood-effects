#Create neighbours matrix - 20 nearest neighbours
allplants.nb3 <- knearneigh(subset(allplants,!is.na(phen)), k=20,longlat=F,RANN=T) #Warning identical points
allplants.listw3 <- knn2nb(allplants.nb3,row.names=subset(allplants,!is.na(phen))$FID) 
allplants.listw3<-nb2listw(allplants.listw3, glist = NULL, style = "W", zero.policy = FALSE)

