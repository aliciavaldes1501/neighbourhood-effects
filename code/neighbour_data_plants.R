#Getting density and neighourhood phenology data for plants

#Read table exported from GIS with data on plant density in different neighbourhoods
#at 0.5-1-2-5-10 m
plants_pldens<-read.table("./gis/tables/plants_pldens.txt",header=T,sep=",",dec=".")
head(plants_pldens)
str(plants_pldens)

allplants<-merge(allplants,plants_pldens)
head(allplants)
  
#Read table exported from GIS with data on phenology in different neighbourhoods
#at 0.5-1-2-5-10 m
plants_phen_neigh<-read.table("./gis/tables/plants_phen_neigh.txt",header=T,sep=",",dec=".")
head(plants_phen_neigh)
str(plants_phen_neigh)
plants_phen_neigh$FID_1<-NULL
plants_phen_neigh$FID_12<-NULL
plants_phen_neigh$FID_12_13<-NULL
plants_phen_neigh$FID_12_13_14<-NULL
plants_phen_neigh$OID_<-NULL

plants_phen_neigh$phen_n05<-with(plants_phen_neigh,ifelse(phen_num==-9999,phen_n05_s/phen_n05_c,
                                   ((phen_n05_s-phen_num)/(phen_n05_c-1))
                                   ))
plants_phen_neigh$phen_n1<-with(plants_phen_neigh,ifelse(phen_num==-9999,phen_n1_s/phen_n1_c,
                                                          ((phen_n1_s-phen_num)/(phen_n1_c-1))
))
plants_phen_neigh$phen_n2<-with(plants_phen_neigh,ifelse(phen_num==-9999,phen_n2_s/phen_n2_c,
                                                         ((phen_n2_s-phen_num)/(phen_n2_c-1))
))
plants_phen_neigh$phen_n5<-with(plants_phen_neigh,ifelse(phen_num==-9999,phen_n5_s/phen_n5_c,
                                                         ((phen_n5_s-phen_num)/(phen_n5_c-1))
))
plants_phen_neigh$phen_n10<-with(plants_phen_neigh,ifelse(phen_num==-9999,phen_n10_s/phen_n10_c,
                                                         ((phen_n10_s-phen_num)/(phen_n10_c-1))
))

plants_phen_neigh$phen_n05[is.nan(plants_phen_neigh$phen_n05)] <- NA
plants_phen_neigh$phen_n1[is.nan(plants_phen_neigh$phen_n1)] <- NA

plants_phen_neigh[c(1,13:17)]

allplants<-merge(allplants,plants_phen_neigh[c(1,13:17)])
head(allplants)

