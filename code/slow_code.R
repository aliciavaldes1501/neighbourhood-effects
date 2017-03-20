library(beepr)
beep(sound=8,expr=NULL)


correlog3aut <- correlog(allplants$x, allplants$y,res1aut,increment=1,resamp=100)
plot(correlog3aut)
correlog3aut_int <- correlog(allplants$x, allplants$y,res1aut_int,increment=1,resamp=100)
plot(correlog3aut_int)
GlobMT2aut<- moran.test(res1aut, listw=allplants.listw)
GlobMT2aut 
GlobMT2aut_int<- moran.test(res1aut_int, listw=allplants.listw) #Error, redo!
GlobMT2aut_int



