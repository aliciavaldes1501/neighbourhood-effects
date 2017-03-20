#' ---
#' title: "Variography with gstat and ggplot2"
#' author: "Bart Rogiers"
#' output:
#'  html_document:
#'    self_contained: FALSE
#' ---
#' 
#' Last year I wrote a short demo on [variography](https://en.wikipedia.org/wiki/Variogram) with 
#' [gstat](https://cran.r-project.org/web/packages/gstat/index.html) and 
#' [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) for a colleague who was planning
#' to migrate to [R](https://www.r-project.org/).
#' Just thought I'd share this here (with some additional stuff) as it might be useful for other people as well.
#' 
#' First, make sure you have the necessary packages installed and loaded:
require('gstat')
require('sp')
require('ggplot2')
#' The [sp](https://cran.r-project.org/web/packages/sp/index.html) package is not really required,
#' but it makes working with spatial data a little bit easier.
#' 
#' I like to work with the black & white [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) theme,
#' and legends positioned above the graphs, so I always adjust the basic settings the following way:
theme_set(theme_bw())
theme_update(legend.position='top')
#' Let's create some synthetic data to work with.
#' I'm using [gstat](https://cran.r-project.org/web/packages/gstat/index.html) here to simulate
#' a random field on a 100x100 regular grid:
x <- 1:100 # x coordinates
y <- 1:100 # y coordinates
dat <- expand.grid(x=x,y=y) # create data frame with all combinations
dat$z <- 1 # initialize z variable
coordinates(dat) <- ~x+y # set coordinates
gridded(dat) <- TRUE # specify data is gridded
g <- gstat(id='z',formula=z~1,model=vgm(0.9,'Sph',60,0.1,anis=c(45,0.1)),data=dat,dummy=TRUE,beta=0,maxdist=20,nmax=10) # create gstat object
dat <- data.frame(predict(g,newdata=dat,nsim=1)) # simulate random field data
names(dat)[3] <- 'z'
head(dat) # show first lines of the data frame
#' Plotting data on a regular grid is easily achieved with geom_raster():
ggplot(dat,aes(x=x,y=y,fill=z)) + geom_raster() + scale_fill_gradientn(colours=rainbow(7))
#' This is the data we will perform some variography on. Alternatively, have a look at [this file](https://cran.r-project.org/doc/manuals/r-release/R-data.pdf) to load your own data.
#' We'll transform our data frame into a spatial points data frame,
#' and create a [gstat](https://cran.r-project.org/web/packages/gstat/index.html) object:
coordinates(dat) <- ~x+y # set coordinate variables
# gridded(dat) <- TRUE # specify that your data is gridded; this speeds up things considerably!
#' I commented the above line, as there is an [issue](https://github.com/edzer/gstat/issues/1) with gstat 1.1-0, which basically mirrors gridded data horizontally before deriving the experimental variogram. Edzer Pebesma, the author of gstat, already [solved the issue](https://github.com/edzer/gstat/commit/364daf98324933efafb8cac9d66f2544284c8a50), so in the latest gstat releases this should work properly with gridded data as well.
g <- gstat(id='z',formula=z~1,data=dat)
#' Calculating an experimental isotropic variogram can then simply be done by:
expvar <- variogram(g)
head(expvar) # show first lines of the gstatVariogram data frame
plot(expvar) # you can plot gstatVariogram objects like this (gstat function)
#' I prefer [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) for plotting,
#' and it turns out that the [gstat](https://cran.r-project.org/web/packages/gstat/index.html) objects are very much
#' suited for use with ggplot2:
ggplot(expvar,aes(x=dist,y=gamma,size=np)) + geom_point()
#' For a single direction at a time, you can just specify the angle alpha and the tolerance on the angle:
expvar <- variogram(g,alpha=0,tol.hor=5)
ggplot(expvar,aes(x=dist,y=gamma,size=np)) + geom_point()
#' Looking at multiple directions is done by providing a vector instead of a single value:
expvar <- variogram(g,alpha=c(0,45,90,135),tol.hor=5)
ggplot(expvar,aes(x=dist,y=gamma,size=np,col=factor(dir.hor))) + geom_point()
#' And cutoff values can be specified as well if the default value does not suffice:
expvar <- variogram(g,alpha=c(0,45,90,135),tol.hor=5,cutoff=30)
ggplot(expvar,aes(x=dist,y=gamma,size=np,col=factor(dir.hor))) + geom_point()
#' Another option is to create a variogram map:
expvar <- variogram(g,width=3,cutoff=30,map=TRUE)
#' which can be plotted again by a gstat function:
plot(expvar)
#' The variogram map can again easily be plotted with ggplot2 and geom_raster():
ggplot(data.frame(expvar),aes(x=map.dx,y=map.dy,fill=map.z))+geom_raster() + scale_fill_gradientn(colours=rainbow(7))
#' Finally, my colleague was interested in the performance of the algorithm. So here are some execution times for my personal laptop,
#' running [Linux Lite 2.6](https://www.linuxliteos.com/), R 3.2.3, and having an Intel(R) Core(TM)2 Duo CPU T5800 @ 2.00GHz processor
#' and 2969MB memory:
system.time(variogram(g))
system.time(variogram(g,alpha=0,tol.hor=5))
system.time(variogram(g,alpha=c(0,45,90,135),tol.hor=5))
system.time(variogram(g,alpha=c(0,45,90,135),tol.hor=5,cutoff=100))
#' That's it! If you want to know more, have a look at
?variogram
#' for more options, and if you want to try this yourself, check out the [source R script](https://docs.google.com/uc?id=0B4xr2UZeAf_mTGRsNUROcU4wR0E&export=download)!


