
#############################
# The code intends to apply functions within bfast package
# for analyzing time series data
############################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet",
          "rasterVis","gridExtra","RColorBrewer","plotly",
          "RStoolbox","sp","IRdisplay","reshape","here", 
          "bfast", "bfastSpatial"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)



# load the package
library(bfastSpatial)
data(tura)
obs <- countObs(tura)
plot(obs)


