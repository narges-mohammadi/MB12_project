#######################################
# The following code tries to resample and subset the S2 tiles in R
# and then only calls the "gpt Biophysical" from SNAP for calculating the LAI
#######################################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet",
          "rasterVis","gridExtra","RColorBrewer","plotly", 
          "cptcity", "mapview","kableExtra","RStoolbox","sp",
          "IRdisplay","reshape","here", "gdalUtils"))

new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)


# Read
setwd(here("Desktop", "Playground_dir_5"))
S2_names <- here("Desktop", "Playground_dir_5")
S2_names_1 <- list.files(S2_names,recursive = FALSE, full.names = TRUE,
                         pattern="S2[A,B]_MSIL2A_[[:alnum:]]{15}_[[:alnum:]]{5}_[[:alnum:]]{4}_[[:alnum:]]{6}_[[:alnum:]]{15}.SAFE$")#S2[A,B]
S2_names_T_20 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                            pattern="*_B[018][[:alnum:]]_20m.jp2$")#B2,3,4,5,6,8A,11,12(20m) & B9(60m)
S2_names_T_60 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                            pattern="*_B09_60m.jp2$")
S2_names_T_combi <- c(S2_names_T_20, S2_names_T_60)
S2_names_T <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                         pattern="*_B[018][[:alnum:]]_20m|*_B09_60m.jp2$")
S2_names_L2A_v1 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                              pattern="L2A_T[[:alnum:]]{5}_[[:alnum:]]{15}_B0[2348]_10m.tif$")
S2_names <- lapply(1:length(S2_names_T), function(x){raster(S2_names_T[x])})                                                      
if(length(S2_names_L2A_v1)){S2_names_L2A <- lapply(1:length(S2_names_L2A_v1), function(x){raster(S2_names_L2A_v1[x])})}
S2_names_20 <- lapply(1:length(S2_names_T_20), function(x){raster(S2_names_T_20[x])})
S2_names_60 <- lapply(1:length(S2_names_T_60), function(x){raster(S2_names_T_60[x])})


# Prepare a band used for resampling("resample_base": is list of 10m bands needed for LAI)
resample_base <-  list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                             pattern="^[T][[:alnum:]]{5}_[[:alnum:]]{15}_B02_10m.tif$")
resample_base_rasters <- lapply(1:length(resample_base), function(x){raster(resample_base[x])})


# Exploration of rasters
res(S2_names[[1]])
extent(S2_names[[1]])
projection(S2_names[[2]])

b <- as(extent(515379.3,519889,4461970.6,4468567.9), 'SpatialPolygons')
crs(b) <- crs(S2_names[[1]])


# Method 1:
# disaggregate 20m bands to 10m also do the same for one 60m, then 
# stack all the bands(now all of them have 10m resolution)
# Now crop them with "b"
num_loop <- 3 # 3 is the number of sentinel tiles in this directory

for(i in 1:num_loop){
  # Stack all the 20m bands needed for LAI 
  S2_names_indexed <- S2_names_20[((i-1)*9+1):((i-1)*9+9)]
  S2_stack <- stack(S2_names_indexed)
  # crop around the study areas
  #S2_stack_crop_bbox <- crop(S2_stack , b)
  #resample_base_crop <- crop(resample_base_rasters[[1]], b)
  
  resample_stack <- lapply(1:nlayers(S2_stack), 
                                     function(x){disaggregate(S2_stack[[x]], 
                                                          fact=2, # to get 10 m resolution out of 20m bands
                                                          method='bilinear',
                                                          format="GTiff",
                                                          filename= paste0(paste(strsplit(S2_stack[[x]]@data@names,"_")[[1]][1:3], 
                                                                    collapse="_"),"_resampled_10m",collapse="_")
  )})

  S2_names_60_bbox <- crop(S2_names_60[[i]] , b)
  disagg_60_crop_bbox <- disaggregate(S2_names_60_bbox, 
                      fact=6, # to get 10 m resolution out of 60m band
                      method='bilinear',
                      format="GTiff",
                      filename= paste0(paste(strsplit(S2_names_60_bbox@data@names,"_")[[1]][1:3], 
                                      collapse="_"),"_resampled_10m",collapse="_")
  )  
  
  
  resample_base_rasters_10_bbox <- crop(resample_base_rasters[[1]], b)
  combi_stack <- stack(resample_base_rasters_10_bbox,
                       resample_stack_crop_bbox[[1]]
                       #resample_stack_crop_bbox,
                       #disagg_60_crop_bbox
                       )

}






# Method 2:

num_loop <- length(S2_names_T) / 10

# Stack all the .jp2 bands(20m,60m)
S2_names_indexed <- S2_names[1:10]#,[11:20],[21:30]

resample_file_1  <- resample(S2_names_indexed[[1]], resample_base_rasters[[1]], 
                          method='bilinear',
                          format="GTiff",
                          filename=#file.path(
                            #here("Desktop", "Playground_dir_5", "resampled_subsetted"), 
                            paste0(paste(strsplit(S2_names_indexed[[1]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_")
                            #)
)
resample_file_2  <- resample(S2_names_indexed[[2]], resample_base_rasters[[1]], 
                             method='bilinear',
                             format="GTiff",
                             filename=#file.path(
                               #here("Desktop", "Playground_dir_5", "resampled_subsetted"), 
                               paste0(paste(strsplit(S2_names_indexed[[2]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_")
                             #)
)
S2_stack <- stack(resample_file_1, resample_file_2)

for (i in 3:10){
  
  resample_file  <- resample(S2_names_indexed[[i]], resample_base_rasters[[1]], 
                               method='bilinear',
                               format="GTiff",
                               filename= paste0(paste(strsplit(S2_names_indexed[[i]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_")
                               )
  S2_stack <- addLayer(S2_stack, resample_file) 
  

}

# crop around the study areas
S2_stack_crop_bbox <- crop(S2_stack , b)

# Define the name for resampled and subsetted raster
Start <- "Subset_S2_MSIL2A_"
subset_name <- paste0(Start,strsplit(S2_stack_crop_bbox@data@names, "_")[[1]][2])

# Export the resampled & subsetted raster
filename <- file.path(here("Desktop","Playground_dir_5","resampled_subsetted"), subset_name)
writeRaster(x = S2_stack_crop_bbox,
            filename = filename,
            format = "GTiff", # save as a tif
            datatype='FLT4S',
            overwrite = TRUE
            )

  

# Method 3:
# Resample via "raster" package
# 24: the length(S2_names)
# This loop is painfully slow
for (i in 1:24){
  resample_file <- resample(S2_names[[i]], resample_base_rasters[[1]], 
           method='bilinear',
           format="GTiff",
           filename=file.path(
                  here("Desktop", "Playground_dir_5", "resampled_subsetted"), 
                  paste0(paste(strsplit(S2_names[[i]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_"))
  )
  
  print(i)
}

base_r <- resample_base_rasters[[1]]

# Make the raster package act faster
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#Define how many cores you want to use
UseCores <- detectCores() -2

#Register CoreCluster
cl <- makeCluster(UseCores)
registerDoParallel(cl)
clusterExport(cl, 'base_r')

#Use foreach loop and %dopar% command

foreach(i=1:length(S2_names)) %dopar% {
  library(raster)
  setwd("C:/Users/sanaz/")
  library(here)
  resample(S2_names[[i]], base_r,#,resample_base_rasters[[1]], 
           method='bilinear',
           format="GTiff",
           filename=file.path(
                  here("Desktop", "Playground_dir_5", "resampled"), 
                  paste0(paste(strsplit(S2_names[[i]]@data@names,"_")[[1]][1:3], collapse="_"),
                         "_resampled_10m",collapse="_"))
  )
  print(i)
}



#end cluster
stopCluster(cl)


for (i in 1:24){
  
  filename=file.path(here("Desktop", "Playground_dir_5", "resampled"), 
             paste0(paste(strsplit(S2_names[[i]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_")
  )
  print(filename)
}


resampled_file <- resample(S2_names[[1]], resample_base_rasters[[1]], 
                           method='bilinear',
                           format="GTiff",
                           filename=file.path(
                                             here("Desktop", "Playground_dir_5", "resampled"), 
                                             paste0(paste(strsplit(S2_names[[1]]@data@names,"_")[[1]][1:3], collapse="_"),"_resampled_10m",collapse="_"))
                           )

# Time amount that the command needs to be performed
time_rater_resample <- system.time(resample(S2_names[[1]], resample_base_rasters[[1]], method='bilinear'))
par(mfrow=c(1,2))
plot(resample_base_rasters[[1]])
plot(resampled_file)







# Resample(all bands needed for LAI to 10m)
# that is B3, B4, B5, B6, B8, B8A, B9, B11, B12
#' Resampling a Raster* object via GDAL
#' 
#' @param r Raster* object to be resampled
#' @param r_base Raster* object with parameters that r
#' should be resampled to.
#' @param method Character. GDAL resampling_method
#' ("near"|"bilinear"|"cubic"|"cubicspline"|
#'  "lanczos"|"average"|"mode"|"max"|"min"|
#'  "med"|"q1"|"q3")
#'
#' @export
gdal_resample <- function(r, r_base, method = 'bilinear') {
  
  #Geometry attributes
  t1 <- c(xmin(r_base), ymin(r_base), 
          xmax(r_base), ymax(r_base))
  
  
  res <- res(r_base)
  
  #Temporal files
  tmp_outname <- "resampled.tif"#sprintf('%s.tif', tempfile())
  tmp_inname <- ".tif"#sprintf('%s.tif', tempfile())
  writeRaster(r, tmp_inname)
  
  #GDAL time!
  gdalwarp(tmp_inname, tmp_outname, 
           tr = res, te = t1, r = method)
  resample_raster = raster(tmp_outname)
  
  return(resample_raster)
}

resampled_file <- gdal_resample(clim_stack[[.x]], resample_base_rasters[[1]])



# Subset(As far as I know it is the equivalent of crop() function in R)


# Write 