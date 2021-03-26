
setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
#install.packages("devtools")
#library(devtools)
#install_local(here("Documents", "R", "ENMTools-master"))
#library(ENMTools)
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet",
          "rasterVis","gridExtra","RColorBrewer","plotly",
          "RStoolbox","sp","IRdisplay","reshape","here", 
          "bfast", "bfastSpatial", "rkt"))# "rkt" : for time series analysis
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)


#2: Load Auxillary data
### Define your area of interest (aoi), which is MFC2 (bacino_MFC_corrected) or bounding_box_MFC or else #
aoi <- rgdal::readOGR(dsn=here("Documents", "MB12-project", "data", "vector", "Site1_MFC2_agroforestry"),layer= "MFC2")
aoi_2 <- rgdal::readOGR(dsn=here("Documents", "MB12-project", "data", "vector","Site2_GOR_forest", "Site2_GOR_forest"), layer="GOR")
artifact_mfc2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/QGIS_part/parking_lot.shp")
artifact2_mfc2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/QGIS_part/house.shp")

# reproject data
artifact_mfc2_new <- spTransform(artifact_mfc2,
                                 crs(aoi))
artifact2_mfc2_new <- spTransform(artifact2_mfc2,
                                  crs(aoi))



MFC2_bbox <- as(extent(515379.3, 516012.9, 4468068.3, 4468567.9), 'SpatialPolygons')
crs(MFC2_bbox) <- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
GOR_bbox <- as(extent(519177.4, 519889, 4461970.6, 4462834), 'SpatialPolygons')
crs(GOR_bbox) <- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")


dem <- raster(file.path(here("Documents", "MB12-project", "data",
                             "DEM_5m_Sarah"),"DEM5m_UARC.tif"))

# crop around each study area and mask
dem_mfc2 <- crop(dem , MFC2_bbox)
dem_mfc2_masked <- mask(dem_mfc2, aoi)

dem_gor1 <- crop(dem , GOR_bbox)
dem_gor1_masked <- mask(dem_gor1, aoi_2)



# load basis rasters used in resampling (the TAs that Sarah sent)
basis_resample_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                        "Gridded_topographic_attributes","TA_MFC2_10m"),"DEM10m.tif"))

basis_resample_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                             "Gridded_topographic_attributes","TA_GOR1_10m"),"DEM10m.tif"))


    
# resample from 5m to 10m DEM
output_dir_mfc2 <- here("Desktop","Playground_dir_14","MFC2")
dem_mfc2_10m <- resample(dem_mfc2_masked, basis_resample_mfc2, method="bilinear", 
                            format="GTiff",
                            overwrite=TRUE,
                            filename=file.path(output_dir_mfc2, "dem_mfc2_10m"))


output_dir_gor1 <- here("Desktop","Playground_dir_14","GOR")
dem_gor1_10m <- resample(dem_gor1_masked, basis_resample_gor1, method="bilinear", 
                         format="GTiff",
                         overwrite=TRUE,
                         filename=file.path(output_dir_gor1, "dem_gor1_10m"))



# Extract DEM valuse for AOIs and save the dataframes as .Rds into drive
# set df=TRUE to return a data.frame rather than a list of values
mfc2_dem_df <- raster::extract(x = dem_mfc2_10m, 
                               y = aoi, 
                               df = TRUE)


saveRDS(mfc2_dem_df, 
        file= file.path(output_dir_mfc2, "Extracted_dfs", "mfc2_dem_df"))



gor1_dem_df <- raster::extract(x = dem_gor1_10m, 
                               y = aoi_2, 
                               df = TRUE)
saveRDS(gor1_dem_df, 
        file= file.path(output_dir_gor1, "Extracted_dfs", "gor1_dem_df"))


#another way to extract info from raster

# Load TAs into R 
AAC10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_MFC2_10m"),"AAC10m.tif"))

AAC10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_GOR1_10m"),"AAC10m.tif"))


Slope10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_MFC2_10m"),"Slope10m.tif"))

Slope10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_GOR1_10m"),"Slope10m.tif"))

Aspect10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                       "Gridded_topographic_attributes","TA_MFC2_10m"),"Aspect10m.tif"))

Aspect10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                       "Gridded_topographic_attributes","TA_GOR1_10m"),"Aspect10m.tif"))


PlanCurv10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                        "Gridded_topographic_attributes","TA_MFC2_10m"),"PlanCurv10m.tif"))

PlanCurv10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                        "Gridded_topographic_attributes","TA_GOR1_10m"),"PlanCurv10m.tif"))

 

ProfileCurv10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                          "Gridded_topographic_attributes","TA_MFC2_10m"),"ProfileCurv10m.tif"))

ProfileCurv10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                          "Gridded_topographic_attributes","TA_GOR1_10m"),"ProfileCurv10m.tif"))

TRI10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                             "Gridded_topographic_attributes","TA_MFC2_10m"),"TRI10m.tif"))

TRI10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                             "Gridded_topographic_attributes","TA_GOR1_10m"),"TRI10m.tif"))

TWI10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_MFC2_10m"),"TWI10m.tif"))

TWI10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_GOR1_10m"),"TWI10m.tif"))

FlowAcc10m_mfc2 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_MFC2_10m"),"FlowAcc10m.tif"))

FlowAcc10m_gor1 <- raster(file.path(here("Documents", "MB12-project", "data",
                                     "Gridded_topographic_attributes","TA_GOR1_10m"),"FlowAcc10m.tif"))



#Topographic Attributes(TAs) for each site
TA_mfc2 <- stack(dem_mfc2_10m, AAC10m_mfc2, Slope10m_mfc2, Aspect10m_mfc2, PlanCurv10m_mfc2, 
                 ProfileCurv10m_mfc2, TRI10m_mfc2, TWI10m_mfc2, FlowAcc10m_mfc2)

TA_gor1 <- stack(dem_gor1_10m, AAC10m_gor1, Slope10m_gor1, Aspect10m_gor1, PlanCurv10m_gor1, 
                 ProfileCurv10m_gor1, TRI10m_gor1, TWI10m_gor1, FlowAcc10m_gor1)

# NDVI stacks (created in "9_Cropped_Ndvi_from_Gtiff_Function.R" script)
# MFC2
ndvi_mfc2_2017 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndvi_stack_MFC2_2017"))

ndvi_mfc2_2018 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndvi_stack_MFC2_2018"))

ndvi_mfc2_2019 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndvi_stack_MFC2_2019"))

ndvi_mfc2_2020 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndvi_stack_MFC2_2020"))

mfc2_NDVI_yrs_stack <- stack(ndvi_mfc2_2017, ndvi_mfc2_2018, ndvi_mfc2_2019, ndvi_mfc2_2020)
    
# GOR
ndvi_gor1_2017 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndvi_stack_GOR_2017"))

ndvi_gor1_2018 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndvi_stack_GOR_2018"))

ndvi_gor1_2019 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndvi_stack_GOR_2019"))

ndvi_gor1_2020 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndvi_stack_GOR_2020"))

gor1_NDVI_yrs_stack <- stack(ndvi_gor1_2017, ndvi_gor1_2018, ndvi_gor1_2019, ndvi_gor1_2020)


#### hydrological seasons ####
# This fold contains code to add hydrological seasons
# After consulting with Sarah, decided to  remove correlating the seasonal NDVIs with TAs.
# As there was no added value by correlating the seasonal NDVI with TAs.
# I will still calculate the seasonal NDVIs to use them later for mean NDVI for each pixel in each year

# Seasons based on Paolo's paper for this area

# the function recieves the x (X32 or X267 or ... )  and decides if the layer is 
# in "wet" , "dry" or "transition" season
three_season <- function(x){
    if(x %in% c(paste0('X', 305:365), paste0('X', 1:60))){
        season <- "wet"
    }else if(x %in% c(paste0('X', 121:245))){
        season <- "dry"
    }else if (x %in% c(paste0('X', 246:304), paste0('X', 61:120))){
        season <- "transition"
    }
    return(season)
}

# derive a 3 layer stack for each year for "GOR1"(wet, dry, transition)
three_season_layers_2017_gor <- lapply(names(ndvi_gor1_2017), FUN = three_season)
means_2017_gor <-  stackApply(ndvi_gor1_2017, indices=three_season_layers_2017_gor, fun=mean)

three_season_layers_2018_gor <- lapply(names(ndvi_gor1_2018), FUN = three_season)
means_2018_gor <-  stackApply(ndvi_gor1_2018, indices=three_season_layers_2018_gor, fun=mean)

three_season_layers_2019_gor <- lapply(names(ndvi_gor1_2019), FUN = three_season)
means_2019_gor <-  stackApply(ndvi_gor1_2019, indices=three_season_layers_2019_gor, fun=mean)

three_season_layers_2020_gor <- lapply(names(ndvi_gor1_2020), FUN = three_season)
means_2020_gor <-  stackApply(ndvi_gor1_2020, indices=three_season_layers_2020_gor, fun=mean)


# derive a 3 layer stack for each year for "MFC2"(wet, dry, transition)
three_season_layers_2017_mfc2 <- lapply(names(ndvi_mfc2_2017), FUN = three_season)
means_2017_mfc2 <-  stackApply(ndvi_mfc2_2017, indices=three_season_layers_2017_mfc2, fun=mean)

three_season_layers_2018_mfc2 <- lapply(names(ndvi_mfc2_2018), FUN = three_season)
means_2018_mfc2 <-  stackApply(ndvi_mfc2_2018, indices=three_season_layers_2018_mfc2, fun=mean)

three_season_layers_2019_mfc2 <- lapply(names(ndvi_mfc2_2019), FUN = three_season)
means_2019_mfc2 <-  stackApply(ndvi_mfc2_2019, indices=three_season_layers_2019_mfc2, fun=mean)

three_season_layers_2020_mfc2 <- lapply(names(ndvi_mfc2_2020), FUN = three_season)
means_2020_mfc2 <-  stackApply(ndvi_mfc2_2020, indices=three_season_layers_2020_mfc2, fun=mean)


# stack of NDVIs four years "GOR", each year 3 layers(wet, dry, transition)
means_gor <- stack(means_2017_gor, means_2018_gor, means_2019_gor, means_2020_gor)
names(means_gor) <- c("wet_17",  "transition_17", "dry_17",
                      "wet_18", "transition_18", "dry_18", 
                      "wet_19",  "transition_19", "dry_19",
                      "wet_20", "transition_20", "dry_20")

# stack of NDVIs four years "MFC2", each year 3 layers(wet, dry, transition)
means_mfc2 <- stack(means_2017_mfc2, means_2018_mfc2, means_2019_mfc2, means_2020_mfc2)
names(means_mfc2) <- c("wet_17",  "transition_17", "dry_17",
                       "wet_18",  "transition_18", "dry_18",
                       "wet_19", "transition_19", "dry_19", 
                       "wet_20", "transition_20", "dry_20" )



#### Dataframes saving   ####
site <-  "MFC2" #   "GOR"
# poly based on each study area
if (site == "MFC2"){
    poly <- fortify(aoi)
    names(poly)[1:2] <- c('x','y')
}else{
    poly <- fortify(aoi_2)
    names(poly)[1:2] <- c('x','y')
}

#Plot raster stacks in different seasons
mapTheme = rasterTheme(region = brewer.pal(10,"RdYlGn")) # Set raster theme (color etc.)
my.at=seq(-1,1,0.1)                               # Set colorkey/ colorbar sequence
myColorkey = list(at=my.at, space="bottom")        # Set colorkey/colorbar to the bottom


write_dir <- here("Desktop","Playground_dir_14","output")
x11()

pdf(here(write_dir, paste0(sprintf("Seasonal_ndvi_%s_over_years", tolower(site)), ".pdf")))

if (site=="MFC2"){
    print(levelplot(means_mfc2, main=sprintf("Seasonal NDVI %s over years ", site),  layout=c(3,4), par.settings = mapTheme,  colorkey=myColorkey, margin = F) + layer(sp.polygons(aoi, col = "black", lwd=0.1)))
} else {
    print(levelplot(means_gor, main=sprintf("Seasonal NDVI %s over years ", site),  layout=c(3,4),  par.settings = mapTheme,  colorkey=myColorkey, margin = F) + layer(sp.polygons(aoi_2, col = "black", lwd=0.1)))
}

dev.off()



# Stack rasters and then add them to brick
# turn the raster stack to raster brick
mfc2_NDVI_yrs_brick <- brick(mfc2_NDVI_yrs_stack)
gor1_NDVI_yrs_stack <- brick(gor1_NDVI_yrs_stack)



# Create dataframe out of the NDVI stack with 3 seasons and TA stack for "GOR1"

# Create a raster stack that has the mean NDVI for each pixel in each season
means_gor_wet <- stack(means_gor[[1]], means_gor[[4]], means_gor[[7]], means_gor[[10]])
means_gor_wet_calc <- calc(means_gor_wet, fun = mean)

means_gor_dry <- stack(means_gor[[3]], means_gor[[6]], means_gor[[9]], means_gor[[12]])
means_gor_dry_calc <- calc(means_gor_dry, fun = mean)

means_gor_transition <- stack(means_gor[[2]], means_gor[[5]], means_gor[[8]], means_gor[[11]])
means_gor_transition_calc <- calc(means_gor_transition, fun = mean)

# Create a raster stack that has mean NDVI for each pixel in each year
stack_gor_2017 <- stack(means_gor[[1]], means_gor[[2]], means_gor[[3]])
means_gor_2017 <- calc(stack_gor_2017, fun = mean)

stack_gor_2018 <- stack(means_gor[[4]], means_gor[[5]], means_gor[[6]])
means_gor_2018 <- calc(stack_gor_2018, fun = mean)

stack_gor_2019 <- stack(means_gor[[7]], means_gor[[8]], means_gor[[9]])
means_gor_2019 <- calc(stack_gor_2019, fun = mean)

stack_gor_2020 <- stack(means_gor[[10]], means_gor[[11]], means_gor[[12]])
means_gor_2020 <- calc(stack_gor_2020, fun = mean)

# stack to get a three layer NDVI to substitute the previous 12 layer one
means_gor_yearly_average <- stack(means_gor_wet_calc, means_gor_transition_calc, means_gor_dry_calc)
names(means_gor_yearly_average) <- c("avg_ndvi_wet", "avg_ndvi_trans", "avg_ndvi_dry")

# stack to get a four layer NDVI stack 
means_gor_avg_pixelwise <- stack(means_gor_2017, means_gor_2018, means_gor_2019, means_gor_2020)
names(means_gor_avg_pixelwise) <- c("avg_ndvi_gor_17", "avg_ndvi_gor_18",
                                    "avg_ndvi_gor_19", "avg_ndvi_gor_20")

# create one single raster layer of NDVI for "GOR1" by averaging the four year stack
gor_ndvi <- calc(means_gor_avg_pixelwise, fun = mean)
names(gor_ndvi) <- "average_NDVI_GOR1"


# to mask out the raster values outside the "GOR" site boundary
clipped_means_gor <- raster::mask(means_gor_yearly_average, aoi_2)
clipped_gor_yearly <- raster::mask(means_gor_avg_pixelwise, aoi_2)
clipped_gor <- raster::mask(gor_ndvi, aoi_2)

# to make the extent of two raster stacks the same
shared_extent <- extent(519180, 519890, 4461970, 4462830)

means_gor_crop <- crop(clipped_means_gor, shared_extent)
TA_gor1_crop <- crop(TA_gor1, shared_extent)
ndvi_gor_yearly_crop <- crop(clipped_gor_yearly, shared_extent) 
gor_ndvi_crop <- crop(clipped_gor, shared_extent)# one layer raster (NDVI average of all years for GOR1 pixel wise)


# convert the raster stacks to dataframes
df_gor_three_season <- as.data.frame(means_gor_crop)
df_gor_TA <- as.data.frame(TA_gor1_crop)
df_gor_yearly_ndvi <- as.data.frame(ndvi_gor_yearly_crop)
df_gor_17_ndvi <- as.data.frame(ndvi_gor_yearly_crop[[1]])
df_gor_18_ndvi <- as.data.frame(ndvi_gor_yearly_crop[[2]])
df_gor_19_ndvi <- as.data.frame(ndvi_gor_yearly_crop[[3]])
df_gor_20_ndvi <- as.data.frame(ndvi_gor_yearly_crop[[4]])
df_gor_ndvi <- as.data.frame(gor_ndvi_crop)

#remove rows with NA values
#df_gor_three_season <- df_gor_three_season[complete.cases(df_gor_three_season), ]
#df_gor_TA <- df_gor_TA[complete.cases(df_gor_TA), ]

df_gor_with_season <- cbind(df_gor_three_season, df_gor_TA)
matrix_gor_with_season <- as.matrix(df_gor_with_season)

df_gor_year_based <- cbind(df_gor_yearly_ndvi, df_gor_TA)
matrix_gor_year_based <- as.matrix(df_gor_year_based)

# Create matrix for each year separately
df_gor_17 <- cbind(df_gor_yearly_ndvi[-c(2:4)], df_gor_TA)
matrix_gor_2017 <- as.matrix(df_gor_17)

df_gor_18 <- cbind(df_gor_yearly_ndvi[-c(1,3:4)], df_gor_TA)
matrix_gor_2018 <- as.matrix(df_gor_18)

df_gor_19 <- cbind(df_gor_yearly_ndvi[-c(1:2,4)], df_gor_TA)
matrix_gor_2019 <- as.matrix(df_gor_19)

df_gor_20 <- cbind(df_gor_yearly_ndvi[-c(1:3)], df_gor_TA)
matrix_gor_2020 <- as.matrix(df_gor_20)

matrix_gor <- list(matrix_gor_2017, matrix_gor_2018, matrix_gor_2019, matrix_gor_2020)


# create matrix for from the NDVI average of all years for GOR1 and TAs
df_gor_avg_yrs <- cbind(df_gor_ndvi, df_gor_TA)
matrix_gor_avg_yrs <- as.matrix(df_gor_avg_yrs)




M_season <- cor(matrix_gor_with_season, use = "pairwise.complete.obs")
M_year <- cor(matrix_gor_year_based, use = "pairwise.complete.obs")
M_2017 <- cor(matrix_gor_2017, use = "pairwise.complete.obs")
M_2018 <- cor(matrix_gor_2018, use = "pairwise.complete.obs")
M_2019 <- cor(matrix_gor_2019, use = "pairwise.complete.obs")
M_2020 <- cor(matrix_gor_2020, use = "pairwise.complete.obs")
list_M <- list(M_2017, M_2018, M_2019, M_2020)
M_avg <- cor(matrix_gor_avg_yrs, use = "pairwise.complete.obs")



ouput_dir <- here("Desktop","Playground_dir_14", "output")

# save the matrix as text file
write.table(matrix_gor_with_season, file=file.path(ouput_dir, 
                                                   "matrix_correlation_seasonal_NDVI_TA_GOR1.txt"))
# save the corresponding corrplot() on drive            
pdf(file = file.path(ouput_dir, "matrix_correlation_seasonal_NDVI_TA_GOR1.pdf"),
    width = 10,
    height = 10)

corrplot::corrplot(M_season, method = "ellipse",
                   type="upper",
                   title= "GOR1 correlation matrix")

dev.off()

# save the matrix as text file & corrplots() for each year
for (i in c(1,2,3,4)){
    
    write.table(matrix_gor[[i]], file=file.path(ouput_dir, 
                                                sprintf("matrix_correlation_%s_NDVI_TA_GOR1.txt", 2016+i)))
    
    pdf(file = file.path(ouput_dir, sprintf("matrix_correlation_%s_NDVI_TA_GOR1.pdf", 2016+i)),
        width = 10,
        height = 10)
    
    corrplot::corrplot(list_M[[i]], method = "ellipse",
                       type="upper",
                       title= sprintf("GOR1 correlation matrix %s", 2016+i))
    
    dev.off()
}

# save the matrix as text file
write.table(matrix_gor_avg_yrs, file=file.path(ouput_dir, 
                                                   "matrix_correlation_avg_NDVI_TA_GOR1.txt"))
# save the corresponding corrplot() on drive            
pdf(file = file.path(ouput_dir, "matrix_correlation_avg_NDVI_TA_GOR1.pdf"),
    width = 10,
    height = 10)

corrplot::corrplot(M_avg, method = "square",
                   type="upper",
                   title= "GOR1 correlation matrix")

dev.off()

# Create dataframe out of the NDVI stack with 3 seasons and TA stack for "MFC2"

# create a raster stack that has the mean NDVI for each pixel in each season
means_mfc2_wet <- stack(means_mfc2[[1]], means_mfc2[[4]], means_mfc2[[7]], means_mfc2[[10]])
means_mfc2_wet_calc <- calc(means_mfc2_wet, fun = mean)

means_mfc2_dry <- stack(means_mfc2[[3]], means_mfc2[[6]], means_mfc2[[9]], means_mfc2[[12]])
means_mfc2_dry_calc <- calc(means_mfc2_dry, fun = mean)

means_mfc2_transition <- stack(means_mfc2[[2]], means_mfc2[[5]], means_mfc2[[8]], means_mfc2[[11]])
means_mfc2_transition_calc <- calc(means_mfc2_transition, fun = mean)


# Create a raster stack that has mean NDVI for each pixel in each year
stack_mfc2_2017 <- stack(means_mfc2[[1]], means_mfc2[[2]], means_mfc2[[3]])
means_mfc2_2017 <- calc(stack_mfc2_2017, fun = mean)

stack_mfc2_2018 <- stack(means_mfc2[[4]], means_mfc2[[5]], means_mfc2[[6]])
means_mfc2_2018 <- calc(stack_mfc2_2018, fun = mean)

stack_mfc2_2019 <- stack(means_mfc2[[7]], means_mfc2[[8]], means_mfc2[[9]])
means_mfc2_2019 <- calc(stack_mfc2_2019, fun = mean)

stack_mfc2_2020 <- stack(means_mfc2[[10]], means_mfc2[[11]], means_mfc2[[12]])
means_mfc2_2020 <- calc(stack_mfc2_2020, fun = mean)


# stack to get a three layer NDVI to substitute the previous 12 layer one
means_mfc2_yearly_average <- stack(means_mfc2_wet_calc, means_mfc2_transition_calc, means_mfc2_dry_calc)
names(means_mfc2_yearly_average) <- c("avg_ndvi_wet", "avg_ndvi_trans", "avg_ndvi_dry")

# stack to get a four layer NDVI stack 
means_mfc2_avg_pixelwise <- stack(means_mfc2_2017, means_mfc2_2018, means_mfc2_2019, means_mfc2_2020)
names(means_mfc2_avg_pixelwise) <- c("avg_ndvi_mfc2_17", "avg_ndvi_mfc2_18",
                                    "avg_ndvi_mfc2_19", "avg_ndvi_mfc2_20")


# create one single raster layer of NDVI for "MFC2" by averaging the four year stack
mfc2_ndvi <- calc(means_mfc2_avg_pixelwise, fun = mean)
names(mfc2_ndvi) <- "average_NDVI_MFC2"


# to mask out the raster values outside the "GOR" site boundary
clipped_means_mfc2 <- raster::mask(means_mfc2_yearly_average, aoi)
clipped_mfc2_yearly <- raster::mask(means_mfc2_avg_pixelwise, aoi)
clipped_mfc2 <- raster::mask(mfc2_ndvi, aoi)

# to make the extent of two raster stacks the same
shared_extent_2 <- extent(515380, 516010, 4468070, 4468570)

means_mfc2_crop <- crop(clipped_means_mfc2, shared_extent_2)
TA_mfc2_crop <- crop(TA_mfc2, shared_extent_2)
ndvi_mfc2_yearly_crop <- crop(clipped_mfc2_yearly, shared_extent_2) 
mfc2_ndvi_crop <- crop(clipped_mfc2, shared_extent_2)# one layer raster (NDVI average of all years for GOR1 pixel wise)


# remove the artifacts from TA raster stack (to get to the same situation as NDVI stack)
TA_mfc2_crop_wo_artifact  <-  TA_mfc2_crop
for (i in 1:nlayers(TA_mfc2_crop)) {
    
    r1 <- TA_mfc2_crop[[i]]
    r1[artifact_mfc2_new] <- 94
    r1[artifact2_mfc2_new] <- 94
    names(r1) <- names(TA_mfc2_crop[[i]])
    rna <- reclassify(r1, cbind(94, NA))
    TA_mfc2_crop_wo_artifact [[i]]  <-  rna
    
}
TA_mfc2_crop <- TA_mfc2_crop_wo_artifact

# convert the raster stacks to dataframes
df_mfc2_three_season <- as.data.frame(means_mfc2_crop)
df_mfc2_TA <- as.data.frame(TA_mfc2_crop)
df_mfc2_yearly_ndvi <- as.data.frame(ndvi_mfc2_yearly_crop)
df_mfc2_17_ndvi <- as.data.frame(ndvi_mfc2_yearly_crop[[1]])
df_mfc2_18_ndvi <- as.data.frame(ndvi_mfc2_yearly_crop[[2]])
df_mfc2_19_ndvi <- as.data.frame(ndvi_mfc2_yearly_crop[[3]])
df_mfc2_20_ndvi <- as.data.frame(ndvi_mfc2_yearly_crop[[4]])
df_mfc2_ndvi <- as.data.frame(mfc2_ndvi_crop)


#remove rows with NA values
#df_mfc2_three_season <- df_mfc2_three_season[complete.cases(df_mfc2_three_season), ]
#df_mfc2_TA <- df_mfc2_TA[complete.cases(df_mfc2_TA), ]


df_mfc2_with_season <- cbind(df_mfc2_three_season, df_mfc2_TA)
matrix_mfc2_with_season <- as.matrix(df_mfc2_with_season)


df_mfc2_year_based <- cbind(df_mfc2_yearly_ndvi, df_mfc2_TA)
matrix_mfc2_year_based <- as.matrix(df_mfc2_year_based)

# Create matrix for each year separately
df_mfc2_17 <- cbind(df_mfc2_yearly_ndvi[-c(2:4)], df_mfc2_TA)
matrix_mfc2_2017 <- as.matrix(df_mfc2_17)

df_mfc2_18 <- cbind(df_mfc2_yearly_ndvi[-c(1,3:4)], df_mfc2_TA)
matrix_mfc2_2018 <- as.matrix(df_mfc2_18)

df_mfc2_19 <- cbind(df_mfc2_yearly_ndvi[-c(1:2,4)], df_mfc2_TA)
matrix_mfc2_2019 <- as.matrix(df_mfc2_19)

df_mfc2_20 <- cbind(df_mfc2_yearly_ndvi[-c(1:3)], df_mfc2_TA)
matrix_mfc2_2020 <- as.matrix(df_mfc2_20)

matrix_mfc2 <- list(matrix_mfc2_2017, matrix_mfc2_2018, matrix_mfc2_2019, matrix_mfc2_2020)

# create matrix for from the NDVI average of all years for GOR1 and TAs
df_mfc2_avg_yrs <- cbind(df_mfc2_ndvi, df_mfc2_TA)
matrix_mfc2_avg_yrs <- as.matrix(df_mfc2_avg_yrs)



M2_season <- cor(matrix_mfc2_with_season, use = "pairwise.complete.obs")
M2_year <- cor(matrix_mfc2_year_based, use = "pairwise.complete.obs")
M2_2017 <- cor(matrix_mfc2_2017, use = "pairwise.complete.obs")
M2_2018 <- cor(matrix_mfc2_2018, use = "pairwise.complete.obs")
M2_2019 <- cor(matrix_mfc2_2019, use = "pairwise.complete.obs")
M2_2020 <- cor(matrix_mfc2_2020, use = "pairwise.complete.obs")
list_M2 <- list(M2_2017, M2_2018, M2_2019, M2_2020)
M2_avg <- cor(matrix_mfc2_avg_yrs, use = "pairwise.complete.obs")



# save the matrix as text file
write.table(matrix_mfc2_year_based, file=file.path(ouput_dir, "matrix_correlation_seasonal_NDVI_TA_MFC2.txt"))

ouput_dir <- here("Desktop","Playground_dir_14", "output")

pdf(file = file.path(ouput_dir, "matrix_correlation_seasonal_NDVI_TA_MFC2.pdf"),
    width = 10,
    height = 10)

corrplot::corrplot(M2_season, method = "ellipse",
         type="upper",
         title= "MFC2 correlation matrix")


dev.off()


for (i in c(1,2,3,4)){
    
    write.table(matrix_mfc2[[i]], file=file.path(ouput_dir, 
                                                sprintf("matrix_correlation_%s_NDVI_TA_MFC2.txt", 2016+i)))
    
    pdf(file = file.path(ouput_dir, sprintf("matrix_correlation_%s_NDVI_TA_MFC2.pdf", 2016+i)),
        width = 10,
        height = 10)
    
    corrplot::corrplot(list_M2[[i]], method = "ellipse",
                       type="upper",
                       title= sprintf("MFC2 correlation matrix %s", 2016+i))
    
    dev.off()
}

# save the matrix as text file
write.table(matrix_mfc2_avg_yrs, file=file.path(ouput_dir, 
                                               "matrix_correlation_avg_NDVI_TA_MFC2.txt"))
# save the corresponding corrplot() on drive            
pdf(file = file.path(ouput_dir, "matrix_correlation_avg_NDVI_TA_MFC2.pdf"),
    width = 10,
    height = 10)

corrplot::corrplot(M2_avg, method = "square",
                   type="upper",
                   title= "MFC2 correlation matrix")

dev.off()



# Method 1 : correlation only for 2 rasters
#Calculate local correlation of two rasters using focal 
# for this part : https://statnmap.com/2018-01-27-spatial-correlation-between-rasters/
# resample
ndvi_raster <- resample(means_mfc2[[1]], TA_mfc2[[1]], method = 'bilinear')#means_mfc2[[1]]: NDVI in 2017 of mfc2 in wet season
st_1 <- stack(ndvi_raster, TA_mfc2)
st_nb <- raster(st_1, 1)
values(st_nb) <- 1:ncell(st_1)

matrix_st <- values(st_1) # stack as raster 

focal_cor <- raster::focal(
    x = st_nb,
    w = matrix(1, 5, 5),
    fun = function(x, y = matrix_st){ 
        cor(y[x, 1], y[x, 2],
            use = "na.or.complete")
    },
    filename = file.path(output_dir_mfc2, "focal_cor.tif"),
    overwrite = TRUE
)

#Load the "focal_cor.tif" for visualizing the correlation
focal_cor <- raster(file.path(output_dir_mfc2, "focal_cor.tif"))


# Plot the correlation
gplot(focal_cor) + geom_tile(aes(fill = value)) +
    #facet_wrap(~ variable) +
    scale_fill_gradient(low = 'red', high = 'green') +
    ggtitle(sprintf("Correlation between %s & %s",
                    deparse(substitute(Slope10m_mfc2)),
                    deparse(substitute(ndvi_RS)) 
    ))+
    coord_equal()





## Method 2: correlation
# Convert the raster file to matrix for further analysis
#dem matrix of mfc2
aa <- raster::as.matrix(dem_mfc2_10m)


# AAC10m_mfc2 matrix of mfc2
bb <- raster::as.matrix(AAC10m_mfc2)

ndvi_mfc2_2020 <- readRDS(file= file.path(output_dir_mfc2, "Extracted_dfs", "ndvi_stack_2020_mfc2"))

#ndvi matrix of MFC2
cc <- raster::as.matrix(ndvi_mfc2_2020[[1]])

# Here as a dry practice for correlation, I will calculate the correlation between the matrices
# to make the dimensions of matrices to match
new_aa <- aa[1:50, 2:64]
cor_cal  <- cor(new_aa, cc)


# Method 3: correlation
# resample
ndvi_RS <- resample(ndvi_mfc2_2020[[1]], AAC10m_mfc2, method = 'bilinear')

#stack raster layers
st <- stack(dem_mfc2_10m, AAC10m_mfc2, Slope10m_mfc2, Aspect10m_mfc2, PlanCurv10m_mfc2, 
            ProfileCurv10m_mfc2, TRI10m_mfc2, TWI10m_mfc2, FlowAcc10m_mfc2, ndvi_RS)

#subsample 5% of pixels and calculate pairwise correlations
cor<- cor(sampleRandom(st, size= ncell(dem_mfc2_10m) * 0.05 ), method = "spearman")

#plot correlation matrix
df <- corrplot::corrplot(cor, method = "number")



# Method 4: correlation
# calculating correlation of raster stack 
st <- stack(dem_mfc2_10m, AAC10m_mfc2, Slope10m_mfc2, Aspect10m_mfc2, PlanCurv10m_mfc2, 
            ProfileCurv10m_mfc2, TRI10m_mfc2, TWI10m_mfc2, FlowAcc10m_mfc2, ndvi_RS)
plot(st[[2]])
plot(st[[10]])
x <- corLocal(st[[4]], st[[10]], test=TRUE )

plot(x)


# Method 5: correlation
# correlation of 2 raster stacks by Robert Hijmann
# source :https://stackoverflow.com/questions/16698048/r-calculating-pearson-correlation-coefficient-in-each-cell-along-time-line

library(raster)
#mfc2_NDVI_yrs_stack
#TA_mfc2
resample_TA_mfc2 <- resample(TA_mfc2, mfc2_NDVI_yrs_stack)
s_mfc2 <- stack(mfc2_NDVI_yrs_stack, resample_TA_mfc2)

#gor1_NDVI_yrs_stack
#TA_gor1
resample_TA_gor1 <- resample(TA_gor1, gor1_NDVI_yrs_stack)


funcal <- function(xy) {
    xy <- na.omit(matrix(xy, ncol=2))
    if (ncol(xy) < 2) {
        NA
    } else {
        cor(xy[, 1], xy[, 2])
    }
}

s <- stack(sa, sb)
calc(s, funcal)







# another method for correlation of raster stack 
# source :https://gist.github.com/bennyistanto/1a691954c68d1bc11d2fb1c4f2df2b6d
# Gridcorts Function

gridcorts <- function(rasterstack, method, type=c("corel","pval","both")){
    # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
    # e.g. nlayers(rasterstack), ncell(rasterstack)... etc.
    print(paste("Start Gridcorts:",Sys.time()))
    print("Loading parameters")
    layers=nlayers(rasterstack);ncell=ncell(rasterstack);
    ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
    extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
    print("Done loading parameters")
    mtrx <- as.matrix(rasterstack,ncol=layers)
    empt <- matrix(nrow=ncell, ncol=2)
    print("Initiating loop operation")
    if (type == "corel"){
        for (i in 1:ncell){
            setTxtProgressBar(pb,i)
            if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
                empt[i,1] <- NA 
            } else 
                if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
                    empt[i,1] <- NA 
                } else 
                    empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate)
        }
        print("Creating empty raster")
        corel <- raster(nrows=nrow,ncols=ncol,crs=crs)
        extent(corel) <- extent
        print("Populating correlation raster")
        values(corel) <- empt[,1]
        print(paste("Ending Gridcorts on",Sys.time()))
        corel
    } 
    else
        if (type == "pval"){
            for (i in 1:ncell){
                setTxtProgressBar(pb,i)
                if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
                    empt[i,2] <- NA 
                } else 
                    if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
                        empt[i,2] <- NA 
                    } else 
                        empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
            }
            pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
            extent(pval) <- extent
            print("Populating significance raster")
            values(pval) <- empt[,2]
            print(paste("Ending Gridcorts on",Sys.time()))
            pval
        }
    else
        if (type == "both"){
            for (i in 1:ncell){
                setTxtProgressBar(pb,i)
                if (all(is.na(mtrx[i,1:(layers/2)])) | all(is.na(mtrx[i,((layers/2)+1):layers]))){ 
                    empt[i,] <- NA 
                } else 
                    if (sum(!is.na(mtrx[i,1:(layers/2)]/mtrx[i,((layers/2)+1):layers])) < 4 ){
                        empt[i,] <- NA 
                    } else {
                        empt[i,1] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$estimate) 
                        empt[i,2] <- as.numeric(cor.test(mtrx[i,1:(layers/2)], mtrx[i,((layers/2)+1):layers],method=method)$p.value)
                    }
            }
            c <- raster(nrows=nrow,ncols=ncol,crs=crs)
            p <- raster(nrows=nrow,ncols=ncol,crs=crs)
            print("Populating raster brick")
            values(c) <- empt[,1]
            values(p) <- empt[,2]
            brk <- brick(c,p)
            extent(brk) <- extent
            names(brk) <- c("Correlation","Pvalue")
            print(paste("Ending Gridcorts on",Sys.time()))
            brk
        }
}
correlation <- gridcorts(rasterstack = st, method = "pearson", type = "corel")

plot(correlation)












#3d dem
plot3D(dem, col = rainbow) 
## or with a custom function using colorRampPalette
myPal <- colorRampPalette(RColorBrewer::brewer.pal(11, 'PuOr'))
plot3D(dem, col = myPal)


#Slope
slope <-  terrain(dem, opt = "slope", unit = "degrees")
plot(slope)


# Aspect
aspect <-  terrain(dem, opt = "aspect")
plot(aspect)

# Hillshade
hill_dem <- hillShade(slope, aspect, 45, 270)
plot(hill_dem, col=grey(0:100/100))
plot(hill_dem)

