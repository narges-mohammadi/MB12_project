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


# NDWI stacks (created in "16_NDWI.R" script)
# MFC2
ndwi_mfc2_2017 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndwi_stack_MFC2_2017"))

ndwi_mfc2_2018 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndwi_stack_MFC2_2018"))

ndwi_mfc2_2019 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndwi_stack_MFC2_2019"))

ndwi_mfc2_2020 <- readRDS(file= file.path(output_dir_mfc2, 
                                          "Extracted_dfs", "selected_ndwi_stack_MFC2_2020"))

mfc2_ndwi_yrs_stack <- stack(ndwi_mfc2_2017, ndwi_mfc2_2018, ndwi_mfc2_2019, ndwi_mfc2_2020)

# GOR
ndwi_gor1_2017 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndwi_stack_GOR_2017"))

ndwi_gor1_2018 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndwi_stack_GOR_2018"))

ndwi_gor1_2019 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndwi_stack_GOR_2019"))

ndwi_gor1_2020 <- readRDS(file= file.path(output_dir_gor1, 
                                          "Extracted_dfs", "selected_ndwi_stack_GOR_2020"))

gor1_ndwi_yrs_stack <- stack(ndwi_gor1_2017, ndwi_gor1_2018, ndwi_gor1_2019, ndwi_gor1_2020)



# created in "16_NDWI.R" script
read_dir_ndwi <- here("Desktop","Playground_dir_8", "NDWI", "output")

site2 <- "GOR"
year1 <- 2017
ndvi_df_gor_2017 <- readRDS(file = file.path(read_dir_ndwi, 
                                             paste0("avg_NDWI_stack_", 
                                                    site2,"_", year1)))
