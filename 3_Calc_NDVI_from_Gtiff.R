#############################
# The following snippet calculates and writes NDVI and RGB on the drive
# The code also intends to select the appropriate tiles(no cloud cover over study areas) based on RGB-10m
############################

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet","rasterVis","gridExtra","RColorBrewer","plotly","RStoolbox","sp","IRdisplay","reshape","here"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)

## set working directory
#setwd("C:/Users/sanaz/Documents/MB12-project/CREODIAS_part/data_from_CREODIAS/L2A_2017")#("C:/Users/sanaz/Desktop/Playground_dir_2")# 
setwd("C:/Users/sanaz/")
setwd(here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017"))

#Create "NDVI" and "RGB" in your working directory
dir.create(path = "NDVI")
dir.create(path = "NDVI/MFC2")
dir.create(path = "NDVI/GOR")
dir.create(path = "RGB")

#2: Load Auxillary data
### Define your area of interest (aoi), which is MFC2 (bacino_MFC_corrected) or bounding_box_MFC or else #
aoi <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site1_MFC2_agroforestry/MFC2.shp")
aoi_2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site2_GOR_forest/Site2_GOR_forest/GOR.shp")
#points_in_MFC2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Points_in_MFC2.shp")


# Load S2 tiles (attention to ^ in pattern)
S2_names <- here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2020") #"C:/Users/sanaz/Desktop/Playground_dir_2"
S2_names_1 <- list.files(S2_names,recursive = FALSE, full.names = TRUE, pattern="S2[A,B]_MSIL2A_[[:alnum:]]{15}_[[:alnum:]]{5}_[[:alnum:]]{4}_[[:alnum:]]{6}_[[:alnum:]]{15}.SAFE$")#S2[A,B]
S2_names_T <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, pattern="^[T][[:alnum:]]{5}_[[:alnum:]]{15}_B0[2348]_10m.tif$")
S2_names_L2A_v1 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, pattern="L2A_T[[:alnum:]]{5}_[[:alnum:]]{15}_B0[2348]_10m.tif$")
S2_names <- lapply(1:length(S2_names_T), function(x){raster(S2_names_T[x])})                                                      
S2_names_L2A <- lapply(1:length(S2_names_L2A_v1), function(x){raster(S2_names_L2A_v1[x])})


b <- as(extent(515379.3,519889,4461970.6,4468567.9), 'SpatialPolygons')
crs(b) <- crs(S2_names[[1]])
num_loop <- length(S2_names_T) / 4
rgb_dir <- here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2020","RGB")
ndvi_dir <- here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017","NDVI")

# The loop calculates NDVI for tiles in which names of 10m bands start with "T"
for(i in 1:num_loop){
  # Stack all the .tiff bands(10m)
  S2_names_indexed <- S2_names[((i-1)*4+1):((i-1)*4+4)]
  S2_stack <- stack(S2_names_indexed)
  
  # crop around the study areas
  S2_stack_crop_bbox <- crop(S2_stack , b)
  
  
  # Derive NDVI 
  NDVI <- list()
  NDVI <- overlay(x=S2_stack_crop_bbox[[3]], y=S2_stack_crop_bbox[[4]], fun=function(x,y){(y-x)/(y+x)})
  names(NDVI) <- paste0("NDVI_",unlist(strsplit(strsplit(S2_names_T[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1]))
  #names(NDVI) <- paste0("NDVI_", unlist(strsplit(strsplit(strsplit(S2_names_T[(i-1)*4+1],"//")[[1]][8],'/')[[1]][2],"[.]"))[1])
  
  
  
  # Export the NDVI raster
  name <- names(NDVI)
  filename=file.path(ndvi_dir, name)
  writeRaster(x = NDVI,
              filename = filename,
              format = "GTiff", # save as a tif
              datatype='FLT4S', 
              progress='text',
              overwrite = TRUE)
  
  
  rgb_10m <- list()
  rgb_10m <- stack(S2_stack_crop_bbox[[1]],S2_stack_crop_bbox[[2]],S2_stack_crop_bbox[[3]])
  rgb_name <- paste0("RGB_10m_",unlist(strsplit(strsplit(S2_names_T[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1]))
  #rgb_name <- paste0("RGB_10m_", unlist(strsplit(strsplit(strsplit(S2_names_T[(i-1)*4+1],"//")[[1]][8],'/')[[1]][2],"[.]"))[1])
  
  
  
  # Export the RGB_10m raster
 name <- rgb_name
 filename=file.path(rgb_dir, rgb_name)
 writeRaster(x = rgb_10m,#if i doesn't work , replace with 1
              filename = filename,
              format = "GTiff", # save as a tif
              datatype='FLT4S', 
              progress='text',
              overwrite = TRUE)
  
}



num_loop_L2A <- length(S2_names_L2A) / 4
# This loop calculates NDVI for tiles in which names of 10m bands start with "L2A"
for(i in 1:num_loop_L2A){
  # Stack all the .tiff bands(10m)
  S2_names_indexed <- S2_names_L2A[((i-1)*4+1):((i-1)*4+4)]
  S2_stack <- stack(S2_names_indexed)
  
  # crop around the study areas
  S2_stack_crop_bbox <- crop(S2_stack , b)
  
  # Derive NDVI 
  NDVI <- list()
  NDVI <- overlay(x=S2_stack_crop_bbox[[((1-1)*4+3)]], y=S2_stack_crop_bbox[[((1-1)*4+4)]], fun=function(x,y){(y-x)/(y+x)})
  names(NDVI) <- paste0("NDVI_",unlist(strsplit(strsplit(S2_names_L2A_v1[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1]))
  #names(NDVI) <- paste0("NDVI_", unlist(strsplit(strsplit(strsplit(S2_names_L2A_v1[(i-1)*4+1],"//")[[1]][8],'/')[[1]][2],"[.]"))[1])
  
  
  # Export the raster
  name <- names(NDVI)
  filename=file.path(ndvi_dir, name)
  writeRaster(x = NDVI,
              filename = filename,
              format = "GTiff", # save as a tif
              datatype='FLT4S', 
              progress='text',
              overwrite = TRUE) 
  
  # Derive RGB
  rgb_10m <- list()
  rgb_10m <- stack(S2_stack_crop_bbox[[((1-1)*4+1)]],S2_stack_crop_bbox[[((1-1)*4+2)]],S2_stack_crop_bbox[[((1-1)*4+3)]])
  rgb_name <- paste0("RGB_10m_",unlist(strsplit(strsplit(S2_names_L2A_v1[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1]))
  
  
  # Export the RGB_10m raster
  name <- rgb_name
  filename=file.path(rgb_dir, rgb_name)
  writeRaster(x = rgb_10m,
              filename = filename,
              format = "GTiff", # save as a tif
              datatype='FLT4S', 
              progress='text',
              overwrite = TRUE) 
  
}


# RGB (for selecting the appropriate tiles for the analysis)
# To choose which tiles to use for analysis based on cloud cover(use of 10m resolution) 
# Read the RGBs and stack them
rgb_dir <- rgb_dir #"C:/Users/sanaz/Documents/MB12-project/Outputs/RGB_10m"
rgb_list <- list.files(rgb_dir,recursive = TRUE, full.names = TRUE, pattern="^RGB_")
rgb_ras <- lapply(1:length(rgb_list), function(x){brick(rgb_list[x])}) 

#define and intialize the select_10m
select_10m <- rep(TRUE, 63)#number varies depend on the number of tiles


# following params are for plotting inside the loop
poly <- fortify(aoi)
poly2 <- fortify(aoi_2)
names(poly)[1:2] <- c('x','y')
names(poly2)[1:2] <- c('x2','y2')
e <- extent(rgb_ras[[1]])

for(i in 1:length(rgb_list)){
  print(ggplot() + ggRGB(img = rgb_ras[[i]],
                         r = 3,
                         g = 2,
                         b = 1,
                         stretch = 'hist',
                         ggLayer = T) + 
          xlim(e[1:2]) + ylim(e[3:4]) + 
          geom_path(aes(x,y), alpha = 0.9, colour = "red" ,data = poly)+
          geom_path(aes(x2,y2), alpha = 0.9,  colour = "red",data = poly2)+
          coord_equal())
  
  select_10m[i] <- as.logical(readline("Select? ")) #c(select_10m, as.logical(readline("Select? ")))# F: FALSE, T: TRUE
}

#save the "select_10m" on the drive("select_10m" based on RGB_10m)
saveRDS(select_10m, file = here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2020","RGB","select10m_2020.Rds"))



#Load the select_10m.Rds ("select_10m" based on "RGB_10m")
select_10m <- readRDS(file = here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017","RGB","select10m.Rds")) 

# Read the NDVIs in and stack them 
ndvi_dir <- ndvi_dir #"C:/Users/sanaz/Documents/MB12-project/Outputs/NDVI"
ndvi_list <- list.files(ndvi_dir, full.names = TRUE, pattern="^NDVI_")#recursive = TRUE,


# select tiles based on select_10m
ndvi_list_df <- as.data.frame(ndvi_list)
select_10m_df <- as.data.frame(select_10m)
ndvi_list_selected <- cbind(ndvi_list_df , select_10m_df)
ndvi_list_selected<- ndvi_list_selected %>% filter(select_10m == TRUE)

# stack based on the selected ndvi list
ndvi_stack <- stack(ndvi_list_selected$ndvi_list)

# Investigate metadata of NDVI stack
#ext_ndvi <- extent(ndvi_stack)
#crs_ndvi_stack <- crs(ndvi_stack)
#y_res_ndvi_stack <- yres(ndvi_stack)
#x_res_ndvi_stack <- xres(ndvi_stack)

# Plotting the NDVI time series
ndvi_stack_df <- as.data.frame(ndvi_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

# Create a separate panel for each time point in the time series
ggplot() +
  geom_raster(data = ndvi_stack_df , aes(x = x, y = y, fill = value)) +
  geom_path(aes(x,y), alpha = 0.9, colour = "black" ,data = poly)+
  geom_path(aes(x2,y2), alpha = 0.9,  colour = "black",data = poly2)+
  facet_wrap(~ variable) +
  #scale_fill_gradientn(colours = terrain.colors(10))+#scale_fill_gradient2( low = "red",mid = "white",high = "green",midpoint = 0)#(high = "#CEE50E", low = "#087F28",name = "NDVI")
  #scale_fill_gradient2(low = "red",
  #mid = "white",
  #high = "green",
  #midpoint = 0)+
  scale_fill_distiller(palette ="RdYlGn", direction = 1)+plot_annotation(
    title = "NDVI 2017"
  )+
  #theme(panel.grid.major = element_line(colour = 'transparent')) +
  coord_equal()

ggsave(here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017","ggplot_ndvi_selected.png"), scale = 3, dpi = 300)

# View Distribution of Raster Values (Histogram)
ggplot(ndvi_stack_df) +
  geom_histogram(aes(value)) +
  plot_annotation(
    title = "Histogram 2017"
  )+
  facet_wrap(~variable)

ggsave(here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017","gghistogram_selected.png"), scale = 3, dpi = 300)


# Calculate Average NDVI

# calculate mean NDVI for each raster
avg_NDVI_stack <- cellStats(ndvi_stack,mean)

# convert output array to data.frame
avg_NDVI_stack <- as.data.frame(avg_NDVI_stack)

# view column name slot
names(avg_NDVI_stack)

# rename the NDVI column
names(avg_NDVI_stack) <- "meanNDVI"

# add a site column to our data
avg_NDVI_stack$site <- "MFC2_GOR_Italy_2017"

# # note the use of the vertical bar character ( | ) is equivalent to "or". This
# allows us to search for more than one pattern in our text strings.
sentinelDates <- gsub("NDVI_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", "", row.names(avg_NDVI_stack))


# plot NDVI
ggplot(avg_NDVI_stack, aes(sentinelDates, meanNDVI), na.rm=TRUE) +
  geom_point(size=4,colour = "PeachPuff4", aes(group=site)) + 
  geom_line(aes(group=site)) +
  ggtitle("Sentinel NDVI \n MFC2, GOR Field Sites for 2017") +
  xlab("Sentinel dates") + ylab("Mean NDVI") +
  theme(text = element_text(size=10))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(here("MB12-project","CREODIAS_part","data_from_CREODIAS","L2A_2017","ndvi_series_selected_2017.png"), scale = 3, dpi = 300)





# Set the layout(define the dimensions of plot)
#options(repr.plot.width = 600, repr.plot.height = 600)
# Create custom color
#breaks <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
#pal <- brewer.pal(11,"RdYlGn")
#mapTheme <- rasterTheme(region= pal)
#mapTheme$fontsize$text = 10
#NDVI as levelplot
#levelplot(ndvi_stack, scales= list(draw=FALSE), colorkey= FALSE , par.settings= mapTheme)


#Statistics summary of NDVI stack
#cellStats(ndvi_stack[[1]],'mean')
#x1 <- calc(ndvi_stack, mean)
# Calculate mean
#r_mean <- calc(ndvi_stack, mean)
# Calculate median
#r_median <- calc(ndvi_stack, median)
# Calculate sd
#r_sd <- calc(ndvi_stack, sd)


# Box plot of the raster
boxplot(ndvi_stack)




#jpeg(filename="NDVIplot_20191129_clip.jpg", width = 500, height = 500)

#plot(ndvi_20191129_clip)

#dev.off()



#jpeg(filename="HIST_NDVI_20181129.jpg", width = 500, height = 500)

#hist(ndvi_20181129_clip, main="Histogram NDVI 20181129 MFC2/Tre Morene", xlab="NDVI")

#dev.off()
