#https://eos.com/ndwi/
###################
# The following code consiste of 2 functions 
# First is used to calculate and saves the NDWIs for different years & sites 
# NDWI is another important VI that measures the liquid water content in canopy 
# that interacts with incoming solar radiation (Gao, 1996).
# Second is for ploting histogram and time series and saving them 
# This code snippet is equivalent for "9_Ndvi_from_Gtiff_Function" for NDWI
##################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr", "rgdal", "ggplot2", "raster",
          "leaflet", "rasterVis","gridExtra", "RColorBrewer",
          "plotly", "RStoolbox", "sp", "sf", "IRdisplay", "reshape", 
          "here", "patchwork", "tidyverse", "cowplot"))
new_pck <- pck[!pck %in% installed.packages()[, "Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck, require, character.only=TRUE)


#2: Load Auxillary data
### Define your area of interest (aoi), which is MFC2 (bacino_MFC_corrected) or bounding_box_MFC or else #
aoi <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site1_MFC2_agroforestry/MFC2.shp")
aoi_2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site2_GOR_forest/Site2_GOR_forest/GOR.shp")
artifact_mfc2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/QGIS_part/parking_lot.shp")
artifact2_mfc2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/QGIS_part/house.shp")

# reproject data
artifact_mfc2_new <- spTransform(artifact_mfc2, crs(aoi))
artifact2_mfc2_new <- spTransform(artifact2_mfc2, crs(aoi))

MFC2_bbox <- as(extent(515379.3, 516012.9, 4468068.3, 4468567.9), 'SpatialPolygons')
crs(MFC2_bbox) <- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
GOR_bbox <- as(extent(519177.4, 519889, 4461970.6, 4462834), 'SpatialPolygons')
crs(GOR_bbox) <- crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")


ndwi_crop_dir <- here("Desktop", "Playground_dir_15")

# This function calculates NDWI for the specific year and study area & writes them on the drive
write_NDWI_site_year <- function(site, year){
    site <- toupper(site)
    # Load S2 tiles 
    year_dir <- paste0("L2A_",year)
    S2_names <- here("Documents","MB12-project","CREODIAS_part",
                     "data_from_CREODIAS", year_dir)
    
    S2_names_1 <- list.files(S2_names, recursive = FALSE, full.names = TRUE, 
                             pattern="*.SAFE$")#S2[A,B]_MSIL2A_[[:alnum:]]{15}_[[:alnum:]]{5}_[[:alnum:]]{4}_[[:alnum:]]{6}_[[:alnum:]]{15}.SAFE$
    # B8
    S2_names_B8_T <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                             pattern="^[T][[:alnum:]]{5}_[[:alnum:]]{15}_B08_10m.tif$")
    S2_names_B8_L2A_v1 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                                  pattern="L2A_T[[:alnum:]]{5}_[[:alnum:]]{15}_B08_10m.tif$")
    S2_names_8 <- lapply(1:length(S2_names_B8_T), function(x){raster(S2_names_B8_T[x])})                                                      
    if(length(S2_names_B8_L2A_v1)){S2_names_L2A_v1_8 <- lapply(1:length(S2_names_B8_L2A_v1), function(x){raster(S2_names_B8_L2A_v1[x])})}
    
    #T33TWE_20180523T095031_B11_20m
    
    # B11
    S2_names_B11_T <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                                pattern="T33TWE_[[:alnum:]]{15}_B11_resampled.tif$")#for 2017: .tif
    S2_names_B11_L2A_v1 <- list.files(S2_names_1, recursive = TRUE, full.names = TRUE, 
                                     pattern="L2A_T[[:alnum:]]{5}_[[:alnum:]]{15}_B11_resampled.tif$")
    #this one has problem(character(0))
    S2_names_11 <- lapply(1:length(S2_names_B11_T), function(x){raster(S2_names_B11_T[x])})                                                      
    if(length(S2_names_B8_L2A_v1)){S2_names_L2A_v1_11 <- lapply(1:length(S2_names_B11_L2A_v1), function(x){raster(S2_names_B11_L2A_v1[x])})}
    
    num_loop <- length(S2_names_B8_T)
    site_dir <- site
    
    # The loop calculates NDWI for tiles in which names of 10m bands start with "T"
    for(i in 1:num_loop){
        # Stack all the .tiff bands(10m)
        S2_stack <- stack(S2_names_B8_T[[i]], S2_names_B11_T[[i]] )

        # crop around each study area
        if (site == "MFC2"){
            crop_box <- MFC2_bbox
        }else{
            crop_box <- GOR_bbox
        }

        S2_stack_site_bbox <- crop(S2_stack , crop_box)

        # Derive NDWI
        NDWI_list <- list()

        NDWI_site <- overlay(x=S2_stack_site_bbox[[2]], y=S2_stack_site_bbox[[1]], 
                             fun=function(x,y){(y-x)/(y+x)})

        names(NDWI_site) <- paste0("NDWI_", tolower(site),"_",
                                   unlist(strsplit(strsplit(S2_names_B8_T[i],'/')[[1]][9],'[.]')[[1]][1]))

        # Export the NDWI raster
        name <- names(NDWI_site)
        if(!dir.exists(file.path(ndwi_crop_dir, site))){dir.create(file.path(ndwi_crop_dir, site))}
        if(!dir.exists(file.path(ndwi_crop_dir, site, as.character(year)))){dir.create(file.path(ndwi_crop_dir, site, as.character(year)))}
        write_dir <- file.path(ndwi_crop_dir, site, as.character(year))
        filename <- file.path(write_dir, name)


        writeRaster(x = NDWI_site,
                    filename = filename,
                    format = "GTiff", # save as a tif
                    datatype='FLT4S',
                    progress='text',
                    overwrite = TRUE
        )


 }
    
    if(length(S2_names_B8_L2A_v1)){
        
        num_loop_L2A <- length(S2_names_B8_L2A_v1)
        
        # This loop calculates NDWI for tiles in which names of 10m bands start with "L2A"
        for(i in 1:num_loop_L2A){
            # Stack all the .tiff bands(10m)
            S2_stack <- stack(S2_names_B8_L2A_v1[[i]], S2_names_B11_L2A_v1[[i]] )
            
            # crop around each study area
            if (site == "MFC2"){
                crop_box <- MFC2_bbox
            }else{
                crop_box <- GOR_bbox
            }
            
            S2_stack_site_bbox <- crop(S2_stack , crop_box)
            
            # Derive NDWI 
            NDWI_list <- list()
            
            NDWI_site <- overlay(x=S2_stack_site_bbox[[2]], 
                                 y=S2_stack_site_bbox[[1]], 
                                 fun=function(x,y){(y-x)/(y+x)})
            
            names(NDWI_site) <- paste0("NDWI_", tolower(site),"_",
                                       unlist(strsplit(strsplit(S2_names_B8_L2A_v1[i],'/')[[1]][9],'[.]')[[1]][1]))
            #paste(strsplit(strsplit(strsplit(S2_names_L2A_v1[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1], "_")[[1]][3], strsplit(strsplit(strsplit(S2_names_L2A_v1[(i-1)*4+1],'/')[[1]][9],'[.]')[[1]][1], "_")[[1]][4],sep="_"))
            
            
            
            name <- names(NDWI_site)
            ndwi_crop_dir <- here("Desktop","Playground_dir_15")
            if(!dir.exists(file.path(ndwi_crop_dir, site))){dir.create(file.path(ndwi_crop_dir, site))}
            if(!dir.exists(file.path(ndwi_crop_dir, site, as.character(year)))){dir.create(file.path(ndwi_crop_dir, site, as.character(year)))}
            write_dir <- file.path(ndwi_crop_dir,site,as.character(year))
            filename <- file.path(write_dir, name)
            
            writeRaster(x = NDWI_site,
                        filename = filename,
                        format = "GTiff", 
                        datatype='FLT4S', 
                        progress='text',
                        overwrite = TRUE
            )
            
        }
    }
    
}

# Change site and year here
site <- "GOR" #"MFC2"
year <- 2020
write_NDWI_site_year(site, year)



# Following function converts the character to DOY
# sample form of x is : "20200507T095029"
char_to_doy <- function(x) {
    # Use of pipes
    DOY <- strsplit(x,"T")[[1]][1] %>%
        lubridate::ymd() %>%
        strftime(format = "%j") %>%
        as.numeric()
    
    return (DOY)
}

# use the following "ndwi_crop_dir" for 2017,2018,2019,2020
if(!dir.exists(here("Desktop", "Playground_dir_8", "NDWI"))){
    dir.create(here("Desktop", "Playground_dir_8", "NDWI"))
}

if(!dir.exists(here("Desktop", "Playground_dir_14", "NDWI"))){
    dir.create(here("Desktop", "Playground_dir_14", "NDWI"))
}

ndwi_crop_dir <- here("Desktop", "Playground_dir_15", site, year)

# This function saves NDWI dataframes to drive, also plots the NDWI time series 
# and writes the dfs & plots to drive
NDWI_dfs_site_year <- function(site, year){
    site <- toupper(site)
    # Read the NDWIs in and stack them
    ndwi_dir <- file.path(ndwi_crop_dir)
    ndwi_list <- list.files(ndwi_dir,recursive = TRUE, 
                            full.names = TRUE, pattern="^NDWI_")
    
    #Load the select_10m.Rds
    # the following "select_10m" is different for each site
    select_10m <-readRDS(file = file.path(here("Desktop", "Playground_dir_8", "select_10m"),
                                          paste0("select10m_", year,"_", toupper(site),".Rds")))
    
  
    # select tiles based on select_10m
    ndwi_list_df <- as.data.frame(ndwi_list)
    select_10m_df <- as.data.frame(select_10m)
    ndwi_list_selected <- cbind(ndwi_list_df, select_10m_df)
    ndwi_list_selected <- ndwi_list_selected %>% filter(select_10m == TRUE)
    
    
    ndwi_stack <- stack(ndwi_list_selected$ndwi_list)
    
    
    # Remove the artifact(parking lot) from NDWI rasterstack for MFC2
    if (site == "MFC2"){
        
        ndwi_stack_wo_artifact  <-  ndwi_stack
        for (i in 1:nlayers(ndwi_stack)) {
            
            r1 <- ndwi_stack[[i]]
            r1[artifact_mfc2_new] <- 94
            r1[artifact2_mfc2_new] <- 94
            names(r1) <- names(ndwi_stack[[i]])
            rna <- reclassify(r1, cbind(94, NA))
            ndwi_stack_wo_artifact [[i]]  <-  rna
            
        }
        ndwi_stack <- ndwi_stack_wo_artifact
    }
    
    
    poly <- fortify(aoi)
    poly2 <- fortify(aoi_2)
    
    
    if (site == "MFC2"){
        pol <- poly
    }else{
        pol <- poly2
    }
    names(pol) <- c('x', 'y', "order", "hole", "piece", "id", "group")
    
    
    if (site == "MFC2"){
        
        sntnlDates <- gsub("NDWI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                           "", 
                           names(ndwi_stack))
        
    }else{
        
        sntnlDates <- gsub("NDWI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                           "", 
                           names(ndwi_stack))
    }
    
    # Convert character dates in raster names  to DOY (used for plot)
    df_date <- as.data.frame(sntnlDates)
    ndwi_stack_renamed <- ndwi_stack
    names(ndwi_stack_renamed) <- apply(df_date, 1, char_to_doy)
    
    # save the selected NDWI stack into drive to use in "correlation"
    save_dir <- here("Desktop","Playground_dir_14", "NDWI")
    
    if(!dir.exists(here("Desktop","Playground_dir_14", "NDWI", toupper(site)))){
        dir.create(here("Desktop","Playground_dir_14", "NDWI", toupper(site)))
    }
    if(!dir.exists(here("Desktop","Playground_dir_14", "NDWI", toupper(site), "Extracted_dfs"))){
        dir.create(here("Desktop","Playground_dir_14", "NDWI", toupper(site), "Extracted_dfs"))
    }
    saveRDS(ndwi_stack_renamed, file.path(save_dir, toupper(site), 
                                          "Extracted_dfs", sprintf("selected_ndwi_stack_%s_%s",site,year)))
    
    
    # Plot method 1
    # use colorbrewer which loads with the rasterVis package to generate
    # a color ramp of yellow to green
    cols <- colorRampPalette(brewer.pal(11, "RdBu"))
    if(!dir.exists(here("Desktop","Playground_dir_8", "NDWI", site))){
        dir.create(here("Desktop","Playground_dir_8", "NDWI",  site))
    }
    
    if(!dir.exists(here("Desktop","Playground_dir_8", "NDWI", site, year))){
        dir.create(here("Desktop","Playground_dir_8", "NDWI",  site, year))
    }
    write_dir <- file.path(here("Desktop","Playground_dir_8", "NDWI",  
                                site,
                                year))
    
    png(here(write_dir, paste0(sprintf("ndwi_%s_%s", tolower(site), year), ".png")))
    
    # define breaks for levelplot()
    my.at <- seq(-1, 1, by = 0.1)
    myColorkey <- list(at=my.at, ## where the colors change
                       labels=list(
                           at=my.at ## where to print labels
                       ),
                       space="bottom")
    
    if (site=="MFC2"){ 
        print(levelplot(ndwi_stack_renamed,main=sprintf("Sentinel2 NDWI %s %s", site, year),col.regions=cols,par.settings=list(layout.heights=list(xlab.key.padding=1)),panel = panel.levelplot.raster, interpolate = TRUE,colorkey = list(space="bottom"), margin = FALSE) + layer(sp.polygons(aoi, col = "black"))) 
    }else{ 
        print(levelplot(ndwi_stack_renamed,main=sprintf("Sentinel2 NDWI %s %s", site, year),col.regions=cols,par.settings=list(layout.heights=list(xlab.key.padding=1)),panel = panel.levelplot.raster, interpolate = TRUE,colorkey = list(space="bottom"), margin = FALSE) + layer(sp.polygons(aoi_2, col = "black")))
    }
    
    
    dev.off()
    
    
    write_dir <- file.path(here("Desktop","Playground_dir_8", "NDWI",
                                site,
                                year))
    
    
    # calculate mean NDWI for each raster
    avg_NDWI_stack <- cellStats(ndwi_stack, mean)
    
    # convert output array to data.frame
    avg_NDWI_stack <- as.data.frame(avg_NDWI_stack)
    
    # view column name slot
    #names(avg_NDWI_stack)
    
    # rename the NDWI column
    names(avg_NDWI_stack) <- "meanNDWI"
    
    # add a site column to our data
    avg_NDWI_stack$site <- sprintf("%s_study_site", site)
    
    # # note the use of the vertical bar character ( | ) is equivalent to "or". This
    # allows us to search for more than one pattern in our text strings.
    if (site == "MFC2"){
        
        sentinelDates <- gsub("NDWI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                              "", 
                              row.names(avg_NDWI_stack))
        
    }else{
        
        sentinelDates <- gsub("NDWI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                              "", 
                              row.names(avg_NDWI_stack))
    }
    
    
    # Convert character dates in dataframe to DOY (used for plot)
    avg_NDWI_stack$date <- as.data.frame(sentinelDates)
    avg_NDWI_stack$doy <- apply(avg_NDWI_stack[,3], 1, char_to_doy)
    
    # Save the avearge NDWI dataframe to drive (will be used in "13_correlation_NDVI_meteo.R")
    if(!dir.exists(here("Desktop","Playground_dir_8", "NDWI", "output"))){
        dir.create(here("Desktop","Playground_dir_8", "NDWI",  "output"))
    }
    
    save_dir <- file.path(here("Desktop", "Playground_dir_8", "NDWI","output"))
    saveRDS(avg_NDWI_stack, file = file.path(save_dir, 
                                             paste0("avg_NDWI_stack_", site, "_", year)) )#, ".Rds"
    
    
    # In the dataframe some of the same doy has the same NDWI too,
    # that is why the number of dots is less than the number of dataframe rows
    # plot NDWI
    ggplot(avg_NDWI_stack, aes(doy, meanNDWI), na.rm=TRUE) +
        geom_point(size=4, colour = "PeachPuff4") + 
        #geom_smooth(method = "loess", span = 0.4) + 
        geom_smooth() +
        geom_line(aes(group=site), linetype= "dashed") +
        labs(title = sprintf("Mean NDWI over %s in %s", site, year))+
        scale_x_continuous(breaks = seq(1, 365, by = 14))+
        xlab("DOY") + 
        ylab("Mean NDWI [-]") +
        ylim(-1, 1) +
        theme(text = element_text(size = 15),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
              aspect.ratio= 12/16)
    
    
    ggsave(here(write_dir, paste0(sprintf("ndwi_series_%s", tolower(site)), ".png")), 
           scale = 3, 
           #width = 15, 
           #height = 10,
           dpi = 300)
    
    
}

NDWI_dfs_site_year(site, year)



