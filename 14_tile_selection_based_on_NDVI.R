###################
# The following code consiste of 2 functions 
# First is used to calculate and saves the NDVIs for different years & sites 
# Second is for ploting histogram and time series and saving them 
##################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster",
          "leaflet","rasterVis","gridExtra","RColorBrewer",
          "plotly","RStoolbox","sp","sf","IRdisplay","reshape", 
          "here", "patchwork", "tidyverse", "cowplot"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)


#2: Load Auxillary data
### Define your area of interest (aoi), which is MFC2 (bacino_MFC_corrected) or bounding_box_MFC or else #
aoi <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site1_MFC2_agroforestry/MFC2.shp")
aoi_2 <- rgdal::readOGR("C:/Users/sanaz/Documents/MB12-project/data/vector/Site2_GOR_forest/Site2_GOR_forest/GOR.shp")
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


ndvi_crop_dir <- here("Desktop","Playground_dir_12")


# Change site and year here
site <-  "MFC2"  #     "GOR"
year <- 2018

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

# define the ndvi_crop_dir to read from
ndvi_crop_dir <- here("Desktop","Playground_dir_12", site, year)

# This function draws histogram, time series plot of NDVI 
# and writes the dfs & plots to drive
NDVI_plots_site_year <- function(site, year){
    site <- toupper(site)
    # Read the NDVIs in and stack them
    ndvi_dir <- file.path(ndvi_crop_dir)
    ndvi_list <- list.files(ndvi_dir,recursive = TRUE, 
                            full.names = TRUE, pattern="^NDVI_")
    # for creating the ndvi from the cleaned list
    new_paths <- file.path(ndvi_dir,paste0(new_clean$names,".tif"))
    
    # Load  all the NDVI without "select_10m" and later in code filter them based on a threshold
    ndvi_list_df <- as.data.frame(ndvi_list)
    ndvi_stack_experimental <- stack(ndvi_list_df$ndvi_list)
    ndvi_stack <- ndvi_stack_experimental
    
    #for new_clean
    ndvi_new_list_df <- as.data.frame(new_paths)
    ndvi_new_stack_experimental <- stack(ndvi_new_list_df$new_paths)
    ndvi_new_stack <- ndvi_new_stack_experimental
    

    # Remove the artifact(parking lot) from NDVI rasterstack for MFC2
    if (site == "MFC2"){
        
        ndvi_stack_wo_artifact  <-  ndvi_stack
        for (i in 1:nlayers(ndvi_stack)) {
            
            r1 <- ndvi_stack[[i]]
            r1[artifact_mfc2_new] <- 94
            r1[artifact2_mfc2_new] <- 94
            names(r1) <- names(ndvi_stack[[i]])
            rna <- reclassify(r1, cbind(94, NA))
            ndvi_stack_wo_artifact [[i]]  <-  rna
            
        }
        ndvi_stack <- ndvi_stack_wo_artifact
    }
    
    # for new_clean
    if (site == "MFC2"){
      
      ndvi_new_stack_wo_artifact  <-  ndvi_new_stack
      for (i in 1:nlayers(ndvi_new_stack)) {
        
        r1 <- ndvi_new_stack[[i]]
        r1[artifact_mfc2_new] <- 94
        r1[artifact2_mfc2_new] <- 94
        names(r1) <- names(ndvi_new_stack[[i]])
        rna <- reclassify(r1, cbind(94, NA))
        ndvi_new_stack_wo_artifact [[i]]  <-  rna
        
      }
      ndvi_new_stack <- ndvi_new_stack_wo_artifact
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
        
        sntnlDates <- gsub("NDVI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                           "", 
                           names(ndvi_stack))
        
    }else{
        
        sntnlDates <- gsub("NDVI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                           "", 
                           names(ndvi_stack))
    }
    
    # for new_clean
    if (site == "MFC2"){
      
      sntnlDates_new <- gsub("NDVI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                         "", 
                         names(ndvi_new_stack))
      
    }else{
      
      sntnlDates_new <- gsub("NDVI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                         "", 
                         names(ndvi_new_stack))
    }
    
    # Convert character dates in raster names  to DOY (used for plot)
    df_date <- as.data.frame(sntnlDates)
    ndvi_stack_renamed <- ndvi_stack
    names(ndvi_stack_renamed) <- apply(df_date, 1, char_to_doy)
    
    # for new_clean
    df_new_date <- as.data.frame(sntnlDates_new)
    ndvi_new_stack_renamed <- ndvi_new_stack
    names(ndvi_new_stack_renamed) <- apply(df_new_date, 1, char_to_doy)
    
    
    # Plot method 1
    # use colorbrewer which loads with the rasterVis package to generate
    # a color ramp of yellow to green
    cols <- colorRampPalette(brewer.pal(9,"RdYlGn"))#"YlGn"
    write_dir <- file.path(here("Desktop","Playground_dir_12", 
                                site,
                                year))
    
    png(here(write_dir, paste0(sprintf("ndvi_selected_%s_%s", tolower(site), year), ".png")))
    
    if(site=="MFC2"){
        # create a level plot - plot
        print(levelplot(ndvi_new_stack_renamed,
                        main=sprintf("Sentinel2 selected NDVI %s %s", site, year),
                        col.regions=cols,
                        par.settings=list(layout.heights=list(xlab.key.padding=1)),
                        panel = panel.levelplot.raster, interpolate = TRUE,
                        colorkey =  list(space="bottom"),
                        margin = FALSE) + 
                  layer(sp.polygons(aoi, col = "black"))
        )
    }else{
        # create a level plot - plot
        print(levelplot(ndvi_new_stack_renamed,
                        main=sprintf("Sentinel2 selected NDVI %s %s", site, year),
                        col.regions=cols,
                        par.settings=list(layout.heights=list(xlab.key.padding=1)),
                        panel = panel.levelplot.raster, interpolate = TRUE,
                        colorkey = list(space="bottom"),
                        margin = FALSE) + 
                  layer(sp.polygons(aoi_2, col = "black"))
        )
        
    }
    
    dev.off()
    
    
    #for new_clean
    if(site=="MFC2"){
      # create a level plot - plot
      print(levelplot(ndvi_stack_renamed,
                      main=sprintf("Sentinel2 NDVI %s %s", site, year),
                      col.regions=cols,
                      par.settings=list(layout.heights=list(xlab.key.padding=1)),
                      panel = panel.levelplot.raster, interpolate = TRUE,
                      colorkey =  list(space="bottom"),
                      margin = FALSE) + 
              layer(sp.polygons(aoi, col = "black"))
      )
    }else{
      # create a level plot - plot
      print(levelplot(ndvi_stack_renamed,
                      main=sprintf("Sentinel2 NDVI %s %s", site, year),
                      col.regions=cols,
                      par.settings=list(layout.heights=list(xlab.key.padding=1)),
                      panel = panel.levelplot.raster, interpolate = TRUE,
                      colorkey = list(space="bottom"),
                      margin = FALSE) + 
              layer(sp.polygons(aoi_2, col = "black"))
      )
      
    }
    
    dev.off()
    
    
    
    write_dir <- file.path(here("Desktop","Playground_dir_12",
                                site,
                                year)) 
    
    
    # open up RGB imagery
    rgb_dir <- here("Desktop","Playground_dir_10", site, year)
    rgb_list <-  list.files(rgb_dir, 
                            full.names=TRUE, 
                            pattern = ".tif$")
    # select tiles based on select_10m
    rgb_list_df <- as.data.frame(rgb_list)
    
    rgb_stack <- stack(rgb_list_df$rgb_list)
    

    # plot rgb :method 2
    png(here(write_dir, paste0(sprintf("rgb_%s_%s", tolower(site), year), ".png")))
    
    # set the initial value of list index
    i <- 1
    
    # create a list with a specific length 
    plot_lst <- vector("list", length = 63) # the length should be manually set
    
    # go through each RGB file and plot it 
    for (aFile in rgb_list_df$rgb_list){
        
        if (site=="MFC2"){
            x <- gsub("RGB_MFC2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}.tif", 
                      "", 
                      basename(aFile))
            doy_x <- char_to_doy(x)
        }else{
            x <- gsub("RGB_GOR_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}.tif", 
                      "", 
                      basename(aFile))
            doy_x <- char_to_doy(x)
        }
        
        RGB.rastStack <- stack(aFile)
      
        g <-  ggRGB(RGB.rastStack, r=3, g=2, b=1) +
            ggtitle(sprintf("DOY %s", doy_x))+
            geom_polygon(col = 'Black',
                         fill = NA,
                         data = pol,#aoi_DataFrame
                         aes(x = x, y = y))+# for "aoi_DataFrame": x=long,y=lat, group = group
            theme(axis.text = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title=element_blank(),
                  plot.margin=unit(c(0,0,0,0),"pt")#reduce or eliminate plot margins
            )
        
        plot_lst[[i]] <- g
        i <- i+1    
        
    }
    
    # Using "patchwork" library for multiple plots
    pp <-  plot_lst[[1]]
    
    for(i in c(2:63)){ # the end number should be manually set based on the number of existing selected tiles
        
        pp <- pp+plot_lst[[i]]
    }
    
    pp + plot_layout(nrow=7, ncol=9) # number of column and rows should be manually set
    
    dev.off()
    
    
    # calculate mean NDVI for each raster
    avg_NDVI_stack <- cellStats(ndvi_stack, mean)
    
    # convert output array to data.frame
    avg_NDVI_stack <- as.data.frame(avg_NDVI_stack)
    new <- data.frame(names = names(ndvi_stack), 
                      meanNDVI = avg_NDVI_stack)
    new$site <- sprintf("%s_study_site", site)
    
    # rename the NDVI column
    names(avg_NDVI_stack) <- "meanNDVI"
    
    # add a site column to our data
    avg_NDVI_stack$site <- sprintf("%s_study_site", site)
    
    # # note the use of the vertical bar character ( | ) is equivalent to "or". This
    # allows us to search for more than one pattern in our text strings.
    if (site == "MFC2"){
        
        sentinelDates <- gsub("NDVI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                              "", 
                              row.names(avg_NDVI_stack))
        
    }else{
        
        sentinelDates <- gsub("NDVI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                              "", 
                              row.names(avg_NDVI_stack))
    }
    
    
    # Convert character dates in dataframe to DOY (used for plot)
    avg_NDVI_stack$date <- as.data.frame(sentinelDates)
    avg_NDVI_stack$doy <- apply(avg_NDVI_stack[,3], 1, char_to_doy)
    
    #for new
    new$date <- as.data.frame(sentinelDates)
    new$doy <- apply(new[,4], 1, char_to_doy)
    
    # In the dataframe some of the same doy has the same NDVI too,
    # that is why the number of dots is less than the number of dataframe rows
    # plot NDVI
    ggplot(avg_NDVI_stack, aes(doy, meanNDVI), na.rm=TRUE) +
        geom_point(size=4, colour = "PeachPuff4") + 
        #geom_smooth(method = "loess", span = 0.4) + 
        geom_smooth() +
        geom_line(aes(group=site), linetype= "dashed") +
        labs(title = sprintf("Mean NDVI over %s in %s", site, year))+
        #ggtitle( sprintf("Sentinel NDVI \n %s Site", site)) +
        scale_x_continuous(breaks = seq(1, 365, by = 14))+
        xlab("DOY") + 
        ylab("Mean NDVI [-]") +
        ylim(0, 1) +
        theme(text = element_text(size = 15),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
              aspect.ratio= 12/16)
    
    ggsave(here(write_dir, paste0(sprintf("full_ndvi_series_%s", tolower(site)), ".png")), 
           scale = 3, 
           #width = 15, 
           #height = 10,
           dpi = 300)
    
    # retain only rows with meanNDVI>0.5
    threshold_value <- 0.3 # choose this value(for each year and site) based on NDVI and RGB plots and comparing them
    avg_NDVI_stack_clean <- subset(avg_NDVI_stack, meanNDVI>threshold_value)
    new_clean <-  subset(new, avg_NDVI_stack>threshold_value)
    
    # Save the avearge NDVI dataframe to drive (will be used in "13_correlation_NDVI_meteo")
    save_dir <- file.path(here("Desktop", "Playground_dir_12", "output"))
    saveRDS(avg_NDVI_stack_clean, file = file.path(save_dir,
                                            paste0("avg_NDVI_stack_threshold_applied_", site, "_", year)) )#, ".Rds"

    ggplot(avg_NDVI_stack_clean, aes(doy, meanNDVI), na.rm=TRUE) +
        geom_point(size=4, colour = "PeachPuff4") + 
        #geom_smooth(method = "loess", span = 0.4) +
        geom_smooth() +
        geom_line(aes(group=site), linetype= "dashed") +
        labs(title = sprintf("Mean cleaned NDVI over %s in %s", site, year))+
        #ggtitle( sprintf("Sentinel NDVI \n %s Site", site)) +
        scale_x_continuous(breaks = seq(1, 365, by = 14))+
        xlab("DOY") + 
        ylab("Mean NDVI [-]") +
        ylim(0, 1) +
        theme(text = element_text(size = 15),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
              aspect.ratio= 12/16)
    ggsave(here(write_dir, paste0(sprintf("threshold_gt_%s_ndvi_series_%s", threshold_value, tolower(site)), ".png")), 
           scale = 3, 
           #width = 15, 
           #height = 10,
           dpi = 300)
    
}

NDVI_plots_site_year(site, year)
