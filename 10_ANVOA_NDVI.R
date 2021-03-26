##########################
# In this piece of code, ANalysis Of Variance(ANOVA) using aov()
# on NDVI and LAI is performed.
##########################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet","rasterVis","gridExtra",
          "RColorBrewer","plotly","RStoolbox","sp","IRdisplay","reshape",
          "here","readr", "lubridate","dplyr", "ggExtra"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)

aoi <- rgdal::readOGR(dsn=here("Documents", "MB12-project", "data", "vector", "Site1_MFC2_agroforestry"),layer= "MFC2")
aoi_2 <- rgdal::readOGR(dsn=here("Documents", "MB12-project", "data", "vector","Site2_GOR_forest", "Site2_GOR_forest"), layer="GOR")

artifact_mfc2 <- rgdal::readOGR(dsn=here("Documents", "MB12-project","QGIS_part"), layer="parking_lot")
artifact2_mfc2 <- rgdal::readOGR(dsn=here("Documents", "MB12-project","QGIS_part"), layer="house")

# reproject data
artifact_mfc2_new <- spTransform(artifact_mfc2,
                                 crs(aoi))
artifact2_mfc2_new <- spTransform(artifact2_mfc2,
                                  crs(aoi))

#"C:\Users\sanaz\Desktop\Playground_dir_12\MFC2\2017"

ndvi_dataframe <- function(year, site) {
  site <- toupper(site)
  base_dir <- here("Desktop","Playground_dir_12")
  ndvi_dir <- here("Desktop","Playground_dir_12",
                   site,
                   year)
  ndvi_list <- list.files(ndvi_dir,recursive = TRUE, 
                          full.names = TRUE, pattern="^NDVI_")
  
  # Situation when select_10m files are different for each site
 
  
  select_10m <-readRDS(file = here("Desktop","Playground_dir_10", 
                                   site,year,
                                   paste0("select10m_",year,"_",
                                          toupper(site),".Rds")))
 

  # select tiles based on select_10m
  ndvi_list_df <- as.data.frame(ndvi_list)
  select_10m_df <- as.data.frame(select_10m)
  ndvi_list_selected <- cbind(ndvi_list_df , select_10m_df)
  ndvi_list_selected <- ndvi_list_selected %>% filter(select_10m == TRUE)
  
  ndvi_stack <- stack(ndvi_list_selected$ndvi_list)
  
  # Remove the artifact(parking lot) from NDVI rasterstack for "MFC2"
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
  
  # Create a dataframe from ndvi_stack
  ndvi_stack_df <- as.data.frame(ndvi_stack, xy = TRUE) %>% #ndvi_stack
    melt(id.vars = c('x','y'))
  
  
  if (site == "MFC2"){
    
    sntnlDates <- gsub("NDVI_mfc2_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                       "", 
                       ndvi_stack_df$variable)
    
  }else{
    
    sntnlDates <- gsub("NDVI_gor_S2[AB]_MSIL2A_|_N[[:digit:]]{4}_R[[:digit:]]{3}_T33TWE_[[:alnum:]]{15}", 
                       "", 
                       ndvi_stack_df$variable)
  }
  
  # Convert character dates in dataframe to date 
  ndvi_stack_df$date <- as.data.frame(sntnlDates)
  
  ndvi_stack_df$char_date <- apply(ndvi_stack_df, 1, 
                                  FUN=function(x){strsplit(x["date"],"T")[[1]][1]})
  
  # Add "year" $ "month" columns to df
  ndvi_stack_df$Year <- apply(ndvi_stack_df, 1, 
                              FUN=function(x){substring(x["char_date"], first = 1, last=4)})

  
  ndvi_stack_df$Month <- apply(ndvi_stack_df, 1, 
                              FUN=function(x){substring(x["char_date"], first = 5, last=6)})
  
  
  new_df_with_date <- ndvi_stack_df

  # Extract the acquisition date and add it to df as "date" column
  # new_df_with_date <- ndvi_stack_df %>% 
  #                     rowwise() %>% 
  #                     mutate(date_n = as.Date(parse_date_time(
  #                                                           strsplit(
  #                                                                 strsplit(as.character(variable), 
  #                                                                         "_")[[1]][5],"T")[[1]][1],
  #                                                                                 orders = "ymd" )))
  # 
 
  
  
  # Meteorological Seasons
  # new_df_with_date <- new_df_with_date %>%
  #   mutate(
  #     season = case_when(
  #       month %in%  9:11 ~ "Fall",
  #       month %in%  c(12, 1, 2)  ~ "Winter",
  #       month %in%  3:5  ~ "Spring",
  #       TRUE ~ "Summer"))
  
  # Seasons based on Paolo's paper for this area
  new_df_with_date <- new_df_with_date %>%
      mutate(
        season = case_when(
          Month %in%  c("11","12","01","02") ~ "Wet",# November–December–January–February
          Month %in%  c("05","06","07","08")  ~ "Dry",#May–June–July–August
          Month %in%  c("03","04","09","10")  ~ "Transition"#March-April,Sep-Oct
          )
      )

  # Add the "site" column to the dataframe
  new_df_with_date$Site <- site
  
  return(new_df_with_date)
}

# Call the previously defined function to create the NDVI_df for ANOVA 
#ndvi_2017_mfc2 <- ndvi_dataframe(2017,"mfc2")
#ndvi_2018_mfc2 <- ndvi_dataframe(2018,"mfc2")
#ndvi_2019_mfc2 <- ndvi_dataframe(2019,"mfc2")
#ndvi_2020_mfc2 <- ndvi_dataframe(2020,"mfc2")




# Combine yearly ndvi dataframes into one 
site_1 <- "gor"
#ndvi_2017_gor1 <- ndvi_dataframe(2017,site_1)
#ndvi_2018_gor1 <- ndvi_dataframe(2018,site_1)
#ndvi_2019_gor1 <- ndvi_dataframe(2019,"gor")
#ndvi_2020_gor1 <- ndvi_dataframe(2020,"gor")

ndvi_df_combi_gor1 <- rbind(ndvi_dataframe(2017,site_1), 
                            ndvi_dataframe(2018,site_1), 
                            ndvi_dataframe(2019,site_1), 
                            ndvi_dataframe(2020,site_1))

dir_name <- file.path(here("Desktop", "Playground_dir_3", "output"))

saveRDS(ndvi_df_combi_gor1, 
        file = file.path(dir_name, paste0("ndvi_df_combi_",site_1,".Rds")))

ndvi_df_combi_gor1 <- readRDS(file = file.path(dir_name, paste0("ndvi_df_combi_",site_1,".Rds")))

site_2 <- "MFC2"
ndvi_df_combi_mfc2 <- rbind(ndvi_dataframe(2017,site_2), 
                           ndvi_dataframe(2018,site_2), 
                           ndvi_dataframe(2019,site_2), 
                           ndvi_dataframe(2020,site_2))

saveRDS(ndvi_df_combi_mfc2, 
        file = file.path(dir_name, paste0("ndvi_df_combi_",site_2,".Rds")))

ndvi_df_combi_mfc2 <- readRDS(file = file.path(dir_name, paste0("ndvi_df_combi_",site_2,".Rds")))

# Combine dfs of two study sites
ndvi_df_combi <- dplyr::bind_rows(ndvi_df_combi_mfc2,
                                  ndvi_df_combi_gor1)

# Before performing the ANOVA, visulize the data
x11()
plot(value~Year, data=ndvi_df_combi_gor1) #ndvi_df_combi_mfc2
boxplot(value~Year , data = ndvi_df_combi_mfc2)

# I reorder the groups order : I change the order of the factor ndvi_df_combi$season
ndvi_df_combi$season <- factor(ndvi_df_combi$season,
                               levels=c( "Dry", "Wet", "Transition"))
boxplot(value~season, 
        data = ndvi_df_combi)# seasonal on one year and one site
# Boxplot
x11()
ggplot(ndvi_df_combi, aes(season, value, colour = Site)) + 
  geom_boxplot()+
  xlab("season") + 
  ylab("NDVI [-]") +
  removeGrid(x = TRUE, y= FALSE) +
  geom_vline(
    aes(xintercept = 1.5),# so that a vertical line appear between seasons
    data = ndvi_df_combi,
    colour = "grey50", alpha = 0.5
  ) +
  geom_vline(
    aes(xintercept = 2.5),# so that a vertical line appear between seasons
    data = ndvi_df_combi,
    colour = "grey50", alpha = 0.5
  ) +
  facet_wrap(~Year,ncol = 1)

ggsave(path = here("Desktop","Playground_dir_3","output"), 
       file="boxplt_ndvi_site_season_year.pdf")


# Before ANOVA, compute descriptive statistics 
ndvi_df_combi_gor1 %>%
  group_by(Year) %>% 
  summarise(mean = mean(value))

ndvi_df_combi_mfc2 %>%
  group_by(Year) %>%
  summarise(median = median(value))

ndvi_df_combi%>%
  group_by(season) %>%
  summarise(median = median(value))



#quantile-quantile plot 
x11()
ggplot(ndvi_df_combi, aes(sample = value)) +
  stat_qq() +
  stat_qq_line()


# ANOVA yearly(2017,2018,2019,2020)
res_aov <- aov(value~Year, data = ndvi_df_combi_gor1)

summary(res_aov)

# Test homogenity of the variance and the normality of the residuals 
plot(res_aov, which = 1) # : Homogenity

plot(res_aov, which = 2) # : Normality


# ANOVA comparison between 2 sites(GOR, MFC2)
res_aov_sites <- aov(value~Site, data = ndvi_df_combi)

summary(res_aov_sites)

# Test homogenity of the variance and the normality of the residuals 
plot(res_aov_sites, which = 1) # : Homogenity

plot(res_aov_sites, which = 2) # : Normality



# ANOVA comparison between different seasons
res_aov_seasons <- aov(value~season, data = ndvi_df_combi)

summary(res_aov_seasons)

# Test homogenity of the variance and the normality of the residuals 
plot(res_aov_seasons, which = 1) # : Homogenity

plot(res_aov_seasons, which = 2) # : Normality






# Manual process of creating the dataframes(w/o using function)

# Read the NDVIs in and stack them _ GOR 
#ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","GOR","GOR_2017")
#ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","GOR","GOR_2018")
#ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","GOR","GOR_2019")

# Read the NDVIs in and stack them _ MFC_2 
#ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","MFC_2","MFC2_2017")
#ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","MFC_2","MFC2_2018")
ndvi_dir <- here("Desktop","Playground_dir_3","NDVI_cropped","MFC_2","MFC2_2019")



ndvi_list <- list.files(ndvi_dir,recursive = TRUE, full.names = TRUE, pattern="^NDVI_")

#Load the select_10m.Rds ("select_10m" based on "RGB_10m")
#select_10m <-readRDS(file = "C:/Users/sanaz/Documents/MB12-project/CREODIAS_part/data_from_CREODIAS/L2A_2017/RGB/select10m.Rds")
#select_10m <-readRDS(file = "C:/Users/sanaz/Documents/MB12-project/CREODIAS_part/data_from_CREODIAS/L2A_2018/RGB/select10m_2018.Rds")
select_10m <-readRDS(file = "C:/Users/sanaz/Documents/MB12-project/CREODIAS_part/data_from_CREODIAS/L2A_2019/RGB/select10m_2019.Rds")



# select tiles based on select_10m
ndvi_list_df <- as.data.frame(ndvi_list)
select_10m_df <- as.data.frame(select_10m)
ndvi_list_selected <- cbind(ndvi_list_df , select_10m_df)
ndvi_list_selected<- ndvi_list_selected %>% filter(select_10m == TRUE)


ndvi_stack <- stack(ndvi_list_selected$ndvi_list)



# Create a dataframe from ndvi_stack
ndvi_stack_df <- as.data.frame(ndvi_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

# add a new column to dataframe indicating the year of NDVI
#ndvi_stack_df$year <-  "2017"
#ndvi_stack_df$year <- "2018"
ndvi_stack_df$year <- "2019"


#ndvi_stack_df_2017 <- ndvi_stack_df
#ndvi_stack_df_2018 <- ndvi_stack_df
ndvi_stack_df_2019 <- ndvi_stack_df



# Combine three ndvi dataframes into one 
#ndvi_df_combi_gor <- rbind(ndvi_stack_df_2017, ndvi_stack_df_2018, ndvi_stack_df_2019 )
ndvi_df_combi_mfc2 <- rbind(ndvi_stack_df_2017, ndvi_stack_df_2018, ndvi_stack_df_2019 )

# Add a new column indicating the name of study site
#ndvi_df_combi_gor$site <- "GOR"
ndvi_df_combi_mfc2$site <- "MFC2"


# Combine ndvi dataframes of two study sites
ndvi_df_combi <- rbind(ndvi_df_combi_gor, ndvi_df_combi_mfc2)




# Extract the acquisition date from each row of the dataframe
new_df_with_date <- ndvi_stack_df_2018 %>% 
                          rowwise() %>% 
                      mutate(date = as.Date(parse_date_time(strsplit(strsplit(as.character(variable), "_")[[1]][5],"T")[[1]][1],orders = "ymd" )))






# Before performing the ANOVA, visulize the data
plot(value~year, data=ndvi_df_combi_gor)#ndvi_df_combi_gor,ndvi_df_combi_mfc2
boxplot(value~year , data = ndvi_df_combi_gor)



# Before ANOVA, compute descriptive statistics 
ndvi_df_combi_gor %>%
  group_by(year) %>% 
  summarise(mean = mean(value))

ndvi_df_combi_gor %>%
  group_by(year) %>%
  summarise(median = median(value))#quantile(value)


# ANOVA yearly(2017,2018,2019,2020)
res_aov <- aov(value~year , data = ndvi_df_combi_gor)

summary(res_aov)

# Test homogenity of the variance and the normality of the residuals 
plot(res_aov, which=1) # : Homogenity

plot(res_aov, which=2) # : Normality


# ANOVA comparison between 2 sites(GOR, MFC2)
res_aov_sites <- aov(value~site , data = ndvi_df_combi)

summary(res_aov_sites)
