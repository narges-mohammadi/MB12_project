#######################
# Find the correlation between NDVI and meteorological params
# First plot both dataframes together 
# Afterwards use corrplot() package to quantify the correlation
#######################

### Preparation ############################# 
setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr", "rgdal", "ggplot2", "raster",
          "leaflet", "rasterVis", "gridExtra", "RColorBrewer",
          "plotly", "RStoolbox", "sp", "IRdisplay", "reshape", 
          "here", "patchwork", "readxl", "ggExtra", "viridis",
          "corrplot", "PerformanceAnalytics", "ggpmisc", "qwraps2",
          "tableone", "pander", "arsenal", "finalfit"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)


### Creation process of dataframes(meteorological and NDVI) ############################# 

# Read in the meteorological dfs created in "11_Meteo_Params.R"
site_1 <- "MFC2" 
read_dir <- here("Desktop","Playground_dir_11","output")
mfc2_meteo_df <- readRDS(file = file.path(read_dir, 
                                     paste0("meteorological_df_", site_1)))

site_2 <- "GOR1"
read_dir <- here("Desktop","Playground_dir_11","output")
gor1_meteo_df <- readRDS(file = file.path(read_dir, 
                                          paste0("meteorological_df_", site_2)))



# Precipitation bar plot
# x11()
# ggplot(mfc2_meteo_df, aes(DOY, Prec_mm, colour = Year)) + 
#     #geom_line()+
#     geom_bar(stat = "identity", position = "identity", fill = NA) +
#     scale_color_viridis(discrete = FALSE, option = "cividis", direction = -1)+# option="inferno"
#     scale_x_continuous(breaks = seq(1, 365, by = 7))+  
#     scale_y_continuous(breaks = seq(0, 100, by = 10))+
#     theme(text = element_text(size = 8),
#           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
#     xlab("DOY") + 
#     ylab("Precipitation [mm]") +
#     removeGrid(x = TRUE, y = FALSE)+
#     #facet_wrap(~site, ncol = 1)
#     facet_wrap(~Year, ncol = 1)

# In this part I should use the NDVI dataframe created from "9_Cropped_Ndvi_from_Gtiff_Function"
# In which contains the average NDVIs
# NDVI ts vs DOY 
# NDVI dataframes from 2017 and 2018 contain multiple meanNDVI for the same "doy"
# There must be sth wrong in the creation of NDVI or their selection
# In 2017 and 2018, sentinel tiles had bands starting with T_* and also with L2A_*



# define constants
site2 <- "GOR"
sites <- list(site_1, site_2)

year1 <- 2017
year2 <- 2018
year3 <- 2019
year4 <- 2020
years <- list(year1, year2, year3, year4)

# created in "9_Cropped_Ndvi_from_Gtiff_Function.R" script
read_dir_ndvi <-here("Desktop","Playground_dir_8","output") 

ndvi_df_gor_2017 <- readRDS(file = file.path(read_dir_ndvi, 
                                          paste0("avg_NDVI_stack_", 
                                                 site2,"_", year1)))
ndvi_df_gor_2017$year <- year1

ndvi_df_mfc2_2017 <- readRDS(file = file.path(read_dir_ndvi, 
                                             paste0("avg_NDVI_stack_", 
                                                    site_1,"_", year1)))
ndvi_df_mfc2_2017$year <- year1

ndvi_df_gor_2018 <- readRDS(file = file.path(read_dir_ndvi, 
                                             paste0("avg_NDVI_stack_", 
                                                    site2,"_", year2)))
ndvi_df_gor_2018$year <- year2

ndvi_df_mfc2_2018 <- readRDS(file = file.path(read_dir_ndvi, 
                                              paste0("avg_NDVI_stack_", 
                                                     site_1,"_", year2)))
ndvi_df_mfc2_2018$year <- year2

ndvi_df_gor_2019 <- readRDS(file = file.path(read_dir_ndvi, 
                                             paste0("avg_NDVI_stack_", 
                                                    site2,"_", year3)))
ndvi_df_gor_2019$year <- year3

ndvi_df_mfc2_2019 <- readRDS(file = file.path(read_dir_ndvi, 
                                              paste0("avg_NDVI_stack_", 
                                                     site_1,"_", year3)))
ndvi_df_mfc2_2019$year <- year3

ndvi_df_gor_2020 <- readRDS(file = file.path(read_dir_ndvi, 
                                             paste0("avg_NDVI_stack_", 
                                                    site2,"_", year4)))
ndvi_df_gor_2020$year <- year4

ndvi_df_mfc2_2020 <- readRDS(file = file.path(read_dir_ndvi, 
                                              paste0("avg_NDVI_stack_", 
                                                     site_1,"_", year4)))
ndvi_df_mfc2_2020$year <- year4

# A plot showing ndvi of GOR site during four years & precipitaion in the same site and year

# change the column name from "DOY" to "doy"
#names(gor1_meteo_df$DOY) <- "doy"
#names(mfc2_meteo_df[8]) <- "doy"

#GOR
meteo_gor_df_2017 <- gor1_meteo_df[gor1_meteo_df$Year == 2017,]
meteo_gor_df_2018 <- gor1_meteo_df[gor1_meteo_df$Year == 2018,]
meteo_gor_df_2019 <- gor1_meteo_df[gor1_meteo_df$Year == 2019,]
meteo_gor_df_2020 <- gor1_meteo_df[gor1_meteo_df$Year == 2020,]

#MFC2
meteo_mfc2_df_2017 <- mfc2_meteo_df[mfc2_meteo_df$Year == 2017,]
meteo_mfc2_df_2018 <- mfc2_meteo_df[mfc2_meteo_df$Year == 2018,]
meteo_mfc2_df_2019 <- mfc2_meteo_df[mfc2_meteo_df$Year == 2019,]
meteo_mfc2_df_2020 <- mfc2_meteo_df[mfc2_meteo_df$Year == 2020,]


# change the name of column "DOY" to "doy"
#GOR
names(meteo_gor_df_2017)[8] <- "doy"
names(meteo_gor_df_2018)[8] <- "doy"
names(meteo_gor_df_2019)[8] <- "doy"
names(meteo_gor_df_2020)[8] <- "doy" 

#MFC2
names(meteo_mfc2_df_2017)[8] <- "doy"
names(meteo_mfc2_df_2018)[8] <- "doy"
names(meteo_mfc2_df_2019)[8] <- "doy"
names(meteo_mfc2_df_2020)[8] <- "doy"


#combine meteo dfs from different sites and years
meteo_df_mfcs_yrs <- rbind(meteo_mfc2_df_2017, meteo_mfc2_df_2018,
                           meteo_mfc2_df_2019, meteo_mfc2_df_2020)

meteo_df_gor_yrs <- rbind(meteo_gor_df_2017, meteo_gor_df_2018,
                           meteo_gor_df_2019, meteo_gor_df_2020)

meteo_df_sites_yrs <- rbind(meteo_gor_df_2017, meteo_gor_df_2018,
                            meteo_gor_df_2019, meteo_gor_df_2020,
                            meteo_mfc2_df_2017, meteo_mfc2_df_2018,
                            meteo_mfc2_df_2019, meteo_mfc2_df_2020)


# merge 4 dataframes (NDVI and Meteorological)
list_meteo_gor_df <- list(meteo_gor_df_2017,
                     meteo_gor_df_2018,
                     meteo_gor_df_2019,
                     meteo_gor_df_2020
                     )

list_ndvi_gor_df <- list(ndvi_df_gor_2017,
                         ndvi_df_gor_2018,
                         ndvi_df_gor_2019,
                         ndvi_df_gor_2020
                        )


# for (i in c(1: length(list_meteo_gor_df))){
#     merged <- merge(list_meteo_gor_df[[i]], list_ndvi_gor_df[[i]], 
#                                by= "doy",
#                                all.x= FALSE, 
#                                all.y= TRUE)
#     
#     row.names(merged) <- c(1:(nrow(merged)))
#     
#     # subset the merged dataframe so that it has only specific columns
#     merged_subset <- subset(merged, 
#                                  select = -c(Month, Day, site.x, date, site.y))
#     
# }

# GOR
merge_df_gor_2017 <- merge(meteo_gor_df_2017, ndvi_df_gor_2017, 
                           by= "doy",
                           all.x= FALSE, 
                           all.y= TRUE)

row.names(merge_df_gor_2017) <- c(1:(nrow(merge_df_gor_2017)))

# subset the merged dataframe so that it has only specific columns
merge_df_gor_2017_subset <- subset(merge_df_gor_2017, 
                                   select = -c(Month, Day, date, site.y)) 

merge_df_gor_2018 <- merge(meteo_gor_df_2018, ndvi_df_gor_2018,
                           by= "doy",
                           all=FALSE
                           #all.x= FALSE,
                           #all.y= TRUE
                           )

#Using merge() function created "NAs" therefore I used "natural_join()" for GOR 2018
#merge_df_gor_2018 <- natural_join(meteo_gor_df_2018, ndvi_df_gor_2018, by="doy")

row.names(merge_df_gor_2018) <- c((nrow(merge_df_gor_2017) + 1): (nrow(merge_df_gor_2017) + nrow(merge_df_gor_2018)))

# subset the merged dataframe so that it has only specific columns
merge_df_gor_2018_subset <- subset(merge_df_gor_2018, 
                                   select = -c(Month, Day, date, site.y)) 

merge_df_gor_2019 <- merge(meteo_gor_df_2019, ndvi_df_gor_2019, 
                   by= "doy",
                   all.x= FALSE, 
                   all.y= TRUE)
row.names(merge_df_gor_2019) <- c((nrow(merge_df_gor_2018) + 1): (nrow(merge_df_gor_2018) + nrow(merge_df_gor_2019)))

# subset the merged dataframe so that it has only specific columns
merge_df_gor_2019_subset <- subset(merge_df_gor_2019, 
                                   select = -c(Month, Day, date, site.y))



merge_df_gor_2020 <- merge(meteo_gor_df_2020, ndvi_df_gor_2020, 
                           by= "doy",
                           all.x= FALSE, 
                           all.y= TRUE)
row.names(merge_df_gor_2020) <- c((nrow(merge_df_gor_2019) + 1): (nrow(merge_df_gor_2019) + nrow(merge_df_gor_2020)))

# subset the merged dataframe so that it has only specific columns
merge_df_gor_2020_subset <- subset(merge_df_gor_2020, 
                                   select = -c(Month, Day, date, site.y))




# MFC2
merge_df_mfc2_2017 <- merge(meteo_mfc2_df_2017, ndvi_df_mfc2_2017, 
                           by= "doy",
                           all.x= FALSE, 
                           all.y= TRUE)

row.names(merge_df_mfc2_2017) <- c(1:(nrow(merge_df_mfc2_2017)))

# subset the merged dataframe so that it has only specific columns
merge_df_mfc2_2017_subset <- subset(merge_df_mfc2_2017, 
                                   select = -c(Month, Day, date, site.y)) 

merge_df_mfc2_2018 <- merge(meteo_mfc2_df_2018, ndvi_df_mfc2_2018, 
                           by= "doy",
                           all.x= FALSE, 
                           all.y= TRUE)

row.names(merge_df_mfc2_2018) <- c((nrow(merge_df_mfc2_2017) + 1): (nrow(merge_df_mfc2_2017) + nrow(merge_df_mfc2_2018)))

# subset the merged dataframe so that it has only specific columns
merge_df_mfc2_2018_subset <- subset(merge_df_mfc2_2018, 
                                   select = -c(Month, Day, date, site.y)) 

merge_df_mfc2_2019 <- merge(meteo_mfc2_df_2019, ndvi_df_mfc2_2019, 
                           by= "doy",
                           all=FALSE,
                           #all.x= FALSE, 
                           #all.y= TRUE
                           )
row.names(merge_df_mfc2_2019) <- c((nrow(merge_df_mfc2_2018) + 1): (nrow(merge_df_mfc2_2018) + nrow(merge_df_mfc2_2019)))

# subset the merged dataframe so that it has only specific columns
merge_df_mfc2_2019_subset <- subset(merge_df_mfc2_2019, 
                                   select = -c(Month, Day, date, site.y))



merge_df_mfc2_2020 <- merge(meteo_mfc2_df_2020, ndvi_df_mfc2_2020, 
                           by= "doy",
                           all.x= FALSE, 
                           all.y= TRUE)
row.names(merge_df_mfc2_2020) <- c((nrow(merge_df_mfc2_2019) + 1): (nrow(merge_df_mfc2_2019) + nrow(merge_df_mfc2_2020)))

# subset the merged dataframe so that it has only specific columns
merge_df_mfc2_2020_subset <- subset(merge_df_mfc2_2020, 
                                   select = -c(Month, Day,  date, site.y))

# Writing dataframe data to text file
write_dir <- here("Desktop","Playground_dir_11","output")

# write the combined dataframes(NDVI and meteo) to drive
# this part needs work so that it can work on the list; 
# the function works fine for just one dataframe ; make it so that it can work on a group of dataframes
dflist <- list(merge_df_gor_2017_subset, 
                merge_df_gor_2018_subset, 
                merge_df_gor_2019_subset, 
                merge_df_gor_2020_subset,
                merge_df_mfc2_2017_subset, 
                merge_df_mfc2_2018_subset, 
                merge_df_mfc2_2019_subset, 
                merge_df_mfc2_2020_subset)


names_dflist <- c("merge_df_gor_2017", 
                   "merge_df_gor_2018", 
                   "merge_df_gor_2019", 
                   "merge_df_gor_2020",
                   "merge_df_mfc2_2017", 
                   "merge_df_mfc2_2018", 
                   "merge_df_mfc2_2019", 
                   "merge_df_mfc2_2020")



# this function writes the dataframe(x) into drive
write <- function(x){write.table(x, file = file.path(write_dir, paste0(deparse(substitute(x)),".txt")), sep = "\t", row.names = TRUE, col.names = NA)}


# this function gets dataframe as "x" and name of the file as "y"
write_2 <- function(x,y){write.table(x, file = file.path(write_dir, paste0(y,".txt")), sep = "\t", row.names = TRUE, col.names = NA)}


# this loop executes the "write_2" function on all years and sites
for (i in c(1: length(dflist))){
    print(names_dflist[i])
    write_2(dflist[i], names_dflist[i])
}


### Start point: Read in the previously created dataframes ############################# 
# In future, If there was a change in your input data(Sentinel 2) tiles 
# you have to execute "Creation process of dataframes(meteorological and NDVI)" section above, otherwise 
# you can execute following lines to load in .txt files and do the plotting 

write_dir <- here("Desktop", "Playground_dir_11", "output")

# GOR1
NAMES_1 <- read.table(file = file.path(write_dir,"merge_df_gor_2017.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_1 <- read.table(file = file.path(write_dir,"merge_df_gor_2017.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_1 <- DATA_1[, 1:10]
names(DATA_1) <- NAMES_1


NAMES_2 <- read.table(file = file.path(write_dir,"merge_df_gor_2018.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_2 <- read.table(file = file.path(write_dir,"merge_df_gor_2018.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_2 <- DATA_2[, 1:10]
names(DATA_2) <- NAMES_2



NAMES_3 <- read.table(file = file.path(write_dir,"merge_df_gor_2019.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_3 <- read.table(file = file.path(write_dir,"merge_df_gor_2019.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_3 <- DATA_3[, 1:10]
names(DATA_3) <- NAMES_3

NAMES_4 <- read.table(file = file.path(write_dir,"merge_df_gor_2020.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_4 <- read.table(file = file.path(write_dir,"merge_df_gor_2020.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_4 <- DATA_4[, 1:10]
names(DATA_4) <- NAMES_4



# MFC2
NAMES_5 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2017.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_5 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2017.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_5 <- DATA_5[, 1:10]
names(DATA_5) <- NAMES_5


NAMES_6 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2018.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_6 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2018.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_6 <- DATA_6[, 1:10]
names(DATA_6) <- NAMES_6



NAMES_7 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2019.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_7 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2019.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_7 <- DATA_7[, 1:10]
names(DATA_7) <- NAMES_7

NAMES_8 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2020.txt")
                      , nrow = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_8 <- read.table(file = file.path(write_dir,"merge_df_mfc2_2020.txt"), 
                     skip = 1, stringsAsFactors = FALSE, sep = "\t")

DATA_8 <- DATA_8[, 1:10]
names(DATA_8) <- NAMES_8



# combine 2017,2018,2019,2020 dfs
df_gor_four_yrs <- rbind(DATA_1,
                      DATA_2,
                      DATA_3,
                      DATA_4
                        )

df_mfc2_four_yrs <- rbind( DATA_5,
                DATA_6,
                DATA_7,
                DATA_8
                    )

df_sites_four_yrs <- rbind(df_gor_four_yrs, df_mfc2_four_yrs)


# the function recieves the x (doy)  and decides if the observation is 
# in "wet" , "dry" or "transition" season
three_season <- function(x){
    if(x %in% c( 305:365, 1:60)){
        season <- "wet"
    }else if(x %in% c(121:245)){
        season <- "dry"
    }else if (x %in% c( 246:304,  61:120)){
        season <- "transition"
    }
    return(season)
}

# add a column to dataframe containing season of rows
df_sites_four_yrs$season <- mapply(three_season, df_sites_four_yrs$doy) 
df_gor_four_yrs$season <- mapply(three_season, df_gor_four_yrs$doy)
df_mfc2_four_yrs$season <- mapply(three_season, df_mfc2_four_yrs$doy)

# write the new dataframes with season to drive 
write.csv(df_gor_four_yrs,
          here("Desktop", "Playground_dir_11", "output", "df_gor1_with_Season.csv"), 
          row.names = FALSE)

write.csv(df_mfc2_four_yrs,
          here("Desktop", "Playground_dir_11", "output", "df_mfc2_with_Season.csv"), 
          row.names = FALSE)

# read in the data 
df_gor_four_yrs <- read.csv(here::here("Desktop", "Playground_dir_11",
                                       "output", "df_gor1_with_Season.csv"))
df_mfc2_four_yrs <- read.csv(here::here("Desktop", "Playground_dir_11",
                                        "output", "df_mfc2_with_Season.csv"))




if(!dir.exists(here("Desktop", "Playground_dir_11", "tables"))){
    dir.create(here("Desktop", "Playground_dir_11", "tables"))
}
if(!dir.exists(here("Desktop", "Playground_dir_11", "tables", "GOR1"))){
    dir.create(here("Desktop", "Playground_dir_11", "tables", "GOR1"))
}
if(!dir.exists(here("Desktop", "Playground_dir_11", "tables", "MFC2"))){
    dir.create(here("Desktop", "Playground_dir_11", "tables", "MFC2"))
}

# "arsenal" package
#GOR1
my_controls <-  tableby.control(
    test = T,
    total = T,
    numeric.test = "anova", 
    cat.test = "chisq",
    numeric.stats = c("meansd", "medianq1q3", "range"),
    cat.stats = c("countpct", "Nmiss2"),
    stats.labels = list(
        meansd = "Mean (SD)",
        medianq1q3 = "Median (Q1, Q3)",
        range = "Min - Max"
    )
)

# Be aware that NDVI in this dataframe represents 
# the mean NDVI value for this area of interest on a given day.
my_labels <- list(
    T_Celsius = "Temperature (°C)",
    Prec_mm = "Precipitation (mm)",
    ET0_mm = "Evapotranspiration (mm)",
    meanNDVI = "mean NDVI over study area"
)

df_gor_four_yrs$season_factor <- as.factor(df_gor_four_yrs$season)
table_two <- tableby(season_factor ~ .,
                     data = df_gor_four_yrs,
                     control = my_controls
)

summary(table_two,
        labelTranslations = my_labels,
        title = "Summary Statistic of GOR1 Dataframe"
)



# MFC2
my_controls <-  tableby.control(
    test = T,
    total = T,
    numeric.test = "anova", 
    cat.test = "chisq",
    numeric.stats = c("meansd", "medianq1q3", "range"),
    cat.stats = c("countpct", "Nmiss2"),
    stats.labels = list(
        meansd = "Mean (SD)",
        medianq1q3 = "Median (Q1, Q3)",
        range = "Min - Max"
    )
)


my_labels <- list(
    T_Celsius = "Temperature (°C)",
    Prec_mm = "Precipitation (mm)",
    ET0_mm = "Evapotranspiration (mm)",
    meanNDVI = "mean NDVI over study area"
)

df_mfc2_four_yrs$season_factor <- as.factor(df_mfc2_four_yrs$season)
table_two <- tableby(season_factor ~ .,
                     data = df_mfc2_four_yrs,
                     control = my_controls
)

summary(table_two,
        labelTranslations = my_labels,
        title = "Summary Statistic of MFC2 Dataframe"
)


# Read the Topographic Attributes (dataframes) for further analysis
# Created in "15_DEM_Gridded_Topographic_Attributes.R" script
# input_dir <- here("Desktop", "Playground_dir_14")
# mfc2_dem_df <- readRDS(file=file.path(input_dir, "MFC2", "Extracted_dfs", "mfc2_dem_df"))
# 
# gor1_dem_df <- readRDS(file=file.path(input_dir, "GOR", "Extracted_dfs", "gor1_dem_df"))


# NDVI and Precipitation in four years for each site 
plot_dir <- here("Desktop","Playground_dir_11", "plots")

x11()

# Change the "site" manually 
site <- "GOR" #   "MFC2"
if(site=="MFC2"){
    data1 <- meteo_df_mfcs_yrs
    data2 <- df_mfc2_four_yrs
} else {
    data1 <- meteo_df_gor_yrs
    data2 <- df_gor_four_yrs
}

ggplot(data1, aes(doy, Prec_mm )) + 
    geom_col(data=data1, aes(doy, Prec_mm))+
    #geom_bar(stat = "identity") + #tells ggplot2 you'll provide the y-values for the barplot, rather than counting the aggregate number of rows for each x value
    geom_point(data =  data2, aes(doy, meanNDVI*100)) + 
    geom_line(data = data2 , aes(doy, meanNDVI*100)) +
    stat_smooth(data = data2 , aes(doy, meanNDVI*100), colour="blue")+
    scale_x_continuous(breaks = seq(1, 365, by = 7))+  
    scale_y_continuous(breaks = seq(0, 100 , by= 10))+ #for Precipitation; 
    scale_y_continuous(sec.axis = sec_axis(~./100, name = "NDVI [-]"))+
    theme(text = element_text(size = 8),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
    xlab("doy") + 
    ylab("Precipitation [mm]") +
    labs(title = sprintf("NDVI and Precipitaion in %s ", site))+
    removeGrid(x = TRUE, y = FALSE)+
    #facet_grid(Year~site.x) 
    facet_wrap(~Year, ncol = 1)# vars(Year, site.x), ncol = 2

ggsave(here(plot_dir, paste0(sprintf("precipitation_ndvi_%s_years", tolower(site)), ".pdf")), 
       scale = 1, 
       #width = 15, 
       #height = 10,
       dpi = 300)

# NDVI vs. Evapotranspiration in years
# remember to change the "site"
x11()
site <-  "GOR"#    "MFC2"
if(site=="MFC2"){
    data1 <- meteo_df_mfcs_yrs
    data2 <- df_mfc2_four_yrs
} else {
    data1 <- meteo_df_gor_yrs
    data2 <- df_gor_four_yrs
}
ggplot(data1, aes(doy, ET0_mm )) + 
    geom_col(data=data1, aes(doy, ET0_mm))+
    #geom_bar(stat = "identity") + #tells ggplot2 you'll provide the y-values for the barplot, rather than counting the aggregate number of rows for each x value
    geom_point(data =  data2, aes(doy, meanNDVI*10)) + 
    geom_line(data = data2 , aes(doy, meanNDVI*10)) +
    stat_smooth(data = data2 , aes(doy, meanNDVI*10), colour="blue")+
    scale_x_continuous(breaks = seq(1, 365, by = 7))+  
    scale_y_continuous(breaks = seq(0, 10 , by= 1))+ #for Evapotranspiration; 
    scale_y_continuous(sec.axis = sec_axis(~./10, name = "NDVI [-]"))+
    theme(text = element_text(size = 8),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
    xlab("doy") + 
    ylab("Evapotranspiration [mm]") +
    labs(title = sprintf("NDVI and Evapotranspiration in %s ", site))+
    removeGrid(x = TRUE, y = FALSE)+
    #facet_grid(Year~site.x) 
    facet_wrap(~Year, ncol = 1)# vars(Year, site.x), ncol = 2

ggsave(here(plot_dir, paste0(sprintf("evapotranspiration_ndvi_%s_years", tolower(site)), ".pdf")), 
       scale = 1, 
       #width = 15, 
       #height = 10,
       dpi = 300)    

# this gives a fun plot to look at 
library(GGally)
#df_ok <- filter(df_mfc2_four_yrs,Prec_mm< 1)
plot_dir <- here("Desktop","Playground_dir_11", "plots")
site <- "GOR1"# "MFC2"
if(site=="MFC2"){
    data <- df_mfc2_four_yrs
} else {
    data <- df_gor_four_yrs
}

# here I save the dataframes to send them to Sarah
write.table(df_mfc2_four_yrs,
            file.path(here("Desktop", "dataframe_to_Sarah"),"mfc2_yrs.txt"), sep="\t",row.names=FALSE)

write.table(df_gor_four_yrs,
            file.path(here("Desktop", "dataframe_to_Sarah"),"gor1_yrs.txt"), sep="\t",row.names=FALSE)


### Correlation Coefficients with P-values appended to Scatter Plot #############################

# Add Correlation Coefficients with P-values to a Scatter Plot
library(ggpubr)

# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
df_sites_four_yrs$Year <- as.character(df_sites_four_yrs$Year)
df_sites_four_yrs$Year <- factor(df_sites_four_yrs$Year, levels = c("2017", "2018", "2019", "2020"))
# Convert "Year" as a grouping variable
df_sites_four_yrs$Year <- as.factor(df_sites_four_yrs$Year)
df_sites_four_yrs$site.x <- as.factor(df_sites_four_yrs$site.x)

df_mfc2_four_yrs$Year <- as.character(df_mfc2_four_yrs$Year)
df_mfc2_four_yrs$Year <- factor(df_mfc2_four_yrs$Year, levels = c("2017", "2018", "2019", "2020"))


# Temperature vs. mean NDVI
method <- "spearman" # "pearson"
sp <- ggscatter(df_sites_four_yrs, x = "T_Celsius", #df_sites_four_yrs
                y = "meanNDVI",
                parse=TRUE,
               # combine = TRUE, ylab = "NDVI",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                fullrange= TRUE, 
                color = "Year",
                title = sprintf("%s correlation coefficient & p-value for Temp. vs. NDVI",method),
                palette = c("blue", "red", "green","orange"),
                facet.by= "site.x",#"Year",##c("Year", "site.x"),
                xlab = "Temperature [°C]", 
                ylab = "NDVI[-]", 
                conf.int = FALSE # Add confidence interval
) 

# Add correlation coefficient   
sp+stat_cor(aes(color = Year), label.x = 4 ,
            method = method,
            #label = paste0("R = ", ..r.., ", P = ", ..p..),
            label.x.npc =  'left', 
            label.y.npc = 'bottom')

ggsave(here(plot_dir, paste0(sprintf("Temp_ndvi_correlation_r_%s_sites_yrs", method), ".pdf")), #spearman
       scale = 2, 
       #width = 15, 
       #height = 10,
       dpi = 300)

# scatterplot with another package

# ggplot(df_sites_four_yrs, aes(x = T_Celsius, y = meanNDVI, group = Year)) +
#     geom_smooth(method="lm")+
#     geom_point()+
#     facet_wrap(~site.x)+
#     ggpmisc::stat_poly_eq(formula = y ~ x, 
#                  aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
#                  parse = TRUE,
#                  label.x.npc = "right",
#                  vstep = 0.05) # sets vertical spacing



# Evapotranspiration vs. mean NDVI
method <- "pearson" #"spearman" # 
sp_evap <- ggscatter(df_sites_four_yrs, x = "ET0_mm", y = "meanNDVI",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                color = "Year",
                parse=TRUE,
                palette = c("blue", "red", "green","orange"),
                facet.by= "site.x",
                title = sprintf("%s correlation coefficient & p-value for Evapotran. vs. NDVI",method),
                xlab = "Evapotranspiration [mm]", 
                ylab = "NDVI[-]", 
                conf.int = FALSE # Add confidence interval
)

# Add correlation coefficient
sp_evap+stat_cor(aes(color = Year), label.x = 4 ,
            method = method,
            #label = paste0("R = ", ..r.., ", P = ", ..p..),
            label.x.npc =  'left', 
            label.y.npc = 'top')

ggsave(here(plot_dir, paste0(sprintf("Evapotranspiration_ndvi_correlation_r_%s_sites_yrs", method), ".pdf")), 
       scale = 2, 
       #width = 15, 
       #height = 10,
       dpi = 300)

# Precipitation vs. mean NDVI
method <- "spearman" #"pearson" 
sp_precip <- ggscatter(df_sites_four_yrs, x = "Prec_mm", y = "meanNDVI",
                     add = "reg.line",  # Add regressin line
                     add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                     color = "Year",
                     parse=TRUE,
                     palette = c("blue", "red", "green","orange"),
                     facet.by= "site.x",#c("Year", "site.x")
                     title = sprintf("%s correlation coefficient & p-value for Precipitation vs. NDVI", method),
                     xlab = "Precipitation [mm]", 
                     ylab = "NDVI[-]",
                     conf.int = FALSE # Add confidence interval
)

# Add correlation coefficient
sp_precip + stat_cor(aes(color = Year), label.x = 4 ,
                     method = method,
                     #label = paste0("R = ", ..r.., ", P = ", ..p..),
                     label.x.npc =  'left', 
                     label.y.npc = 'top')


ggsave(here(plot_dir, paste0(sprintf("Precipitation_ndvi_correlation_r_%s_sites_yrs", method), ".pdf")), 
       scale = 2, 
       #width = 15, 
       #height = 10,
       dpi = 300)

# Group by hydrological seasons and correlation
# Meteoparam vs. mean NDVI

method <- "spearman" #"pearson"

# change the parameter each time in the next line
meteo_param <- "ET0_mm"  #"Prec_mm" #"T_Celsius"# #  #

#"param_name" is chosen and used in the title of plots & plot names
if ( meteo_param == "T_Celsius") {
    param_name <- "Temperature"
} else if ( meteo_param == "Prec_mm") {
    param_name <- "Precipitation"
} else if ( meteo_param == "ET0_mm") {
    param_name <- "Evapotranspiration"
} 
sp_1 <- ggscatter(df_sites_four_yrs, x = meteo_param , y = "meanNDVI",
                       add = "reg.line",  # Add regressin line
                       add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
                       color = "season",
                       palette = "jco", 
                       title=sprintf("Seasonal %s correlation coefficient & p-value for %s vs. NDVI", method, param_name),
                       facet.by= c("site.x","Year"),#, "site.x"
                       conf.int = FALSE # Add confidence interval
) 

# Add correlation coefficient
sp_1 + stat_cor(aes(color = season), show.legend = FALSE, 
                     method = method, label.x = 3)# label.x is the number of unique values in "season"


ggsave(here(plot_dir, paste0(sprintf("Seasonal %s_ndvi_correlation_r_%s_sites_yrs", param_name, method), ".pdf")), 
       scale = 2, 
       #width = 15, 
       #height = 10,
       dpi = 300)




# plot the correlation plot of dataframes 
ggpairs(data %>% select(ET0_mm, Prec_mm, meanNDVI), #df_mfc2_four_yrs
        title = site)#

ggsave(here(plot_dir, paste0(sprintf("evapo_precip_ndvi_%s_correlation", tolower(site)), ".pdf")), 
       scale = 1, 
       #width = 15, 
       #height = 10,
       dpi = 300)





# combine two plots 
# source :https://github.com/tidyverse/ggplot2/wiki/Align-two-plots-on-a-page


# subset the merged dataframe so that it has only specific columns
merge_df_gor_2019_subset <- subset(merge_df_gor_2019, 
                          select = -c(Month, Day, DATE, site.x, year, date, site.y))



# calculating correlation 
# method = c("pearson", "kendall", "spearman")
M <- cor(merge_df_gor_2019_subset)
corrplot(M, method = "circle")

M_pearson <- cor(merge_df_gor_2019_subset,
         method = "pearson")
corrplot(M_pearson, method = "number", # or "circle"
                    type = "upper")


res <- cor.test(merge_df_gor_2019_subset$meanNDVI, merge_df_gor_2019_subset$T_Celsius, 
         method=c("pearson", "kendall", "spearman"))

# Extract the p.value
res$p.value

# Extract the correlation coefficient
res$estimate



# Use chart.Correlation(): Draw scatter plots
chart.Correlation(merge_df_gor_2019_subset, 
                  histogram = TRUE, pch = 19)





x11()
ggplot(meteo_gor_df_2019, aes(doy, Prec_mm)) + # , colour = year
    geom_bar(stat = "identity") + #tells ggplot2 you'll provide the y-values for the barplot, rather than counting the aggregate number of rows for each x value
    geom_point(data = ndvi_df_gor_2019 , aes(doy, meanNDVI)) +
    #scale_color_viridis(discrete=FALSE, option = "cividis", direction = -1)+# option="inferno"
    scale_x_continuous(breaks = seq(1, 365, by = 7))+  
    #scale_y_continuous(breaks = seq(0, 1, by=0.1))+
    scale_y_continuous(breaks = seq(0, 40, by=1))+
    theme(text = element_text(size = 8),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
    xlab("DOY") + 
    #ylab("NDVI [-]") +
    removeGrid(x = TRUE, y = FALSE)

x11()
ggplot(meteo_gor_df_2019, aes(x = doy, y =Prec_mm))+
        geom_bar(stat = "identity")+
        geom_point(data = ndvi_df_gor_2019 ,aes(x=doy,y = meanNDVI*20))+
        #geom_smooth(data= DATA_3 , aes(y=meanNDVI, x=doy,y~x), 
        #             method = "loess", span = 0.4) + 
        #geom_line(data = ndvi_df_gor_2019, linetype= "dashed") +
        scale_y_continuous(sec.axis = sec_axis(~./30, name = "NDVI [-]"))+
        labs(y = "Precipitation [mm]",
                x = "Day of Year",
                colour = "Parameter")





