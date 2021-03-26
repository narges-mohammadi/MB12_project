#######
# The following code is for 
# Exploratory Data ploting & analysis of Meteorological Inputs
# of 2 study sites GOR & MFC2
########


setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster",
          "leaflet","rasterVis","gridExtra","RColorBrewer",
          "plotly","RStoolbox","sp","IRdisplay","reshape", 
          "here", "patchwork","readxl", "ggExtra","viridis"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)

# read the -xls file into R
meteo_GOR <- read_xlsx(here("Desktop","Playground_dir_11", 
                    "20210222_TABLE_ClimateData_MFC2_GOR1_updated.xlsx"),  #GOR1_P_ET0.xls
                    col_names = c("Year", "Month", "Day", "Prec_mm", "T_Celsius", "ET0_mm", "DATE","DOY"),
                    range = "GOR1!A2:H1876" #cell_rows(2:1876)
                    )

#"Budget!B2:G14"
meteo_MFC2 <- read_xlsx(here("Desktop","Playground_dir_11", 
                             "20210222_TABLE_ClimateData_MFC2_GOR1_updated.xlsx"),  #MFC2_P_ET0.xls
                        col_names = c("Year", "Month", "Day", "Prec_mm", "T_Celsius", "ET0_mm", "DATE", "DOY"),
                        range = "MFC2!A2:H1749"
                        )


char_date  <-  function(x){
  if (x["Day"] < 10 ) 
    if (x["Month"] >= 10)
      return(gsub(" ", "",paste0(x["Year"], x["Month"], paste0(0,x["Day"]))))
  
  else 
    return(gsub(" ", "", paste0(x["Year"], paste0(0,x["Month"]), paste0(0,x["Day"]))))
  
  
  else if (x["Day"] >= 10 ) 
    if (x["Month"] >= 10)
      return(gsub(" ", "",paste0(x["Year"],x["Month"],x["Day"])))
  
  else
    return(gsub(" ", "", paste0(x["Year"], paste0(0,x["Month"]), x["Day"])))
  
}


char_to_doy <- function(x){
  return(as.numeric(strftime(lubridate::ymd(x["char_date"]),format = "%j")))
}

# Add "char_date" column to dataframes (needed for DOY(next step))
# meteo_GOR$char_date <- apply(meteo_GOR, 1, 
#                              FUN=char_date)
# 
# meteo_MFC2$char_date <- apply(meteo_MFC2, 1, 
#                               FUN=char_date)

# Add "DOY" column to dataframes
# meteo_GOR$DOY <- apply(meteo_GOR, 1, 
#                        FUN=char_to_doy)
#   
# meteo_MFC2$DOY <- apply(meteo_MFC2, 1, 
#                               FUN=char_to_doy)

# Add "site" column to dataframes
#meteo_GOR$site <- "GOR1"
#meteo_MFC2$site <- "MFC2"

# Add "site" column to '"tbl_df" "tbl" "data.frame"'
meteo_GOR <- meteo_GOR %>% tibble::add_column(site="GOR1")
meteo_MFC2 <- meteo_MFC2 %>% tibble::add_column(site="MFC2")

# Combine 2 dfs into one
#meteo_combi <- rbind(meteo_MFC2, meteo_GOR)

# Combine 2 '"tbl_df" "tbl" "data.frame"'
meteo_combi <- dplyr::bind_rows(meteo_MFC2, meteo_GOR)

# Save the dfs to the drive 
save_dir <- file.path(here("Desktop", "Playground_dir_11", "output"))
saveRDS(meteo_GOR, file = file.path(save_dir, 
                                    paste0("meteorological_df_",meteo_GOR$site[1])))

saveRDS(meteo_MFC2, file = file.path(save_dir, 
                                    paste0("meteorological_df_",meteo_MFC2$site[1])))


## Plots 

site <- meteo_GOR$site[1] #   meteo_MFC2$site[1] 
# Precipitation
x11()
ggplot(meteo_GOR, aes(DOY, Prec_mm, colour = Year)) + 
  geom_line()+
  scale_color_viridis(discrete=FALSE, option = "cividis" , direction = -1)+# option="inferno"
  scale_x_continuous(breaks = seq(1, 365, by = 7))+  
  scale_y_continuous(breaks = seq(0, 100, by = 10))+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))+
  xlab("DOY") + 
  ylab("Precipitation [mm]") +
  labs(title = sprintf("Precipitaion near %s Site", 
                       site))+
  ggExtra::removeGrid(x = TRUE, y = FALSE)+
  #facet_wrap(~site,ncol = 1)
  facet_wrap(~Year,ncol = 1)

write_dir <- here("Desktop", "Playground_dir_11","plots")
ggsave(here(write_dir, paste0(sprintf("Precipitation_%s", tolower(site)), ".pdf")), 
       #scale = 2, 
       dpi = 300
)

# Evatranspiration
site <- meteo_GOR$site[1]   # meteo_MFC2$site[1]
if (site=="MFC2") data <- meteo_MFC2 else data <- meteo_GOR

x11()
ggplot(data, aes(DOY, ET0_mm, colour = Year)) +
  geom_line()+
  scale_color_viridis(discrete=FALSE, option = "inferno", direction = -1)+ # To change default coloring
  scale_x_continuous(breaks = seq(1, 365, by = 7))+  # "breaks" used to point out the DOY 
  scale_y_continuous(breaks = seq(0, 10, by = 1))+
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
  xlab("DOY") + 
  ylab("Evatranspiration [mm]") +
  labs(title = sprintf("Evatranspiration(mm) near %s", site))+
  ggExtra::removeGrid(x = TRUE, y= FALSE) + # To get rid of vertical lines in the plot
  facet_wrap(~Year, ncol = 1) 

write_dir <- here("Desktop", "Playground_dir_11","plots")
ggsave(here(write_dir, paste0(sprintf("Evatranspiration_%s", tolower(site)), ".pdf")), 
       #scale = 3,
       dpi = 300
)

# Box plot of monthly rainfall 
site <-  meteo_MFC2$site[1]# meteo_MFC2_ok$site[1]   meteo_GOR$site[1]
if (site=="MFC2") data_2 <- meteo_MFC2 else data_2 <- meteo_GOR
x11()
#meteo_MFC2_ok <- filter(meteo_MFC2, Prec_mm > 0,Prec_mm <= 15)
ggplot(data_2, aes(Month, Prec_mm)) + #meteo_MFC2: original data with outliers in precipitation
      geom_boxplot(aes(Month, Prec_mm, group = cut_width(Month, 1))
                   )+ #,outlier.shape = NA
      scale_x_continuous(breaks = seq(1, 12, by = 1))+
      scale_y_continuous(breaks = seq(0, 90, by = 5))+
      ylab("Precipitation [mm]") +
      labs(title = paste0(sprintf("Monthly Precipitaion near %s Site", 
                           site)," over years"))+
      theme(text = element_text(size = 10),
        aspect.ratio= 12/16)+
      facet_wrap(~Year,ncol =3)

write_dir <- here("Desktop", "Playground_dir_11","plots")

ggsave(here(write_dir, paste0(sprintf("boxplot_monthly_precipitation_%s", 
                                       tolower(site)), 
                                       ".pdf")), 
       #scale = 3, 
       dpi = 300)      

boxplot(Prec_mm~Month, 
        data = meteo_MFC2)

# A small change in dataframe to have the month abbreviation 
# instead of number of the month in plot
meteo_MFC2$month_abb <- month.abb[meteo_MFC2$Month]
meteo_GOR$month_abb <- month.abb[meteo_GOR$Month]
meteo_combi$month_abb <- month.abb[meteo_combi$Month]

# temperature
x11()
site <-  meteo_GOR$site[1] # meteo_MFC2$site[1]
ggplot(meteo_GOR, aes(month_abb, T_Celsius)) +
  geom_boxplot(aes(month_abb, T_Celsius, group = cut_width(Month, 1))) +
  labs(title = sprintf("Monthly Temperature near %s Site", site)) +
  scale_x_discrete(limits = month.abb)+ # to order the months chronologically
  xlab("month") + 
  ylab("Temperature [°C]") +
  facet_wrap(~Year,ncol = 1)

# boxplot(T_Celsius~Month, 
#         data = meteo_MFC2)
  
write_dir <- here("Desktop", "Playground_dir_11","plots")
ggsave(here(write_dir, paste0(sprintf("boxplot_monthly_temperature_%s", 
                                      tolower(site)), 
                                      "_years.pdf")), 
       #scale = 3, 
       dpi = 300) 

# Analysis
summary(meteo_combi)
summary(meteo_MFC2)

