##########################
# The following code intends to calculate the LAI using .xml and SNAP
# but starting from .SAFE folders (that have .tiff) differs from "6_LAI_snap_Rwrapper.R"
##########################

setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet","rasterVis","gridExtra","RColorBrewer","plotly","RStoolbox","sp","IRdisplay","reshape","here", "XML","methods","xml2"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)

setwd("C:/Users/sanaz/Desktop/Playground_dir_6/")
Input_dir <- here("Desktop","Playground_dir_6","Input")
Output_dir <- here("Desktop","Playground_dir_6","Output")


# Define constant variables 
nameStart <- "Subset_S2_MSIL2A_"
nameEnd <- ".dim"

# Function to preprocess the zip files(read, resample, subset and write)
preprocess <- function(i){
  x <- read_xml("C:/Users/sanaz/Desktop/Playground_dir_6/STEP1_Graph_Prep.xml")
  product_basename <- strsplit(i, "/")[[1]][7]
  ac_date <- strsplit(product_basename, "_")[[1]][3]
  Output_pathname <- file.path(Output_dir, paste0(nameStart, ac_date, nameEnd) )
  # Modify .xml file used in "gpt" & save it to drive
  in_1 <- xml_contents(xml_find_all(xml_child(x, 2), "//file"))[1] 
  in_2 <- xml_contents(xml_find_all(xml_child(x, 2), "//file"))[2]
  xml_text(in_1) <- i
  xml_text(in_2) <- Output_pathname
  write_xml(x, file.path(Output_dir,"Modified_graph.xml"), options="no_declaration")
  # Run a shell command in r
  command <- "gpt C://Users//sanaz//Desktop//Playground_dir_6//Output//Modified_graph.xml "
  shell(command)
  
}


# find .SAFE files in directory
safe_folders <- file.path(Input_dir, list.files(path = Input_dir, pattern = ".SAFE$"))
# Execute the function on all the .SAFE sentinel tiles
lapply(safe_folders, FUN = preprocess )
