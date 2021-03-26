#######################################
# The following code is in essence a batch script that
# automates calling "Biophysical Operator" of SNAP in R
# The procedure follows this manual: "https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjGn9vPsIPuAhXHyaQKHZhECbkQFjAAegQIAxAC&url=https%3A%2F%2Frus-copernicus.eu%2Fportal%2Fwp-content%2Fuploads%2Flibrary%2Feducation%2Ftraining%2FLAND11_VegetationMonitoring4Agri_Italy_Tutorial.pdf&usg=AOvVaw2obiJ9d76n2dBMoP9JfNAQ"
# In case the previous doesn't work: this youtube channel: RUS 
#######################################


setwd("C:/Users/sanaz/")

#1: Load R packages
## Install & load packages
pck <- (c("tidyr","rgdal","ggplot2","raster","leaflet","rasterVis","gridExtra","RColorBrewer","plotly","RStoolbox","sp","IRdisplay","reshape","here", "XML","methods","xml2"))
new_pck <- pck[!pck %in% installed.packages()[,"Package"]]
if(length(new_pck)){install.packages(new_pck)}
sapply(pck , require, character.only=TRUE)

setwd("C:/Users/sanaz/Desktop/Playground_dir_4/")
Input_dir <- here("Desktop","Playground_dir_4","Input")
Output_dir <- here("Desktop","Playground_dir_4","Output")
# Read the .xml file created in SNAP in R
x <- read_xml("C:/Users/sanaz/Desktop/Playground_dir_4/ STEP1_Graph_Prep.xml")# one space before the name of .xml is necessary!!(wierd but true)
xml_name(x)
#xml_children(x)
#xml_text(x)
#xml_find_all(x, ".//graph")
#xml_structure(x)

# Replace the input address with Input directory
#xml_text(xml_contents(xml_find_all(xml_child(x, 2), "//file"))[1]) <- Input_dir
#xml_text(xml_contents(xml_find_all(xml_child(x, 2), "//file"))[2]) <- Output_dir

# Write the modified .xml file into drive
#write_xml(x, file.path(Output_dir,"Modified_graph.xml"), options="no_declaration")


# Define constant variables 
nameStart <- "Subset_S2_MSIL2A_"
nameEnd <- ".dim"

# Function to preprocess the zip files(read, resample, subset and write)
preprocess <- function(i){
  x <- read_xml("C:/Users/sanaz/Desktop/Playground_dir_4/ STEP1_Graph_Prep.xml")
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
  command <- "gpt C://Users//sanaz//Desktop//Playground_dir_4//Output//Modified_graph.xml "
  shell(command)
  
}


# find .zip files in directory
zip_folders <- file.path(Input_dir, list.files(path = Input_dir, pattern = ".zip$"))
# Execute the function on all the .zip sentinel tiles
lapply(zip_folders, FUN = preprocess )


# find .dim files in directory
dim_files <- file.path(Output_dir, list.files(path = Output_dir, pattern = ".dim$"))
new_End <- "_BioOp.tif"

biophysical_operator_r <- function(dim_file) {
  LAI_dir <-  here("Desktop","Playground_dir_4","Output", "Processed")
  base_name <- strsplit(strsplit(dim_file, "/")[[1]][7], "[.]")[[1]][1]
  LAI_pathname <- file.path(LAI_dir, paste0(base_name, new_End) )
  # Run  shell command in r
  command_lai <- sprintf("gpt BioPhysicalOp -Ssource='%s\'  -t '%s\' -f 'GeoTIFF' ", dim_file, LAI_pathname )
  shell(command_lai)
  
}
# Execute the function on all the .dim preprocessed files
lapply(dim_files, FUN = biophysical_operator_r )




