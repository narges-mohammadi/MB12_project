###################################################
# The following script is meant for zip folders under 4G of size, to unzip them,
# and remove the zip folders afterwards
###################################################

#set working directory
setwd("C:\\Users\\sanaz\\Documents\\MB12-project\\CREODIAS_part\\data_from_CREODIAS\\L2A_2017")# #"C:/Users/sanaz/Desktop/Playground_dir"

# Load Sentinel2 zip tiles
S2_names_zip <- "C:\\Users\\sanaz\\Documents\\MB12-project\\CREODIAS_part\\data_from_CREODIAS\\L2A_2017"#C:/Users/sanaz/Documents/MB12-project/CREODIAS_part/data_from_CREODIAS"#\\L2A_2017
S2_names_list <- list.files(S2_names_zip,recursive = FALSE, 
                            full.names = TRUE, pattern=".zip$")
# Unzip and remove the zip folder 
S2_names <- lapply(1:length(S2_names_list), 
                   function(x){unzip(S2_names_list[x], exdir="."); 
                       unlink(S2_names_list[x], recursive=TRUE, force = TRUE)}) 



