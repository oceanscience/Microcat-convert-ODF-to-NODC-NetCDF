# write a csv file with CRUISE_DESCRIPTION and CRUISE_NAME  from the ODF Cruise Header
# Review and edit this file with correct info before writing ODF to NetCDF

# author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

# Install and load specific package versions
#install.packages("oce")
library(oce)

# directory where R expects to find the local code
readRenviron("~/.Renviron")
source_code_directory <- Sys.getenv("Working_Directory")
setwd(source_code_directory)

# path to ODF files
pathrawdata <- Sys.getenv("ODF_Input_Directory")

# path to where we want the netcdf files to end up
pathprocesseddata <- Sys.getenv("NetCDF_Output_Directory")

file <- list.files(path=pathrawdata, pattern = "MCTD*...*.ODF", full.names = TRUE)
filenames <- list.files(path=pathrawdata, pattern = "MCTD*...*.ODF", full.names = FALSE)

for ( i in 1:length(file) ){
obj <- read.odf(file[i])
cruisedetials1 <- obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION
cruisedetials2 <- obj@metadata$header$CRUISE_HEADER$CRUISE_NAME
#cruisedetials2 <- obj@metadata$header$EVENT_HEADER$EVENT_COMMENTS_1
mp <- data.frame(cruisedetials1, cruisedetials2,filenames[i])
write.table(mp, file = paste0(pathprocesseddata,"/","mooringprojects.csv"), append = TRUE, sep = ",",
            row.names = FALSE,
            col.names = FALSE)
 }

mpall <- read.csv(paste0(pathprocesseddata,"/","mooringprojects.csv"), header = FALSE)
names(mpall) <- c("CRUISE_DESCRIPTION","CRUISE_NAME","File_name")
mpall <- cbind(mpall, "CRUISE_NAME2"=0, "program"=0,"project"=0, "url"=0, "location_description"=0)
write.table(mpall, file = paste0(pathprocesseddata,"/","mooringprojectsh.csv"), append = FALSE, sep = ",",
            row.names = FALSE,col.names = TRUE)

