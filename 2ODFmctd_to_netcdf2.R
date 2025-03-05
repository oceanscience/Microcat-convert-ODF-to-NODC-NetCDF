# writing ODF to NetCDF for CIOOS - Microcat

# author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

# Install and load specific package versions
#install.packages("oce")
library(oce)
library(ncdf4)

# directory where R expects to find the local code
readRenviron("~/.Renviron")
source_code_directory <- Sys.getenv("Working_Directory")
setwd(source_code_directory)

# path to ODF files
pathrawdata <- Sys.getenv("ODF_Input_Directory")

# path to where we want the netcdf files to end up
pathprocesseddata <- Sys.getenv("NetCDF_Output_Directory")

# file <- list.files(path=pathrawdata, pattern = "MCTD*...*.ODF", full.names = TRUE)
# filenames <- list.files(path=pathrawdata, pattern = "MCTD*...*.ODF", full.names = FALSE)


# function file names
source("mctd_nc_edit5.R")
source("asP01.R")

file <- list.files(path=pathrawdata, pattern = "MCTD*...*.ODF") # list of ODF files
gf32p01 <- read.csv('GF3 Code Map_final_v2.csv', header = TRUE) # GF3 to BODC mapping
#list of projects for given year, created using ODFmctd_title_program_project.R and manually editing
projectlist <- read.csv(paste0(pathprocesseddata,"/","mooringprojectsh.csv"), header = TRUE)
vessel_codes <- read.csv('Shared_models_vessel_BIO.csv', header = TRUE) # list of ship names and info

#  loop through each file, convert CRAT to conductivity, add program and project names, correct ship name
for ( f in file ){

obj <- read.odf(paste0(path=pathrawdata,"/",f))

datanames <- names(obj@data)
#check if conductivity exists

# check if conductivity exists and is conductivity Ratio
if (exists("conductivity", where <- obj@metadata$dataNamesOriginal) && obj@metadata$dataNamesOriginal$conductivity == "CRAT_01"){
#change data name of conductivity to conductivityratio and add conductivity
indexcond <- which(grepl("conductivity",datanames))
datanames <-sub("conductivity", "conductivityratio", datanames)
names(obj@data) <- c(datanames)

#Calculate Electrical Conductivity(CNDC_01): c0S/m: conductivity [S/m] from
#conductivity ratio (S.Data.CNDC_01*10.0)./sw_c3515)
#add conductivity to obj
#calculate conductivity, 42.914 mS/cm is conductivity at a standard of 35 salinity, 15 °C temperature, 0 dbar pressure
#10 converts mS/cm to S/m
#eval(' (CNDC_01 * 10)/sw_c3515');',
obj@data[[length(obj@data)+1]] <- (as.numeric(obj@data$conductivityratio)*42.914)/10
names(obj@data)[length(obj@data)] <- c("conductivity")
datanames2 <- names(obj@data)
#add units of conductivity
obj@metadata$units[[length(obj@data)]] <- "unit"
names(obj@metadata$units) <- c(datanames2)
obj@metadata$units$conductivity <- list("unit", "scale")
names(obj@metadata$units$conductivity) <- c("unit", "scale")
obj@metadata$units$conductivity$unit <- "S/m"
#add gf3 name of conductivity
obj@metadata$dataNamesOriginal[[length(obj@data)]]<- "name"
names(obj@metadata$dataNamesOriginal) <- c(datanames2)
obj@metadata$dataNamesOriginal$conductivity <- "CNDC_01"
}

#get project, program and location description
index_projectlist <- grep(f, projectlist$File_name, value = FALSE)
obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION <- projectlist$program[index_projectlist]
obj@metadata$header$CRUISE_HEADER$CRUISE_NAME <- projectlist$project[index_projectlist]
location_description <- projectlist$location_description[index_projectlist]
cruise_name <- "mooring deployment" #projectlist$CRUISE_NAME2[index_projectlist]
info_url <- projectlist$url[index_projectlist]

#get vessel ices code and NERC url
obj@metadata$header$CRUISE_HEADER$PLATFORM<-gsub("'","",obj@metadata$header$CRUISE_HEADER$PLATFORM)
obj@metadata$header$CRUISE_HEADER$PLATFORM<-gsub("[()]","",obj@metadata$header$CRUISE_HEADER$PLATFORM)

index_vessel_codes <- grep(obj@metadata$header$CRUISE_HEADER$PLATFORM, vessel_codes$name, value = FALSE,
                           ignore.case = TRUE, fixed = FALSE)
vessel_icescode <- vessel_codes$ICES_SHIPC_ship_codes[index_vessel_codes]
vessel_NERC <- vessel_codes$BODC.Vocabulary[index_vessel_codes]
print(obj@metadata$header$CRUISE_HEADER$PLATFORM)
print(f)
obj[['institute']] <- 'DFO BIO'

obj[['model']] <- noquote(paste0(obj@metadata$header$INSTRUMENT_HEADER$DESCRIPTION_1," V",
                                 obj@metadata$header$INSTRUMENT_HEADER$MODEL_1))

mctd_nc(obj,location_description,info_url,cruise_name,vessel_icescode,vessel_NERC,f)

}







