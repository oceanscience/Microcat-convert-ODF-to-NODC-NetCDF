####mctd NC template####

#' Moored CTD netCDF template
 #'
#' @param obj an odf object from oce which contains mctd data
#' @param metadata a csv file following the standard template which includes all
#'   necessary metadata
#' @param filename the desired name for the netCDF file produced, if left NULL
#'   the default will conform to BIO naming conventions
#'
#'
#' @return netCDF file with a maximum of 12 variables
#' @export
#'
#' @examples
#' file <- list.files('.', pattern = "MCTD*...*.ODF")
#' obj <- read.odf(file)
#' metadata <- 'MCTD_SAMPLE_METADATA.csv'
#' mctd_nc(obj, metadata)
#'

mctd_nc <- function(obj,location_description, info_url, cruise_name, 
                    vessel_icescode,vessel_NERC, f, metadata, filename = NULL){
  require(oce)
  require(ncdf4)
  
  library("lubridate")
  library("dplyr")
  library("anytime")
  
  v <- names(obj@data)
  var <- obj@metadata$dataNamesOriginal

  #remove SYTM from var list
  tr <- grep(v, pattern = 'time')
  v <- v[-tr]
  vt <- grep(var, pattern = 'SYTM_01')
  var <- var[-vt]

  #POPULATE VARIABLES WITH APPROPRIATE CODES

  for ( i in 1:length(var)){
    var[[i]] <- asP01(var[[i]])
  }
  i <- 1

  for ( vv in var ){

    eval(parse(text = paste0("variable_", i, "<- '" , v[[i]], "'")))
    eval(parse(text= paste0("var",i," <-'", vv$gf3,"'")))
    eval(parse(text = paste0("units", i, " <-'", vv$units, "'")))
    eval(parse(text = paste0('P01_VAR', i," <- paste0('SDN:P01::', vv$P01)" )))
    eval(parse(text = paste0('P01_name_var', i," <-'" , vv$P01name , "'")))
    eval(parse(text = paste0('P06_var', i, "<-'" , vv$P06 , "'")))
    eval(parse(text = paste0('P06_name_var', i,  "<- '" , vv$P06name , "'")))
    eval(parse(text = paste0('var', i, 'max <-', -10000)))
    eval(parse(text = paste0('var', i, 'min <-' , 10000)))
    if(!is.null(vv$std)){
      eval(parse(text = paste0("std_variable_", i, " <- '", vv$std, "'")))
    }else{
      eval(parse(text = paste0("std_variable_", i, " <- NULL")))
    }
    #check if variable also has quality flag
    if (v[[i]] %in% names(obj[['flags']])) {
      eval(parse(text = paste0("var", i, "_QC <- '", vv$gf3, "_QC'")))
      eval(parse(text = paste0("variable", i , "_QC <- 'quality flag for " , v[[i]], "'")))
    }
    i <- i+1

  }
#CHECK LENGTH OF VARIABLES
  numvar <- length(var)

#FILENAME
if(missing(filename)){
  filename <- paste("MCTD", obj[['cruiseNumber']], obj[['eventNumber']], obj[['serialNumber']], obj[['samplingInterval']], sep = '_')
}
filename <- paste("MCTD", obj[['cruiseNumber']], obj[['eventNumber']], obj[['serialNumber']], obj[['samplingInterval']], sep = '_')  
ncpath <- pathprocesseddata  # "./"
ncfname <- paste(ncpath,"/", filename, ".nc", sep = "")


#DIMENSIONS
timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(obj[['time']]))
stationdim <- ncdim_def("station", "counts", as.numeric(obj[['station']]))
londim <- ncdim_def("lon", "degrees_east" , as.double(obj[['longitude']]))
latdim <- ncdim_def("lat", "degrees_north", as.double(obj[['latitude']]))
dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)

#FILLVALUE
FillValue <- 1e35


#VARIABLES


dlname <- 'lon'
lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')

dlname <- 'lat'
lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')

dlname <- "datetime"
t_def <- ncvar_def(longname ="date_time", units ="seconds since 1970-01-01T00:00:00Z", dim = list( stationdim, timedim), missval = FillValue, name = dlname, prec = "double")

# dlname <- "time_string"
# ts_def <- ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")

dlname <- "depth"
d_def <- ncvar_def(longname ="distance below the surface", units = 'meters', dim = stationdim, name = dlname, prec = 'double')

##added this loop to get rid og the multiple looops need to test

for (vr in 1:numvar){
  
  assign(paste0("v",vr,"_def"),ncvar_def(get(paste0("var",vr)), get(paste0("units", vr)), list(timedim, stationdim), FillValue, get(paste0("variable_", vr)), prec = 'double'))
}

#####write out definitions to new nc file####
defs <- grep(ls(), pattern = '_def', value = TRUE)
dd <- NULL
for ( i in 1:length(defs)){
  eval(parse(text = paste0("dd[[i]] <- ", defs[[i]])))
}
ncout <-
  nc_create(ncfname,dd,force_v4 = TRUE)

####INSERT DATA####
#ncvar_put(ncout, ts_def, obj[['time']])
ncvar_put(ncout, t_def, as.POSIXct(obj[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
ncvar_put(ncout, lon_def, obj[['longitude']])
ncvar_put(ncout, lat_def, obj[['latitude']])
ncvar_put(ncout, d_def, obj[['depthMin']])
ncatt_put(ncout, d_def, "positive", "down")

# obj@data[[length(obj@data)+1]] <- (obj@data$conductivityratio*-99)
# names(obj@data)[length(obj@data)] <- c("pressure")

for (vr in 1:numvar){
ncvar_put(ncout, get(paste0("v",vr,"_def")), obj[[get(paste0("variable_", vr))]])
}  
# ncvar_put(ncout, v1_def, obj[[variable_1]])
# obj@data$pressure <- NULL
####metadata####
ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
ncatt_put(ncout, 'station', 'standard_name', 'platform')
ncatt_put(ncout, 'station', 'cf_role', 'timeseries_id')
ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
ncatt_put(ncout, 'time', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
ncatt_put(ncout, 'time', 'time_zone', 'UTC')

#FROM ODF
ncatt_put(ncout, 0, 'inst_type', obj[['type']])
ncatt_put(ncout, 0, 'sampling_interval', obj[['samplingInterval']])
ncatt_put(ncout, 0, 'country_code', obj[['countryInstituteCode']])
ncatt_put(ncout, 0, 'sdn_country_id', 'SDN:C18::18')
ncatt_put(ncout, 0, 'sdn_country_vocabulary', 'http://vocab.nerc.ac.uk/collection/C18/current/')
ncatt_put(ncout, 0, 'cruise_number', obj[['cruiseNumber']])
ncatt_put(ncout, 0, "cruise_name", cruise_name)
ncatt_put(ncout, 0, "mooring_number", obj[['station']])
ncatt_put(ncout, 0, "cdm_data_type", "station")
ncatt_put(ncout, 0, "serial_number", obj[['serialNumber']])
ncatt_put(ncout, 0, "data_type", 'moored CTD')
ncatt_put(ncout, 0, "longitude", obj[['longitude']])
ncatt_put(ncout, 0, "latitude", obj[['latitude']])
ncatt_put(ncout, 0, "sounding", obj[['sounding']])
ncatt_put(ncout, 0, "water_depth", obj[['waterDepth']])
ncatt_put(ncout, 0, "instrument_offbottom_depth", obj[['depthOffBottom']])
ncatt_put(ncout, 0, "instrument_depth", obj[['depthMin']])
ncatt_put(ncout, 0, "chief_scientist", obj[['scientist']])
ncatt_put(ncout, 0, "institution", obj[['institute']])
ncatt_put(ncout, 0, 'sdn_institution_id', 'SDN:EDMO::1811', prec = 'text')
ncatt_put(ncout, 0, 'sdn_institution_vocabulary', 'https://edmo.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ORGANISATIONS (EDMO)', prec = 'text')
ncatt_put(ncout, 0, "source", f)  #obj@metadata$header$ODF_HEADER$FILE_SPECIFICATION_1)

####variable ATTRIBUTES####

#ncatt_put(ncout, var1, 'reference_scale', 'IPTS-68')
if (var$temperature$gf3 == "TEMP_01"){ #exists("TEMP_01", where <- var$temperature)
  ncatt_put(ncout, "TEMP_01", 'reference_scale', attval = 'ITS-90')
}
if (var$temperature$gf3 == "TE90_01"){ 
  ncatt_put(ncout, "TE90_01", 'reference_scale', attval = 'ITS-90')
}
#ncatt_put(ncout, "TEMP_01", 'reference_scale', 'ITS-90')


####variables####
#sensor type, sensor depth and serial number for each variable
#generic nameS
#STANDARD NAMES
#data max and min
#VALID MIN AND MAX
#p01 and p06 names

ncatt_put(ncout, var1, "sensor_type", obj[['model']])
ncatt_put(ncout, var1, "sensor_depth", obj[['depthMin']])
ncatt_put(ncout, var1, "serial_number", obj[['serialNumber']])
ncatt_put(ncout, var1, "generic_name", variable_1)
ncatt_put(ncout, var1, "sdn_parameter_urn", P01_VAR1)
ncatt_put(ncout, var1, "sdn_parameter_name", P01_name_var1)
ncatt_put(ncout, var1, "sdn_uom_urn", P06_var1)
ncatt_put(ncout, var1, "sdn_uom_name", P06_name_var1)
if (!is.null(std_variable_1)){
  ncatt_put(ncout, var1, "standard_name", std_variable_1)
  }else{
    ncatt_put(ncout, var1, "standard_name", "")
  }
ncatt_put(ncout, var1, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
ncatt_put(ncout, var1, "data_max", max(obj[[variable_1]], na.rm = TRUE))
ncatt_put(ncout, var1, "data_min", min(obj[[variable_1]], na.rm = TRUE))
ncatt_put(ncout, var1, "valid_max", var1max)
ncatt_put(ncout, var1, "valid_min", var1min)
ncatt_put(ncout, var1, "coverage_content_type","physicalMeasurement")


if (numvar > 1){
  ncatt_put(ncout, var2, "sensor_type", obj[['model']])
  ncatt_put(ncout, var2, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var2, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var2, "generic_name", variable_2)
  ncatt_put(ncout, var2, "sdn_parameter_urn", P01_VAR2)
  ncatt_put(ncout, var2, "sdn_parameter_name", P01_name_var2)
  ncatt_put(ncout, var2, "sdn_uom_urn", P06_var2)
  ncatt_put(ncout, var2, "sdn_uom_name", P06_name_var2)
  if (!is.null(std_variable_2)){
  ncatt_put(ncout, var2, "standard_name", std_variable_2)
    }else{
      ncatt_put(ncout, var2, "standard_name", "")
    }
  
  ncatt_put(ncout, var2, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
  ncatt_put(ncout, var2, "data_max", max(obj[[variable_2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "data_min", min(obj[[variable_2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "valid_max", var2max)
  ncatt_put(ncout, var2, "valid_min", var2min)
  ncatt_put(ncout, var2, "coverage_content_type","physicalMeasurement")

  if (numvar >2){
    ncatt_put(ncout, var3, "sensor_type", obj[['model']])
    ncatt_put(ncout, var3, "sensor_depth", obj[['depthMin']])
    ncatt_put(ncout, var3, "serial_number", obj[['serialNumber']])
    ncatt_put(ncout, var3, "generic_name", variable_3)
    ncatt_put(ncout, var3, "sdn_parameter_urn", P01_VAR3)
    ncatt_put(ncout, var3, "sdn_parameter_name", P01_name_var3)
    ncatt_put(ncout, var3, "sdn_uom_urn", P06_var3)
    ncatt_put(ncout, var3, "sdn_uom_name", P06_name_var3)
    if (!is.null(std_variable_3)){
    ncatt_put(ncout, var3, "standard_name", std_variable_3)
     } else{
        ncatt_put(ncout, var3, "standard_name", "")
      }
    
    ncatt_put(ncout, var3, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
    ncatt_put(ncout, var3, "data_max", max(obj[[variable_3]], na.rm = TRUE))
    ncatt_put(ncout, var3, "data_min", min(obj[[variable_3]], na.rm = TRUE))
    ncatt_put(ncout, var3, "valid_max", var3max)
    ncatt_put(ncout, var3, "valid_min", var3min)
    ncatt_put(ncout, var3, "coverage_content_type","physicalMeasurement")

    if (numvar >3){
      ncatt_put(ncout, var4, "sensor_type", obj[['model']])
      ncatt_put(ncout, var4, "sensor_depth", obj[['depthMin']])
      ncatt_put(ncout, var4, "serial_number", obj[['serialNumber']])
      ncatt_put(ncout, var4, "generic_name", variable_4)
      ncatt_put(ncout, var4, "sdn_parameter_urn", P01_VAR4)
      ncatt_put(ncout, var4, "sdn_parameter_name", P01_name_var4)
      ncatt_put(ncout, var4, "sdn_uom_urn", P06_var4)
      ncatt_put(ncout, var4, "sdn_uom_name", P06_name_var4)
      if (!is.null(std_variable_4)){
      ncatt_put(ncout, var4, "standard_name", std_variable_4)
        }else{
          ncatt_put(ncout, var4, "standard_name", "")
        }
      ncatt_put(ncout, var4, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
      ncatt_put(ncout, var4, "data_max", max(obj[[variable_4]], na.rm = TRUE))
      ncatt_put(ncout, var4, "data_min", min(obj[[variable_4]], na.rm = TRUE))
      ncatt_put(ncout, var4, "valid_max", var4max)
      ncatt_put(ncout, var4, "valid_min", var4min)
      ncatt_put(ncout, var4, "coverage_content_type","physicalMeasurement")

      if (numvar >4){
        ncatt_put(ncout, var5, "sensor_type", obj[['model']])
        ncatt_put(ncout, var5, "sensor_depth", obj[['depthMin']])
        ncatt_put(ncout, var5, "serial_number", obj[['serialNumber']])
        ncatt_put(ncout, var5, "generic_name", variable_5)
        ncatt_put(ncout, var5, "sdn_parameter_urn", P01_VAR5)
        ncatt_put(ncout, var5, "sdn_parameter_name", P01_name_var5)
        ncatt_put(ncout, var5, "sdn_uom_urn", P06_var5)
        ncatt_put(ncout, var5, "sdn_uom_name", P06_name_var5)
        if (!is.null(std_variable_5)){
        ncatt_put(ncout, var5, "standard_name", std_variable_5)
          }else{
            ncatt_put(ncout, var5, "standard_name", "")
          }
        
        ncatt_put(ncout, var5, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
        ncatt_put(ncout, var5, "data_max", max(obj[[variable_5]], na.rm = TRUE))
        ncatt_put(ncout, var5, "data_min", min(obj[[variable_5]], na.rm = TRUE))
        ncatt_put(ncout, var5, "valid_max", var5max)
        ncatt_put(ncout, var5, "valid_min", var5min)
        ncatt_put(ncout, var5, "coverage_content_type","physicalMeasurement")

        if (numvar >5){
          ncatt_put(ncout, var6, "sensor_type", obj[['model']])
          ncatt_put(ncout, var6, "sensor_depth", obj[['depthMin']])
          ncatt_put(ncout, var6, "serial_number", obj[['serialNumber']])
          ncatt_put(ncout, var6, "generic_name", variable_6)
          ncatt_put(ncout, var6, "sdn_parameter_urn", P01_VAR6)
          ncatt_put(ncout, var6, "sdn_parameter_name", P01_name_var6)
          ncatt_put(ncout, var6, "sdn_uom_urn", P06_var6)
          ncatt_put(ncout, var6, "sdn_uom_name", P06_name_var6)
          if (!is.null(std_variable_6)){
          ncatt_put(ncout, var6, "standard_name", std_variable_6)
          }else{
            ncatt_put(ncout, var6, "standard_name", "")
          }
          ncatt_put(ncout, var6, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
          ncatt_put(ncout, var6, "data_max", max(obj[[variable_6]], na.rm = TRUE))
          ncatt_put(ncout, var6, "data_min", min(obj[[variable_6]], na.rm = TRUE))
          ncatt_put(ncout, var6, "valid_max", var6max)
          ncatt_put(ncout, var6, "valid_min", var6min)
          ncatt_put(ncout, var6, "coverage_content_type","physicalMeasurement")

          if (numvar > 6){
            ncatt_put(ncout, var7, "sensor_type", obj[['model']])
            ncatt_put(ncout, var7, "sensor_depth", obj[['depthMin']])
            ncatt_put(ncout, var7, "serial_number", obj[['serialNumber']])
            ncatt_put(ncout, var7, "generic_name", variable_7)
            ncatt_put(ncout, var7, "sdn_parameter_urn", P01_VAR7)
            ncatt_put(ncout, var7, "sdn_parameter_name", P01_name_var7)
            ncatt_put(ncout, var7, "sdn_uom_urn", P06_var7)
            ncatt_put(ncout, var7, "sdn_uom_name", P06_name_var7)
            if (!is.null(std_variable_7)){
            ncatt_put(ncout, var7, "standard_name", std_variable_7)
            }else{
              ncatt_put(ncout, var7, "standard_name", "")             
            }
            ncatt_put(ncout, var7, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
            ncatt_put(ncout, var7, "data_max", max(obj[[variable_7]], na.rm = TRUE))
            ncatt_put(ncout, var7, "data_min", min(obj[[variable_7]], na.rm = TRUE))
            ncatt_put(ncout, var7, "valid_max", var7max)
            ncatt_put(ncout, var7, "valid_min", var7min)
            ncatt_put(ncout, var7, "coverage_content_type","physicalMeasurement")

            if (numvar > 7){
              ncatt_put(ncout, var8, "sensor_type", obj[['model']])
              ncatt_put(ncout, var8, "sensor_depth", obj[['depthMin']])
              ncatt_put(ncout, var8, "serial_number", obj[['serialNumber']])
              ncatt_put(ncout, var8, "generic_name", variable_8)
              ncatt_put(ncout, var8, "sdn_parameter_urn", P01_VAR8)
              ncatt_put(ncout, var8, "sdn_parameter_name", P01_name_var8)
              ncatt_put(ncout, var8, "sdn_uom_urn", P06_var8)
              ncatt_put(ncout, var8, "sdn_uom_name", P06_name_var8)
              if (!is.null(std_variable_8)){
              ncatt_put(ncout, var8, "standard_name", std_variable_8)
              }else{
                ncatt_put(ncout, var8, "standard_name", "")
              }
              ncatt_put(ncout, var8, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
              ncatt_put(ncout, var8, "data_max", max(obj[[variable_8]], na.rm = TRUE))
              ncatt_put(ncout, var8, "data_min", min(obj[[variable_8]], na.rm = TRUE))
              ncatt_put(ncout, var8, "valid_max", var8max)
              ncatt_put(ncout, var8, "valid_min", var8min)
              ncatt_put(ncout, var8, "coverage_content_type","physicalMeasurement")

              if (numvar > 8){
                ncatt_put(ncout, var9, "sensor_type", obj[['model']])
                ncatt_put(ncout, var9, "sensor_depth", obj[['depthMin']])
                ncatt_put(ncout, var9, "serial_number", obj[['serialNumber']])
                ncatt_put(ncout, var9, "generic_name", variable_9)
                ncatt_put(ncout, var9, "sdn_parameter_urn", P01_VAR9)
                ncatt_put(ncout, var9, "sdn_parameter_name", P01_name_var9)
                ncatt_put(ncout, var9 , "sdn_uom_urn", P06_var9)
                ncatt_put(ncout, var9, "sdn_uom_name", P06_name_var9)
                if (!is.null(std_variable_9)){
                ncatt_put(ncout, var9, "standard_name", std_variable_9)
                }else{
                  ncatt_put(ncout, var9, "standard_name", "")
                }
                ncatt_put(ncout, var9, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
                ncatt_put(ncout, var9, "data_max", max(obj[[variable_9]], na.rm = TRUE))
                ncatt_put(ncout, var9, "data_min", min(obj[[variable_9]], na.rm = TRUE))
                ncatt_put(ncout, var9, "valid_max", var9max)
                ncatt_put(ncout, var9, "valid_min", var9min)
                ncatt_put(ncout, var9, "coverage_content_type","physicalMeasurement")

                if (numvar >9){
                  ncatt_put(ncout, var10, "sensor_type", obj[['model']])
                  ncatt_put(ncout, var10, "sensor_depth", obj[['depthMin']])
                  ncatt_put(ncout, var10, "serial_number", obj[['serialNumber']])
                  ncatt_put(ncout, var10, "generic_name", variable_10)
                  ncatt_put(ncout, var10, "sdn_parameter_urn", P01_VAR10)
                  ncatt_put(ncout, var10, "sdn_parameter_name", P01_name_var10)
                  ncatt_put(ncout, var10, "sdn_uom_urn", P06_var10)
                  ncatt_put(ncout, var10, "sdn_uom_name", P06_name_var10)
                  if (!is.null(std_variable_10)){
                  ncatt_put(ncout, var10, "standard_name", std_variable_10)
                  }else{
                    ncatt_put(ncout, var10, "standard_name", "")
                  }
                  ncatt_put(ncout, var10, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
                  ncatt_put(ncout, var10, "data_max", max(obj[[variable_10]], na.rm = TRUE))
                  ncatt_put(ncout, var10, "data_min", min(obj[[variable_10]], na.rm = TRUE))
                  ncatt_put(ncout, var10, "valid_max", var10max)
                  ncatt_put(ncout, var10, "valid_min", var10min)
                  ncatt_put(ncout, var10, "coverage_content_type","physicalMeasurement")

                  if (numvar >10){
                    ncatt_put(ncout, var11, "sensor_type", obj[['model']])
                    ncatt_put(ncout, var11, "sensor_depth", obj[['depthMin']])
                    ncatt_put(ncout, var11, "serial_number", obj[['serialNumber']])
                    ncatt_put(ncout, var11, "generic_name", variable_11)
                    ncatt_put(ncout, var11, "sdn_parameter_urn", P01_VAR11)
                    ncatt_put(ncout, var11, "sdn_parameter_name", P01_name_var11)
                    ncatt_put(ncout, var11, "sdn_uom_urn", P06_var11)
                    ncatt_put(ncout, var11, "sdn_uom_name", P06_name_var11)
                    if (!is.null(std_variable_11)){
                    ncatt_put(ncout, var11, "standard_name", std_variable_11)
                    }else{
                      ncatt_put(ncout, var11, "standard_name", "")
                    }
                    ncatt_put(ncout, var11, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
                    ncatt_put(ncout, var11, "data_max", max(obj[[variable_11]], na.rm = TRUE))
                    ncatt_put(ncout, var11, "data_min", min(obj[[variable_11]], na.rm = TRUE))
                    ncatt_put(ncout, var11, "valid_max", var11max)
                    ncatt_put(ncout, var11, "valid_min", var11min)
                    ncatt_put(ncout, var11, "coverage_content_type","physicalMeasurement")

                    if (numvar >11){
                      ncatt_put(ncout, var12, "sensor_type", obj[['model']])
                      ncatt_put(ncout, var12, "sensor_depth", obj[['depthMin']])
                      ncatt_put(ncout, var12 , "serial_number", obj[['serialNumber']])
                      ncatt_put(ncout, var12, "generic_name", variable_12)
                      ncatt_put(ncout, var12, "sdn_parameter_urn", P01_VAR12)
                      ncatt_put(ncout, var12, "sdn_parameter_name", P01_name_var12)
                      ncatt_put(ncout, var12, "sdn_uom_urn", P06_var12)
                      ncatt_put(ncout, var12, "sdn_uom_name", P06_name_var12)
                      if (!is.null(std_variable_12)){
                      ncatt_put(ncout, var12, "standard_name", std_variable_12)
                      }else{
                        ncatt_put(ncout, var12, "standard_name", "")
                      }
                      ncatt_put(ncout, var12, "standard_name_url", "https://cfconventions.org/Data/cf-standard-names/current/build/cf-standard-name-table.html")
                      ncatt_put(ncout, var12, "data_max", max(obj[[variable_12]], na.rm = TRUE))
                      ncatt_put(ncout, var12, "data_min", min(obj[[variable_12]], na.rm = TRUE))
                      ncatt_put(ncout, var12, "valid_max", var12max)
                      ncatt_put(ncout, var12, "valid_min", var12min)
                      ncatt_put(ncout, var12, "coverage_content_type","physicalMeasurement")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# obj@data$pressure <- NULL
####CF conventions & BODC standards####

ncatt_put(ncout, 0, 'Conventions', 'CF-1.6,ACDD-1.3,IOOS-1.2')
ncatt_put(ncout, 0, "date_created", iso8601(as.POSIXct(Sys.Date())))
ncatt_put(ncout, 0, "standard_name_vocabulary","CF Standard Name Table v80")
ncatt_put(ncout, 0, "creator_type", "person")
ncatt_put(ncout, 0, "creator_name", "Diana Cardoso")
ncatt_put(ncout, 0, "creator_country", "Canada")
ncatt_put(ncout, 0, "creator_email", "BIO.Datashop@dfo-mpo.gc.ca")
ncatt_put(ncout, 0, "creator_institution", "Bedford Institute of Oceanography")
ncatt_put(ncout, 0, "creator_address", "P.O. Box 1006, 1 Challenger Dr.")
ncatt_put(ncout, 0, "creator_city", "Dartmouth")
ncatt_put(ncout, 0, "creator_sector",'gov_federal')
ncatt_put(ncout, 0, "creator_url", "https://www.bio.gc.ca/index-en.php")
ncatt_put(ncout, 0, "publisher_name", "Fisheries and Oceans Canada (DFO)" )
ncatt_put(ncout, 0, "publisher_country", "Canada")
ncatt_put(ncout, 0, "publisher_email", "BIO.Datashop@dfo-mpo.gc.ca")
ncatt_put(ncout, 0, "publisher_institution",  "Bedford Institute of Oceanography")
ncatt_put(ncout, 0, "publisher_type","institution")
ncatt_put(ncout, 0, "publisher_sector", 'gov_federal')
ncatt_put(ncout, 0, "publisher_url", "https://www.bio.gc.ca/index-en.php")
ncatt_put(ncout, 0, 'sdn_custodian_id', 'SDN:EDMO::1811', prec = 'text')
ncatt_put(ncout, 0, 'sdn_originator_id', 'SDN:EDMO::1811', prec = 'text')
ncatt_put(ncout, 0, 'sdn_creator_id', 'SDN:EDMO::1811', prec = 'text')
ncatt_put(ncout, 0, 'sdn_publisher_id', 'SDN:EDMO::1811', prec = 'text')
ncatt_put(ncout, 0, 'sdn_distributor_id', 'SDN:EDMO::1979', prec = 'text')

ncatt_put(ncout, 0, "featureType", "timeseries")
ncatt_put(ncout, 0, "processing_level", "Pre and Post deployment records removed and spike removal")
#ncatt_put(ncout, 0, "coverage_content_type","physicalMeasurement")
ncatt_put(ncout, 0, "id", f) #obj@metadata$header$ODF_HEADER$FILE_SPECIFICATION_1)
ncatt_put(ncout, 0, "naming_authority", "ca.gc.bio" )
#ncatt_put(ncout, 0, "infoUrl", info_url) 
ncatt_put(ncout, 0, "license", "Open Government Licence - Canada, https://open.canada.ca/en/open-government-licence-canada")
ncatt_put(ncout, 0, "summary", paste0(obj[['institute']], ',',
                                gsub("'","",obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION),
                                ',', gsub("'","",obj@metadata$header$CRUISE_HEADER$CRUISE_NAME),
                                "," , "moored CTD model ",obj[['model']],",",
                                "from ", obj[['time']][[1]],' to ',tail(obj[['time']], n = 1)))
ncatt_put(ncout, 0, "title", paste0(obj[['institute']], ',',
                          gsub("'","",obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION),
                          ',',gsub("'","",obj@metadata$header$CRUISE_HEADER$CRUISE_NAME),
                          ", moored CTD "))
ncatt_put(ncout, 0, "project", obj@metadata$header$CRUISE_HEADER$CRUISE_NAME)
if (!is.na(charmatch("Ocean Tracking Network (OTN)", obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION))) { 
  ncatt_put(ncout, 0, 'sdn_project_id',  'SDN:EDMO::4487', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_project_vocabulary',  'https://edmo.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ORGANISATIONS (EDMO)', prec = 'text')
}

if (!is.na(charmatch("Overturning in the Subpolar North Atlantic Program (OSNAP)", obj@metadata$header$CRUISE_HEADER$CRUISE_NAME))){ 
  ncatt_put(ncout, 0, 'sdn_project_id',  'SDN:EDMERP::12381', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_project_vocabulary',  'https://edmerp.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ENVIRONMENTAL RESEARCH PROJECTS (EDMERP)', prec = 'text')
}

if (!is.na(charmatch("Rapid Climate Change Program (RAPID)", obj@metadata$header$CRUISE_HEADER$CRUISE_NAME))){ 
  ncatt_put(ncout, 0, 'sdn_project_id',  'SDN:EDMERP::8825', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_project_vocabulary',  'https://edmerp.seadatanet.org, EUROPEAN DIRECTORY OF MARINE ENVIRONMENTAL RESEARCH PROJECTS (EDMERP)', prec = 'text')
}

ncatt_put(ncout, 0, "program", obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION)
ncatt_put(ncout, 0, "keywords", "Time-series, Marine-data, oceans, climate, water-temperature,
                                salinity, mooring, moored-ctd, water-density, conductivity, pressure")

ncatt_put(ncout, 0, "location_description", location_description)
ncatt_put(ncout, 0, "mission_description", obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION)

ncatt_put(ncout, 0, "platform", "mooring")
ncatt_put(ncout, 0, "platform_name", obj[['station']])
ncatt_put(ncout, 0, "platform_id", obj[['station']])
ncatt_put(ncout, 0, 'sdn_platform_id', 'SDN:L06::48, SDN:L06::43', prec = 'text')
ncatt_put(ncout, 0, "sdn_platform_vocabulary", "https://vocab.nerc.ac.uk/collection/L06/current/")
ncatt_put(ncout, 0, "deployment_platform_name", paste0(obj@metadata$header$CRUISE_HEADER$PLATFORM, ', ',
                                                       vessel_icescode, ', ', vessel_NERC))
ncatt_put(ncout, 0, 'sdn_deployment_platform_id', paste0('SDN:C17::', vessel_icescode), prec = 'text')

ncatt_put(ncout, 0, "sdn_deployment_platform_vocabulary", "https://vocab.nerc.ac.uk/collection/C17/current/")
# fix instrument model  #MCTD_BCD2011901_HFX008_0081_3600.ODF has Aanderaa 3830, MCTD_BCD2011901_HFX008B_0095_3600.ODF just optode
if (obj[['model']] == "'Satlantic'"){ 
  ncatt_put(ncout, 0, 'instrument_model', obj@metadata$header$INSTRUMENT_HEADER$DESCRIPTION_1)
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL0020, SDN:L22::TOOL1247', prec = 'text')
  obj[['model']] <- "SBE37-SIP"
  ncatt_put(ncout, 0, "instrument", paste0("Sea-Bird Microcat",",","model number ",obj[['model']],",", 
                                          "Aanderaa dissolved oxygen sensor, model number 4330, ", 
                                          "Satlantic Benthic Pod serial number ",obj[['serialNumber']]))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::350, SDN:L05::134, SDN:L05::WPS, SDN:L05::351', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text')
  
}else if (grepl("SBE37-IM", obj[['model']]) ||  grepl("SBE37IM", obj[['model']])){
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL1450', prec = 'text')
  ncatt_put(ncout, 0, 'instrument_model', obj[['model']])
  ncatt_put(ncout, 0, "instrument", paste0(obj[['type']],",","model number ",obj[['model']],",", 
                                           "serial number ",obj[['serialNumber']] ))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::350, SDN:L05::134, SDN:L05::WPS', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text')

}else if (grepl("SBE37-SMP", obj[['model']]) ||  grepl("SBE37SMP", obj[['model']])){
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL1457', prec = 'text')
  ncatt_put(ncout, 0, 'instrument_model', obj[['model']])
  ncatt_put(ncout, 0, "instrument", paste0(obj[['type']],",","model number ",obj[['model']],",", 
                                           "serial number ",obj[['serialNumber']] ))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::350, SDN:L05::134, SDN:L05::WPS', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text') 

}else if (grepl("Aanderaa Optode", obj[['model']]) ||  grepl("Aanderaa", obj[['model']])){
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL1247', prec = 'text')
  ncatt_put(ncout, 0, 'instrument_model', obj@metadata$header$INSTRUMENT_HEADER$DESCRIPTION_1)
  ncatt_put(ncout, 0, "instrument", paste0(obj[['model']],",", 
                                           "Satlantic Benthic Pod serial number ",obj[['serialNumber']] ))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::WPS, SDN:L05::351', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text') 

}else if (grepl("Seacat", obj[['type']]) ||  grepl("SEACAT", obj[['type']])){
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL0023', prec = 'text')
  ncatt_put(ncout, 0, "instrument", paste0("Sea-Bird SeaCat",",", " model number ", obj[['model']],",", 
                                           "serial number ",obj[['serialNumber']] ))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::WPS, SDN:L05::351', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text')
  
}else{
  ncatt_put(ncout, 0, 'sdn_instrument_id', 'SDN:L22::TOOL1456', prec = 'text')
  ncatt_put(ncout, 0, 'instrument_model', obj[['model']])
  ncatt_put(ncout, 0, "instrument", paste0(obj[['type']],",","model number ",obj[['model']],",", 
                                           "serial number ",obj[['serialNumber']] ))
  ncatt_put(ncout, 0, 'sdn_instrument_vocabulary', "http://vocab.nerc.ac.uk/collection/L22/current/")
  ncatt_put(ncout, 0, 'sdn_device_category_id', 'SDN:L05::350, SDN:L05::134, SDN:L05::WPS', prec = 'text')
  ncatt_put(ncout, 0, 'sdn_device_category_vocabulary', 'http://vocab.nerc.ac.uk/collection/L05/current/', prec = 'text')
}

minres <- minute(seconds_to_period(as.numeric(obj[['samplingInterval']])))
hourres <- hour(seconds_to_period(as.numeric(obj[['samplingInterval']])))

ncatt_put(ncout, 0, "time_coverage_resolution", paste0("P0Y0M0DT",hourres,"H",minres, 'M0S')) #as.character(intervalmin)
#ISO8601:2004 standard, package anytime iso8601(pt), https://rdrr.io/cran/anytime/man/iso8601.html

daysdur <- tail(obj[['time']], n = 1) - obj[['time']][[1]]
daysdur <- as.numeric(daysdur)
daysdur2 <-floor(daysdur)
hourscal <- (daysdur-floor(daysdur))*24
hoursdur <-floor(hourscal)
minscal <- (hourscal-floor(hourscal))*60
minsdur <-round(minscal)

ncatt_put(ncout, 0, "time_coverage_duration", 
          paste0("P",daysdur2,"DT",hoursdur,"H",minsdur, "M",0, "S"))
ncatt_put(ncout, 0, "time_coverage_start", as.character(iso8601(as.POSIXct(obj[['time']][1]))))  #ISO 8601:2004
ncatt_put(ncout, 0, "time_coverage_end", as.character(iso8601(as.POSIXct(tail(obj[['time']], n= 1)))))

ncatt_put(ncout, 0, "geospatial_lat_min", obj[['latitude']])
ncatt_put(ncout, 0, "geospatial_lat_max", obj[['latitude']])
ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
ncatt_put(ncout, 0, "geospatial_lon_min", obj[['longitude']])
ncatt_put(ncout, 0, "geospatial_lon_max", obj[['longitude']])
ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
ncatt_put(ncout, 0, "geospatial_vertical_max", obj[['depthMax']])
ncatt_put(ncout, 0, "geospatial_vertical_min", obj[['depthMin']])
ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
ncatt_put(ncout, 0, "geospatial_bounds", paste0('POINT(', paste(obj[['latitude']],obj[['longitude']]),')'))  
ncatt_put(ncout, 0,"geospatial_bounds_crs",  "EPSG:4326")
ncatt_put(ncout, 0,"geospatial_bounds_vertical_crs","EPSG:5831")
#ncatt_put(ncout,0, "_FillValue", "1e35")  ##look at this
ncatt_put(ncout, 0,"product_version",  "R version 4.2.1 (2022-06-23 ucrt)")
ncatt_put(ncout, 0, "date_modified", iso8601(as.POSIXct(Sys.Date())))


####BODC P01 names####
ncatt_put(ncout, "datetime", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
ncatt_put(ncout, "depth", "sdn_parameter_urn", "SDN:P01::ADEPZZ01")

ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
ncatt_put(ncout, 'datetime', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")

ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
ncatt_put(ncout, "datetime", "sdn_uom_urn", "SDN:P06::TISO")
ncatt_put(ncout, "depth", "sdn_uom_urn", "SDN:P06::ULAA")

ncatt_put(ncout, "lon", "sdn_uom_name", "Degrees east")
ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
ncatt_put(ncout, "datetime", "sdn_uom_name", "Seconds")
ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
ncatt_put(ncout, "depth", "sdn_uom_name", "Metres")

#####CF standard names####
ncatt_put(ncout, "datetime", "standard_name", "time")
ncatt_put(ncout, "lat", "standard_name", "latitude")
ncatt_put(ncout, "lon", "standard_name", "longitude")
ncatt_put(ncout, "depth", "standard_name", "depth")
####data max and min####

#metadata from spreadsheet

if (!missing(metadata)) {
  metad <- read.csv(metadata, header = TRUE)

  mn <- as.character(metad[,1])
  mv <- as.character(metad[,2])


  md <- as.list(mv)
  names(md) <- mn

  for (m in seq_along(md)) {
    ncatt_put(ncout, 0, names(md)[m], md[[m]])
  }
}

####preserve ODF history header####
if (!is.null(obj@metadata$header)){
  if (length(obj@metadata$header) != 0){
  head <- obj@metadata$header
  hi <- list(grep(names(head), pattern = "HISTORY"))
  hist <- NULL
  for ( i in 1:length(hi[[1]])){
    hist[[i]] <- unlist(head[[hi[[1]][i]]])
  }
  histo <- unlist(hist)
  histor <- NULL
  for (i in 1:length(histo)){
    histor[[i]] <- paste(names(histo)[[i]],":", histo[[i]])
  }

  history <- unlist(histor)
  
  history1 <- history[[1]] 
  for (i in 2:length(history)){
    history1 <- paste0(history1, history[[i]], sep = "\n")  
  }
  
  ncatt_put(ncout, 0, "history", history1) # https://pjbartlein.github.io/REarthSysSci/netCDF.html
  
####preserve ODF event header####
      eventheader <- unlist(obj@metadata$header$EVENT_HEADER)
  
  ev <- NULL
  for (i in 1:length(eventheader)){
    ev[[i]] <- paste(names(eventheader)[[i]],":", eventheader[[i]])
  }
  ev2 <- unlist(ev)
  
  eventheader1 <- ev2[[1]] 
  for (i in 2:length(ev2)){
    eventheader1 <- paste0(eventheader1, ev2[[i]], sep = "\n")  
  }
        ncatt_put(ncout, 0, "ODF_EVENT_header", eventheader1)
        
  ####preserve ODF instrument header####
 sensorheader <- unlist(obj@metadata$header$INSTRUMENT_HEADER)
        
 sen <- NULL
 for (i in 1:length(sensorheader)){
          sen[[i]] <- paste(names(sensorheader)[[i]],":", sensorheader[[i]])
 }
 sen2 <- unlist(sen)
                
sensorheader1 <- sen2[[1]] 
  for (i in 2:length(sen2)){
    sensorheader1 <- paste0(sensorheader1, sen2[[i]], sep = "\n")  
    #cat(sensorheader1)
  }    
      ncatt_put(ncout, 0, "ODF_INSTRUMENT_header", sensorheader1)

  }
}

####nc close####

  nc_close(ncout)

}


