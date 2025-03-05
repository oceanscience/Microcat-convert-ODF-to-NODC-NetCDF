Description:  Script to read ODF files from moored Microcats, add meta data, make compliant with ACDD, CF and IOOS and convert to NetCDF
install packages: 0install_libs.R

INPUT: ODF files of moored Microcat data, GF3 Code Map_final_v2.csv, Shared_models_vessel_BIO.csv, 
OUTPUT: writes 2 csv files saved in the processed folder; mooringprojects.csv, Shared_models_vessel_BIO.csv, and NetCDF file

Folder structure:
working directory with code and some inputs
data folder with ODF files and output

Code:  
1ODFmctd_title_program_project.R - run this first to create mooringprojectsh.csv file that lists certain metadata.  edit by hand before creating NetCDF
2ODFmctd_to_netcdf2.R - run this next to create NetCDF.  
ODFmctd_ships.R -  this can be run if needed to list the ships ensure the ships are listed in Shared_models_vessel_BIO.csv

Notes:  
- ensure ship used for mission is included in file Shared_models_vessel_BIO.csv
- edit the Shared_models_vessel_BIO.csv to include program, project and mission information
- ensure conductivity has correct units and conductivity ration is converted to conductivity
- ensure temperature scale is T90
- ensure correct instrument model and serial numbers, and correct BODC code is used.  refer to MicrocatSBE_models.txt file
- creator_name: is who created the NetCDF
- publisher_name: is Fisheries and Oceans Canada (DFO)

sessionInfo()
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 10 x64 (build 19045)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] anytime_0.3.9   dplyr_1.1.4     lubridate_1.9.4 ncdf4_1.23      oce_1.8-3       gsw_1.2-0      

loaded via a namespace (and not attached):
 [1] utf8_1.2.4        R6_2.5.1          tidyselect_1.2.1  magrittr_2.0.3    glue_1.7.0        tibble_3.2.1     
 [7] pkgconfig_2.0.3   timechange_0.3.0  generics_0.1.3    lifecycle_1.0.4   cli_3.6.3         fansi_1.0.6      
[13] vctrs_0.6.5       compiler_4.4.1    rstudioapi_0.17.1 tools_4.4.1       pillar_1.9.0      Rcpp_1.0.13-1    
[19] rlang_1.1.4      
> 
