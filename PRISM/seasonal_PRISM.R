#############
# This script will download monthly precip data from PRISM and process it to seasonal values.
# The rasters will be averaged over the basins and values for each basin/season/year will 
# be extracted.
#############

####################################################
# Packages Vector
packages <- c("tidyverse", "sf", "raster", "prism", "exactextractr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial")
prism_set_dl_dir("~/Documents/GitHub/Base-Flow-Spatial/PRISM")

####################################################
#download PRISM data for winters first
get_prism_monthlys("ppt", years = 1990:2020, mon = c(1,2,12), keepZip = FALSE)

#Format to pull produce raster:
ppt_2013<- pd_to_file(prism_archive_subset("ppt", "monthly", years = 2013, mon = 1))
ppt_2013_rast <- raster(ppt_2013)

#HUC8 shapefile
huc8_shape <- shapefile("~/Documents/GitHub/-Hydrograph-Testing/Data/Shapefiles/HUC8/HUC8_AZ.shp")

#set CRS to the same
huc8_shape <- spTransform(huc8_shape, crs(ppt_2013_rast))


#Assign winter precip to each HUC8 for period of record
winter_precip <- data.frame(HUC = huc8_shape$HUC8) #initialize
count <-  1

for (i in 1991:2020){
  count <- count + 1
  
  dec_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i-1, mon = 12)) #previous year Dec
  dec_tmp_rast <- raster(dec_rast_file) #create raster
  
  jan_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 1)) #read raster current Jan
  jan_tmp_file <- raster(jan_rast_file) #create raster
  
  feb_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 2)) #read raster current Feb
  feb_tmp_file <- raster(feb_rast_file) #create raster
  
  rast_stack <- stack(dec_tmp_rast, jan_tmp_file, feb_tmp_file)
  
  tmp_mean <- exact_extract(rast_stack, huc8_shape, fun = "mean")
  tmp_mean$AVG <- rowMeans(tmp_mean) #get average value for the season
  
  winter_precip[,count] <- round(tmp_mean$AVG,2)
  colnames(winter_precip)[count] <- paste0("wint_", as.character(i))
}


#Assign spring precip to each HUC8 for period of record
spring_precip <- data.frame(HUC = huc8_shape$HUC8) #initialize
count <-  1

for (i in 1991:2020){
  count <- count + 1
  
  mar_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i-1, mon = 12)) #previous year mar
  mar_tmp_rast <- raster(mar_rast_file) #create raster
  
  apr_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 1)) #read raster current apr
  apr_tmp_file <- raster(apr_rast_file) #create raster
  
  may_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 2)) #read raster current may
  may_tmp_file <- raster(may_rast_file) #create raster
  
  rast_stack <- stack(mar_tmp_rast, apr_tmp_file, may_tmp_file)
  
  tmp_mean <- exact_extract(rast_stack, huc8_shape, fun = "mean")
  tmp_mean$AVG <- rowMeans(tmp_mean) #get average value for the season
  
  spring_precip[,count] <- round(tmp_mean$AVG,2)
  colnames(spring_precip)[count] <- paste0("spr_", as.character(i))
}


#Assign summer precip to each HUC8 for period of record
summer_precip <- data.frame(HUC = huc8_shape$HUC8) #initialize
count <-  1

for (i in 1991:2020){
  count <- count + 1
  
  jun_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i-1, mon = 12)) #previous year jun
  jun_tmp_rast <- raster(jun_rast_file) #create raster
  
  jul_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 1)) #read raster current jul
  jul_tmp_file <- raster(jul_rast_file) #create raster
  
  aug_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 2)) #read raster current aug
  aug_tmp_file <- raster(aug_rast_file) #create raster
  
  rast_stack <- stack(jun_tmp_rast, jul_tmp_file, aug_tmp_file)
  
  tmp_mean <- exact_extract(rast_stack, huc8_shape, fun = "mean")
  tmp_mean$AVG <- rowMeans(tmp_mean) #get average value for the season
  
  summer_precip[,count] <- round(tmp_mean$AVG,2)
  colnames(summer_precip)[count] <- paste0("summ_", as.character(i))
}


#Assign fall precip to each HUC8 for period of record
fall_precip <- data.frame(HUC = huc8_shape$HUC8) #initialize
count <-  1

for (i in 1991:2020){
  count <- count + 1
  
  sep_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i-1, mon = 12)) #previous year sep
  sep_tmp_rast <- raster(sep_rast_file) #create raster
  
  oct_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 1)) #read raster current oct
  oct_tmp_file <- raster(oct_rast_file) #create raster
  
  nov_rast_file <- pd_to_file(prism_archive_subset("ppt", "monthly", years = i, mon = 2)) #read raster current nov
  nov_tmp_file <- raster(nov_rast_file) #create raster
  
  rast_stack <- stack(sep_tmp_rast, oct_tmp_file, nov_tmp_file)
  
  tmp_mean <- exact_extract(rast_stack, huc8_shape, fun = "mean")
  tmp_mean$AVG <- rowMeans(tmp_mean) #get average value for the season
  
  fall_precip[,count] <- round(tmp_mean$AVG,2)
  colnames(fall_precip)[count] <- paste0("fall_", as.character(i))
}


#Need to group together the seasonal precip dataframes
#Then they can be assigned to each row in the full data dataframe
#each site will be assigned the 4 columns of seasonal precip based on which HUC it falls into
#can use JMP to look for trends between seasonal precip and recharge