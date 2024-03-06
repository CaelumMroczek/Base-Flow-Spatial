#data path contains lat/long points | 
#Model path is the path to the model desired |
#whichComp indicates computer being used to run code
BFI.predictor <- function(input_dataframe, model_path, whichComp) {
  
  ###--------Set up function---------###
  packages <- c("dplyr", "sf", "raster", "terra", "ggplot2", "readr", "boot", "progress")
  invisible(lapply(packages, library, character.only = TRUE))

  if(whichComp == "office"){
    basePath <- "~/GitHub"
  }else if(whichComp == "laptop"){
    basePath <- "/Users/caelum/Documents/GitHub/BFI_Research"
  }else if(whichComp == "desktop"){
    basePath <- "####"
  }
  
  set.seed(313)
  
  xgb_model <- readRDS(model_path)
  River_Points <- input_dataframe
  
  ###------------Clean input file------------###
  
  year_list <- c(seq(from = 1991, to = 2020))
  River_Points$ID <- 1:nrow(River_Points)
  
  #add years to each site
  expanded_years <- expand.grid(LAT = unique(River_Points$LAT), YEAR = year_list) 
  River_Points <- merge(River_Points,expanded_years, by = "LAT", all.x = TRUE) 
  River_Points <- River_Points[ order(River_Points$ID, River_Points$YEAR),]
  River_Points <- River_Points[,c("ID","YEAR","LAT","LONG")]
  colnames(River_Points) <- c("ID","YEAR","LAT","LONG")
  
  ###-----------Load Data-----------###
  #Need to set up to be run on home computers
  
  # Load HUC8 basin raster
  # HUC8_Basins <- terra::rast("S:/CEFNS/SESES/GLG/Open/Mroczek,Caelum/Data/HUC8_rasters/huc8.tif")
  # HUC8_Basins <- project(HUC8_Basins, "+proj=longlat +datum=WGS84")
  # 
  # # Load DEM raster
  # DEM <- rast("S:/CEFNS/SESES/GLG/Open/Mroczek,Caelum/Data/DEM_30M/AZ_DEM_30M_latlong.tif")
  # 
  # #Load precip csv
  # precip_df <- read.csv("~/GitHub/Base-Flow-Spatial/Data/HUC_precip.csv")
  # 
  # # Load temp data
  # temp_df <- read.csv("~/GitHub/Base-Flow-Spatial/Data/HUC_temp.csv")
  # 
  # #Load ET data
  # et_df <- read.csv("~/GitHub/Base-Flow-Spatial/Data/HUC_annualET.csv")
  # 
  # # Load HUC predictors
  # HUC_Predictors <- read_csv("~/GitHub/Base-Flow-Spatial/Data/HUC_Dataset.csv", show_col_types = FALSE)
  
  ###-----------Assign Data to Points-----------###
  
  pb <- progress_bar$new(total = nrow(River_Points),
                         format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  for (i in 1:nrow(River_Points)) {
    p <- vect(River_Points[i,], geom = c("LONG", "LAT"))
    huc <- terra::extract(HUC8_Basins, p)
    elev <- terra::extract(DEM, p)
    River_Points$HUC8[i] <- as.numeric(as.character(huc[, 2]))
    River_Points$ELEVATION_FT[i] <- (elev[, 2]) * 3.281
    pb$tick()
  }
  
  print("Added HUC and Elevation to poins")
  
  # Pre-calculate indices pptHUC and etHUC
  pptHUC <- match(River_Points$HUC8, precip_df$HUC)
  tempHUC <- match(River_Points$HUC8, temp_df$HUC8)
  etHUC <- match(River_Points$HUC8, et_df$HUC8)
  
  pb <- progress_bar$new(total = nrow(River_Points),
                         format = "[:bar] :percent eta: :eta")
  pb$tick(0)
  # Loop over years
  for (i in 1:nrow(River_Points)) {
    year <- River_Points$YEAR[i]
    huc <- River_Points$HUC8[i]
    whichYear <- paste0("X", year)
    
    temp <- et_df[etHUC[i], whichYear]
    River_Points$TEMP_C[i] <- round(temp, 2)
    
    ppt <- precip_df[pptHUC[i], whichYear]
    River_Points$PRECIP_MM[i] <- round(ppt, 2)
    
    et <- et_df[etHUC[i], whichYear]
    River_Points$ET_MM[i] <- round(et, 2)
    
    pb$tick()
  }
  
  ###-----------Merge Datasets-----------###

  RiverPoints_AllData <- merge(River_Points, HUC_Predictors, by = "HUC8", all.x = TRUE)
  RiverPoints_AllData <- RiverPoints_AllData[, -which(names(RiverPoints_AllData) == "NAME")]
  RiverPoints_AllData <- RiverPoints_AllData[, c(1, 3:51)]
  
  ###-----------Predict BFI w/ XGBoost-----------###
  feature_names <- xgb_model$feature_names
  RiverPoints_AllData <- RiverPoints_AllData[, c("HUC8", "YEAR", "LAT",  "LONG", feature_names)]
  
  RiverPoints_AllData$predictedBFI <- inv.logit(predict(object = xgb_model, newdata = as.matrix(RiverPoints_AllData)[,5:50]))
  
  return(RiverPoints_AllData)
}
