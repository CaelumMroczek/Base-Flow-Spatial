packages <- c("dplyr", "sf", "raster", "terra", "ggplot2", "readr", "boot")
invisible(lapply(packages, library, character.only = TRUE))

############################################################################
############################################################################

PredictorPoints <- read_csv("/Users/caelum/Library/Mobile Documents/com~apple~CloudDocs/NAU/Research/AZ_Basin_Baseflow/BFI-Data/BasinPredictorPoints.csv")
Points_LatLong <- PredictorPoints[,2:3]

colnames(Points_LatLong) <- c("LAT", "LONG")

#Run on laptop w/ NAU connection 
# This uses 20 random points from each HUC as inputs and predicts BFI for each
# Takes ~6.5 minutes to run w/ 1680 rows
start.time <- Sys.time()
point_annualBFI <- BFI.predictor(Points_LatLong, "~/Documents/GitHub/BFI_Research/XGB_Training/XGB_12122023")
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

## Need to link the dataframes to produce one with HUCs and each point averaged
# average across HUC for each point within it
PredictorPoints <- merge(point_annualBFI, PredictorPoints, by.x = "LAT", by.y = "Lat")
PredictorPoints <- PredictorPoints[,c(3,1,4,2)]


######################################################
#----- Annual Recharge by instrumented record-----#
######################################################

#calculate recharge from BFI and P-ET for each year
HUC_Recharge <- HUC_data$BFI * (HUC_data$PRECIP_MM - HUC_data$ET_MM)
HUC_Recharge <- ifelse(HUC_Recharge < 0, 0, HUC_Recharge) #0 recharge is least

HUC_Recharge <- as.data.frame(HUC_Recharge)
HUC_Recharge <- cbind(HUC_data$HUC8, HUC_Recharge)
colnames(HUC_Recharge) <- c("HUC8", "Recharge_mm")

#get mean Recharge for each HUC from instrumented record
averages_HUC <- HUC_Recharge %>%
  group_by(HUC8) %>%
  summarize(Mean_R_mm = mean(Recharge_mm, na.rm=TRUE))

averages_HUC <- merge(averages_HUC, HUC_data, by= "HUC8")

#get selected values into one DF
averages_HUC <- averages_HUC %>%
  dplyr::select(HUC8, Mean_R_mm, AREA_KM2, PRECIP_MM) %>%
  group_by(HUC8, AREA_KM2) %>%
  summarize(Mean_R_mm = mean(Mean_R_mm, na.rm=TRUE),
            Precip_mm = mean(PRECIP_MM))


averages_HUC$Mean_R_afy <- mmy_afy(averages_HUC$Mean_R_mm, averages_HUC$AREA_KM2)
averages_HUC$R_P_Percent <- averages_HUC$Mean_R_mm/averages_HUC$Precip_mm #calc r as percent of p

#Add province to dataset
for(i in 1:nrow(averages_HUC)){
  this <- which(provinces$HUC8 == averages_HUC$HUC8[i])
  
  averages_HUC$Province[i] <- provinces[this,2]
}

mean(averages_HUC$R_P_Percent[which(averages_HUC$Province == 'COLORADO PLATEAUS',)])
mean(averages_HUC$R_P_Percent[which(averages_HUC$Province == 'BASIN AND RANGE',)])
mean(averages_HUC$R_P_Percent)



###############
###Average BFI for each Physiographic Region###
###############

HUC_BFI <- HUC_data$BFI
HUC_BFI <- ifelse(HUC_BFI < 0, 0, HUC_BFI) #0 recharge is least

HUC_BFI <- as.data.frame(HUC_BFI)
HUC_BFI <- cbind(HUC_data$HUC8, HUC_BFI)
colnames(HUC_BFI) <- c("HUC8", "BFI")

averageBFI_HUC <- HUC_BFI %>%
  group_by(HUC8) %>%
  summarize(Mean_BFI = mean(BFI, na.rm=TRUE))

for(i in 1:nrow(averageBFI_HUC)){
  this <- which(provinces$HUC8 == averageBFI_HUC$HUC8[i])
  
  averageBFI_HUC$Province[i] <- provinces[this,2]
}

mean(averageBFI_HUC$Mean_BFI[which(averageBFI_HUC$Province == 'COLORADO PLATEAUS',)])
mean(averageBFI_HUC$Mean_BFI[which(averageBFI_HUC$Province == 'BASIN AND RANGE',)])
mean(averageBFI_HUC$Mean_BFI)
