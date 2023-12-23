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


HUC_Recharge <- HUC_data$BFI * (HUC_data$PRECIP_MM - HUC_data$ET_MM)
HUC_Recharge <- ifelse(HUC_Recharge < 0, 0, HUC_Recharge)

HUC_Recharge <- as.data.frame(HUC_Recharge)
HUC_Recharge <- cbind(HUC_data$HUC8, HUC_Recharge)


