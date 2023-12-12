PredictorPoints <- read_csv("/Users/caelum/Library/Mobile Documents/com~apple~CloudDocs/NAU/Research/AZ_Basin_Baseflow/BFI-Data/BasinPredictorPoints.csv")
Points_LatLong <- PredictorPoints[,2:3]

#Run on laptop w/ NAU connection
point_annualBFI <- BFI.predictor(Points_LatLong[1:10,], "~/Documents/GitHub/BFI_Research/XGB_Training/XGB_12122023")


## Need to link the dataframes to produce one with HUCs and each point averaged
# average across HUC for each point within it
PredictorPoints <- merge(point_annualBFI, PredictorPoints, by.x = "LAT", by.y = "Lat")
