packages <- c("dplyr", "sf", "raster", "terra", "ggplot2", "readr", "boot")
invisible(lapply(packages, library, character.only = TRUE))
install.packages("devtools")
devtools::install_github("slowkow/ggrepel")
library(ggrepel)
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

#Get mean annual recharge for each HUC for each year
point_BFI <- point_annualBFI[,c(1:5,10,51)]

point_BFI <- point_BFI %>%
  mutate(Recharge = predictedBFI * (PRECIP_MM - ET_MM))

point_BFI$Recharge <- ifelse(point_BFI$Recharge < 0, 0, point_BFI$Recharge)

# Calculate mean annual recharge for each HUC and each year
mean_recharge <- point_BFI %>%
  group_by(HUC8, YEAR) %>%
  summarise(mean_recharge = mean(Recharge, na.rm = TRUE))

#Recharge trend for each HUC
u_df <- unique(mean_recharge$HUC8)
recharge_trend <- data_frame()
for (i in 1:84){
  huc_vect <- which(mean_recharge$HUC8 == u_df[i])
  temp <- mean_recharge[huc_vect,]
  subset_temp <- temp[which(temp$YEAR > 1990 & temp$YEAR < 2021), ]
  
  lm <- lm(mean_recharge ~ YEAR, data = subset_temp)
  summ <- summary.lm(lm)
  p_val <- summ$coefficients[8]
  coeff_val <- summ$coefficients[2]
  
  l <- c(u_df[i], p_val, coeff_val)
  recharge_trend <- rbind(recharge_trend, l)
}
names(recharge_trend) <- c("huc", "pval", "coeff")
sig_r <- which(recharge_trend$pval < .05)
sig_recharge <- recharge_trend[sig_r,]



## Need to link the dataframes to produce one with HUCs and each point averaged
# average across HUC for each point within it
PredictorPoints <- merge(point_annualBFI, PredictorPoints, by.x = "LAT", by.y = "Lat")
PredictorPoints <- PredictorPoints[,c(3,1,4,2)]


######################################################
#----- Annual Recharge by instrumented record-----#
######################################################
HUC_data <- read_csv("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/Data/HUC_Data_12082023.csv")
#calculate recharge from BFI and P-ET for each year
HUC_Recharge <- HUC_data$BFI * (HUC_data$PRECIP_MM - HUC_data$ET_MM)
HUC_Recharge <- ifelse(HUC_Recharge < 0, 0, HUC_Recharge) #negative value to 0 recharge

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
  this <- which(province_HUC$HUC8 == averages_HUC$HUC8[i])
  
  averages_HUC$Province[i] <- province_HUC[this,2]
}
averages_HUC$Province <- unlist(averages_HUC$Province)
averages_HUC$Province <- as.factor(averages_HUC$Province)

mean(averages_HUC$R_P_Percent[which(averages_HUC$Province == 'COLORADO PLATEAUS',)])
mean(averages_HUC$R_P_Percent[which(averages_HUC$Province == 'BASIN AND RANGE',)])
mean(averages_HUC$R_P_Percent)

#decide outlier or not
find_outlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

averages_HUC <- averages_HUC %>%
  group_by(Province) %>%
  mutate(outlier = ifelse(find_outlier(Mean_R_afy), HUC8, NA))


ggplot(averages_HUC, aes(x = as.factor(Province), y = Mean_R_afy))+
  geom_boxplot() +
  xlab("Physiographic Province")+
  ylab("Recharge (af/yr)")+
  geom_text_repel(aes(label=outlier), na.rm=TRUE, hjust=-.2)


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
  this <- which(province_HUC$HUC8 == averageBFI_HUC$HUC8[i])
  
  averageBFI_HUC$Province[i] <- province_HUC[this,2]
}

averageBFI_HUC$Province <- unlist(averageBFI_HUC$Province)
averageBFI_HUC$Province <- as.factor(averageBFI_HUC$Province)

mean(averageBFI_HUC$Mean_BFI[which(averageBFI_HUC$Province == 'COLORADO PLATEAUS',)])
mean(averageBFI_HUC$Mean_BFI[which(averageBFI_HUC$Province == 'BASIN AND RANGE',)])
mean(averageBFI_HUC$Mean_BFI)

#outlier or not
averageBFI_HUC <- averageBFI_HUC %>%
  group_by(Province) %>%
  mutate(outlier = ifelse(find_outlier(Mean_BFI), HUC8, NA))

ggplot(averageBFI_HUC, aes(x = as.factor(Province), y = Mean_BFI))+
  geom_boxplot() +
  xlab("Physiographic Province")+
  ylab("BFI")+
  geom_text_repel(aes(label=outlier), na.rm=TRUE, hjust=-.2)
