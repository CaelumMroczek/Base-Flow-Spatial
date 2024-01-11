HUC_data <-
  read.csv("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/Data/HUC_Data_12082023.csv")
provinces <-
  read.csv("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/Data/province_HUC.csv")

df <- data_frame()

for (i in 1:nrow(HUC_data)) {
  huc <- HUC_data$HUC8[i]
  yr <- HUC_data$YEAR[i]
  bfi <- HUC_data$BFI[i]
  p <- HUC_data$PRECIP_MM[i]
  
  q <- HUC_data$PRECIP_MM[i] - HUC_data$ET_MM[i] #P-ET
  
  r <- bfi * q
  r <- ifelse(r < 0, 0, r) #if recharge is negative, set to 0
  
  rpercent <- r / p #percent of p that is r
  
  l <- c(huc, yr, bfi, round(r, 3), round(rpercent, 3))
  
  df <- rbind(df, l)
}
colnames(df) <- c("HUC", "Year", "BFI", "Recharge_mm", "R.Percent")

#Assign physiographic region to each HUC
for(i in 1:nrow(df)){
  this <- which(provinces$HUC8 == df$HUC[i])
  
  df$Province[i] <- provinces[this,2]
}

################################################################
#BFI trend for each HUC

u_df <- unique(df$HUC)
bfi_trend <- data_frame()
for (i in 1:49){
  huc_vect <- which(df$HUC == u_df[i])
  temp <- df[huc_vect,]
  
  subset_temp <- temp[which(temp$Year > 1990 & temp$Year < 2021), ]
  lm <- lm(BFI ~ Year, data = subset_temp)
  summ <- summary.lm(lm)
  p_val <- summ$coefficients[8]
  coeff_val <- summ$coefficients[2]
  
  l <- c(u_df[i], p_val, coeff_val)
  bfi_trend <- rbind(bfi_trend, l)
}
names(bfi_trend) <- c("huc", "pval", "coeff")
sig_b <- which(bfi_trend$pval < .05)
sig_bfi <- bfi_trend[sig_b,]

#Recharge trend for each HUC
u_df <- unique(df$HUC)
recharge_trend <- data_frame()
for (i in 1:49){
  huc_vect <- which(df$HUC == u_df[i])
  temp <- df[huc_vect,]
  subset_temp <- temp[which(temp$Year > 1990 & temp$Year < 2021), ]
  
  lm <- lm(Recharge_mm ~ Year, data = subset_temp)
  summ <- summary.lm(lm)
  p_val <- summ$coefficients[8]
  coeff_val <- summ$coefficients[2]
  
  l <- c(u_df[i], p_val, coeff_val)
  recharge_trend <- rbind(recharge_trend, l)
}
names(recharge_trend) <- c("huc", "pval", "coeff")
sig_r <- which(recharge_trend$pval < .05)
sig_recharge <- recharge_trend[sig_r,]

#R/P trend for each HUC 
# IDENTICAL TO RECHARGE
u_df <- unique(df$HUC)
r_p_trend <- data_frame()
for (i in 1:49){
  huc_vect <- which(df$HUC == u_df[i])
  temp <- df[huc_vect,]
  subset_temp <- temp[which(temp$Year > 1990 & temp$Year < 2021), ]
  
  lm <- lm(Recharge_mm ~ Year, data = subset_temp)
  summ <- summary.lm(lm)
  p_val <- summ$coefficients[8]
  
  l <- c(u_df[i], p_val)
  r_p_trend <- rbind(r_p_trend, l)
}
names(r_p_trend) <- c("huc", "pval")
which(r_p_trend$pval < .05)
################################################################


################################################################
#Rillito recharge over period of record
tmp <- which(df$HUC == "15050302")
temp <- df[tmp, ]

temp$Year <- as.numeric(temp$Year)
annual_averages <-
  aggregate(Recharge_mm ~ Year, data = temp, FUN = mean)

rill <-
  ggplot(annual_averages[which(annual_averages$Year > 1990 &
                                 annual_averages$Year < 2021), ], aes(Year, Recharge_mm)) +
  geom_line() +
  geom_point()

#Hassayampa recharge estimate to compare to model
hassa <- HUC_data[which(HUC_data$SITENUM == 9517000 | HUC_data$SITENUM == 9515500 | HUC_data$SITENUM == 9516500),] 
hassa <- select(hassa, c('BFI', 'PRECIP_MM', 'ET_MM', 'AREA_KM2'))
hassa$Recharge <- hassa$BFI*(hassa$PRECIP_MM - hassa$ET_MM)
hassa$Recharge <- ifelse(hassa$Recharge < 0, 0, hassa$Recharge)
hassa_R <- mean(hassa$Recharge)
hassa_R_afy <- mmy_afy(hassa_R, hassa$AREA_KM2[1])

######################################################
#-----Avg Annual Recharge by Physiographic Region-----#
######################################################

#All HUC mean annual recharge for period of record by physiographic province
temp <- df
temp$Year <- as.character(temp$Year)

# Loop through unique combinations of 'HUC' and 'Year' and calculate the mean
# for each HUC and Year
average_values <- data.frame()

for (huc in unique(temp$HUC)) {
  for (year in unique(temp$Year)) {
    subset_data <- temp[temp$HUC == huc & temp$Year == year, ]
    if (nrow(subset_data) > 0) {
      average_row <- data.frame(
        HUC = huc,
        Year = year,
        average_BFI = mean(subset_data$BFI),
        average_Recharge_mm = mean(subset_data$Recharge_mm),
        average_R_Percent = mean(subset_data$R.Percent)
      )
      average_values <- rbind(average_values, average_row)
    }
  }
}

#assign province to each HUC
for(i in 1:nrow(average_values)) {
  this <- which(provinces$HUC8 == average_values$HUC[i])
  
  average_values$Province[i] <- provinces[this, 2]
}

avg_vals <- average_values[which(average_values$Year>1991 & average_values$Year<2020),]


#separate to different datasets
avg_vals_BR <- avg_vals[which(avg_vals$Province == "BASIN AND RANGE"),]
avg_vals_BR$Year <- as.Date(avg_vals_BR$Year, "%Y")

avg_vals_CP <- avg_vals[which(avg_vals$Province == "COLORADO PLATEAUS"),]
avg_vals_CP$Year <- as.Date(avg_vals_CP$Year, "%Y")


ggplot()+
  geom_point(avg_vals_BR, mapping= aes(Year, average_Recharge_mm), color = 'steelblue')+
  geom_point(avg_vals_CP, mapping= aes(Year, average_Recharge_mm),color = 'orangered')+
  theme_minimal()+
  ylab("Recharge (mm)")





