HUC_recharge <- read.csv("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/HUC_Recharge_final.csv")
province_HUC <- read.csv("~/Documents/GitHub/BFI_Research/Base-Flow-Spatial/Data/province_HUC.csv")

recharge <- merge(HUC_recharge, province_HUC, by = "HUC8")
recharge$Recharge_af <- as.numeric(recharge$Recharge_af)

total <- sum(recharge$Recharge_af)

ind_CoPl <- which(recharge$PROVINCE == "COLORADO PLATEAUS")
CoPl <- sum(recharge$Recharge_af[ind_CoPl])

p.CoPl <- CoPl/total

##################
e <- read.csv("/Users/caelum/Library/Mobile Documents/com~apple~CloudDocs/NAU/Research/AZ_Basin_Baseflow/VariableData/HUC_Variables/OLD/HUC_Data_Full.csv")
elev <- e[,c(1,6)]
recharge <- merge(recharge, elev, by = "HUC8")

