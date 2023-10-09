#Caelum Mroczek
#10/8/23


# Create a function to find missing years for a specific SiteNum
find_missing_years <- function(site_data) {
  all_years <- 1993:2022
  site_years <- as.numeric(unlist(strsplit(site_data$Years, ",")))
  missing_years <- setdiff(all_years, site_years)
  
  # Create a dataframe with SiteNum and missing years
  result_df <- data.frame(SiteNum = site_data$SiteNum[1], MissingYears = missing_years)
  
  return(result_df)
}

# Apply the function to each SiteNum in the GaugeList
missing_years_list <- lapply(split(GaugeList, GaugeList$SiteNum), find_missing_years)


NumYearsMissing <- list()
for (site in length(missing_years_list)){
  gage <- missing_years_list[site]
  
  NumYearsMissing[[site]][1] <- names(gage[1])
  NumYearsMissing[[site]][2] <- length(gage[[3]])
}


