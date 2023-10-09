#Caelum Mroczek
#10/8/23


# Create a function to find missing years for a specific SiteNum
find_missing_years <- function(site_data) {
  all_years <- as.numeric(1991:2020)
  site_years <- as.numeric(unlist(strsplit(site_data$Years, ",")))
  missing_years <- setdiff(all_years, site_years)

  return(c(missing_years))
}


# Apply the function to each SiteNum in the GaugeList
missing_years_list <- lapply(split(GaugeList, GaugeList$SiteNum), find_missing_years)


#iterate through the list to access which years are missing with [i][[1]][j] format
missing_years_list[5][[1]][2]
