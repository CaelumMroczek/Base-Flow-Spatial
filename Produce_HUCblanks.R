site_calcs <- function(dataset){
  
  dataset = cbind(dataset, BFI = 0) #Create BFI column in dataset
  dataset = cbind(dataset, Discharge_cfs = 0)
  errors = c()
  
  for (i in 1:nrow(dataset)){
    site_no <- as.character(dataset[i,][2]) #retrieve site number for every site
    
    gauge <- readNWISdv(siteNumbers = toString(site_no), 
                        parameterCd = "00060", statCd = "00003",
                        startDate = "1991-01-01",
                        endDate = "2020-12-31") #Retrieves data from USGS stream data based on site number
    
    gauge <- na.omit(gauge)
    
    if (nrow(gauge)!=0){
      bf <- BaseflowSeparation(gauge$X_00060_00003,passes = 3)#conduct baseflow separation
      
      BFsum <- sum(bf$bt)
      Tsum <- sum(bf$qft) + sum(bf$bt)
      bfi <- BFsum/Tsum
      
      dataset$BFI[i] = bfi
    } else{
      errors = append(errors,i)
    }
  }
  
  output = list('dataset' = dataset, 'errors' = errors)
  return(output)
  
}