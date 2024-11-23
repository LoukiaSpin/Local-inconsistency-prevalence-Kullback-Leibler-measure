#*******************************************************************************
#*
#*                Function to turn a gemtc dataset (long format)                                
#*                     into an rnmamod dataset (wide format)                                                                                                                                                                    
#*
#* Author: Loukia M. Spineli
#* Date: July 2024
#* 
#*******************************************************************************

convert_long_to_wide <- function (dataset) {

  # Calculate the number of arms per study
  na <- table(dataset[, 1])
  
  # whether the treatment ids are ascending
  treat_id_ascending <- unlist(lapply(split(dataset, dataset$study), 
                                      function(x) !is.unsorted(x$treatment)))
  
  # Add an indicator variable to facilitate reshaping
  dataset$numbers <- 
    unlist(lapply(1:length(na), 
                  function(x) if (treat_id_ascending[x] == TRUE) 1:na[x] else na[x]:1)) 
  
  # Get the desire data format for 'rnmamod'
  dataset_fin0 <- reshape(dataset, 
                          idvar = "study", 
                          timevar = "numbers", 
                          direction = "wide")
  
  # Sort columns by column name
  dataset_fin <- dataset_fin0[ , order(names(dataset_fin0))]
  
  # Rename columns to fit the 'rnmamod requirements
  colnames(dataset_fin) <- gsub("responders", "r", 
                                gsub("sampleSize", "n", 
                                     gsub("treatment", "t", colnames(dataset_fin)))) 
  
  return(dataset_fin)
}
