# function to calculate min and max ages from sensitivity analysis
# function written by Syaf on 8August2025

fullUnc_sensi <- function(inFile) {
  
  # to get the max (value = 0) and min (value = 0.00001)
  sensitivity_maxmin <- inFile %>% 
    dplyr::filter(value == 0 | near(value, 1e-05))
  
  # creating a blank df
  
  col_names <- c("sampleID", "minAge", "maxAge", "mean", "sd")
  full_uncertainty <- data.frame(matrix(nrow = length(unique(inFile$sampleID)), ncol = 5))
  colnames(full_uncertainty) <- col_names
  full_uncertainty$sampleID <- unique(sensitivity_maxmin$sampleID)
  
  for (i in 1:nrow(full_uncertainty)) {
    
    sample_i <- full_uncertainty[i, 1]
    
    # for each sample, get max and min
    results_i_min <- sensitivity_maxmin %>% dplyr::filter(sampleID == sample_i & near(value, 1e-05))
    results_i_max <- sensitivity_maxmin %>% dplyr::filter(sampleID == sample_i & value == 0)
    full_uncertainty[i,2] <-  results_i_min$corr_mean - results_i_min$corr_2sd # minage
    full_uncertainty[i,3] <-  results_i_max$corr_mean + results_i_max$corr_2sd # maxage
  }
  
  # calculating mean and sd
  full_uncertainty$mean <- (full_uncertainty$maxAge + full_uncertainty$minAge)/2
  full_uncertainty$sd <- full_uncertainty$maxAge - full_uncertainty$mean
  
  # saving the file
 return(full_uncertainty)
  
}