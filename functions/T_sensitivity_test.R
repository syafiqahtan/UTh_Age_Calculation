# This function helps to conduct a sensitivity analysis to assess if uncorr and corr ages change wrt to 
# varying initial thorium values. By changing the value from 0 to 10e-6, in increments of 1e-6, this code
# uses an MC approach to calculating ages.

# This function was written by Syafiqah, and last modified on 24 March 2025  
  #25Feb25: original function written
  #24Mar25: added more argumemnts so that user can change min and max ages from main script

T_sensitivity_test <- function(x, minBound, maxBound) { # x calls the data frame with the lab reported data
  
  ## ------ CALCULATING AGES FOR VARIOUS INITIAL THORIUM VALUES USING MC APPROACH ------ ##
  
  # results_list <- list() # blank list to store results
  final_results <- list() # creating a blank list to store MC iterations
  
  for (i in 1:nrow(x)) { # looping all the unique samples in the lab sheet
    
    df <- x[i,]
    final_results[[i]] <- list() # initialising `final_results[[i]]` as a list
    
    for (value in seq(0, 10e-6, by = 1e-6)) {  # looping through various initial thorium ratios
      
      v_i <- lapply(1:n.iter, function(iter) {  # for each specific thorium value, iterate million times to get t_uncorr and t_corr
        
        Th230_U238_a_i = rnorm(1,mean=df$X230Th_238_a,sd=df$X230Th_238_1sd_a) # normal distribution
        d234U_m_i= rnorm(1, mean=df$d234U_m,sd=df$d234U_1sd_m) # normal distribution
        Th230_Th232_atom_i = value
        Th232ppt_i=rnorm(1,mean=df$X232Th_ppt,sd=df$X232Th_1sd_ppt) # normal distribution
        U238ppt_i=rnorm(1,mean=df$X238U_ppb,sd=df$X238U_1sd_ppb)*1000 # normal distribution; divided by 1000 to convert ppb to ppt
        
        sampleID_i <- df$sampleID
        labID_i <- df$labID
        
        result <- CalcT_i(Th230_U238_a_i, d234U_m_i, Th230_Th232_atom_i, Th232ppt_i, U238ppt_i, minBound, maxBound)
        
        # Convert result to data frame with sample identifiers
        data.frame(sampleID = sampleID_i, labID = labID_i, result)
        
      }) %>% rbindlist() # ending the calculation for each iteration per specific test_value. will give v_i
      
      # Write each set of results to a file immediately (prevents memory overload) edit properly later
      write.csv(v_i, paste0('results/UTh sensitivity analysis/t_sen_', df$labID, '_', format(value, scientific = TRUE), '.csv'), row.names = FALSE)
      
      final_results[[i]][[as.character(value)]] <- v_i #final output is a list
      # sample_results[[as.character(value)]] <- v_i  # Store this test_value result
      
    } # finishing the calculation for all the test_value 
    
  }
  
  ## ------ SUMMARY CALCULATION ------ ##
  
  # creating a summary df to store results
  sens_summary <- data.frame(
    sampleID = NA,
    labID = c(1:nrow(x)),
    value = rep(seq(0, 10e-6, by = 1e-6), times = nrow(x)),
    uncorr_mean = NA,
    uncorr_2sd = NA,
    corr_mean = NA,
    corr_2sd = NA)
  
  for (i in 1:nrow(x)) {
    
    for (value in seq(0, 10e-6, by = 1e-6)) {
      
      subset_df <- final_results[[i]][[as.character(value)]] # extracting the nested list we want
      # subset_df <- final_results %>% filter(labID == id, Th230_Th232_atom_i == value)
      
      sampleID_i <- subset_df[1,1]
      uncorr_mean_i <- mean(subset_df$t_uncorr_i)
      uncorr_2sd_i <- 2 * sd(subset_df$t_uncorr_i)
      corr_mean_i <- mean(subset_df$t_corr_i)
      corr_2sd_i <- 2  * sd(subset_df$t_corr_i)
      
      # storing in results df
      row_index <- which(sens_summary$labID == i & sens_summary$value == value)
      sens_summary[row_index, 1] <- sampleID_i
      sens_summary[row_index, 4] <- uncorr_mean_i
      sens_summary[row_index, 5] <- uncorr_2sd_i
      sens_summary[row_index, 6] <- corr_mean_i
      sens_summary[row_index, 7] <- corr_2sd_i
      
    }
    
  }
  
  return(sens_summary)
  
}