# This function runs a straightforward age correction using an assumed initial thorium ratio
# Script modified from Tan Fangyi, whose script was last modified on 24th April 2024

# This function written by Syafiqah, last modified on 25 February 2025
  #25Feb25: wrote the script. in case user wants to modify the initial value, must change Th230_Th232_atom_i here
  #24Mar25: added more arguments so that user can change min and max ages and initial thorium ratio from main script

CalcT_assumed <- function(x, minBound, maxBound, initialThoriumMean, sdThorium) { # include the max and min, initial thorium, x is the df
  
  # t_i <- list() # creating a blank list to store results
  
  df <- x # focusing on each sample 
  
  t_i <- lapply(1:n.iter, function(iter) {
    
    Th230_U238_a_i = rnorm(1,mean=df$X230Th_238_a,sd=df$X230Th_238_1sd_a) # normal distribution
    d234U_m_i= rnorm(1, mean=df$d234U_m,sd=df$d234U_1sd_m) # normal distribution
    Th230_Th232_atom_i=rnorm(1,mean=initialThoriumMean,sd=sdThorium) # normal distribution (assumed avg value for upper continental crust as conventionally done) 
    Th232ppt_i=rnorm(1,mean=df$X232Th_ppt,sd=df$X232Th_1sd_ppt) # normal distribution
    U238ppt_i=rnorm(1,mean=df$X238U_ppb,sd=df$X238U_1sd_ppb)*1000 # normal distribution; divided by 1000 to convert ppb to ppt
    
    CalcT_i(Th230_U238_a_i, d234U_m_i, Th230_Th232_atom_i, Th232ppt_i, U238ppt_i, minBound, maxBound)
  }
  ) %>% rbindlist()
  
  write.csv(x=t_i,paste0('results/UTh MC calculations/t_conventional_',df$labID,'.csv'))
  
  # calculate mean and sd of t_uncorr
  uncorr_mean=mean(t_i$t_uncorr_i) # to change to yrs BP
  uncorr_sd=sd(t_i$t_uncorr_i)
  df=df %>% mutate(t_uncorr=uncorr_mean) %>% mutate(t_uncorr_2sd=2*uncorr_sd) 
  
  # calculate mean and sd of t_corr
  mean=mean(t_i$t_corr_i)-73 # to change to yrs BP #lab work done in 2023
  sd=sd(t_i$t_corr_i)
  df=df %>% mutate(t_corr=mean) %>% mutate(t_corr_2sd=2*sd) 
  
  # calculate mean and sd of d234Ui
  d234Umean = mean(t_i$d234U_i_i) 
  d234Usd = sd(t_i$d234U_i_i) 
  df=df %>% mutate(d234Ui=d234Umean) %>% mutate(d234Ui_2sd=2*d234Usd) 
  
  return(df)
  
}