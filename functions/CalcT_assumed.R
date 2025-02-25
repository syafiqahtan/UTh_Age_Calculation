# This function runs a straightforward age correction using an assumed initial thorium ratio
# Script modified from Tan Fangyi, whose script was last modified on 24th April 2024

# This function written by Syafiqah, last modified on 25 February 2025
  # in case user wants to modify the initial value, change Th230_Th232_atom_i

CalcT_assumed <- function(x) {
  
  # extract df for a given xth sample
  df = UTh_lab_raw[x,] #calls the lab-calculated ratios for each unique sample
  
  t_i <- lapply(1:n.iter, function(iter) {
    
    Th230_U238_a_i = rnorm(1,mean=df$X230Th_238_a,sd=df$X230Th_238_1sd_a) # normal distribution
    d234U_m_i= rnorm(1, mean=df$d234U_m,sd=df$d234U_1sd_m) # normal distribution
    Th230_Th232_atom_i=rnorm(1,mean=4*10^-6,sd=2*10^-6) # normal distribution (assumed avg value for upper continental crust as conventionally done) 
      #change assumed ratio if needed
    Th232ppt_i=rnorm(1,mean=df$X232Th_ppt,sd=df$X232Th_1sd_ppt) # normal distribution
    U238ppt_i=rnorm(1,mean=df$X238U_ppb,sd=df$X238U_1sd_ppb)*1000 # normal distribution; divided by 1000 to convert ppb to ppt
    
    CalcT_i(Th230_U238_a_i, d234U_m_i, Th230_Th232_atom_i, Th232ppt_i, U238ppt_i)
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
  
  return(df)
  
}