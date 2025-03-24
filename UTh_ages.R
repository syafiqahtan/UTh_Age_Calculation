# ------ READ ME ------ 
# This script is for calculating UTh uncorrected and corrected ages of samples
# Using lab-reported isotopic ratios
# This script is modified from the script written by Tan Fangyi, which was last updated it on 24 April 2024
# TFY script has been converted into functions which can be found at wd/functions
# Additional functions created to run sensitivity analysis of corrected ages with various initial thorium ratios

# TL;DR: this script can calculate ages using one assumed initial thorium ratio (currently it is 4e-6, sd = 2e-6) 
# TL;DR: it can also conduct sensitivity analysis, where we calculate ages using a range of initial thorium values

# This script and any additional functions were written by Syafiqah, last modified on 24 March 2025
  # 9mar: changed the plotting function. added another sub-directory
  # 24mar: added variables that can be changed: upper and lower age bounds, initial thorium ratio (for conventional calculation), 
  #.       d344Ui results in the conventional list


# CREATNG BLANK DIRECTORIES TO STORE OUTPUTS -----

if (!dir.exists("results")) {
  dir.create("results")
}

  # To store when correcting ages for one assumed ratio
if(!dir.exists('results/UTh MC calculations')){
  dir.create('results/UTh MC calculations')
}

  # To store outputs from sensitivity analysis
if(!dir.exists('results/UTh sensitivity analysis')){
  dir.create('results/UTh sensitivity analysis')
}

if(!dir.exists('figures')){ 
  dir.create('figures')
}

# To store figure outputs from sensitivity analysis
if(!dir.exists('figures/sensitivity')){
  dir.create('figures/sensitivity')
}

# IMPORTANT: make sure that lab data is in a file called 'data'

# LOADING THE REQUIRED PACKAGES -----

library(writexl)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(cowplot)
library(readxl)
library(data.table)
library(readr)
library(ggridges)
library(plotly)
library(cmna) # for root-finding function (bisection method)

# LOADING THE FUNCTIONS ---- 
UTh_functions <- list.files(file.path(getwd(), "functions"))
UTh_functions <- UTh_functions[grepl(".R", UTh_functions)]
sapply(paste0("functions/", UTh_functions), source)

# LOADING THE LAB FILE AND DEFINING THE CONSTANTS 
  # loading the excel file with the lab-calculated ratios
UTh_lab_raw = readxl::read_xlsx('data/subset_TKKR-LZRS_UTh_labmeasurements_raw_formatted.xlsx')

  # specifying decay constants
lambda_230 = 9.1705*10^-6 # Th230 (Cheng et al., 2013)
lambda_234 = 2.82206*10^-6 # U234 (Cheng et al., 2013)
lambda_238 = 1.55125*10^-10 # U238 (Jaffey et al., 1971)

  # setting up random number generators
set.seed(42) # set seed for random number generator
n.iter=1000000 # number of iterations to run MC sampling

# CALCULATING AGES USING AN ASSUMED INITIAL THORIUM RATIO -----

# t_corr_conventional=lapply(X=1:nrow(UTh_lab_raw),FUN=CalcT_assumed) %>% rbindlist()
# write_xlsx(t_corr_conventional,'UTh_t_corr_MC_conventional.xlsx') #saving the results

t_corr_conventional = lapply(seq_len(nrow(UTh_lab_raw)), function(i) {
  CalcT_assumed(UTh_lab_raw[i, , drop=FALSE], # drop = FALSE keeps each row as df
                   maxBound = 15000, # change the following accordingly
                   minBound = 4000,
                   initialThoriumMean = 4*10^-6,
                   sdThorium = 2*10^-6) 
}) %>% rbindlist()

write_xlsx(t_corr_conventional, 'UTh_t_corr_MC_conventional.xlsx') # Saving results

# SENSITIVITY ANALYSIS: CALCULATING CORRECTED AGES WITH A RANGE OF INITIAL THORIUM VALUES ----

  # for now, changing the ratio from 0 to 10e-6. Need to go into function if user wants to change this.   

t_sensitivity <- T_sensitivity_test(UTh_lab_raw, 
                                       minBound = 4000, # change the bounds accordingly
                                       maxBound = 15000) 
write_xlsx(t_sensitivity, "UTh_sensitivity_analysis_test.xlsx") #saving the results

# Plotting the results from the sensitivity analysis

sens_plot <- sensitivity_plot(t_sensitivity) # ribbons show 2sd uncertainty
   
