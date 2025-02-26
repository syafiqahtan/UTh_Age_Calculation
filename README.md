# Calculating uncorrected and corrected ages from lab-reported U-Th isotopic ratios.

In this repo, the script 'UTh_ages.R' contains the script to calculate ages for two scenarios using a MC approach with a million iterations:
1. With an assumed initial thorium ratio (in this script set as 4 +/- 2 * 10-6, but can change if needed), calculating uncorrected and corrected ages.
2. By varying the initial thorium ratios from 0 to 10e06, in increments of 1e-6, analysing whether the corrected ages change much to observe impacts of changing ratios.

Code was modified from Tan Fangyi's code. Her original codes are uploaded in a separate repo. 

The folder 'functions' contains the functions needed to run the main script. The main script will call in the functions.
 
