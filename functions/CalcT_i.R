# This function calculates uncorrected and corrected ages for each iteration, as well as d234U initial (i.e. in this case, for each of the million iterations)
# Using a MC approach, calculates the ages.
# Modified off the script written by Tan Fangyi, which was last updated on 24 April 2024

# Script written by Syafiqah, last modified on 24 March 2025
  # 25Feb25: wrote the code. tolerance set to 0.1
  # 24Mar25: added more arguments so that when used in nested functions, can change variables
            # such as min and max age bounds

CalcT_i <- function(Th230_U238_a_i, d234U_m_i, Th230_Th232_atom_i, Th232ppt_i, U238ppt_i, minBound, maxBound) {
  
  # eqn1 of Edwards et al., 2003
  eqn1=function(t){
    -Th230_U238_a_i+1-exp(-lambda_230*t)+((d234U_m_i/1000)*(lambda_230/(lambda_230-lambda_234))*(1-exp(-(lambda_230-lambda_234)*t)))}
  
  # calculate for a given ith random sample of Th230_U238_a and d234U_m, an output for t_uncorr (uncorrected 230Th ages)
  t_uncorr_i=bisection(f=eqn1, # equation to solve for t
                       a=minBound,b=maxBound, # search bounds for root # need to change this according to the suspected corrected age range
                       tol=0.00001) # tolerance
  
  # eqn 3 of Edwards et al., 2003 rearranged to use values measured and reported by the lab
  eqn3 = function(t){
    
    -Th230_U238_a_i+1-exp(-lambda_230*t)+((d234U_m_i/1000)*(lambda_230/(lambda_230-lambda_234))*(1-exp(-(lambda_230-lambda_234)*t)))+(((Th232ppt_i/232.032)/(U238ppt_i/238.051))*(lambda_230/lambda_238)*Th230_Th232_atom_i*exp(-lambda_230*t)) # eqn 1plus additional term for initial 230Th correction
  }
  
  # calculate for a given ith random sample of Th230_U238_a and d234U_m, an output for t_corr (age corrected for initial 230Th)
  t_corr_i=bisection(f=eqn3, # equation to solve
                     a=minBound,b=maxBound, # search bounds for root # need to change this according to the suspected corrected age range
                     tol=0.00001) # tolerance
  
  # eqn 2 of Edwards et al., 2003 rearranged: to calculate initial d234U_i (to check for open system behaviour)
  d234U_i_i=d234U_m_i/(exp(-lambda_234*t_corr_i))
  
  # Return the results as a named list
  return(list(Th230_Th232_atom_i = Th230_Th232_atom_i,
              t_uncorr_i = t_uncorr_i, 
              t_corr_i = t_corr_i, 
              d234U_i_i = d234U_i_i))
}

