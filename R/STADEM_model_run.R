#------------------------------------------------------------------------------
# Script gathers data and runs the STADEM model.  A loop is built in to cycle
# through Chinook and steelhead and all years begining in 2010.
#
# Author: Ryan Kinzer
#------------------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
library(forcats)
library(STADEM)

# set species and spawn year
species = c('Chinook', 'Steelhead')  # either Chinook or Steelhead
year = 2018        # tagging operations started at Lower Granite with spawn year 2009.

## Trap database
# file path to .csv version of LGR trap database - dnload 02/20/2018 (afternoon)
trap_filepath = 'data/tblLGDMasterCombineExportJodyW.csv'
#trap_filepath = 'data/LGTrappingExportJodyW.accdb'

# Loop through species and years
for(i in 1:length(species)){
  for(j in 1:length(year)){
    
    spp <- species[i]
    yr <- year[j]
    
    if(spp == 'Chinook'){
      incl_jacks = TRUE
    } else{
      incl_jacks = FALSE
    }
    

stadem_list = compileGRAdata(spp = spp,
                             start_date = paste0(yr,'0301'),
                             end_date = paste0(yr,'0817'),
                             strata_beg = 'Mon',
                             trap_path = trap_filepath,
                             incl_jacks = incl_jacks)   # incl_jacks needs to be true for Chinook, b/c window counts from DART are from two seperate columns

# creates JAGs data list.
jags_data_list = prepJAGS(stadem_list[['weeklyData']])

# Recommended MCMC parameters are:
# * `mcmc_chains`: 4
# * `mcmc_chainLength`: 40,000
# * `mcmc_burn`: 10,000
# * `mcmc_thin`: 30

# name of JAGS model to write
# JAGs needs to access a .txt file of the model code
model_file_nm = './ModelFiles/STADEM_LGR_model.txt'

# what distribution to use for window counts?
win_model = c('pois', 'neg_bin', 'neg_bin2', 'quasi_pois', 'log_space')[2]

# run model - runSTADEMmodel sets the params to save inside the fnc, and it
# does not same all the params available in the model!
stadem_mod = runSTADEMmodel(file_name = model_file_nm,
                            mcmc_chainLength = 40000,
                            mcmc_burn = 10000,
                            mcmc_thin = 30,
                            mcmc_chains = 4,
                            jags_data = jags_data_list,
                            seed = 5,
                            weekly_params = T,
                            win_model = win_model)

# save results
save(stadem_mod, stadem_list,
     file = paste0('./STADEM_results/LGR_STADEM_', spp, '_', yr, '.rda'))

  } # close j loop
} # close i loop
