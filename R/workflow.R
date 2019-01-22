# Script to run STADEM, PITcleanr and DABOM

library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
library(forcats)
library(PITcleanr)
library(DABOM)
library(STADEM)

# set species and spawn year
spp = 'Chinook'  # either Chinook or Steelhead
yr = 2016        # tagging operations started at Lower Granite with spawn year 2009.


## Trap database

# file path to .csv version of trap database
trap_path = 'data/tblLGDMasterCombineExportJodyW.csv'


# Total Escapement with **STADEM**

# the compileGRAdata fnc queries DART for window count data and
# the LowerGraniteDB for trap data at the file path listed above
# weekly strata start on the day defined with the strata_beg arguement

# return four objects
# 1. strata date intervals
# 2. LGRtrap data
# 3. summarized window, fish and tag counts by day
# 4. summarized window, fish and tag counts by week

stadem_list = compileGRAdata(yr = yr,
                             spp = spp,
                             strata_beg = 'Mon')


# compile everything into a list to pass to JAGS
# creates JAGs data list.
jags_data_list = prepJAGS(stadem_list[['weeklyData']])

## Run STADEM

## Suggestions
# 
# Recommended MCMC parameters are:
# 
# * `mcmc_chains`: 4
# * `mcmc_chainLength`: 40,000
# * `mcmc_burn`: 10,000
# * `mcmc_thin`: 30

# name of JAGS model to write
# JAGs needs to access a .txt file of the model code
model_file_nm = 'ModelFiles/STADEM_LGR_model.txt'

# what distribution to use for window counts?
win_model = c('pois', 'neg_bin', 'neg_bin2', 'quasi_pois', 'log_space')[2]

#-----------------------------------------------------------------
# run STADEM model
#-----------------------------------------------------------------
# creates a list with 22 objects
# the 'summary' object contains estimates
# other objects has MCMC information

stadem_mod = runSTADEMmodel(file_name = model_file_nm,
mcmc_chainLength = 40,  #40000
mcmc_burn = 10, #10000
mcmc_thin = 3, #30
mcmc_chains = 4,
jags_data = jags_data_list,
seed = 5,
weekly_params = T,
win_model = win_model)

#------------------------------------------------------------------------------
# Process Capture Histories with **PITcleanr**
#------------------------------------------------------------------------------

## Configuration Table

org_config = buildConfig()

# customize some nodes based on DABOM framework
my_config = org_config %>%
  mutate(Node = ifelse(SiteID %in% c('VC2', 'VC1', 'LTR', 'MTR', 'UTR'),
  SiteID,
  Node),
  Node = ifelse(SiteID == 'SC2',
  'SC2B0',
  Node),
  Node = ifelse(SiteID %in% c('CROTRP',
  'CRT',
  'REDTRP',
  'REDR',
  'RRT'),
  'SC2A0',
  Node),
  Node = ifelse(SiteID == 'AFC',
  ifelse(grepl('MAINSTEM', AntennaGroup),
  'AFCB0',
  'AFCA0'),
  Node),
  Node = ifelse(SiteID == 'HBC',
  'HYCA0',
  Node),
  Node = ifelse(SiteID %in% c('TUCH', 'TFH'),
  'TUCH',
  Node),
  Node = ifelse(SiteID == 'MCCA',
  'STR',
  Node),
  Node = ifelse(SiteID == 'CARMEC',
  'CRCA0',
  Node),
  Node = ifelse(SiteID == 'BIG2C',
  'TAYA0',
  Node),
  Node = ifelse(SiteID == 'WIMPYC',
  'WPCA0',
  Node),
  Node = str_replace(Node, '^BTC', 'BTL'),
  Node = ifelse(SiteID %in% c('YANKFK', 'CEY'),
  'YFKA0',
  Node),
  Node = ifelse(SiteID == 'SAWT',
  'STL',
  Node),
  Node = ifelse(SiteID == 'LOOH',
  'LOOKGC',
  Node),
  Node = ifelse(SiteID == 'RPDTRP',
  'RAPH',
  Node),
  Node = ifelse(SiteID == 'CHARLC',
  'CCAB0',
  Node),
  Node = ifelse(Node == 'KEN',
  'KENB0',
  Node),
  Node = ifelse(Node == 'HYC',
  'HYCB0',
  Node),
  Node = ifelse(Node == 'YFK',
  'YFKB0',
  Node),
  Node = ifelse(Node == 'LLR',
  'LLRB0',
  Node),
  Node = ifelse(Node == 'LRW',
  'LRWB0',
  Node),
  Node = ifelse(SiteID == '18M',
  str_replace(Node, '18M', 'HEC'),
  Node)) %>%
  distinct()

## Node Network 

site_df = writeLGRNodeNetwork()

# remove some sites that have been combined with others (see the modifications to the configuration file)
site_df = site_df %>%
filter(!SiteID %in% c('TFH',
'MCCA',
'WIMPYC',
'YANKFK', 'CEY',
'SAWT',
'LOOH',
'CARMEC',
'BIG2C',
'RPDTRP'))

# a sample of what this looks like
head(site_df)


# Parent child table

parent_child = createParentChildDf(site_df,
my_config,
startDate = ifelse(spp == 'Chinook',
paste0(yr, '0301'),
paste0(yr-1, '0701')))


# add one site in Kenney Creek, if its been dropped
# needs to be done to keep the correct indexing for the JAGs model
# early years Kenney Creek only had one array and all carcasses were 
# grouped into KENA0 fictitous site.  

if(sum(grepl('KENA0', parent_child$ChildNode)) == 0) {
  lineNum = which(parent_child$ChildNode == 'KENB0')
  
  parent_child = parent_child %>%
    slice(1:lineNum) %>%
    bind_rows(parent_child %>%
                slice(lineNum) %>%
                mutate(ParentNode = 'KENB0',
                       ChildNode = 'KENA0')) %>%
    bind_rows(parent_child %>%
                slice((lineNum + 1):n()))
}



## Valid Tags

valid_df = filterLGRtrapDB(trap_path = trap_path,
                           species = spp,
                           spawnYear = yr,
                           saveValidTagList = T,
                           validTagFileNm = paste0('data/ValidTagLists/LGR_', spp, '_', yr, '.txt'))


## Complete Tag History
### Through PTAGIS

observations = read_csv(paste0('data/CompleteTagHistories/LGR_', spp, '_', yr, '.csv'))


### Through DART

# observations = valid_tag_df %>%
#   select(TagID) %>%
#   as.matrix() %>%
#   as.character() %>%
#   as.list() %>%
#   map_df(.f = queryCapHist,
#          configuration = my_config)



## Process Raw Observations

# returns a list with 5 objects
# 1. ValidPaths - path of fish from GRA to each Node; created with parent child table and 
#                   the getValidPaths() functions
#------------------------------------------------------------------------------
# 2. NodeOrder - metadata for each Node - possible DABOM model information and parameter settingss
#                     runs from createNodeOrder(), writeLGRNodeNetwork(), createParentChildDf control
#                     much of the DABOM groups and model landscape
#------------------------------------------------------------------------------
# 3. ValidTrapData - LowerGraniteDB for the correct species and spawnyear and no 'H' in SRR
# 4. ValidObs - duplicates in ValidTrapData removed and trap date is min(CollectionDate),
#                 LGR trap data is combined with observation data and Nodes are assigned,
#                 observations are also truncated to only valid nodes and obs dates and 
#                 the extra detections at a site (min obs date within an uninterupted sequence by node.)
# 5. ProcCapHist - from writeCapHistOutput(), has processed observations using two seperate
#               cleaning algorithms, direction of travel and migration direction

library(WriteXLS)
proc_list = processCapHist_LGD(species = spp,
                               spawnYear = yr,
                               configuration = my_config,
                               parent_child,
                               trap_path,
                               filter_by_PBT = T,
                               observations,
                               truncate = T,                                                                                                                                                                                                                    site_df,
                               step_num = 3,
                               save_file = T,
                               file_name = paste0('data/ProcCapHist_', spp, '_', yr, '.xlsx'))

# save to feed into DABOM package
save(proc_list,
     my_config,
     parent_child,
     file = paste0('data/PreppedData/LGR_', spp, '_', yr, '.rda'))

#-------------------------------------------------
# This section doesn't want to work!!!
#-------------------------------------------------
# library(readxl)
# proc_ch = read_excel(paste0('data/ProcCapHist_', spp, '_', yr, '.xlsx')) %>%
#   filter(UserProcStatus)

 

# set 'Biologist Call' to the auto call for now and our example!
proc_ch = proc_list$ProcCapHist %>%
  mutate(UserProcStatus = AutoProcStatus) %>%
  filter(UserProcStatus)


## Biological Summaries
  # assigns spawn location, last observation date and filters tag obs for only those
  # tags used in the DABOM model.

tag_summ = summariseTagData(capHist_proc = proc_ch,
                            trap_data = proc_list$ValidTrapData)

head(tag_summ)

#------------------------------------------------------------
# what is the agreement with IDFG
#------------------------------------------------------------
length(which(tag_summ$AssignSpawnSite==tag_summ$PtagisEventLastSpawnSite)) # agree 1489
length(which(tag_summ$AssignSpawnSite!=tag_summ$PtagisEventLastSpawnSite)) # disagree 179
# 179/2733  # 6.5 of the spawn sites disagree

#------------------------------------------------------------
# Estimate Movement with **DABOM**
#--------------------------------------------------------------

# should initial movement probabilities be time-varying?
time_varying = T

# write initial model
basic_modNm = 'ModelFiles/LGR_DABOM.txt'
writeDABOM_LGD(file_name = basic_modNm,
               time_varying = time_varying)


# set parameters to save
# the fnc is hard coded and needs to be update if there are changes!
jags_params = setSavedParams_LGD(time_varying = time_varying)


# extract datafrom PITcleanr
valid_paths = proc_list$ValidPaths

# extract datafrom PITcleanr
# extract data.frame of node order
node_order = proc_list$NodeOrder


# Remove all nodes with in steelhead branches that we don't care about for Chinook
if(spp == 'Chinook') {
  # remove some Chinook detections
  sthd_only_nodes = node_order %>%
    filter(Group %in% c('Asotin', 'Lapwai', 'Potlatch', 'JosephCreek', 'CowCreek', 'CarmenCreek', 'Almota', 'Alpowa', 'Penawawa')) %>%
    select(Node) %>%
    as.matrix() %>%
    as.character()
  
  proc_ch = proc_ch %>%
    filter(!Node %in% sthd_only_nodes)
  
}

#-------------------------------------------------
# Alter default model code for species and year of 
# interest
#-------------------------------------------------
# mark some detection probabilities 0 or 100%
mod_path = paste0('ModelFiles/LGR_DABOM_', spp, '_', yr, '.txt')


fixNoFishNodes(basic_modNm,
               mod_path,
               proc_ch,
               node_order)


# split observations into matrices to feed directly to JAGS
dabom_list = createDABOMcapHist(proc_ch,
                                node_order,
                                root_site = 'GRA',
                                split_matrices = T)

# create a function to spit out initial values for MCMC chains
init_fnc = setInitialValues_LGD(dabom_list)

# pull together all data to feed to JAGS, in a named list
jags_data = createJAGSinputs_LGD(dabom_list)


# Need to change the format of TrapDate in proc_ch
proc_ch <- proc_ch %>%
  mutate(TrapDate = lubridate::mdy_hms(TrapDate))

if(time_varying) {
  jags_data = c(jags_data,
                addTimeVaryData(proc_ch,
                                spawn_yr = yr,
                                spp = spp))
}


## Suggestions

# Recommended MCMC parameters are:
#   
#   * `n.chains`: 4
# * `n.iter`: 5,000
# * `n.burnin`: 2,500
# * `n.thin`: 10
# 4*(5000+2500) = 30000
# 1 iteration takes about .18 minutes
# about 3.75 days!!!!!


# This provides a sample of 1,000 draws from the posterior distribution. Through trial and error, we have also determined the appropriate burn-in length and thinning interval to meet MCMC posterior checks. 


# run JAGS model
library(jagsUI)
set.seed(12)
dabom_mod <- jags.basic(data = jags_data,
                        inits = init_fnc,
                        parameters.to.save = jags_params,
                        model.file = mod_path,
                        n.chains = 2,
                        n.iter = 20,
                        n.burnin = 10,
                        n.thin = 1,
                        DIC = T)

# save the results
save(dabom_mod, dabom_list, node_order, configuration,
     file = paste0('DABOM_results/LGR_DABOM_', spp, '_', yr, '.rda'))


load(paste0('DABOM_results/LGR_DABOM_', spp, '_', yr, '.rda'))


## Detection Probabilities

Directly from `DABOM`, we can extract estimates of the detection probability of the observation nodes. These are average probabiliities across the entire season of the run, and how these nodes match up to actual arrays and antennas is defined in the configuration file.

```{r}
detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                   capHist_proc = proc_ch)
head(detect_summ)



# Estimate Escapement with **STADEM** and **DABOM**

# compiles posteriors of escapement
trib_summ = calcTribEscape_LGD(dabom_mod,
                               stadem_mod,
                               stadem_param_nm = 'X.new.wild',
                               bootstrap_samp = 5, #2000
                               node_order = node_order,
                               summ_results = T,
                               pt_est_nm = 'median',
                               cred_int_prob = 0.95)

trib_summ %>%
  filter(!is.na(cv)) %>%
  head() %>%
  pander::pander()


report_summ = calcRepGrpEscape_LGD(dabom_mod = dabom_mod,
                                   stadem_mod = stadem_mod,
                                   node_order = node_order,
                                   pt_est_nm = 'median',
                                   spp = spp)


## Save Results

list('Report Groups' = report_summ,
     'All Escapement' = trib_summ,
     'Detection' = detect_summ) %>%
  WriteXLS(ExcelFileName = paste0('DABOM_results/Escapement_', spp, '_', yr, '.xlsx'),
           AdjWidth = T,
           AutoFilter = T,
           BoldHeaderRow = T,
           FreezeRow = 1)


  