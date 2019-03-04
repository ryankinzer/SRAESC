#------------------------------------------------------------------------------
# Script loads DABOM and STADEM model results and organizes into data frames.
#
# Author: Ryan Kinzer
#------------------------------------------------------------------------------
# Load packages
#------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(PITcleanr)
library(DABOM)

#------------------------------------------------------------------------------
# set species and spawn year
species <- c('Chinook', 'Steelhead')  # either Chinook or Steelhead
year <- 2016:2018        # tagging operations started at Lower Granite with spawn year 2009.

dabom_files <- list.files('./DABOM_results/', pattern = '.rda') 
stadem_files <- list.files('./STADEM_results/', pattern = '.rda') 

timestp <- gsub('[^0-9]','', Sys.Date())
#------------------------------------------------------------------------------
# could build a loop around species and year vectors to load data.

proc_ch <- NULL
life_hist <- NULL
detect_summ <- NULL
wk_trans_summ <- NULL
trib_summ <- NULL
report_summ <- NULL

for(i in 1:length(species)){
  for(j in 1:length(year)){

    spp <- species[i]
    yr <- year[j]
#------------------------------------------------------------------------------
# Load the DABOM results
spp_files <- dabom_files[str_detect(dabom_files, spp)]
tmp_file <- spp_files[str_detect(spp_files, as.character(paste0("_",yr,"_")))]

load(file = paste0('./DABOM_results/',tmp_file))

# Load the STADEM results
spp_files <- stadem_files[str_detect(stadem_files, spp)]
tmp_file <- spp_files[str_detect(spp_files, as.character(paste0("_",yr)))]

load(paste0('./STADEM_results/', tmp_file))

#------------------------------------------------------------------------------
# Gather Raw Detection Data
#------------------------------------------------------------------------------
tmp_proc_ch <- proc_list$proc_ch %>%
  mutate(spawn_yr = yr,
         species = spp) %>%
  select(spawn_yr, species, everything())

proc_ch <- bind_rows(proc_ch, tmp_proc_ch)

#------------------------------------------------------------------------------
# Gather Life History Data by Spawn Site
#------------------------------------------------------------------------------

tmp_life_hist <- proc_list$life_hist %>%
  mutate(spawn_yr = yr,
         species = spp) %>%
  select(spawn_yr, species, everything())

life_hist <- bind_rows(life_hist, tmp_life_hist)

#------------------------------------------------------------------------------
## Gather Detection Probabilities
#------------------------------------------------------------------------------
tmp_detect_summ = summariseDetectProbs(dabom_mod = dabom_mod,
                                   capHist_proc = tmp_proc_ch) %>%
  mutate(spawn_yr = yr,
         species = spp,
         cv = sd/median) %>%
  select(spawn_yr, species, Node, n_tags, estimate = median, sd, cv, lowerCI, upperCI)

detect_summ <- bind_rows(detect_summ, tmp_detect_summ)

#------------------------------------------------------------------------------
## Gather All Transition Probabilities for STADEM
#------------------------------------------------------------------------------
# tmp_trans_summ <- summariseTransProbs_LGD(dabom_mod,
#                                       cred_int_prob = 0.95) %>%
#   mutate(spawn_yr = yr,
#          species = spp) %>%
#   select(spawn_yr, species, everything())
# 
# trans_summ <- bind_rows(trans_summ, tmp_trans_summ)

#------------------------------------------------------------------------------
## Gather Weekly Transition Probabilities 
#------------------------------------------------------------------------------
tmp_wk_trans_summ <- compileWeekTransProbs(dabom_mod) %>%
  mutate(spawn_yr = yr,
         species = spp) %>%
  group_by(spawn_yr, species, week, branch) %>%
  summarise(estimate = median(prob),
            sd = sd(prob),
            cv = sd/estimate,
            lowerCI = quantile(prob, probs = .025),
            upperCI = quantile(prob, probs = .975)) 

wk_trans_summ <- bind_rows(wk_trans_summ, tmp_wk_trans_summ)

#------------------------------------------------------------------------------
## Gather Tributary Estimates 
#------------------------------------------------------------------------------
tmp_trib_summ = calcTribEscape_LGD(dabom_mod,
                               stadem_mod,
                               stadem_param_nm = 'X.new.wild',
                               bootstrap_samp = 2000, #2000
                               node_order = proc_list$NodeOrder,
                               summ_results = T,
                               pt_est_nm = 'median',
                               cred_int_prob = 0.95) %>%
  mutate(spawn_yr = yr,
         species = spp) %>%
  select(spawn_yr, species, everything())


trib_summ <- bind_rows(trib_summ, tmp_trib_summ)

#------------------------------------------------------------------------------
## Gather Reporting Group Estimates 
#------------------------------------------------------------------------------
pop_df <- definePopulations(spp,
                            node_order = proc_list$NodeOrder) %>%
  rename(ReportGrp = TRT_POPID)


tmp_report_summ = calcRepGrpEscape_LGD(dabom_mod = dabom_mod,
                                   stadem_mod = stadem_mod,
                                   report_grp = pop_df,
                                   node_order = proc_list$NodeOrder,
                                   pt_est_nm = 'median',
                                   spp = spp) %>%
  mutate(spawn_yr = yr,
         species = spp) %>%
  select(spawn_yr, species, everything())

report_summ <- bind_rows(report_summ, tmp_report_summ)

    } # end j loop
  } # end i loop

#------------------------------------------------------------------------------
# Join estimate data frames with extra information for summarizing and grouping
#------------------------------------------------------------------------------

# Gather some metadata
my_config <- proc_list$my_config %>%
  select(Node, Latitude, Longitude) %>%
  distinct(Node, .keep_all = TRUE)

sitedf_filepath <- './data/ConfigurationFiles/site_df_20180207.csv'

site_df <- read_csv(sitedf_filepath) %>%
  select(SiteID, Area = Step2)

node_order <- proc_list$NodeOrder %>%
  select(Group, BranchNum, SiteID = NodeSite, Node, RKM, RKMTotal) %>%
  inner_join(my_config, by = 'Node') %>%
  inner_join(site_df, by = 'SiteID') %>%
  select(Area, everything())

report_df <- definePopulations('Steelhead', proc_list$NodeOrder) %>%
  mutate(species = 'Steelhead') %>%
  bind_rows(definePopulations('Chinook', proc_list$NodeOrder) %>%
              mutate(species = 'Chinook')) %>%
  #rename(Node = lowNode, SiteID = NodeSite) %>%
  left_join(node_order %>%
              select(SiteID, Area, Latitude, Longitude) %>%
              distinct(SiteID, .keep_all = TRUE), by = 'SiteID')

# Attach metadata to estimate data frames

proc_ch <- proc_ch %>%
  left_join(node_order)

life_hist <- life_hist %>%
  left_join(node_order %>%
              rename(AssignSpawnNode = Node)) %>%
  left_join(report_df %>%
              select(TRT_POPID, SiteID), by = c('AssignSpawnSite' = 'SiteID'))

detect_summ <- detect_summ %>%
  left_join(node_order, by = 'Node')

wk_trans_summ <- wk_trans_summ %>%
  rename(BranchNum = branch) %>%
  left_join(node_order %>%
              distinct(BranchNum, .keep_all=TRUE) %>%
              select(BranchNum, Area, Group), by = 'BranchNum')

# No common field yet, need to change the tribsummary fun!!!
# trib_summ <- trib_summ %>%
# rename(BranchNum = branch) %>%
#   inner_join(node_order %>%
#                select(BranchNum, Area, Group), by = 'BranchNum')
#

trt_sites <- report_df %>%
  select(TRT_POPID, SiteID) %>%
  group_by(TRT_POPID) %>%
  summarise(Sites_Included = toString(SiteID)) %>%
  ungroup()


report_summ <- report_summ %>%
  rename(TRT_POPID = ReportGrp) %>%
  left_join(trt_sites) %>%
  select(TRT_POPID,Sites_Included, species, spawn_yr, everything())

# 
# trib_summ <- trib_summ %>%
#   mutate(tmp = gsub('_bb','',SiteID)) %>%
#   left_join(detect_summ %>%
#               distinct(SiteID, Area, Group), by = c('tmp' = 'SiteID')) %>%
#   select(-tmp)

save(proc_ch, life_hist, detect_summ, wk_trans_summ, trib_summ, report_summ,
     file = paste0('DABOM_estimates/LGR_PIT_estimates_',timestp,'.rda'))

#------------------------------------------------------------------------------
# Load model run/estimates
load('./DABOM_estimates/LGR_PIT_estimates_20180216.rda')

library(WriteXLS)
#testPerl()

WriteXLS(x = c('detect_summ', 'trib_summ', 'report_summ', 'life_hist', 'proc_ch'),
         ExcelFileName = paste0('./DABOM_estimates/LGR_PIT_estimates_',timestp,'.xlsx'),
         SheetNames = c('Detection_Eff','Site_ests','Population_ests','Life_hist_data','Observation_data'))

# write_csv(report_locs, path = './data/ConfigurationFiles/reporting_groups.csv')
# write_csv(node_order, path = './data/ConfigurationFiles/node_order.csv')
# write_csv(trib_locs, path = './data/ConfigurationFiles/trib_locs.csv')


