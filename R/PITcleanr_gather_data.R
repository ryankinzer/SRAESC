#------------------------------------------------------------------------------
# Script runs PITcleanr to gathering and clean DABOM data.
#
# Author: Ryan Kinzer
#------------------------------------------------------------------------------
# Load Packages
#------------------------------------------------------------------------------
library(tidyverse)
#library(stringr)
#library(lubridate)
#library(magrittr)
#library(forcats)
library(PITcleanr)

#------------------------------------------------------------------------------
# Identify which species and spawn year data is needed. 
#------------------------------------------------------------------------------

timestp <- gsub('[^0-9]','', Sys.Date())

spp = 'Chinook'  # either 'Chinook' or 'Steelhead'
yr = 2018        # tagging operations started in spawn year 2009

#------------------------------------------------------------------------------
# Identify file path to most current version of Lower Granite database 
#------------------------------------------------------------------------------
trap_filepath = 'data/tblLGDMasterCombineExportJodyW.csv'

#------------------------------------------------------------------------------
# Download all PTAGIS interrogation and MRR sites, antennas and configuations,
#  then it appends Node names.
#------------------------------------------------------------------------------

org_config = buildConfig()
#View(org_config)
#timestp <- gsub('[^0-9]','', Sys.time())
#save(org_config, file = paste0('./data/ConfigurationFiles/org_config_',timestp,'.csv'))

#------------------------------------------------------------------------------
# Change and customize the configuration file.
# NEED TO UPDATE TO CAPTURE THE 
#------------------------------------------------------------------------------

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
         Node = ifelse(Node == 'SALREF',
                       'SALEFT',
                       Node),
         Node = ifelse(SiteID == '18M',
                       str_replace(Node, '18M', 'HEC'),
                       Node)) %>%
  distinct()

#------------------------------------------------------------------------------
# Save customized configuration file as .csv with time stamp.
#------------------------------------------------------------------------------

write.csv(my_config, file = paste0('./data/ConfigurationFiles/my_config_',timestp,'.csv'),
          row.names = FALSE)

#------------------------------------------------------------------------------
# Reload customized configuration file if changes have been made to the .csv
# file outside of the R script.
#------------------------------------------------------------------------------
config_filepath <- './data/ConfigurationFiles/my_config_20190107.csv'
my_config <- read_csv(config_filepath)

#------------------------------------------------------------------------------
# Create Lower Granite site network and available paths; function is hard
# coded and needs be changed if the model branch/site network changes.
# We will change fnc name to writeLGRSiteNetwork() because it develops site
# paths and not nodes.
#
#  Site Detection Landscape
#
# Function develops and creates the model hierarchy to clean observations. 
# There is one record for each model detection site.
# Step 1 = Granite
# Step 2 = Geographical location (could be changed)
# Step 3 = first main branch name, needs to match JAGs code exactly
#         (needs to change as sites change.)
# Step 4-X = site names in upstream order ending at SiteID
#
# could be maintained externally
#------------------------------------------------------------------------------

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

#View(site_df)
# save output

write.csv(site_df, file = paste0('./data/ConfigurationFiles/site_df_',timestp,'.csv'),
          row.names = FALSE)

# load site_df if maintained externally
sitedf_filepath <- './data/ConfigurationFiles/site_df_20180207.csv'
site_df <- read_csv(sitedf_filepath)

#------------------------------------------------------------------------------
# Create parent-child node network based on site_df and customized
# configuration.
#
# Term: Node Detection Landscape 
#
# Joins DABOM site hierarchy with config table to get node names, then it 
# only keeps node available and needed for DABOM model
#
# dates are used to identify which antenna configuration in PTAGIS to use
#
# could be maintained externally
#------------------------------------------------------------------------------

parent_child = createParentChildDf(site_df,
                                   my_config,
                                   startDate = ifelse(spp == 'Chinook',
                                                      paste0(yr, '0301'),
                                                      paste0(yr-1, '0701')))

#------------------------------------------------------------------------------
# add one site in Kenney Creek, if its been dropped
# needs to be done to keep the correct indexing for the JAGs model
# early years Kenney Creek only had one array and all carcasses were 
# grouped into KENA0 fictitious site.
#
# THIS IS AN IMPORTANT STEP TO MAKE SURE THE MODEL RUNS CORRECTLY
#------------------------------------------------------------------------------

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


write.csv(parent_child, 
     file = paste0('./data/ConfigurationFiles/parent_child_',timestp,'.csv'),
     row.names = FALSE)

# Load parent_child table if maintained externally
parentchild_filepath <- './data/ConfigurationFiles/parent_child_20180207.csv'
parent_child <- read_csv(parentchild_filepath)

#------------------------------------------------------------------------------
## Get valid tags from Lower Granite trapping database and relavent fields,
# and saves a .txt file of tag codes for upload into PTAGIS complete tag 
# history query
#
# retrieves all specified species and spawn year,
#                        returning adults (LGDLifeStage = RF)
#                        LGDValid = 1,
#                         LGDMarkAD = AI
#                       fish with tag codes.
#
#  AND ONLY INCLUDES SRR = 15X (trap call not PTAGIS mark call) 
#       if species = 'Chinook';
#    spring/summer chinook catch at trap MUST BE CALLED SRR = '15X'
#------------------------------------------------------------------------------
valid_df = filterLGRtrapDB(trap_path = trap_filepath,
                           species = spp,
                           spawnYear = yr,
                           saveValidTagList = T,
                           validTagFileNm = paste0('data/ValidTagLists/LGR_', spp, '_', yr, '.txt'))

#------------------------------------------------------------------------------
# Read in the result of the PTAGIS complete tag history query
#------------------------------------------------------------------------------
observations = read_csv(paste0('data/CompleteTagHistories/LGR_', spp, '_', yr, '.csv'))

observations$`Antenna ID` <- str_pad(observations$`Antenna ID`, 2, pad = '0')

#------------------------------------------------------------------------------
# Combines node names with raw PTAGIS observations and runs cleaning/processing
# algorithms
#
# returns a list with 5 objects
# 1. ValidPaths - path of fish from GRA to each Node; created with parent child table and 
#                   the getValidPaths() functions
# 2. NodeOrder - metadata for each Node - possible DABOM model information and parameter settingss
#                     runs from createNodeOrder(), writeLGRNodeNetwork(), createParentChildDf control
#                     much of the DABOM groups and model landscape
# 3. ValidTrapData - LowerGraniteDB for the correct species and spawnyear and no 'H' in SRR
# 4. ValidObs - duplicates in ValidTrapData removed and trap date is min(CollectionDate),
#                 LGR trap data is combined with observation data and Nodes are assigned,
#                 observations are also truncated to only valid nodes and obs dates and 
#                 the extra detections at a site (min obs date within an uninterupted sequence by node.)
# 5. ProcCapHist - from writeCapHistOutput(), has processed observations using two seperate
#               cleaning algorithms, direction of travel and migration direction
#
# Fix/add max date to ProcCapHist object which comes from writeCapHistOutput().
#    need Obsdate to be minObsdate at the node and need to include a 
#    maxObsdate at the node for each detection sequence.
#
# Fix ModelObs call - which comes from the writeSpwnPaths or assignNodes
#     need to flag all fish seen in two branches
#
# Fix maxUpDate field from writeSpwnPaths() could be incorrect if a fish moves downstream
# and then goes partially back up, maybe just flag observations for reveiw
#
# Fix truncate = F, currently doesn't run.
#
# Fix save_file portion if we think its needed. Change in function to save as .csv
#
# Fix filter_by_PBT to include/exclude wild, hnc
#
# step_num = points to main branch column in site_df - NEEDS TO BE 3
# filter_by_PGT = removes all 'XXH' from SRR field in Lower Granite DB
#------------------------------------------------------------------------------

proc_list = processCapHist_LGD(species = spp,
                               spawnYear = yr,
                               configuration = my_config,
                               parent_child = parent_child,
                               trap_path = trap_filepath,
                               filter_by_PBT = T,
                               observations = observations,
                               truncate = T,
                               site_df = site_df,
                               step_num = 3,
                               save_file = F,
                               file_name = paste0('data/ProcCapHist_', spp, '_', yr, '.csv'))

# save list object as data frame for saving
proc_ch <- proc_list$ProcCapHist %>%
  as.tibble()

#------------------------------------------------------------------------------
# Get a quick summary of detections
#------------------------------------------------------------------------------
# number of tags observed at each node
n_tags <- proc_ch %>%
  group_by(Group, Node) %>%
  distinct(TagID, .keep_all = TRUE) %>%
  summarise(n = n()) %>%
  arrange(Group, n) 
# nodes with zero tags in proc_ch file
zero_tags <- proc_list$NodeOrder %>%
  anti_join(proc_ch, by = 'Node') %>%
  arrange(Group)
# nodes with zero tags that have observations in proc_list$ValidObs
miss_obs <- inner_join(zero_tags, proc_list$ValidObs, by = 'Node') %>%
  group_by(Group, SiteID, Node) %>%
  summarise(n = n_distinct(TagID))


#------------------------------------------------------------------------------
# Save Data
#------------------------------------------------------------------------------
write.csv(proc_ch, 
     file = paste0('./data/PreppedData/ProcCapHist_NULL_', spp, '_', yr,'_',timestp,'.csv'),
     row.names = FALSE)

proc_list[["my_config"]] <- my_config
proc_list[["parent_child"]] <- parent_child

# save entire list to feed into DABOM package
save(proc_list,
     file = paste0('data/PreppedData/LGR_', spp, '_', yr,'_',timestp,'.rda'))

#------------------------------------------------------------------------------
# Read in processed observation data after final biologist call is made.
#------------------------------------------------------------------------------
#proc_ch = read_csv('./data/CleanedData/ProcCapHist_EDITED', spp, '_', yr,'_',timestp,'.csv')

#------------------------------------------------------------------------------
# May be needed if setting call to automatic algorithm output
#------------------------------------------------------------------------------
# set 'Biologist Call' to the auto call for now and our example!
 proc_ch = proc_ch %>%
   mutate(UserComment = ifelse(UserProcStatus != AutoProcStatus, 'Changed UserProcStatus to equal AutoProcStatus', ''),
     UserProcStatus = AutoProcStatus)

write.csv(proc_ch, 
     file = paste0('./data/CleanedData/ProcCapHist_EDITED_', spp, '_', yr,'_',timestp,'.csv'),
     row.names = FALSE)

