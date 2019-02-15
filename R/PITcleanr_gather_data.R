#------------------------------------------------------------------------------
# Script runs PITcleanr to gathering and clean DABOM data.
#
# Author: Ryan Kinzer and Rick Orme
#------------------------------------------------------------------------------
# Load Packages
#------------------------------------------------------------------------------
library(tidyverse)
library(tidygraph)
library(ggraph)
#library(stringr)
#library(lubridate)
#library(magrittr)
#library(forcats)
#detach("package:PITcleanr", unload = TRUE)
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
  mutate(Node = case_when(
    # Tucannon Branch
    SiteID == 'TUCR'~ 'AB_LTR',
    SiteID == 'PATACH' ~ 'AB_MTR',
    SiteID %in% c('TUCH', 'TFH', 'CURP') ~ 'AB_UTR',
    # Lapwai Branch
    SiteID == 'LAPC' ~ 'AB_LAP',
    SiteID == 'SWEETC' ~ 'AB_SWT',
    SiteID == 'WEBBC' ~ 'AB_WEB',
    SiteID == 'MISSC' ~ 'AB_MIS',
    # Potlatch Branch
    SiteID %in% c('BOBSC','BOUL3C','CEDA2C','CORRAC','LBOULC','LEOPOC','MPOTLC','PIVASC','POTR','POTREF') ~ 'AB_JUL',
    SiteID %in% c('LBCWF', 'LBEARC') ~ 'AB_KHS',
    SiteID %in% c('COUG2C', 'FEATHC', 'MOOS3C', 'POTRWF','PURDUC') ~ 'AB_HLM',
    SiteID == 'PINE2C' ~ 'AB_PCM',
    SiteID == 'BIGBEC' ~ 'AB_BBA',
    # Lolo Creek Branch
    SiteID %in% c('ELDORC', 'LOLOC') ~ 'AB_LC2',
    # SF Clearwater Branch
    SiteID %in% c('AMERR','CROOKP','CROOKR','CROP','CROTRP','CROW','CRT','FIVEMC','JOHNC','LUGUAF','MEAD2C','MILL2C',
                  'NEWSOC','REDP','REDRSF','REDTRP','RELIEC','RRT','TENMIC','TWNMIC','TWNMIT') ~ 'AB_SC2',
    # Clear Creek Branch
    SiteID %in% c('CLEARC', 'KOOS') ~ 'AB_CLC',
    # Selway Branch
    SiteID %in% c('CEFLAF', 'OHARAC') ~ 'AB_SW1',
    SiteID %in% c('3LINKC','BEARC','EAGLEC','GEDCWF','GEDNEC','MEADOC','MINKC','MOOS2C','MOOS2N','RUNNIC','SELWY2','WHITCC') ~ 'AB_SW2',
    # Lochsa Branch
    SiteID %in% c('BIGFLC','BOULDC','BOUTRP','BRUSHC','CONYOC','CFCTRP','COLTC','COLTKC','CROOKC','DEADMC','OLDMAC',
                  'PAPOOC','PETEKC','POSTOC','POWP','SQUAWC','STORMC','WARMSC','WHITSC') ~ 'AB_LRU',
    SiteID %in% c('FISHC', 'HUNGC') ~ 'AB_FISTRP',
    # Clearwater Other Branch
    SiteID %in% c('BCCAP','BEDRKC','BIGCAC','CLJ','CLWH','CLWHNF','CLWR','CLWRMF','CLWRNF','CLWRSF','CLWTRP','COTNWC','DWL','DWOR','DWORMS','DWORNF',
                  'JACKSC','LITCAC','LOCTRP','NPTH','OROFC','SELWY1','SWSP') ~ 'CLEARWATER_OBS',
    # Asotin Branch
    SiteID %in% c('ASOTNF', 'ASOTSF') ~ 'AB_AFC',
    SiteID == 'CHARLC' ~ 'AB_CCA',
    # Snake Other Branch
    SiteID %in% c('CATHEC','CJRAP','COTP','COUGRC','CTWD3C','GRAND1','GRAND2','GRNTRP','HCD','HCDTAL',
                  'IMJ','IMNAHR','IMNTRP','OXBO','PLAP','SNAKE3','SNAKE4','SNJ','SNKTRP','WALLOR') ~ 'SNAKE_OBS',
    # Wenaha Branch
    SiteID %in% c('WENR', 'WENRNF', 'WENRSF') ~ 'AB_WEN',
    # LookingGlass Branch
    SiteID == 'LOOH' ~ 'LOOKGC',
    # Upper Grande Ronde Branch
    SiteID %in% c('CATCMF','CATCNF','CATCSF','CATHEP','CCP','LCATHC') ~ 'AB_CCW',
    # Wallowa Branch
    SiteID == 'BCANF' ~ 'AB_WR1',
    SiteID == 'MINAMR' ~ 'AB_MR1',
    SiteID == 'WALH' ~ 'AB_WR2',
    SiteID == 'LOSTIR' ~ 'AB_LOSTIW',
    # Cow Creek Branch
    SiteID == 'COWCR' ~ 'AB_COC',
    #Imanah Branch
    SiteID == 'IML' & AntennaID %in% c('11', 'F1', '07') ~ 'IML_C0',
    SiteID == 'IML' & AntennaID %in% c('12', 'F2', '08') ~ 'IML_B0',
    SiteID == 'IML' & AntennaID %in% c('09') ~ 'IML_A0',
    SiteID == 'LITNGC' ~ 'AB_IR1',
    SiteID == 'CAMP4C' ~ 'AB_CMP',
    SiteID %in% c('BSHEEC', 'LICK2C', 'SALTC') ~ 'AB_BSC',
    SiteID %in% c('LSHEEC', 'CANALC', 'MCCULC', 'REDMOC') ~ 'AB_LSHEEF',
    SiteID == 'CRAZYC' ~ 'AB_CZY',
    # Rapid River Branch
    SiteID %in% c('RAPIWF', 'RPDTRP', 'RPJ') ~ 'RAPH',
    # SF Salmon Branch
    SiteID == 'STR' & AntennaID %in% c('A1', 'A2') ~ 'STR_A0',
    SiteID == 'STR' & AntennaID %in% c('A3', 'A4') ~ 'STR_B0',
    SiteID %in% c('ELK2C', 'FITSUC', 'LSFTRP', 'SAEFSF', 'SFSRKT') ~ 'AB_SFG',
    SiteID %in% c('ALEXC','FLATC','GROUSC','LAKEC','LICKC','PIAHC','RUBYC','SECESR','SECTRP','SUMITC','ZENAC','ZENAWF') ~ 'AB_ZEN',
    SiteID %in% c('JOHTRP', 'SUGARC') ~ 'AB_ESS',
    SiteID == 'YPPL' ~ 'AB_YPP',
    SiteID == 'BURNLC' ~ 'AB_JOHNSC',
    SiteID %in% c('BCKHRC', 'CAMP3C', 'GOATC', 'PHOEBC', 'SALRSF') ~ 'AB_KRS',
    SiteID %in% c('KNOXB', 'MCCA', 'SFSTRP') ~ 'SALSFW',
    SiteID %in% c('BEAR4C', 'RICEC', 'STOLP') ~ 'AB_SALSFW',
    #Big Creek
    SiteID %in% c('BEAV4C','BIG2C','BIG2CT','BRAMYC','BUCK2C','CABINC','CAVEC','CROO2C','LOGANC','MONCWF','MONUMC','RUSHC','RUSHWF','SMITHC','SNOSLC') ~ 'AB_TAY',
    # Bear Valley Creek
    SiteID == 'BEARVC' ~ 'AB_BRC',
    # Panther Creek
    SiteID %in% c('MOYERC', 'MUSCRC', 'PANTHC') ~ 'AB_PCA',
    # North Fork Salmon
    SiteID == 'SALRNF' ~ 'AB_NFS',
    # Carmen Creek
    SiteID == 'CARMEC' ~ 'AB_CRC',
    # Lemhi Branch
    SiteID %in% c('HAYNSC', 'LEMHIR', 'LLRTP', 'MCDEVC', 'PATTEC', 'PRATTC', 'S2I', 'S2O', 'WITHGC') ~ 'AB_LLR',
    SiteID %in% c('BOHANC', 'BOHEFC') ~ 'AB_BHC',
    SiteID == 'WHIMPYC' ~ 'AB_WPC',
    SiteID == 'KENYC' ~ 'AB_KEN',
    SiteID %in% c('AGNCYC', 'COW2C', 'FLUMEC') ~ 'AB_AGC',
    SiteID %in% c('BASINC', 'BUCK4C', 'BVAL2C', 'HYDEF', 'HAYDNC', 'HYDTRP', 'TRAILC', 'WRIGTC') ~ 'AB_HYC',
    SiteID %in% c('BIGB2C', 'DEERC', 'LEMHIW', 'LEMTRP', 'LIT8MC', 'MILL5C', 'QKASPC', 'RESVRC', 'YRIANC') ~ 'AB_LRW',
    SiteID == 'LLSPRC' ~ 'AB_LLS',
    SiteID == 'BIG8mC' ~ 'AB_LB8',
    SiteID == 'BIGSPC' ~ 'AB_LBS',
    SiteID == 'LEEC' ~ 'AB_LCL',
    SiteID == 'BTMBC' ~ 'AB_BTL',
    SiteID == 'LTIMBC' ~ 'AB_BTM',
    SiteID == 'BASN2C' ~ 'AB_BTU',
    SiteID %in% c('CANY2C', 'CRUIKS', 'WILDCC') ~ 'AB_CAC',
    SiteID == 'HAWLYC' ~ 'AB_HEC',
    SiteID %in% c('18MILC', 'TEXASC') ~ 'AB_18M',
    # Upper Salmon Branch
    SiteID %in% c('CHALLC','ELK3C','HERDC','IRONC','POISNC','REDFL','REDFLC','RFL','RLCTRP','SALR4','SLAT2C','SQAW2C','SQUAWP','STANLC') ~ 'AB_USI',
    SiteID == 'SALEFW' ~ 'SALEFT',
    SiteID %in% c('PAHH', 'PAHP', 'PAHTRP') ~ 'AB_PAHSIW',
    SiteID %in% c('BASN3C', 'CEY', 'STANLE', 'YANKFK', 'YANKWF') ~ 'AB_YNK',
    SiteID %in% c('VALEYC', 'VC1') ~ 'AB_VC2',
    SiteID == 'STL' & AntennaID %in% c('A1', 'A2') ~ 'STL_A0',
    SiteID == 'STL' & AntennaID %in% c('A3', 'A4') ~ 'STL_B0',
    SiteID %in% c('4JULYC','ALTULC','ALTURL','BEAVEC','CHAMPC','DECKEC','FISHEC','FRENCC','GOLDC','HELLRC','HUCKLC',
                  'PETTL','PETTLC','POLEC','SAWTRP','SMILEC','VATC','WILLIC','YELLLC') ~ 'AB_SWT',
    # Salmon Other Branch
    SiteID %in% c('4JUL2C','BARGAC','BIGMAC','BOUL2C','CHAMBC','CHAMWF','CROOC','FLOSSC','FRENCH','HARDC','HAZARC','HORSEC','JERSEC',
                  'LSALR','MOOSEC','NFSTRP','PAHSIR','PARTRC','RAPIDR','SABEC','SAJ','SALR1','SALR2','SALR3','SALREF','SALTRP',
                  'SHEEPC','SLATEC','TOWERC','WBIRDC','WINDR') ~ 'SALMON_OBS',
    # MFSR Branch
    SiteID %in% c('BEAVC','BOUNDC','CAMASC','CAPEHC','DAGGEC','ELKC','FALLC','INDIAC','KNAPPC','LOONC','MARSHC','MARTR2',
                  'MARTRP','PISTOC','RAPR','SALMF1','SALMF2','SALRMF','SHEPC','SULFUC','WILSOC','YELLJC') ~ 'MFSR_OBS',
    # BELOW_GRA_OTHER Branch              
    SiteID %in% c('158', 'SUN', 'SAT', '15D', '15MILC', '15R', '1890SC', '18N', '21MILC', '30M', '30MILC', '30ML2C',
                  '3D1', '3D2', '3D3', '3D4', '3MILC', '3MILIS', '85M', '9MILEC', 'AB1', 'AB2', 'AB3', 'AB4', 'ABEH',
                  'ABERC', 'AEN', 'AENEAC', 'AHSH', 'AHTANC', 'AHTANF', 'AMERIR', 'ANT', 'ANTELC', 'ANTOIC', 'APROOK',
                  'B1J', 'B2A', 'B2J', 'BAD', 'BADGEI', 'BAKEOC', 'BANKSL', 'BARNAC', 'BBC', 'BBP', 'BBT', 'BCC', 'BCHINL',
                  'BCKROC', 'BCL', 'BCLFBY', 'BCLTAL', 'BCP', 'BCTRAP', 'BDP', 'BEAR2C', 'BEAR3C', 'BEAR5C', 'BEAV2C',
                  'BEAV3C', 'BEAV3P', 'BEECEF', 'BEECHC', 'BERRYC', 'BGM', 'BGMTAL', 'BHL', 'BIDDLP', 'BIG1C', 'BIG3C',
                  'BIGC', 'BIGMEC', 'BIGWSP', 'BIRCHC', 'BIRCHE', 'BIRCHW', 'BLDRCK', 'BLDRNF', 'BLDRSF', 'BLKBAS',
                  'BLNDSL', 'BLUEC', 'BMT', 'BO1', 'BO1BYP', 'BO1GWL', 'BO2', 'BO2BCC', 'BO2BYP', 'BO2GAT', 'BO2GWL', 
                  'BO2ORI', 'BO2RRR', 'BO3', 'BO4', 'BON', 'BONAFF', 'BONAPC', 'BONH', 'BONLD1', 'BONLD2', 'BONLD3',
                  'BONMRT', 'BONP', 'BOSTCC', 'BPC', 'BR0', 'BR1', 'BR2', 'BR3', 'BRDG2C', 'BRDG3C', 'BREITR', 'BRENNC',
                  'BRIDGC', 'BRIDWB', 'BRUS2C', 'BSH', 'BSHPCK', 'BUCK3C', 'BUCKAP', 'BUCKC', 'BUCKHC', 'BUCKSL', 'BUMPR', 
                  'BURL', 'BUTCHC', 'BUTCHP', 'BVC', 'BVJ', 'BVP', 'BVRLYI', 'BVT', 'BVX', 'BWL', 'CABLNF', 'CAL', 'CALAPR', 
                  'CAMP2C', 'CAMPC', 'CANDLC', 'CANY3C', 'CANY4C', 'CAP', 'CARP', 'CARS', 'CASC', 'CASS', 'CBD', 'CBDFBY',
                  'CBL', 'CBLAIS', 'CBS', 'CCC', 'CCM', 'CCS', 'CCT', 'CDP', 'CEDARC', 'CFCT', 'CFD', 'CFDNSC', 'CFDSF2', 
                  'CFDSF3', 'CFDTAL', 'CFF', 'CFJ', 'CGJ', 'CGR', 'CGRTAL', 'CHANDL', 'CHEL', 'CHELAR', 'CHEWEC', 'CHEWUP', 
                  'CHEWUR', 'CHFARF', 'CHIKAC', 'CHINOR', 'CHIP', 'CHIW', 'CHIWAC', 'CHIWAR', 'CHIWAT', 'CHJO', 'CHL', 'CHM',
                  'CHP', 'CHU', 'CHUMSC', 'CHW', 'CIC', 'CISPUR', 'CJP', 'CLARFP', 'CLCKMS', 'CLE', 'CLEA2C', 'CLEA3C', 'CLEARP',
                  'CLEBYP', 'CLEE', 'CLEFBY', 'CLELMD', 'CLELMR', 'CLETAL', 'CLP', 'CLRBR', 'COLR1', 'COLR2', 'COLR3', 'COLR4', 
                  'COLR5', 'COLR6', 'COLR7', 'COLR8', 'COLR9', 'COLVLE', 'COONSC', 'COP', 'COPPEC', 'COTN2C', 'COTTWC', 
                  'COULTC', 'COULTP', 'COWEAP', 'COWEER', 'COWICC', 'COWISF', 'COWLR1', 'COWLR2', 'COWS', 'COWT', 'COYO2C',
                  'COYOTC', 'CR1', 'CR2', 'CR3', 'CRABP', 'CRESCB', 'CRESIS', 'CROOK1', 'CROWC', 'CRTRAP', 'CRU', 'CRW', 
                  'CUBC', 'CUNNSL', 'CWP', 'DAVISC', 'DAYP', 'DBH', 'DBO', 'DEAD2C', 'DEAD3C', 'DEADNF', 'DEEPCK', 'DESCH1',
                  'DESCH2', 'DET', 'DETFBY', 'DETTAL', 'DEX', 'DEXTAL', 'DHORNC', 'DILACC', 'DOGRVR', 'DRANOL', 'DRIFTC', 
                  'DRM', 'DRNP', 'DRP', 'DRY', 'DRY3C', 'DRYC', 'DRYFBY', 'DRYFCC', 'DRYP', 'DRYTAL', 'DSF', 'DUNCAN', 'DXP', 
                  'EAG2NF', 'EAGH', 'EAGL2C', 'EARLWC', 'EASTOP', 'EBNK', 'EBO', 'ECH', 'ECL', 'EDEERC', 'EFD', 'EFHORC', 
                  'EHL', 'EIGH2C', 'EIGHTC', 'ELRH', 'EMC', 'ENA', 'ENF', 'ENL', 'ENM', 'ENS', 'ENTH', 'ENTIAR', 'ENTRTP', 
                  'ESANIS', 'ESJ', 'ESX', 'ETIENC', 'EWC', 'FAL', 'FALL2C', 'FALTAL', 'FDC', 'FDD', 'FDDFBY', 'FDDTAL',
                  'FEEDCN', 'FIRSTC', 'FIVE2C', 'FOGDEW', 'FOL', 'FOS', 'FOSFBY', 'FOSTAL', 'FOSTC', 'FOUNDI', 'FOXC', 
                  'FST', 'GABLEC', 'GATEC', 'GEECR', 'GERMC', 'GL2', 'GLC', 'GOA', 'GOATWP', 'GOJ', 'GOLD2C', 'GOLD3C', 
                  'GOLD4C', 'GOOS2I', 'GOOSEI', 'GRAYH', 'GRAYSR', 'GRBLDC', 'GREENR', 'GREEPC', 'GRJ', 'GRX', 'GWP', 
                  'HADENC', 'HADESF', 'HALLC', 'HANGC', 'HANSPC', 'HANTWN', 'HANWTB', 'HATRCK', 'HCR', 'HCRFBY', 'HCRREG',
                  'HCRTUR', 'HERMAC', 'HINDOC', 'HLK', 'HLKTAL', 'HLX', 'HN1', 'HN2', 'HN3', 'HOODEF', 'HOODMF', 'HOODR', 
                  'HOODWF', 'HORS2C', 'HRM', 'HS1', 'HS2', 'HSL', 'HSM', 'HST', 'HSU', 'I-90B', 'ICH', 'ICICLC', 'ICL', 
                  'ICM', 'ICTRAP', 'ICU', 'IHA', 'IHR', 'IHRBYP', 'IHRCOL', 'IHRSPF', 'IHRSPL', 'IHRTAL', 'IHRTRB', 'IMQP',
                  'INDI2C', 'INDINC', 'INKANC', 'IRON2C', 'IRRI', 'IS18', 'JACK2C', 'JACKC', 'JACKCP', 'JCJ', 'JD1', 'JDA',
                  'JDABYP', 'JDACOL', 'JDAFBY', 'JDAGAT', 'JDAGWL', 'JDALD1', 'JDALD2', 'JDAMRT', 'JDAR1', 'JDAR2', 'JDARMF', 
                  'JDARNF', 'JDARRR', 'JDARSF', 'JDATAL', 'JDJ', 'JDM', 'JNSCAS', 'JO1', 'JO2', 'JOH', 'JOHN2C', 'JPT', 'JSFBC',
                  'JSFDC', 'JSFMC', 'JSFWC', 'JUNGLC', 'KALA', 'KALAMR', 'KCB', 'KETTLR', 'KLICKR', 'KLIH', 'KLR', 'LAKE2C', 
                  'LAKE3C', 'LAKEBR', 'LASTC', 'LAURLK', 'LBC', 'LBLDRC', 'LBRIC', 'LBT', 'LD1', 'LD2', 'LD3', 'LD4', 'LEA', 
                  'LEAB', 'LEABYP', 'LEAFBY', 'LEATAL', 'LEAV', 'LEB', 'LEBFBY', 'LEBLDS', 'LEL', 'LEMONC', 'LENLKE', 'LEWCLK', 
                  'LEWH', 'LEWIEF', 'LEWISP', 'LEWISR', 'LFF', 'LGR', 'LGRBPS', 'LGRBYP', 'LGRCOL', 'LGRDTG', 'LGRDWT', 'LGRFBY', 
                  'LGRGAT', 'LGRGWL', 'LGRICE', 'LGRMRT', 'LGROFL', 'LGRORI', 'LGRRBR', 'LGRRRR', 'LGRRTR', 'LGRRXR', 'LGRSEP', 
                  'LGRSPL', 'LGRSTS', 'LGRTAL', 'LGRTRB', 'LGS', 'LGSBPS', 'LGSBYP', 'LGSCOL', 'LGSDTG', 'LGSDWT', 'LGSFBY', 
                  'LGSGAT', 'LGSGWL', 'LGSICE', 'LGSMRT', 'LGSOFL', 'LGSORI', 'LGSRBR', 'LGSRRR', 'LGSRTR', 'LGSRXR', 'LGSSEP',
                  'LGSSPL', 'LGSSTS', 'LGSTAL', 'LGSTRB', 'LHC', 'LIBBYC', 'LICK3C', 'LINEC', 'LITTLC', 'LKLICR', 'LKR', 'LLC',
                  'LMA', 'LMC', 'LMEMIS', 'LMILIS', 'LMJ', 'LMN', 'LMNBPS', 'LMNBYP', 'LMNCOL', 'LMNDTG', 'LMNDWT', 'LMNFBY',
                  'LMNGAT', 'LMNGWL', 'LMNICE', 'LMNMRT', 'LMNOFL', 'LMNORI', 'LMNRBR', 'LMNRRR', 'LMNRTR', 'LMNRXR', 'LMNSEP', 
                  'LMNSPL', 'LMNSTS', 'LMNTAL', 'LMNTRB', 'LMONIS', 'LMR', 'LMT', 'LNF', 'LNR', 'LNSANR', 'LOGCC', 'LOGYC', 
                  'LONERC', 'LOONYC', 'LOP', 'LOPTAL', 'LOR', 'LOSTC', 'LOSTR', 'LOUIEC', 'LOUPLC', 'LRSNKC', 'LTNACR', 'LTNANF',
                  'LTP', 'LUNION', 'LWBEAR', 'LWCEDR', 'LWD', 'LWDTAL', 'LWE', 'LWENAT', 'LWISSQ', 'LWL', 'LWN', 'LWSALR', 
                  'LWSCCL', 'LWSCFC', 'LWSCMC', 'LWSCML', 'LWSH', 'LYFE', 'LYLFAT', 'M3R', 'MAD', 'MADRVR', 'MANASC', 'MANCRS',
                  'MARI', 'MARIOC', 'MARION', 'MASON', 'MC1', 'MC2', 'MCD', 'MCI', 'MCITAL', 'MCJ', 'MCKA2C', 'MCKAYC', 'MCKE', 
                  'MCKER', 'MCKESF', 'MCL', 'MCN', 'MCNBPS', 'MCNBYP', 'MCNCOL', 'MCNDTG', 'MCNDWT', 'MCNFBY', 'MCNGAT', 'MCNGWL', 
                  'MCNICE', 'MCNLD1', 'MCNMRT', 'MCNOFL', 'MCNORI', 'MCNRBR', 'MCNRRR', 'MCNRTR', 'MCNRXR', 'MCNSEP', 'MCNSPL',
                  'MCNSRR', 'MCNSTS', 'MCNTAL', 'MCNTRB', 'MCX', 'MDR', 'MDSH', 'MDVAP', 'MEACHC', 'MEACHE', 'MEACHN', 'MEG',
                  'MEGC', 'MERH', 'METH', 'METHR', 'METOLR', 'METRO', 'METTRP', 'MFD', 'MFDFTF', 'MFDLBN', 'MFDLBS', 'MFDSEP', 
                  'MFLBC', 'MHB', 'MHP', 'MHPFBY', 'MHPTAL', 'MILL3C', 'MILL4C', 'MILLC', 'MINP', 'MIS2EF', 'MISS2C', 'MISSNC', 
                  'MJ1', 'MLAK2C', 'MLRSNI', 'MOLALR', 'MONT', 'MOONSC', 'MOSESC', 'MRB', 'MRC', 'MRT', 'MRW', 'MSANTR', 'MSB', 
                  'MSC', 'MSH', 'MSKF', 'MSLA2C', 'MTD', 'MUDDYP', 'MUDDYR', 'MULTCH', 'MURDSF', 'MVF', 'MVFLAP', 'MVP', 'MWC',
                  'MWE', 'MWF', 'MXWLCN', 'NAF', 'NAL', 'NAPEEC', 'NAS', 'NASONC', 'NATCHR', 'NAU', 'NBA', 'NBG', 'NBGFBY',
                  'NBGTAL', 'NEALC', 'NES', 'NESPR', 'NFCHEW', 'NFLBC', 'NFT', 'NFTEAN', 'NFW', 'NILEC', 'NMC', 'NNANAC',
                  'NSANTR', 'NSB', 'NSCAPC', 'NSM', 'NSS', 'OAKC', 'OASP', 'OBF', 'OCHOCC', 'OKANR', 'OKC', 'OKI', 'OKL', 
                  'OKP', 'OKRCRY', 'OKS', 'OKV', 'OKW', 'OMAKC', 'OMAKP', 'OMF', 'OMK', 'OMP', 'ONAF', 'ONC', 'ONIONC', 'ORB',
                  'OSOYBR', 'OSOYHA', 'OSOYOL', 'OXBH', 'PANT2C', 'PARK', 'PAT', 'PATITC', 'PD1', 'PD2', 'PD7', 'PEARSC', 'PEL',
                  'PELTON', 'PENP', 'PER', 'PERAFT', 'PERTAL', 'PES', 'PESHAR', 'PEU', 'PINEC', 'PINHC', 'POTATC', 'POTHOL', 
                  'PRA', 'PRD', 'PRDFBY', 'PRDGWL', 'PRDH', 'PRDLD1', 'PRDMRT', 'PRDTAL', 'PRH', 'PRJ', 'PRO', 'PROFBY', 'PROH',
                  'PROSRD', 'PROSRR', 'PROTAL', 'PROTRP', 'PRV', 'PWD', 'PWDFBY', 'QRTZC', 'RAMSYC', 'RATTLC', 'RBF', 'RCJ', 
                  'RCK5RF', 'RCL', 'RCS', 'RCT', 'RCX', 'REECEC', 'RFP', 'RGRSPC', 'RI2BYP', 'RIA', 'RICEIS', 'RICHIS', 'RINH',
                  'RIS', 'RISFWC', 'RISMRT', 'RISTAL', 'RIVERP', 'ROARC', 'ROBU', 'ROCK2C', 'ROCK3C', 'ROCK4C', 'ROCK5C', 
                  'ROCKC', 'ROCKIS', 'ROLFIP', 'ROSAD', 'ROU', 'ROUFTF', 'ROUTAL', 'ROZ', 'ROZBYP', 'ROZFBY', 'ROZLDR', 
                  'ROZTAL', 'RRE', 'RREBYP', 'RRERRR', 'RRETAL', 'RRF', 'RRJ', 'RSB', 'RSH', 'RSNAKC', 'RSNANF', 'RSSNIS', 
                  'RVP', 'RYANC', 'RZF', 'SA0', 'SA1', 'SALMOC', 'SANPR', 'SANPWF', 'SANTIR', 'SATUSC', 'SCAPPB', 'SCAPPC', 
                  'SCHAFC', 'SCL', 'SCP', 'SERH', 'SFCHEW', 'SFL', 'SFLBC', 'SGOLDC', 'SGP', 'SHATFD', 'SHERFT', 'SHIMC', 
                  'SHINGC', 'SHK', 'SHP', 'SHPFBY', 'SHPTAL', 'SHTIKC', 'SHUTTC', 'SILVEC', 'SIMCNF', 'SIMCOC', 'SIMILP', 
                  'SIMILR', 'SIMP', 'SIP', 'SJ1', 'SJ2', 'SKA', 'SKAHAL', 'SKAM', 'SKATAL', 'SKIPAR', 'SKIPAW', 'SKOKNF',
                  'SNAKE1', 'SNAKE2', 'SNANAC', 'SND', 'SNDTAP', 'SNYDEC', 'SPOKR1', 'SPOKR2', 'SPRC', 'SQAW3C', 'SQAW4C',
                  'SQAWC', 'SR1', 'SR2', 'SSANTR', 'SSC', 'SSD', 'SSDSRR', 'SSDTAL', 'SSIDEC', 'SSIDES', 'SSJ', 'STAFFC',
                  'STAPAC', 'STFH', 'STMARP', 'STOR2C', 'SUC', 'SUJ', 'SUL', 'SULBPS', 'SULBYP', 'SULFBY', 'SULFDS',
                  'SULSEP', 'SULTRB', 'SUMI2C', 'SUMI3C', 'SWALEC', 'SWAUKC', 'SWC', 'SWK', 'TAN', 'TANESF', 'TANEUC', 
                  'TANNEC', 'TC4', 'TCM', 'TCT', 'TD1', 'TD2', 'TDA', 'TDAICE', 'TDAMRT', 'TDASPF', 'TDASPL', 'TDASPT', 
                  'TDATAL', 'TDATRB', 'TEANAR', 'TEANMF', 'TEANWF', 'TENNAI', 'TEPEEC', 'TEPEEF', 'TEXC', 'THOMC', 'THOP',
                  'TIETNR', 'TILLIC', 'TLT', 'TMA', 'TMF', 'TMFFBY', 'TMFTAL', 'TMJ', 'TNK', 'TON', 'TONASC', 'TOP', 
                  'TOPPEC', 'TOUCHR', 'TOULOU', 'TOUT', 'TOUTLR', 'TOUTNF', 'TR1', 'TR2', 'TRC', 'TRINR', 'TRONC', 'TROU2C', 
                  'TROUTC', 'TRYOC', 'TSHIMC', 'TSUGCR', 'TUF', 'TUM', 'TUMFBY', 'TUNKC', 'TURNCR', 'TURO', 'TUTUIC', 
                  'TWIS2P', 'TWISPP', 'TWISPR', 'TWISPW', 'TWITRP', 'TWR', 'TWX', 'TY1', 'TY2', 'TY3', 'TY4', 'UCT', 
                  'UKNWRC', 'UM1', 'UM2', 'UMAH', 'UMAR', 'UMATNF', 'UMATSF', 'UMC', 'UMF', 'UMPQAR', 'UMR', 'UMT', 
                  'UMTANC', 'UMW', 'UNT', 'UPT', 'URT', 'UWE', 'UWH', 'VASEUX', 'VDS3', 'VGISNB', 'VIENTC', 'VNGRC', 
                  'WAHA', 'WAJ', 'WALLAR', 'WALLIS', 'WALLNF', 'WALLSF', 'WAN', 'WANACC', 'WANBYP', 'WANFBY', 'WANGWL', 
                  'WANLD1', 'WANLD2', 'WANMRT', 'WANSPL', 'WANTAL', 'WANTRB', 'WAP', 'WAPATC', 'WAPATD', 'WAPATS', 'WAPSRR', 
                  'WAPTAL', 'WARMSR', 'WASHOR', 'WASHWF', 'WE1', 'WE2', 'WEA', 'WEH', 'WEJ', 'WEL', 'WELFBY', 'WELH', 
                  'WELLD1', 'WELLD2', 'WELSBR', 'WELTAL', 'WENA2T', 'WENA3T', 'WENA4T', 'WENASC', 'WENATL', 'WENATR', 
                  'WENATT', 'WFC', 'WFF', 'WHC', 'WHEELC', 'WHITEC', 'WHITER', 'WHITPC', 'WHITSR', 'WHITWF', 'WHS', 'WHSH',
                  'WIDOWC', 'WILD2C', 'WILDSC', 'WILH', 'WILL', 'WILL2C', 'WILLR1', 'WILLR2', 'WILLR3', 'WILMOC', 'WILPB', 
                  'WILRMF', 'WILSN2', 'WILSNC', 'WIND2R', 'WINT', 'WINTBC', 'WL1', 'WL2', 'WLT', 'WOLFC', 'WOODIS', 'WOPTXD',
                  'WPJ', 'WPT', 'WPX', 'WPXTAL', 'WRNFMF', 'WRP', 'WRU', 'WSH', 'WSPH', 'WSR', 'WTL', 'WW1', 'WW2', 'WYCHUC',
                  'WZDF', 'Y1J', 'YAKIM1', 'YAKIM2', 'YELHKC', 'YHC', 'YOUNGB', 'ZSL') ~ 'Below_GRA_Obs',
    TRUE ~ Node
    ) # case_when
  ) %>% # mutate
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
config_filepath <- './data/ConfigurationFiles/my_config_20190125.csv'
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

# May no longer be needed
# site_df = site_df %>%
#   filter(!SiteID %in% c('TFH',
#                         'MCCA',
#                         'WIMPYC',
#                         'YANKFK', 'CEY',
#                         'SAWT',
#                         'LOOH',
#                         'CARMEC',
#                         'BIG2C',
#                         'RPDTRP'))

#View(site_df)
# save output

#--------------------------
# Double check SiteId names
#--------------------------
# not_in_sitedf <- anti_join(site_df, my_config, by = 'SiteID')
# not_in_myconfig <- anti_join(my_config, site_df, by = 'SiteID')
# write.csv(not_in_myconfig, file = './not_in_myconfig.csv')


write.csv(site_df, file = paste0('./data/ConfigurationFiles/site_df_',timestp,'.csv'),
          row.names = FALSE)

# load site_df if maintained externally
sitedf_filepath <- './data/ConfigurationFiles/site_df_20190125.csv'
site_df <- read_csv(sitedf_filepath, col_types = cols(.default = 'c'))

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

write.csv(parent_child, 
     file = paste0('./data/ConfigurationFiles/parent_child_',timestp,'.csv'),
     row.names = FALSE)

# Load parent_child table if maintained externally
parentchild_filepath <- './data/ConfigurationFiles/parent_child_20180207.csv'
parent_child <- read_csv(parentchild_filepath)

#------------------------------------------------------------------------------
# # Plot nodes
#------------------------------------------------------------------------------
nodes <- parent_child %>%
  gather('loc_type','SiteID', ParentNode, ChildNode) %>%
  distinct(SiteID) %>%
  rowid_to_column('id')

edges <- parent_child %>%
  left_join(nodes, by = c('ParentNode' = 'SiteID')) %>%
  rename(from = id) %>%
  left_join(nodes, by = c('ChildNode' = 'SiteID')) %>%
  rename(to = id)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(routes_tidy, layout = 'tree', , circular = TRUE) +   #, layout = 'tree', circular = TRUE
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = SiteID)) + #, repel = TRUE
  theme_graph()

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
  as_tibble()

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
# Then summarize for final spawn location and IDFG genetic work.
#------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(PITcleanr)

spp <- 'Steelhead'
yr <- 2018
timestp <- '20190125' # for file path

#------------------------------------------------------------------------------
# Load configuration, parent_child and processed datasets from PITcleanr 
#------------------------------------------------------------------------------
load(paste0('data/PreppedData/LGR_', spp, '_', yr,'_', timestp,'.rda'))


proc_ch = read_delim(paste0('./data/CleanedData/ProcCapHist_EDITTED_', spp, '_', yr,'_',timestp,'.txt'),
                     delim = '\t') %>%
   mutate(TrapDate = mdy_hms(TrapDate),
        ObsDate = mdy_hms(ObsDate),
        lastObsDate = mdy_hms(lastObsDate)) %>%
   filter(ModelObs)

call_diff <- proc_ch %>%
  mutate(call_diff = case_when(
    AutoProcStatus != UserProcStatus ~ 1,
    TRUE ~ 0)) %>%
  group_by(TagID) %>%
  mutate(error = ifelse(sum(call_diff)>0,'error','ok'))%>%
  filter(error == 'error')

n_distinct(call_diff$TagID) # missed 25 (includes AB_nodes and downstream
# mainstem xxxx_Other.)

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
  group_by(Group, SiteID, Node)# %>%
#  summarise(n = n_distinct(TagID))

valid_paths <- getValidPaths(proc_list$parent_child, 'GRA')
node_order <- createNodeOrder(valid_paths, proc_list$my_config, site_df, step_num = 3)
eff <- nodeEfficiency(proc_ch, node_order, direction = 'upstream')


#------------------------------------------------------------------------------
# Biological Summaries for IDFG and Life History
# assigns spawn location, last observation date and filters tag obs for only those
# tags used in the DABOM model.
#------------------------------------------------------------------------------

lifehistory_summ = summariseTagData(capHist_proc = proc_ch,
                                    trap_data = proc_list$ValidTrapData) %>%
  mutate(equal_robots = ifelse(PtagisEventLastSpawnSite == AssignSpawnSite, TRUE,FALSE),
         equal_robots = ifelse(AssignSpawnNode == 'GRA', TRUE, equal_robots),
         equal_robots = ifelse(is.na(equal_robots), FALSE, equal_robots))

#------------------------------------------------------------------------------
# what is the agreement with IDFG
#------------------------------------------------------------------------------

# proportion of final spawn sites that disagree
length(which(!lifehistory_summ$equal_robots))/length(lifehistory_summ$equal_robots)

# number of final spawn sites that disagree
length(which(!lifehistory_summ$equal_robots))

# save file for John Powell
write.csv(lifehistory_summ, 
          file = paste0('./data/LifeHistoryData/lifehistory_summ_', spp, '_', yr,'_',timestp,'.csv'))

not_equal <- lifehistory_summ %>%
  filter(!equal_robots) %>%
  select(TagID:TagPath, contains('Ptagis'))



