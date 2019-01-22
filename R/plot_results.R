# Create figures for NOSA presentation.

library(tidyverse)
library(lubridate)
library(DABOM)
library(RColorBrewer)

# Package testing
spp <- 'Steelhead'
yr <- '2017'

load(file = paste0('./DABOM_results/LGR_DABOM_Bio_Steelhead_2017_20180213.rda'))
load(file = paste0('./STADEM_results/LGR_STADEM_',spp,'_',yr,'.rda'))


trib_summ = calcTribEscape_LGD(dabom_mod,
                                   stadem_mod,
                                   stadem_param_nm = 'X.new.wild',
                                   bootstrap_samp = 2000, #2000
                                   node_order = proc_list$NodeOrder,
                                   summ_results = T,
                                   pt_est_nm = 'median',
                                   cred_int_prob = 0.95)

pop_df <- definePopulations(spp,
                               node_order = proc_list$NodeOrder) %>%
  rename(ReportGrp = TRT_POPID)

pop_summ = calcRepGrpEscape_LGD(dabom_mod = dabom_mod,
                                         stadem_mod = stadem_mod,
                                         report_grp = pop_df,
                                         node_order = proc_list$NodeOrder,
                                         pt_est_nm = 'median',
                                         spp = spp)
  
#------------------------------------------------------------------------------
# Load and work with DABOM Stuff
#------------------------------------------------------------------------------

load(file = paste0('DABOM_estimates/LGR_PIT_estimates20180216.rda'))

pop_node <- read_csv('./Document/Presentation/pop_node.csv')


pop_node <- pop_node %>%
  left_join(proc_list$NodeOrder %>%
              select(Node, NodeOrder))

# pop_node <- proc_list$NodeOrder %>%
#   filter(NodeOrder == 2) %>%
#   select(Group,SiteID = NodeSite) %>%
#   right_join(proc_list$NodeOrder %>%
#                select(Group, Node), by = 'Group') %>%
#   left_join(definePopulations(spp, proc_list$NodeOrder) %>%
#               select(-Group),
#             by = 'SiteID') %>%
#   left_join(pop_df %>%
#               filter(Species == spp) %>%
#               select(ESU_DPS:POP_NAME), by = 'TRT_POPID')

detect_summ <- detect_summ %>%
  left_join(pop_node %>%
              select(Node, TRT_POPID, ESU_DPS:NodeOrder), by = 'Node')


tmp <- detect_summ %>%
  filter(!is.na(Group)) %>%
  group_by(species, spawn_yr, Group, BranchNum) %>%
  nest(Node, NodeOrder, n_tags)

ugr <- tmp$data[2][[1]]

y <- up_obs(ugr)

up_obs <- function(x){
  
  x$up_tags <- rep(NA, dim(x)[1])
  
  for(i in 1:dim(x)[1]){
    node_ <- x$Node[i] # change row to i
    nodeord_ <- x$NodeOrder[i] # change row to i

    tmp <- x %>%
    filter(NodeOrder > nodeord_) %>%
    summarise(up_tags = sum(n_tags)) %>%
    pull(up_tags)
    
    x$up_tags[i] <- tmp
    
  }
  
  return(x)
}
  


spp <- 'Steelhead'
yr <- '2017'

b_sample <- wk_trans_summ %>%
  ungroup() %>%
  select(Group, BranchNum) %>%
  distinct(Group, .keep_all = TRUE)

big_b <- c(8, 9, 11, 13, 15, 19, 21, 26)
big_b <- c(5, 9, 11, 13, 15, 26)

n <- length(big_b)
my.cols <- brewer.pal(n, 'Dark2')
#my.cols[1] <- '#000000'
#sample(1:n_distinct(wk_trans_summ$BranchNum), 10)



tran_probs <- wk_trans_summ %>%
  filter(species == spp) %>%
  filter(BranchNum %in% big_b) %>%
  mutate(Group = as.character(Group),
         Group = ifelse(is.na(Group), 'Black-box', Group),
         Area = ifelse(is.na(Area), 'Black-box', Area)) %>%
  ggplot(aes(x = week, y = estimate, group = Group)) +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill = Group), alpha = .2) +
  geom_line(aes(colour = Group)) +
  scale_color_manual(values = my.cols) +
  scale_fill_manual(values = my.cols) +
  #scale_color_brewer(palette = 'Dark2') +
  #scale_fill_brewer(palette = 'Dark2') +
  facet_wrap(~ spawn_yr) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Trap Week',
       y = 'Transition Probability',
       colour = 'Main Branch',
       fill = 'Main Branch')

ggsave(file = './Document/Presentation/tran_probs.png', tran_probs,units = 'in', width = 10, height = 6)




  
  
report_summ_tmp <- report_summ %>%
  left_join(pop_df %>%
              select(MPG, TRT_POPID, POP_NAME, Population), by = 'TRT_POPID')


fact_ord <- report_summ_tmp %>%
  filter(species == spp) %>%
  group_by(POP_NAME) %>%
  summarise(mu = mean(estimate)) %>%
  arrange(desc(mu)) %>%
  pull(POP_NAME)


report_summ_tmp %>%
  filter(species == spp) %>%
  filter(Population) %>%
  mutate(POP_NAME = factor(POP_NAME, levels = fact_ord)) %>%
  ggplot(aes(x = POP_NAME, y = estimate)) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI, colour = as.factor(spawn_yr)), position = position_dodge(width = .2)) +
  geom_point(aes(colour = as.factor(spawn_yr)), position = position_dodge(width = .2)) +
  geom_text(aes(label = round(cv,2)), nudge_x = .3) +
  scale_colour_brewer(palette = 'Dark2') +
  #facet_wrap(~MPG, scales = 'free_y') +
  theme_bw() +
  coord_flip() +
  theme(legend.position = 'bottom') +
  labs(x = '',
       y = 'Natural Origin Spawner Abundance',
       colour = 'Spawn Year')

ggsave(file = './Document/Presentation/grp_abund.png', grp_abund ,units = 'in', width = 10, height = 6)


fact_ord <- detect_summ %>%
  filter(species == spp) %>%
  filter(spawn_yr == yr) %>%
  group_by(TRT_POPID, Node) %>%
  summarise(mu = mean(estimate)) %>%
  arrange(TRT_POPID, desc(mu)) %>%
  pull(Node)


detect_probs <- detect_summ %>%
  filter(species == spp) %>%
  filter(spawn_yr == yr) %>%
  filter(TRT_POPID %in% c('IRMAI-s', 'SRUMA-s', 'SNASO-s')) %>%
  #filter(n_tags >= 1) %>%
  filter(Node != 'GRA') %>%
  #filter(estimate != 1) %>%
  mutate(Node = factor(Node, levels = fact_ord)) %>%
  ggplot(aes(x = Node, y = estimate)) +
  geom_point(colour = 'darkblue', position = position_dodge(width = .2)) +
  geom_errorbar(colour = 'darkblue', aes(ymin = lowerCI, ymax = upperCI),position = position_dodge(width = .2)) +
  geom_text(aes(y = lowerCI, label = n_tags), nudge_y = -.04) +
  coord_flip() +
  facet_wrap(~POP_NAME, scales = 'free_y', nrow = 2) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Model Node',
       y = 'Detection Probability')

ggsave(file = './Document/Presentation/detect_probs.png', detect_probs ,units = 'in', width = 10, height = 6)


detect_error <- detect_summ %>%
  filter(Node != 'GRA') %>% 
  filter(n_tags >= 1) %>%
  filter(estimate != 1) %>%
  ggplot(aes(x = n_tags, y = sd)) +
  geom_point(aes(size = n_tags, alpha = n_tags), shape = 21, colour = 'darkblue', fill = 'darkblue') +
  scale_alpha_continuous(range = c(.2,1)) +
  theme_bw() +
  labs(x = 'Observed Tags',
       y = 'Standard Error',
       size = 'Observed Tags',
       alpha = 'Observed Tags') +
  theme(legend.position = c(.78,.73))
  
ggsave(file = './Document/Presentation/detect_error.png', detect_error ,units = 'in', width = 4.9, height = 2.75)



# Now get some life history stuff.

life_hist <- life_hist %>%
  select(-TRT_POPID) %>%
  left_join(pop_node %>%
              select(Node, TRT_POPID, ESU_DPS:NodeOrder), by = c('AssignSpawnNode' = 'Node'))

life_fem <- life_hist %>%
  filter(species == spp) %>%
  group_by(spawn_yr, species, MPG, POP_NAME, TRT_POPID, GenSex) %>%
  summarise(n = n()) %>%
  filter(GenSex %in% c('F', 'M')) %>%
  ungroup()

pop_fem <- life_fem %>%
  filter(TRT_POPID != 'SRLSR-s') %>%
  mutate(POP_NAME = factor(POP_NAME, levels = fact_ord),
         GenSex = factor(GenSex, levels = c('M', 'F'))) %>%
  ggplot(aes(x = POP_NAME, y = n, fill = GenSex), colour = 'black') +
  geom_bar(stat = 'identity', position = 'fill') + 
  #scale_fill_brewer(palette = 'Accent') +
  scale_fill_manual(values = c('darkblue', 'violetred')) +
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.position = 'bottom') +
  labs(x = '',
       y = 'Proportion',
       fill = 'Sex')

ggsave(file = './Document/Presentation/pop_fem.png', pop_fem ,units = 'in', width = 5, height = 6)





life_age <- life_hist %>%
  filter(species == spp) %>%
  select(spawn_yr, species, MPG, POP_NAME, TRT_POPID, BioScaleFinalAge) %>%
  separate(BioScaleFinalAge, into = c('Fresh', 'Ocean'), sep = ':') %>%
  mutate(Fresh = as.numeric(ifelse(Fresh == '?', NA, Fresh)),
         Ocean = as.numeric(ifelse(Ocean == '?', NA, Ocean)),
         age = Fresh + Ocean) %>%
  group_by(spawn_yr, species, MPG, POP_NAME, TRT_POPID, age) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(!is.na(age)) %>%
  group_by(spawn_yr, species, MPG, POP_NAME, TRT_POPID) %>%
  mutate(p = n/sum(n, na.rm = TRUE)) %>%
  ungroup()

life_age <- na.omit(life_age)


fact_ord <- life_age %>%
  filter(TRT_POPID != 'SRLSR-s') %>%
  filter(age <= 4) %>%
  group_by(POP_NAME) %>%
  summarise(mu = mean(p)) %>%
  arrange(desc(mu)) %>%
  pull(POP_NAME)

pop_age <- life_age %>%
  filter(TRT_POPID != 'SRLSR-s') %>%
  mutate(POP_NAME = factor(POP_NAME, levels = fact_ord)) %>%
  mutate(age = fct_rev(factor(age))) %>%
ggplot(aes(x = POP_NAME, y = n, fill = age), colour = 'black') +
  geom_bar(stat = 'identity', position = 'fill') + 
  scale_fill_brewer(palette = 'Dark2') +
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.position = 'bottom') +
  labs(x = '',
       y = 'Proportion',
       fill = 'Total Age')

ggsave(file = './Document/Presentation/pop_age.png', pop_age ,units = 'in', width = 5, height = 6)


#------------------------------------------------------------------------------
# Load and work with STADEM Stuff
#------------------------------------------------------------------------------
# set species and spawn year
species = c('Steelhead')  # either Chinook or Steelhead
year = 2010:2017        # tagging operations started at Lower Granite with spawn year 2009.


stadem_sum <- as.tibble()
week_dat <- as.tibble()

for(i in 1:length(species)){
  for(j in 1:length(year)){
    
    spp <- species[i]
    yr <- year[j]
    
    load(file = paste0('./STADEM_results/LGR_STADEM_', spp, '_', yr, '.rda'))
    
    #tmp_params <- row.names(stadem_mod$summary)
    
    tmp_sum <- stadem_mod$summary %>%
      as.data.frame() %>%
      mutate(var = rownames(.),
             species = spp,
             spawn_year = yr,
             mod_run = 'new')
    
    stadem_sum <- bind_rows(stadem_sum, tmp_sum)
    
    tmp_week <- stadem_list$weeklyData %>%
      as.data.frame() %>%
      mutate(species = spp,
             spawn_year = yr,
             mod_run = 'new')
    
    week_dat <- bind_rows(week_dat, tmp_week)
    
  }
}

rm(stadem_list, stadem_mod, tmp_sum, tmp_week)

#------------------------------------------------------------------------------
# Organize Data
#------------------------------------------------------------------------------
stadem_sum <- stadem_sum %>%
  mutate(org_group = ifelse(grepl('wild', var), 'Wild',
                            ifelse(grepl('hatch', var), 'Hatchery',
                                   ifelse(grepl('hnc', var), 'Hatchery No-Clip',
                                          ifelse(grepl('all', var), 'Total', 'All')))))


total_df <- stadem_sum %>%
  filter(grepl('X.tot.new.', var)) %>%
  select(species, spawn_year, org_group, var, estimate = `50%` , sd, lower95 = `2.5%`, upper95 = `97.5%`)


#------------------------------------------------------------------------------
# Total Data
#------------------------------------------------------------------------------
total_df %>%
  filter(org_group != 'Total') %>%
  ggplot(aes(x = as.factor(spawn_year), y = estimate, colour = org_group)) +
  geom_point(position = position_dodge(width = .2)) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95),
                position = position_dodge(width = .2)) +
  scale_color_brewer(palette = 'Set1') +
  facet_grid(org_group~species, scales = 'free_y') +
  labs(x = 'Spawn Year',
       y = 'Lower Granite Escapement (95% CI)',
       colour = '') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#------------------------------------------------------------------------------
# Weekly Data
#------------------------------------------------------------------------------
week_est = stadem_sum[grep('^X.all', stadem_sum$var),] %>%
  mutate(mod_week = as.integer(str_extract(var, "[0-9]+")),
         param = str_extract_all(var, "[:alpha:]+", simplify = T)[,3],
         param = ifelse(param == '', 'all', param)) %>%
  left_join(week_dat %>%
              filter(window_open | trap_open) %>%
              group_by(species, spawn_year) %>%
              mutate(mod_week = 1:n())) %>%
  mutate(p_day = day_tags/tot_tags,
         adj_win = win_cnt / p_day,
         adj_win = ifelse(adj_win == Inf, NA, adj_win))

# Plot weekly data
stadem_esc <- week_est %>%
  filter(param == 'all') %>%
  filter(spawn_year >= 2014) %>%
  ggplot(aes(x = Start_Date,
             y = `50%`)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              alpha = 0.2) +
  #geom_line(aes(y = adj_win,
  #              color = 'Window (adj)')) +
  #geom_point(aes(y = adj_win,
  #               color = 'Window (adj)')) +
  geom_line(aes(y = win_cnt,
                color = 'Window (raw)')) +
  geom_point(aes(y = win_cnt,
                 color = 'Window (raw)')) +
  geom_line(aes(y = trap_est,
                color = 'Trap')) +
  geom_point(aes(y = trap_est,
                 color = 'Trap')) +
  geom_line(aes(color = 'Process Model')) +
  geom_point(aes(color = 'Process Model')) +
  scale_color_manual(values = c('Process Model' = 'black',
                                'Window (raw)' = 'blue',
                                'Window (adj)' = 'lightblue',
                                'Trap' = 'red')) +
  scale_x_date(date_labels = format('%b-%d')) +
  #facet_grid(spawn_year ~ species, scales = 'free') +
  facet_wrap(~spawn_year, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Date',
       y = 'Escapement',
       color = 'Source',
       title = 'Estimated total passage for window and trap observation models and the state-process')

ggsave(file = './Document/Presentation/stadem_esc.png', stadem_esc,units = 'in', width = 10, height = 6)
#------------------------------------------------------------------------------
# Passage Rates
#------------------------------------------------------------------------------

rate_est = stadem_sum[grepl('^day.true', stadem_sum$var) | 
                        grepl('^reasc.true', stadem_sum$var),] %>%
  as.data.frame() %>%
  mutate(mod_week = as.integer(str_extract(var, "[0-9]+")),
         param = str_extract_all(var, "[:alpha:]+", simplify = T)[,1]) %>%
  tbl_df() %>%
  select(var, param, mod_week, everything()) %>%
  inner_join(week_dat %>%
               filter(window_open | trap_open) %>%
               group_by(species, spawn_year) %>%
               mutate(mod_week = 1:n()))


pass_rates <- rate_est %>%
  filter(spawn_year >= 2016) %>%
  filter(tot_tags > 0) %>%
  ggplot(aes(x = Start_Date,
             y = `50%`)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`,
                  fill = param),
              alpha = 0.2) +
  geom_line(aes(color = param)) +
  geom_point(aes(y = day_tags / tot_tags,
                 color = 'day')) +
  geom_point(aes(y = reascent_tags / tot_tags,
                 color = 'reasc')) +
  scale_x_date(date_labels = format('%b-%d')) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_color_manual(values = c('darkred', 'darkblue'),
                     name = 'Rate',
                     labels = c('day' = 'Daytime Passage',
                                'reasc' = 'Re-acension')) +
  scale_fill_manual(values = c('darkred', 'darkblue'),
                    name = 'Rate',
                    labels = c('day' = 'Daytime Passage',
                               'reasc' = 'Re-acension')) +
  ylim(c(0,1)) +
  facet_wrap(~ spawn_year, nrow = 2, scales = 'free_x') +
  labs(y = 'Passage Rate',
       x = 'Date')

ggsave(file = './Document/Presentation/pass_rates.png', pass_rates,units = 'in', width = 5, height = 6)

# passage numbers
pass_sum <- stadem_sum %>%
  filter(grepl('X.night', var) | 
           grepl('X.reasc', var) |
           grepl('X.all', var)) %>%
  mutate(mod_week = as.integer(str_extract(var, "[0-9]+")),
         param = str_extract_all(var, "[:alpha:]+", simplify = T)[,2]) %>%
  inner_join(week_dat %>%
               filter(window_open | trap_open) %>%
               group_by(species, spawn_year) %>%
               mutate(mod_week = 1:n()))

pass_sum %>%
  filter(org_group == 'All') %>%
  ggplot(aes(x = Start_Date,
             y = `50%`)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`,
                  colour = param,
                  fill = param),
              alpha = 0.2) +
  geom_line(aes(colour = param)) +
  theme_bw() +
  scale_color_manual(name = 'Rate',
                     labels = c('night' = 'Night-time',
                                'reasc' = 'Re-acension'),
                     values = c('blue', 'red')) +
  scale_fill_manual(name = 'Rate',
                    labels = c('night' = 'Night-time',
                               'reasc' = 'Re-acension'),
                    values = c('blue', 'red')) +
  facet_wrap(~spawn_year, scales = 'free_x') +
  labs(y = 'Estimate',
       x = 'Week',
       title = 'Weekly estimated night-time and re-ascension passage totals') +
  theme_bw() +
  theme(legend.position = 'bottom')


pass_xy <- pass_sum %>%
  filter(spawn_year >= 2016) %>%
  filter(org_group == 'All' | org_group == 'Total') %>%
  select(species, spawn_year, param, `50%`, week_num) %>%
  spread(param, `50%`) %>%
  ggplot(aes(x = night, y = reasc, size = all)) +
  geom_point(colour = 'darkblue') +
  geom_abline(slope = 1, intercept = 0, colour = 'red', linetype = 2) +
  facet_wrap(~spawn_year, nrow = 2) +
  xlim(c(0,750)) +
  ylim(c(0,750)) +
  labs(x = 'Night-time',
       y = 'Re-ascension',
       size = 'Total Passage') +
  theme_bw() +
  theme(legend.position = 'bottom')


ggsave(file = './Document/Presentation/pass_xy.png', pass_xy,units = 'in', width = 5, height = 6)

#------------------------------------------------------------------------------
# Trap Rates
#------------------------------------------------------------------------------
library(scales)
trap_rate <- week_dat %>% 
  filter(spawn_year >= 2015) %>%
  mutate(lowerCI = trap_rate - 1.96*trap_rate_se,
         upperCI = trap_rate + 1.96*trap_rate_se,
         lowerCI = ifelse(lowerCI <= 0, 0, lowerCI)) %>%
  ggplot(aes(x = Start_Date, y = trap_rate)) +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), fill = 'darkblue', alpha = .25) +
  geom_line(colour = 'darkblue') +
  geom_point(colour = 'darkblue') +
  scale_x_date(date_labels = format('%b-%d')) +
  facet_wrap(~spawn_year, scales = 'free_x', nrow = 2) + #
  theme_bw() +
  labs(x = 'Date',
       y = 'Trap Rate',
       title = 'Weekly trap rate estimated from all previously tagged fish ascending the ladder')

ggsave(file = './Document/Presentation/trap_rate.png', trap_rate,units = 'in', width = 10, height = 6)

trap_error <- week_dat %>%
  filter(trap_rate_se != 0) %>%
  ggplot(aes(x = n_poss_tags, y = trap_rate_se)) +
  geom_point(aes(size = win_cnt, alpha = win_cnt), shape = 21, colour = 'darkblue', fill = 'darkblue') +
  scale_alpha_continuous(range = c(.2,1)) +
  theme_bw() +
  labs(x = 'Total Tags',
       y = 'Standard Error',
       size = 'Window Count',
       alpha = 'Window Count') +
  theme(legend.position = c(.8,.7))

ggsave(file = './Document/Presentation/trap_error.png', trap_error,units = 'in', width = 4.75, height = 2.75)



