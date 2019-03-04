siteParentChild <- function(site_df){
  
  tmp <- site_df %>%
    select(SiteID, path, area = Step2, branch = Step3, everything()) %>%
    gather(Step, parent, contains('Step')) %>%
    arrange(SiteID) %>%
      filter(!is.na(parent)) %>%
      group_by(SiteID) %>%
      mutate(child = lead(parent)) %>%
      ungroup() %>%
      filter(!is.na(child)) %>%
      mutate(child = ifelse(grepl('Other', branch), branch, child)) %>%
      mutate(child = ifelse(grepl('AB_', branch), branch, child)) %>%
      select(parent, child) %>%
      distinct()
    
}
