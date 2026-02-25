# RST data for microchem 
# Feb 2026



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")
options(scipen=9999)


# Load biodata -----------------
rst.biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling", guess_max=3000) %>%
  filter(grepl("RST|IPT", gear, ignore.case=T), !grepl("recap", comments, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv")) %>%
              mutate_at("statWeek", as.character)) %>%
  mutate(species_stage_simple = case_when(grepl("coho", species, ignore.case=T) ~ stringr::str_to_title(paste0(species, " ", life_stage)),
                                          grepl("chum", species, ignore.case=T) ~ "Chum",
                                          grepl("chinook", species, ignore.case=T) ~ stringr::str_to_title(paste0(ad_clip, " ", species)),
                                          TRUE ~ species),
         size_bin = case_when(fork_length_mm >= 50 ~ "overincl_50",
                              fork_length_mm < 50 ~ "under_50")) %>%
  print()




# Load catch data -----------------
setTotals <-  readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                   pattern = "^R_OUT - San Juan PSSI master database",
                                                   full.names = T),
                                 sheet = "set_totals") %>% 
  filter(grepl("rst|ipt", gear, ignore.case=T), species=="Chinook", clip_status=="unclipped") %>%
  janitor::clean_names() 




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ===================== Proprotion of fork lengths of outmigrants >50mm and <50mm =====================

## Summarize by day and apply % by size bin to unsampled catch ----------------- 
cn_expanded <- left_join(setTotals %>% 
                           group_by(date) %>%
                           summarize(total_caught_excl_recaps = sum(total_caught_excl_recaps, na.rm=T),
                                     morts = sum(number_morts, na.rm=T),
                                     #lethals = sum(number_lethals, na.rm=T),
                                     released_unsampled1 = sum(number_tallied_released_alive_dyed, na.rm=T),
                                     released_unsampled2 = sum(number_tallied_released_alive_undyed, na.rm=T),
                                     sampled1 = sum(number_biosampled_released_alive_dyed, na.rm=T),
                                     sampled2 = sum(number_biosampled_released_alive_undyed, na.rm=T)) %>%
                           mutate(total_live = total_caught_excl_recaps-morts,
                                  unsampled = released_unsampled1 + released_unsampled2,
                                  sampled = sampled1 + sampled2) %>%
                           select(-c(total_caught_excl_recaps, morts,  released_unsampled1, released_unsampled2, sampled1, sampled2)) %>%
                           relocate(sampled, .after = total_live),
                         
                         rst.biodat.fish %>%
                           filter(species=="Chinook", !is.na(size_bin), hatchery_origin!="Hatchery") %>%
                           group_by(date, size_bin) %>%
                           summarize(n=n()) %>% 
                           group_by(date) %>%
                           mutate(total=sum(n),
                                  propn=n/total) %>%
                           select(-c(total)) %>%
                           pivot_wider(names_from = size_bin, values_from = c(propn, n)) %>%
                           mutate(across(c(propn_under_50:n_overincl_50), ~case_when(is.na(.) ~ 0, 
                                                                                     TRUE ~ .))) %>%
                           relocate(n_under_50, .before = propn_under_50) %>%
                           relocate(n_overincl_50, .before = propn_overincl_50),
                         
                         
                         by="date") %>%
  mutate(expanded_n_under_50 = case_when(unsampled>0 ~ (unsampled*propn_under_50) + n_under_50,
                                         TRUE ~ n_under_50),
         expanded_n_overincl_50 = case_when(unsampled>0 ~ (unsampled*propn_overincl_50) + n_overincl_50,
                                            TRUE ~ n_overincl_50),
         check = (round(expanded_n_under_50,0) + round(expanded_n_overincl_50,0))==total_live)  



# Re-calculate % by size bin (expanded) ----------------- 
cn_expanded %>%
  select(date, expanded_n_under_50, expanded_n_overincl_50) %>%
  mutate(year = lubridate::year(date)) %>%
  pivot_longer(c(expanded_n_under_50, expanded_n_overincl_50), names_to = "size_bin", values_to = "n") %>%
  group_by(year, size_bin) %>%
  summarize(n = sum(n, na.rm=T)) %>%
  group_by(year) %>%
  mutate(total = sum(n),
         propn = n/total)



