# San Juan RST 
# CPUE/infilling/abundance calculations
# May 2025



# ========================= SET UP =========================

# Load libraries -------------------
library(tidyverse)

# Load helpers ---------------
"%notin%" <- Negate("%in%")


# ========================= LOAD JUVENILE DATA =========================

# Sample events ----------------- 
eventMeta <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                pattern="^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                                sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear)) %>% 
  mutate(datetime_start = lubridate::ymd_hm(paste0(date_start, time_start)),
         datetime_stop = lubridate::ymd_hm(paste0(date_stop, time_stop))) %>% 
  print()



# Environmentals ----------------- 
enviros <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                pattern = "^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                              sheet="enviro") %>% 
  filter(grepl("RST|IPT", usid, ignore.case=T))



# Catch totals ----------------- 
setTotals <-  readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                   pattern = "^R_OUT - San Juan PSSI master database",
                                                   full.names = T),
                                 sheet = "set_totals") %>% 
  filter(grepl("RST|IPT", gear)) %>%
  mutate(life_stage = case_when(grepl("coho", species, ignore.case=T) & life_stage=="fry" ~ "subyearling",
                                grepl("coho", species, ignore.case=T) & life_stage=="smolt" ~ "yearling",
                                TRUE ~ life_stage),
         species_stage = case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow parr",
                                   grepl("cutthroat", species, ignore.case=T) ~ "Cutthroat parr",
                                   grepl("chinook", species, ignore.case=T) & clip_status == "clipped" ~ paste0(species, " ", "(hatchery)"),
                                   !is.na(life_stage) ~ paste0(species, " ", life_stage),
                                   grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                   grepl("sculpin|stickleback", species, ignore.case=T) ~ "Other fish",
                                   TRUE ~ species),
         species_stage_simple = case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow parr",
                                          grepl("cutthroat", species, ignore.case=T) ~ "Cutthroat parr",
                                          grepl("chinook", species, ignore.case=T) & clip_status == "clipped" ~ paste0(species, " ", "(hatchery)"),
                                          grepl("chinook", species, ignore.case=T) & clip_status != "clipped" ~ paste0(species, " ", "(natural)"),
                                          !is.na(life_stage) ~ paste0(species, " ", life_stage),
                                          grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                          grepl("sculpin|stickleback", species, ignore.case=T) ~ "Other fish",
                                          TRUE ~ species),
         species_simple = stringr::str_to_sentence(case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow",
                                                             grepl("chinook", species, ignore.case=T) & clip_status == "clipped" ~ paste0(species, " (hatchery)"),
                                                             grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                                             grepl("sculpin|stickleback", species, ignore.case=T) ~ "Other fish",
                                                             TRUE ~ species))) %>%
  janitor::clean_names()


# Mark-release ----------------- 
release <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                pattern = "^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                              sheet="mark-release")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== VISUALIZE FISHING EVENTS ===============

# For 2023 ------------
ggplot() +
  geom_segment(data=eventMeta %>% 
                 filter(year==2023), aes(x=datetime_start, xend=datetime_stop, y=NA, yend=NA, colour=set_type), size=10, alpha=0.7) +
  scale_x_datetime(date_breaks="1 day", date_labels = "%b %d %H:%M") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

# For 2024 ------------
ggplot() +
  geom_segment(data=eventMeta %>% 
                 filter(year==2024), aes(x=datetime_start, xend=datetime_stop, y=NA, yend=NA, fill=set_type), size=10, alpha=0.7) +
  scale_x_datetime(date_breaks="1 day", date_labels = "%b %d %H:%M") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



# =============== JOIN META + TOTALS ===============

# Link set totals to events so that I have the start/end times associated with the totals
eventMeta_totals <- full_join(eventMeta %>%
                                select(year, gear, date_start, datetime_start, date_stop, datetime_stop, set_type, usid),
                              setTotals %>% 
                                filter(species %in% c("chinook", "chum", "coho")) %>%
                                select(usid, doy, species_stage_simple, total_caught_excl_recaps),
                              by="usid"
) %>%
  group_by(date_stop, species_stage_simple) %>% 
  summarize(year, gear, usid, set_type, date_start, datetime_start, 
            datetime_stop, 
            set_total = sum(total_caught_excl_recaps),
            doy) %>%
  relocate(date_stop, .before=datetime_stop) %>% 
  relocate(species_stage_simple, .after=datetime_stop) %>% 
  distinct(species_stage_simple, date_stop, set_total, .keep_all = T) %>%
  ungroup() %>%
  filter(!is.na(species_stage_simple)) %>%
  pivot_wider(names_from = "species_stage_simple", values_from = "set_total") %>%
  
  arrange(date_stop) %>%
  group_by(year) %>%
  complete(date_stop = seq.Date(min(as.Date(date_stop)), max(as.Date(date_stop)), by="day")) %>%
  relocate(date_stop, .after=datetime_start) %>%
  mutate(hrs_fished = case_when(!is.na(usid) ~ as.numeric(datetime_stop-datetime_start),
                                TRUE ~ 0),
         estimate_type = case_when(is.na(usid) ~ "infill",
                                   TRUE ~ "observed"),
         across(c(`chinook (natural)`:`chinook (hatchery)`), ~case_when(!is.na(usid) & is.na(.) ~ 0,
                                                                  TRUE ~ .)),
         chinook_natural_obs_validation = `chinook (natural)`,
         coho_subyearling_obs_validation = `coho subyearling`,
         coho_yearling_obs_validation = `coho yearling`,
         chum_fry_obs_validation = `chum fry`,
         chinook_hatchery_obs_validation = `chinook (hatchery)`) %>%
  janitor::clean_names() %>%
  ungroup() %>%
  rename_with(~ paste(., "obs", sep="_"), c(chinook_natural:chinook_hatchery)) %>%
  print()


# =============== CREATE IMPUTATION VALIDATION DATA SET ===============

# Identify annual sample sizes representing 20% of the data (non-NA days) each year ------------
#     These sample sizes are going to be used to randomly select a number of days from each year to validate imputation models
#     Here I just used the natural Chinook series as a representative variable - all species counts have the same NA days so it is arbitrary which variable is
#     used. Could have also used coho_subyearling_obs and gotten the same calculation 
random.sample.sizes <- data.frame(year=c(2023,2024,2025),
                                  sample_size = c(
                                    eventMeta_totals %>% 
                                      filter(year==2023 & !is.na(chinook_natural_obs)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n),
                                    eventMeta_totals %>% 
                                      filter(year==2024 & !is.na(chinook_natural_obs)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n),
                                    eventMeta_totals %>% 
                                      filter(year==2025 & !is.na(chinook_natural_obs)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n)))


# Random selection of values to replace per year for each of the focal species being infilled: 
set.seed(3)
random.selection <- eventMeta_totals %>%
  group_split(year) %>%
  map2_dfr(random.sample.sizes$sample_size, ~slice_sample(.x[.x$estimate_type=="observed",], n=.y)) %>%         # Select random dates based on year-specific sample sizes defined above
  mutate(chinook_natural_obs_validation = NA,
         coho_subyearling_obs_validation = NA,
         coho_yearling_obs_validation = NA,
         chum_fry_obs_validation = NA,
         chinook_hatchery_obs_validation = NA)                                                                  # Overwrite those dates with NAs for model validation


# Rejoin to full dataset, but just simplify a bit for infilling/validation --------------
eventMeta_totals_impValFull <- full_join(eventMeta_totals %>%
                                filter(date_stop %notin% random.selection$date_stop),
                              
                              random.selection) %>%
  mutate(validation_type = case_when(!is.na(chinook_natural_obs) & is.na(chinook_natural_obs_validation) ~ "validation",
                                     !is.na(chinook_natural_obs) & !is.na(chinook_natural_obs_validation) ~ "observed",
                                     is.na(chinook_natural_obs) & is.na(chinook_natural_obs_validation) ~ "infill")) %>%
  select(-c(gear, usid, set_type, date_start, datetime_start, datetime_stop, hrs_fished)) %>%
  relocate(c(estimate_type, validation_type), .after=doy) %>%
  mutate(doy = lubridate::yday(date_stop)) %>%
  #pivot_longer(cols=c(chinook_natural_obs:chinook_hatchery_obs_validation), names_to = "species_stage", values_to = "estimate") %>%
  #pivot_longer(cols=c(chinook_natural_obs_validation:chinook_hatchery_obs_validation), names_to = "species_stage_val", values_to = "estimate_val") %>%
  arrange(date_stop)




# After trying to expand the whole series by 30-min intervals to differentially expand/infill missed daytime vs nighttime catch, I
# think I'm over-reaching what I can do with the data on hand. 
# I think I'm over-thinking this. i'm only expanding 2024 because 2023 was a pilot, and in 2024 the fishing was pretty consistent, 
# sure there were a few daytime instances of fishing but i just need to look at the spread of hours fished vs. unfished and expand 
# a tiny bit, maybe using the lower of the cowichan expansions for daytime, and maybe only for a few situations.

# MOVING FORWARD:  i should just pull out 2024 from the "fishing periods" sheet, and quickly use it to enumerate hours unfished vs
# fished. 
# then go back to my regular catch sheet (without the expanded time series) and expand most of the fishing days a tiny bit to 
# Account for unfished daytime (exclude the few daytime shifts)
# THEN infill the missing days. 



# Avg/range of hours fished ------------
TBL.operational_hours_summary <- eventMeta_totals %>%
  filter(!is.na(usid)) %>%
  group_by(year) %>% 
  summarize(mean_hrs = mean(hrs_fished, na.rm=T),
            min_hrs = min(hrs_fished, na.rm=T),
            max_hrs = max(hrs_fished, na.rm=T))







# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~




# INFILLING MISSED DAYS 

# Moving forward I'm going to start by infilling missed days based on daily CPUE, not CPUE_24 (expand for entire 24hr period). 

# Expanding for 24 hours adds quite a lot of fish in some cases (~700 coho fry on one day in 2024 are estimated to have migrated during
# the day with the 24-hr expansion method used by LGL!)

# I will explore a few methods of infilling. I'm going to start with 2024 as it was a much more comprehensive season and then
# determine how to approach 2023 pilot season afterwards (likely won't be expanded). 
  # Using methods within imputeTS (https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf)



# Notes from https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf: 
# "In general, for most time series one algorithm out of na_kalman, na_interpolation and na_seadec will yield the best results. Meanwhile, na_random, na_mean, 
# na_locf will be at the lower end accuracy wise for the majority of input time series."


# =============== MISSED DAY SUMMARY REPORT ===============
# Again using Natural Chinook to assess prevalence/extent of NAs, but could have been any of the species counts. 
imputeTS::statsNA(ts(eventMeta_totals_impValFull[eventMeta_totals_impValFull$year==2024,]$chinook_natural_obs))
imputeTS::statsNA(ts(eventMeta_totals_impValFull[eventMeta_totals_impValFull$year==2025,]$chinook_natural_obs))

# About half of days missing each year, sadly, but c'est la vie. 



# =============== MODEL VALIDATION ===============

# Each focal species/stage was split out into their own scripts so that infilling models could be thoroughly documented and examined. The critcial final
#   components (metrics, plots) are loaded with the excel calls below: 
#   Do NOT replace this with source() calls - it creates an endless loop of source()ing! 

# See each unique sub-script, e.g., 01-1-cpue-infilling-abundance-CN_NO.R



# =============== FINAL INFILLING! ===============
# *** next day: need the paper sheets in office to put in the final choice for each species!



# Hatchery-origin Chinook:
# **** next day: need to truncate hatchery chinook time series based on release dates and re-do infilling from the new "day 0"

# Sub-yearling Coho:

# Yearling Coho:

# Chum:






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
