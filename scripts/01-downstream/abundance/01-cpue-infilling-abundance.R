# San Juan RST 
# CPUE/infilling/abundance calculations
# May 2025



# ========================= SET UP =========================

## Load libraries -------------------
library(tidyverse)

## Load helpers ---------------
"%notin%" <- Negate("%in%")
options(scipen=99999)


# ========================= LOAD JUVENILE DATA =========================

## Sample events ----------------- 
eventMeta <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                pattern="^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                                sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear)) %>% 
  mutate(datetime_start = lubridate::ymd_hm(paste0(date_start, time_start)),
         datetime_stop = lubridate::ymd_hm(paste0(date_stop, time_stop))) %>% 
  print()



## Environmentals ----------------- 
enviros <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                pattern = "^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                              sheet="enviro") %>% 
  filter(grepl("RST|IPT", usid, ignore.case=T))



## Catch totals ----------------- 
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


## Mark-release ----------------- 
release <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                pattern = "^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                              sheet="mark-release") %>%
  mutate_at("total_released", as.numeric)  %>%
  mutate_at("release_date", as.Date) %>%
  mutate(life_stage = case_when(grepl("parr", life_stage) ~ "smolt",
                                TRUE ~ life_stage),
         release_doy = lubridate::yday(release_date),
         year = lubridate::year(release_date))


## Hatchery release dates ----------------- 
hatchery_releases <- readxl::read_excel(path = here::here("data", "hatchery-releases", "upperSJ_hatchery_releases_2024-2025.xlsx"),
                                        sheet=1) %>%
  mutate(doy = lubridate::yday(date))


# ========================= LOAD HYDROMET DATA =========================
hydro <- full_join(
  read.csv(file=list.files(path = here::here("data", "enviro"),
                           pattern = "SANJUAN_historical*",
                           full.names = TRUE),
           skip=1) %>%
    mutate(PARAM = case_when(PARAM==1 ~ "discharge (cms)",
                             PARAM==2 ~ "level (m)"),
           Date = lubridate::ymd(Date),
           year = lubridate::year(Date),
           month = lubridate::month(Date, label=T, abbr=T),
           DOY = lubridate::yday(Date)),
  
  read.csv(file=list.files(path = here::here("data", "enviro"),
                           pattern = "SANJUAN_realtime*",
                           full.names = TRUE),
           skip=9) %>%
    mutate(Parameter = case_when(Parameter==46 ~ "level (m)",
                                 Parameter==6 ~ "discharge (cms)"),
           Date = lubridate::ymd(stringr::str_sub(string=Date..PST., start=1, end=10)),
           time = stringr::str_sub(string=Date..PST., start=12, end=19),
           year = lubridate::year(Date..PST.),
           month = lubridate::month(Date..PST., label=T, abbr=T),
           DOY = lubridate::yday(Date..PST.)) %>% 
    rename(PARAM=Parameter,
           Value=Value..m..s.)
) %>%
  janitor::clean_names()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== VISUALIZE FISHING EVENTS/EFFORT ===============

## For 2023 ------------
ggplot() +
  geom_segment(data=eventMeta %>% 
                 filter(year==2023), aes(x=datetime_start, xend=datetime_stop, y=NA, yend=NA, colour=set_type), size=10, alpha=0.7) +
  scale_x_datetime(date_breaks="1 day", date_labels = "%b %d %H:%M") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

## For 2024 ------------
ggplot() +
  geom_segment(data=eventMeta %>% 
                 filter(year==2024), aes(x=datetime_start, xend=datetime_stop, y=NA, yend=NA, fill=set_type), size=10, alpha=0.7) +
  scale_x_datetime(date_breaks="1 day", date_labels = "%b %d %H:%M") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## For 2025 ------------
ggplot() +
  geom_segment(data=eventMeta %>% 
                 filter(year==2025), aes(x=datetime_start, xend=datetime_stop, y=NA, yend=NA, fill=set_type), size=10, alpha=0.7) +
  scale_x_datetime(date_breaks="1 day", date_labels = "%b %d %H:%M") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ===================== OBSERVED CATCH DATA =====================

pdf(file = here::here("outputs", "figures", "RST infill-CPUE-abundance", "RST observed catches - all years, species.pdf"),   
    width = 16, # The width of the plot in inches
    height = 14) # The height of the plot in inches

## All species/years (plot) -------------------
ggplot() +
  geom_bar(data=eventMeta_totals %>% 
             pivot_longer(cols=c(chinook_natural_obs:chinook_hatchery_obs), names_to = "species_life_stage", values_to = "count") %>%
             mutate(species_life_stage = case_when(grepl("chinook_hatchery", species_life_stage) ~ "Chinook (hatchery)",
                                                   grepl("chinook_natural", species_life_stage) ~ "Chinook (natural)",
                                                   grepl("chum", species_life_stage) ~ "Chum",
                                                   grepl("coho_alevin", species_life_stage) ~ "Coho alevin",
                                                   grepl("coho_subyearling", species_life_stage) ~ "Coho fry (sub-yearling)",
                                                   grepl("coho_yearling", species_life_stage) ~ "Coho smolt (yearling)")) %>%
             filter(count>0),
           aes(x=as.Date(doy, origin="2022-12-31"), y=count, fill=species_life_stage, colour=species_life_stage), 
           stat="identity", position="stack", alpha=0.9, width=1) +
  geom_point(data=hatchery_releases, 
             aes(x=as.Date(doy, origin="2022-12-31"), y=0), colour="black", fill="black", size=2.5, shape=24, alpha=0.7) +
  scale_x_date(date_labels="%b %d", date_breaks="3 day") +
  scale_fill_manual(values=c("Chinook (hatchery)" = "#8dd3c7",
                             "Chinook (natural)" = "#7caed1",
                             "Chum" = "#b3de69",
                             "Coho alevin" = "#ffffb3",
                             "Coho fry (sub-yearling)" = "#fdb462",
                             "Coho smolt (yearling)" = "#fb8072")) +
  scale_colour_manual(values=c("Chinook (hatchery)" = "#8dd3c7",
                             "Chinook (natural)" = "#7caed1",
                             "Chum" = "#b3de69",
                             "Coho alevin" = "#ffffb3",
                             "Coho fry (sub-yearling)" = "#fdb462",
                             "Coho smolt (yearling)" = "#fb8072")) +
  labs(y="Observed catch", fill="Species/life history", colour="Species/life history") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=19),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=23),
        axis.title.x = element_blank(),
        legend.title = element_text(face="bold", size=20),
        legend.text = element_text(size=19),
        strip.text = element_text(size=20, face="bold")) +
  facet_wrap(~year, scales="free", nrow=3)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                      SET UP DATA FOR IMPUTATION (INFILLING)


# =============== JOIN eventMeta + setTotals ===============

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

## Identify annual sample sizes representing 20% of the data (non-NA days) each year ------------
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


### Random selection of values to replace per year for each of the focal species being infilled ------
set.seed(3)
random.selection <- eventMeta_totals %>%
  group_split(year) %>%
  map2_dfr(random.sample.sizes$sample_size, ~slice_sample(.x[.x$estimate_type=="observed",], n=.y)) %>%         # Select random dates based on year-specific sample sizes defined above
  mutate(chinook_natural_obs_validation = NA,
         coho_subyearling_obs_validation = NA,
         coho_yearling_obs_validation = NA,
         chum_fry_obs_validation = NA,
         chinook_hatchery_obs_validation = NA)                                                                  # Overwrite those dates with NAs for model validation


## Rejoin to full dataset, but just simplify a bit for infilling/validation --------------
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



## Avg/range of hours fished ------------
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


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== FINAL INFILLING! ===============

# The top models selected were:
# 2024:
  # NO chinook - na_kalman (structTS)
  # coho subY - na_kalman (ARIMA)
  # coho Y - na_interp (stine)
# 2025:
  # NO chinook - na_interp (linear) or na_kalman (either)  **thinking kalman/ARIMA because it was chosen most times 
  # coho subY - na_ma (2 or 3 day window - pick 2 day)
  # coho Y - na_kalman (ARIMA)

## Apply the chosen infill models ------------------
eventMeta_totals_INFILLEDFINAL <- full_join(
  eventMeta_totals_impValFull %>%
    filter(year == 2024) %>%
    select(-c(contains("validation"))) %>%
    mutate(chinook_natural_obs = imputeTS::na_kalman(ts(chinook_natural_obs), model="StructTS"),
           coho_subyearling_obs = imputeTS::na_kalman(ts(coho_subyearling_obs), model="auto.arima"),
           coho_yearling_obs = imputeTS::na_interpolation(ts(coho_yearling_obs), option="stine")),
  
  eventMeta_totals_impValFull %>%
    filter(year == 2025) %>%
    select(-c(contains("validation"))) %>%
    mutate(chinook_natural_obs = imputeTS::na_interpolation(ts(chinook_natural_obs), option="linear"),
           coho_subyearling_obs = imputeTS::na_ma(ts(coho_subyearling_obs), weighting="simple", k=2),
           coho_yearling_obs = imputeTS::na_kalman(ts(coho_yearling_obs), model="auto.arima")),
) %>%
  full_join(.,
            eventMeta_totals_impValFull %>%
              filter(year == 2023) %>%
              select(-c(contains("validation")))) %>%
  rename (chinook_natural = chinook_natural_obs,
          coho_subyearling = coho_subyearling_obs,
          coho_yearling = coho_yearling_obs,
          chum_fry = chum_fry_obs,
          coho_alevin = coho_alevin_obs,
          chinook_hatchery = chinook_hatchery_obs) %>%
  pivot_longer(cols=c(chinook_natural:chinook_hatchery), values_to = "count", names_to = "species") %>%
  mutate(species = case_when(species=="chinook_natural" ~ "Chinook (natural)",
                             species=="coho_subyearling"~ "Coho fry (sub-yearling)",
                             species=="coho_yearling"~ "Coho smolt (yearling)",
                             species=="chum_fry"~ "Chum fry",
                             species=="chinook_hatchery"~ "Chinook (hatchery)",
                             species=="coho_alevin"~ "Coho alevin"
                             ))
  
  

## Plot ------------------
pdf(file = here::here("outputs", "figures", "RST infill-CPUE-abundance", "RST infilled.pdf"),   
    width = 16, # The width of the plot in inches
    height = 14) # The height of the plot in inches

ggplot(data=eventMeta_totals_INFILLEDFINAL %>%
         filter(species %in% c("Chinook (natural)", "Coho fry (sub-yearling)", "Coho smolt (yearling)"),
                year != 2023)) +
  geom_point(aes(x=as.Date(doy, origin="2023-12-31"), y=count, fill=species, colour=species, shape=estimate_type), 
             alpha=0.8, size=4, stroke=2) +
  geom_line(aes(x=as.Date(doy, origin="2023-12-31"), y=count, colour=species), 
            alpha=0.8, linewidth=1) +
  scale_fill_manual(values=c("Chinook (natural)" = "#7caed1",
                             "Coho fry (sub-yearling)" = "#fdb462",
                             "Coho smolt (yearling)" = "#fb8072")) +
  scale_colour_manual(values=c("Chinook (natural)" = "#7caed1",
                             "Coho fry (sub-yearling)" = "#fdb462",
                             "Coho smolt (yearling)" = "#fb8072")) +
  scale_shape_manual(values=c("infill" = 4,
                               "observed" = 21),
                     labels = c("infill"="Infilled",
                                "observed" = "Observed")) +
  scale_x_date(date_breaks="7 day", date_labels = "%b %d") +
  labs(y="Juvenile salmon counts", fill="Species/life history", colour="Species/life history", shape="Count type") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=19),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=23),
        axis.title.x = element_blank(),
        legend.title = element_text(face="bold", size=20),
        legend.text = element_text(size=19),
        strip.text = element_text(size=20, face="bold")) +
  facet_wrap(~year, scales="free", nrow=2)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================= TRAP EFFICIENCY ======================= 

release_summary <- release %>%
  filter(total_released > 0) %>%
  group_by(year, life_stage) %>%
  summarize(release_by_stage = sum(total_released)) %>%
  print()

ggplot() +
  geom_point(data=release %>%
               group_by(year, release_doy, life_stage) %>%
               summarize(n = sum(total_released, na.rm=T)) %>%
               filter(n > 0),
             aes(x=release_doy, y=n), size=4, shape=21, fill="gray70", stroke=1) +
  
  geom_point(data=setTotals %>%
               filter(bismarck_recaps > 0) %>%
               mutate(life_stage = case_when(life_stage=="subyearling" ~ "fry",
                                             life_stage=="yearling" ~ "smolt",
                                             TRUE ~ life_stage)) %>%
               group_by(year, doy, life_stage) %>%
               summarize(n = sum(bismarck_recaps, na.rm=T)) %>%
               filter(n > 0) ,
             aes(x=doy, y=n), size=4, shape=21, fill="red", stroke=1) +
  #scale_x_date(date_breaks="2 day", date_labels="%b %d") +
  scale_x_continuous(breaks=seq(0,366, by=2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  facet_wrap(year ~ life_stage, scales="free")
  


mark_rel_sum <- left_join(
  
  release %>% 
    group_by(year, release_date, life_stage) %>%
    summarize(total_marks_released_M = sum(total_released, na.rm=T)),
  
  
  setTotals %>%
    filter(grepl("chinook|coho", species, ignore.case=T)) %>%
    select(year, date, life_stage, total_caught_incl_recaps, bismarck_recaps) %>%
    mutate(life_stage = case_when(life_stage=="subyearling" ~ "fry",
                                  life_stage=="yearling" ~ "smolt",
                                  TRUE ~ life_stage))  %>%
    group_by(year, date, life_stage) %>%
    summarize(total_caught_markunmarked_nC = sum(total_caught_incl_recaps, na.rm=T),
              total_recaps_mR = sum(bismarck_recaps, na.rm=T)) %>%
    rename(catch_date = date) %>%
    mutate(catch_date_lagged = as.Date(catch_date)-1),

  
  by=c("release_date" = "catch_date_lagged",
    "life_stage", "year")
) %>%
  filter(year != 2023) %>%
  select(year, life_stage, release_date, catch_date, total_marks_released_M, total_caught_markunmarked_nC, total_recaps_mR) %>%
  arrange(year, life_stage)


write.csv(mark_rel_sum, "mark_rel_sum test2.csv", row.names=F)
