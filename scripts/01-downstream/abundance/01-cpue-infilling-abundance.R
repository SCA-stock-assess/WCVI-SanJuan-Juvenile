# San Juan RST 
# CPUE/infilling/abundance calculations
# May 2025



# ========================= SET UP =========================

## Load libraries -------------------
library(tidyverse)

## Load helpers ---------------
"%notin%" <- Negate("%in%")
options(scipen=99999999)


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
                                filter(species %in% c("Chinook", "Chum", "Coho")) %>%
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
         across(c(`Chinook (natural)`:`Chinook (hatchery)`), ~case_when(!is.na(usid) & is.na(.) ~ 0,
                                                                  TRUE ~ .)),
         chinook_natural_obs_validation = `Chinook (natural)`,
         coho_subyearling_obs_validation = `Coho subyearling`,
         coho_yearling_obs_validation = `Coho yearling`,
         chum_fry_obs_validation = `Chum fry`,
         chinook_hatchery_obs_validation = `Chinook (hatchery)`) %>%
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
pdf(file = here::here("outputs", "figures", "RST infill-CPUE-abundance", "RST infilled - 2024 final.pdf"),   
    width = 16, # The width of the plot in inches
    height = 14) # The height of the plot in inches

ggplot(data=eventMeta_totals_INFILLEDFINAL %>%
         filter(species %in% c("Chinook (natural)", "Coho fry (sub-yearling)"#, "Coho smolt (yearling)"
                               ),
                year == 2024
                )) +
  geom_point(aes(x=as.Date(doy, origin="2023-12-31"), y=count, fill=species, colour=species, shape=estimate_type), 
             alpha=0.8, size=4, stroke=2) +
  geom_line(aes(x=as.Date(doy, origin="2023-12-31"), y=count, colour=species), 
            alpha=0.8, linewidth=1) +
  scale_fill_manual(values=c("Chinook (natural)" = "#7caed1",
                             "Coho fry (sub-yearling)" = "#fdb462" #,
                             #"Coho smolt (yearling)" = "#fb8072"
                             )) +
  scale_colour_manual(values=c("Chinook (natural)" = "#7caed1",
                             "Coho fry (sub-yearling)" = "#fdb462" #,
                             #"Coho smolt (yearling)" = "#fb8072"
                             )) +
  scale_shape_manual(values=c("infill" = 4,
                               "observed" = 21),
                     labels = c("infill"="Infilled",
                                "observed" = "Observed")) +
  scale_x_date(date_breaks="7 day", date_labels = "%b %d") +
  scale_y_continuous(breaks=scales::pretty_breaks(n=10)) +
  labs(y="Daily count", fill="Species/life history", colour="Species/life history", shape="Count type") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=19),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=23),
        axis.title.x = element_blank(),
        legend.title = element_text(face="bold", size=20),
        legend.text = element_text(size=19),
        strip.text = element_text(size=20, face="bold"),
        plot.margin = margin(t=0.5, r=0.2, b=0.2, l=0.5, unit="cm")) #+
  #facet_wrap(~year, scales="free", nrow=2)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================= CATCH AND FLOW RELATIONSHIPS ======================= 
## Plot of 2024 catch + flow ~ time ---------- 
ggplot() +
  geom_line(data=hydro %>%
              filter(year==2024, param=="discharge (cms)", month%in%c("Feb", "Mar", "Apr", "May", "Jun")),
            aes(x=as.Date(doy, origin="2024-01-01"), y=value*5), colour="dodger blue", size=1) +
  geom_bar(data=setTotals %>%
             filter(species=="Coho", life_stage=="subyearling", year==2024) %>%
             group_by(doy) %>%
             summarize(total_cn = sum(total_caught_excl_recaps, na.rm=T)),
           aes(x=as.Date(doy, origin="2024-01-01"), y=total_cn), stat="identity", fill="orange", colour="orange", alpha=0.6) +
  annotate(geom="text", x=as.Date(60, origin="2024-01-01"), y=1600, label="2024 Coho subyearling", fontface="bold") +
  scale_x_date(date_labels = "%b %d", date_breaks="2 day") +
  scale_y_continuous(sec.axis = sec_axis(~./5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


## Plot of 2025 catch + flow ~ time ---------- 
ggplot() +
  geom_line(data=hydro %>%
              filter(year==2025, param=="discharge (cms)", month%in%c("Feb", "Mar", "Apr", "May", "Jun")),
            aes(x=doy, y=value), colour="dodger blue", size=1) +
  geom_bar(data=setTotals %>%
             filter(species=="Coho", life_stage=="subyearling", year==2025) %>%
             group_by(doy) %>%
             summarize(total_cn = sum(total_caught_excl_recaps, na.rm=T)),
           aes(x=doy, y=total_cn), stat="identity", fill="orange", colour="orange") +
  scale_y_continuous(sec.axis = sec_axis(~./1)) +
  theme_bw() 


# Summary of releases: 
# release_summary <- release %>%
#   filter(total_released > 0) %>%
#   group_by(year, species, life_stage) %>%    #add spp
#   summarize(release_by_stage = sum(total_released)) %>%
#   print()


## Plot of ALL catch with ALL releases (unlagged) ---------- 
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



# ===================== COHO + CHINOOK FRY ABUNDANCE =======================
# For these purposes, pool Chinook and Coho total catch/release/recaptures to calculate efficiency ~ flow

## Total number of "new" fish passing the trap, infilled --------
cocn_catch <- eventMeta_totals_INFILLEDFINAL %>%
  filter(species %in% c("Coho fry (sub-yearling)", "Chinook (natural)"), year %in% c(2024,2025)) %>%
  pivot_wider(names_from = species, values_from = count) %>%
  select(-c(estimate_type, doy)) %>%
  rename(cncount_Ucn = `Chinook (natural)`,
         cocount_Uco = `Coho fry (sub-yearling)`) %>%
  mutate(totalcount_U = cncount_Ucn+cocount_Uco) %>%
  print()


## Total number of releases --------
cocn_releases <- release %>%
  mutate(case_when(species=="coho" & life_stage=="smolt" ~ "yearling",
                   TRUE ~ life_stage)) %>%
  filter(species%in%c("coho","chinook"), life_stage!="yearling") %>%
  group_by(year, release_date, species) %>%
  summarize(marks = sum(total_released, na.rm=T)) %>%
  pivot_wider(names_from = species, values_from = marks) %>%
  rename(markedCN_released_Mcn = chinook,
         markedCO_released_Mco = coho) %>%
  rowwise() %>%
  mutate(total_marks_released_M = sum(markedCN_released_Mcn,markedCO_released_Mco, na.rm=T)) %>%
  print()


## Total number of recaptures --------
# Not lagged for time: 
cocn_recaptures_nolag <- setTotals %>%
  mutate_at("date", as.Date) %>%
  filter(species%in%c("Coho", "Chinook"), life_stage!="yearling", year %in% c(2024,2025)) %>%
  group_by(year, date, species) %>%
  summarize(total_recaps = sum(bismarck_recaps, na.rm=T)) %>%
  pivot_wider(names_from = species, values_from = total_recaps) %>%
  rename(cnrecap_Rcn0 = Chinook,
         corecap_Rco0 = Coho) %>%
  rowwise() %>%
  mutate(`total_recaps_R_lag0` = sum(cnrecap_Rcn0, corecap_Rco0, na.rm=T)) %>%
  print()

# Lagged for time to line up recaptures with their respective release groups (usually lagged by 1 day, occasionally >1 day)
cocn_recaptures_lag <- setTotals %>%
  mutate_at("date", as.Date) %>%
  filter(species%in%c("Coho", "Chinook"), life_stage!="yearling", year %in% c(2024,2025)) %>%
  group_by(year, date, species) %>%
  summarize(total_recaps = sum(bismarck_recaps, na.rm=T)) %>%
  pivot_wider(names_from = species, values_from = total_recaps) %>%
  rename(`cnrecap_Rcn-1` = Chinook,
         `corecap_Rco-1` = Coho) %>%
  rowwise() %>%
  mutate(`total_recaps_R_lag-1` = sum(`cnrecap_Rcn-1`, `corecap_Rco-1`, na.rm=T),
         date = case_when(date == as.Date("2025-05-16") ~ date-2,   # This case lagged by 2 days - no other possible release groups
                          date == as.Date("2024-04-23") ~ date-6,   # This case lagged by 6 days - no other possible release groups
                          TRUE ~ date-1)) %>%                       # Default lagged by 1 day
  print()


## Total number marked+unmarked fish (second sample) --------
cocn_markunmark <- setTotals %>%
  filter(species%in%c("Coho", "Chinook"), life_stage!="yearling", year %in% c(2024,2025)) %>%
  group_by(year, date, species) %>%
  summarize(total_markunmark = sum(total_caught_incl_recaps, na.rm=T)) %>%
  pivot_wider(names_from = species, values_from = total_markunmark) %>%
  mutate(across(c(Chinook, Coho), ~ case_when(is.na(.) ~ 0,
                                      TRUE ~ round(.,0)))) %>%
  rename(CN_markunmarked_Ccn = Chinook,
         CO_markunmarked_Cco = Coho) %>%
  rowwise() %>%
  mutate(total_markunmark_C = sum(CN_markunmarked_Ccn, CO_markunmarked_Cco, na.rm=T)) %>%
  print()



## JOIN: M-R SUMMARY --------
cocn_abundance <- left_join(cocn_catch,            
                            cocn_releases, 
                            by=c("year",
                                 "date_stop" = "release_date")) %>%
  left_join(.,
            cocn_recaptures_lag,
            by = c("year",
                   "date_stop" = "date")) %>%
  left_join(.,
            cocn_recaptures_nolag,
            by = c("year",
                   "date_stop" = "date")) %>%
  mutate(across(c(markedCN_released_Mcn:`total_recaps_R_lag0`), ~case_when(is.na(.) ~ 0,
                                                                           TRUE ~ .))) %>%
  left_join(.,
            cocn_markunmark,
            by=c("year",
                 "date_stop"="date")) %>% 
  mutate(CN_markunmarked_Ccn = case_when(is.na(CN_markunmarked_Ccn) ~ round(cncount_Ucn,0),
                                         TRUE ~ CN_markunmarked_Ccn),
         CO_markunmarked_Cco = case_when(is.na(CO_markunmarked_Cco) ~ round(cocount_Uco,0),
                                     TRUE ~ CO_markunmarked_Cco),
         total_markunmark_C = case_when(is.na(total_markunmark_C) ~ round(totalcount_U,0),
                                        TRUE ~ total_markunmark_C)) %>%
  left_join(.,
            hydro %>%
              filter(param=="discharge (cms)", year %in% c(2024,2025)) %>%
              select(date, value, year) %>%
              rename(discharge=value),
            by=c("year",
                 "date_stop" = "date")) %>%
  mutate(trap_efficiency_e = case_when(`total_recaps_R_lag-1`>0 ~ `total_recaps_R_lag-1`/total_marks_released_M),
         efficiency_type = case_when(!is.na(trap_efficiency_e) ~ "observed",
                                     TRUE ~ "modelled")) %>%
  print()


### Export to manually smooth discharge across release and recapture periods ----
# This is too difficult to do in R as there are differentially lagged periods. As we don't know when exactly a fish entered the trap
#   post-release, it cannot be necessarily attributed to one single day's discharge (i.e., one point in time). Instead, smooth (average)
#   discharge across the release and recapture period to attribute an average discharge to each trap efficiency score

  # write.csv(cocn_abundance, file=here::here("outputs", "R_OUT - mark_recapture_summary.csv"), row.names=F)

# Calculate average discharge over "smoothed" days:
hydro_sm <- hydro %>%
  filter(param=="discharge (cms)", date %in% c(as.Date("2024-03-27"), as.Date("2024-03-28"),
                                               as.Date("2024-04-03"), as.Date("2024-04-04"),
                                               as.Date("2024-04-17"):as.Date("2024-04-23"),
                                               as.Date("2024-05-07"), as.Date("2024-05-08"),
                                               as.Date("2024-05-28"), as.Date("2024-05-29"),
                                               as.Date("2025-05-14"):as.Date("2025-05-16"),
                                               as.Date("2025-05-28"), as.Date("2025-05-29"))) %>%
  mutate(efficiency_strata = case_when(date %in% c(as.Date("2024-03-27"), as.Date("2024-03-28")) ~ "1",
                                       date %in% c(as.Date("2024-04-03"), as.Date("2024-04-04")) ~ "2",
                                       date %in% c(as.Date("2024-04-17"), as.Date("2024-04-18"), as.Date("2024-04-19"),
                                                   as.Date("2024-04-20"), as.Date("2024-04-21"), as.Date("2024-04-22"),
                                                   as.Date("2024-04-23")) ~ "3",
                                       date %in% c(as.Date("2024-05-07"), as.Date("2024-05-08")) ~ "4",
                                       date %in% c(as.Date("2024-05-28"), as.Date("2024-05-29")) ~ "5",
                                       date %in% c(as.Date("2025-05-14"), as.Date("2025-05-15"), as.Date("2025-05-16")) ~ "6",
                                       date %in% c(as.Date("2025-05-28"), as.Date("2025-05-29")) ~ "7",
                                       TRUE ~ NA)) %>%
  group_by(efficiency_strata) %>%
  summarize(mean_Q = mean(value, na.rm=T)) %>%
  print()


# Read manual averaged discharge values back in: 
cocn_abundance_smooth <- readxl::read_excel(path=here::here("outputs", "R_IN - mark_recapture_summary_Qsmoothed-manual.xlsx"),
                                            skip=1)



## Efficiency ~ Flow ---------
# Plot:
ggplot(data=cocn_abundance_smooth %>% 
         filter(!is.na(trap_efficiency_e))) +
  geom_point(aes(x=discharge_smoothed, y=trap_efficiency_e), size=3) +
  geom_smooth(aes(x=discharge_smoothed, y=trap_efficiency_e), method = "lm", se = FALSE, formula = y ~ x, color = "blue") +
  geom_smooth(aes(x=discharge_smoothed, y=trap_efficiency_e), method = "lm", se = FALSE, formula = y ~ poly(x, 2), color = "#EC2049") +
  geom_smooth(aes(x=discharge_smoothed, y=trap_efficiency_e), method = "lm", se = FALSE, formula = y ~ poly(x, 3), color = "green")

# Create a simplified dataframe just of observed E ~ discharge: 
lmdf <- cocn_abundance_smooth %>% 
  filter(!is.na(trap_efficiency_e)) %>%
  mutate(discharge_smoothed2 = discharge_smoothed^2,
         discharge_smoothed3 = discharge_smoothed^3)


### Evaluate models ----
# Simple linear:
summary(lm(trap_efficiency_e ~ discharge_smoothed, data=lmdf))

# Log-linear transformed:
summary(lm(log(trap_efficiency_e) ~ discharge_smoothed, data=lmdf)) #** 
summary(lm(trap_efficiency_e ~ log(discharge_smoothed), data=lmdf))
summary(lm(log(trap_efficiency_e) ~ log(discharge_smoothed), data=lmdf))

# Polynomials: 
summary(lm(trap_efficiency_e ~ discharge_smoothed + discharge_smoothed2, data=lmdf))
#summary(lm(trap_efficiency_e ~ discharge_smoothed + discharge_smoothed2 + discharge_smoothed3, data=lmdf))    # truly terrible


### Model selection ----
# Based on R2, lm(log(trap_efficiency_e) ~ discharge_smoothed, data=lmdf) is best

# Visualize: 
ggplot(data=cocn_abundance_smooth %>% 
         filter(!is.na(trap_efficiency_e))) +
  geom_point(aes(x=discharge_smoothed, y=log(trap_efficiency_e)), size=3) +
  geom_smooth(aes(x=discharge_smoothed, y=log(trap_efficiency_e)), method="lm", se=T, formula = y~x, color="blue") +
  labs(x="Discharge (cms)", y="log(trap efficiency)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17))

# Define model:
logE.lm <- lm(log(trap_efficiency_e) ~ discharge_smoothed, data=lmdf)
logE.lm.b <- logE.lm$coefficients[1]
logE.lm.m <- logE.lm$coefficients[2]
logE.lmSum <- summary(logE.lm)
logE.lmSum$r.squared


### Predict E with model -------
# y = mx + b
# log(y) = -0.07222482*discharge - 2.317334 
# exp(y)
cocn_abundance_smooth.pred <- cocn_abundance_smooth %>%
  mutate(trap_efficiency_e = case_when(efficiency_type=="modelled" ~ round(exp((logE.lm.m*discharge_smoothed) + logE.lm.b), 4),
                                       TRUE ~ round(trap_efficiency_e, 4)))


#### Plot ----
cocn_abundance_smooth.pred$efficiency_type <- factor(cocn_abundance_smooth.pred$efficiency_type, levels=c("observed", "modelled"), 
                                                     ordered=T)

pdf(file = here::here("outputs", "figures", "RST infill-CPUE-abundance", "Trap efficiency ~ discharge.pdf"),   
    width = 14, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggpubr::ggarrange(
  ggplot() +
    annotate(geom = "text", x = 5, y=-1, label="A", fontface="bold", size=6) +
    geom_smooth(data=cocn_abundance_smooth %>% 
                  filter(!is.na(trap_efficiency_e)),
                aes(x=discharge_smoothed, y=log(trap_efficiency_e)), method="lm", se=T, formula = y~x, color="blue") +
    geom_point(data=cocn_abundance_smooth %>% 
                 filter(!is.na(trap_efficiency_e)),
               aes(x=discharge_smoothed, y=log(trap_efficiency_e)), size=4, shape=21, fill="black", alpha=0.8) +
    labs(x="Discharge (cms)", y="log(Trap efficiency)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          plot.margin = margin(10, 75, 10, 10)) ,   #t, r, b, l),
  
  ggplot(data=cocn_abundance_smooth.pred %>% 
           filter(!is.na(trap_efficiency_e))) +
    annotate(geom = "text", x = 2, y=0.11, label="B", fontface="bold", size=6) +
    geom_point(aes(x=discharge_smoothed, y=trap_efficiency_e, fill=efficiency_type, colour=efficiency_type, alpha=efficiency_type), 
               size=4, shape=21) +
    scale_fill_manual(breaks=waiver(), values=c("black", "blue")) +
    scale_colour_manual(breaks=waiver(), values=c("black", "blue")) +
    scale_alpha_manual(breaks=waiver(), values=c(0.8, 0.3), guide="none") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x="Discharge (cms)", y="Trap efficiency", colour="Trap efficiency:", fill="Trap efficiency:") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=17),
          axis.title = element_text(face="bold", size=19),
          legend.title = element_text(size=18, face="bold"),
          legend.text = element_text(size=15),
          plot.margin = margin(10, 0, 10, 0))
)

dev.off()

## Daily Chinook estimates -------------
cocn_abundance_smooth.predabund <- cocn_abundance_smooth.pred %>%
  mutate(CNdaily_abundance_est = round(cncount_Ucn/trap_efficiency_e,0),
         cncount_Ucn1 = cncount_Ucn+1,
         CNdaily_abundance_est1 = round(cncount_Ucn1/trap_efficiency_e,0),
         
         COdaily_abundance_est = round(cocount_Uco/trap_efficiency_e,0),
         cocount_Uco1 = cocount_Uco+1,
         COdaily_abundance_est1 = round(cocount_Uco1/trap_efficiency_e,0)) %>%
  print()


## Annual abundance estimates -------------
#### Based on trap efficiency ----
efficiency.abundest <- cocn_abundance_smooth.predabund %>%
  group_by(year) %>%
  summarize(CNest = sum(CNdaily_abundance_est, na.rm=T),
            CNest1 = sum(CNdaily_abundance_est1, na.rm=T),
            
            COest = sum(COdaily_abundance_est, na.rm=T),
            COest1 = sum(COdaily_abundance_est1, na.rm=T)) %>%
  print()


### Based on life history calculations ----
  # Chinook: 
    # 2023 parental return: 2,642 spawners
    # Assume 40% female: 1,057 females
    # Assume 3000 eggs/female: 3,171,000 eggs
    # Egg-fry mortality 90%: 317,100
  # Coho:
    # 2023 parental return: 3,578 spawners
    # Assume 40% female: 1,431 females
    # Assume fecundity 3000 eggs/female: 4,293,000 eggs
    # Egg-fry mortality 80%: 858,720

cn.LH.abundest <- ((2642*0.4)*3500)*0.1
co.LH.abundest <- ((3578*0.4)*3000)*0.2    #this is likely not appropriate as we'd need the yearling component


### Coho Pooled Petersen estimate ----
coho.pp.abundest <- cocn_abundance_smooth.predabund %>%
  group_by(year) %>%
  summarize(M = sum(markedCO_released_Mco),
            R = sum(corecap_Rco.y),
            C = sum(CO_markunmarked_Cco)) %>%
  mutate(N = (((M+1)*(C+1))/(R+1))-1,
         var = ((M+1)*(C+1)*(M-R)*(C-R))/(((R+1)^2)*(R+2)),
         SE = sqrt(var),
         UCL = N + (1.96*SE),
         LCL = N - (1.96*SE)) %>%
  print()


# M is the number of fish caught, marked, and released in first sample
# R is the number of recaptures in the second sample (i.e., fish that were marked and released in the first sample),
# C is the total number of marked and unmarked fish caught in second sample


# ============= Export Table for Report ============= 
write.csv(x = data.frame(species = c(rep("chinook", 2), rep("coho_subY", 3)),
                         method = c("LH", "TE", "LH", "TE", "PP"),
                         estimate = c(cn.LH.abundest, efficiency.abundest[efficiency.abundest$year=="2024",]$CNest1,
                                      co.LH.abundest, efficiency.abundest[efficiency.abundest$year=="2024",]$COest1, 
                                      coho.pp.abundest[coho.pp.abundest$year=="2024",]$N),
                         UCI = c(NA, NA, NA, NA, coho.pp.abundest[coho.pp.abundest$year=="2024",]$UCL),
                         LCI = c(NA, NA, NA, NA, coho.pp.abundest[coho.pp.abundest$year=="2024",]$LCL)),
          file = here::here("outputs", "R_OUT - Outmigration abundance estimates 2024.csv"),
          row.names=F
          )

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

  
  
  


