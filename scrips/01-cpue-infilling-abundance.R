# San Juan RST 
# CPUE/infilling/abundance calculations
# May 2025



# ========================= SET UP =========================

# Load libraries -------------------
library(tidyverse)

# Load helpers ---------------


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
  mutate(species_stage_simple = case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow parr",
                                          grepl("cutthroat", species, ignore.case=T) ~ "Cutthroat parr",
                                          grepl("chinook", species, ignore.case=T) & clip_status == "clipped" ~ paste0(species, " ", life_stage, " ", "(hatchery)"),
                                          !is.na(life_stage) ~ paste0(species, " ", life_stage),
                                          grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                          grepl("lamprey|sculpin|stickleback", species, ignore.case=T) ~ "Other fish",
                                          TRUE ~ species),
         species_simple = stringr::str_to_sentence(case_when(grepl("rainbow|steelhead", species, ignore.case=T) | life_stage=="rainbow" ~ "Rainbow",
                                                             grepl("chinook", species, ignore.case=T) & clip_status == "clipped" ~ paste0(species, " (hatchery)"),
                                                             grepl("newt|toad", species, ignore.case=T) ~ "Amphibian",
                                                             grepl("lamprey|sculpin|stickleback", species, ignore.case=T) ~ "Other fish",
                                                             TRUE ~ species)))


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

# Link set totals to events 
eventMeta_totals <- full_join(eventMeta %>%
                                select(year, gear, date_start, datetime_start, date_stop, datetime_stop, set_type, usid),
                              setTotals %>% 
                                filter(species %in% c("chinook", "chum", "coho")) %>%
                                select(usid, DOY, species_stage_simple, total_caught_excl_recaps),
                              by="usid"
) %>%
  group_by(date_stop, species_stage_simple) %>% 
  summarize(year, gear, usid, set_type, date_start, datetime_start, 
            datetime_stop, 
            set_total = sum(total_caught_excl_recaps),
            DOY) %>%
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
         across(c(`chinook fry`:`chinook smolt (hatchery)`), ~case_when(!is.na(usid) & is.na(.) ~ 0,
                                                                        TRUE ~ .))) %>%
  print()



# Remove some observations to test imputation methods:
set.seed(123)
random.sample.sizes <- c(`2023` = eventMeta_totals %>% 
                           filter(year==2023 & !is.na(`chinook fry`)) %>%
                           summarize(n=round(n()*0.2, 0)) %>%
                           pull(n),
                         `2024` = eventMeta_totals %>% 
                           filter(year==2024 & !is.na(`chinook fry`)) %>%
                           summarize(n=round(n()*0.2, 0)) %>%
                           pull(n),
                         `2025` = eventMeta_totals %>% 
                           filter(year==2025 & !is.na(`chinook fry`)) %>%
                           summarize(n=round(n()*0.2, 0)) %>%
                           pull(n))

eventMeta_totals_imputation <- eventMeta_totals %>%
  mutate(`chinook fry IMPTEST` = `chinook fry`)


  group_by(year) %>%
  group_modify(~ {
    non_na_rows <- which(!is.na(.x$`chinook fry`))
    n_select <- min(sample_sizes[unique(.x$group)], length(non_na_rows))
    selected_rows <- sample(non_na_rows, size = n_select, replace = FALSE)
    .x$selected[selected_rows] <- "selected"
    .x
  }) %>%
  ungroup()


View(eventMeta_totals %>% filter(!is.na(`chinook fry`) & is.na(`chinook fry IMPTEST`)))



## ************ RE ASSESS NEXT DAY *********** 
# After trying to expand the whole series by 30-min intervals to differentially expand/infill missed daytime vs nighttime catch, I
# think I'm over-reaching what I can do with the data on hand. 
# I think i'm over-thinking this. i'm only expanding 2024 because 2023 was a pilot, and in 2024 the fishing was pretty consistent, 
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
  # 1. Using methods within imputeTS (https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf)
  # 2. Weighted "either side" days (LGL method).  
  # 3. Rolling window average centered around the missed day (window size TBD)
  # 4. zoo:rollapply ? 

# I'm going to try to do this for all salmon species all at once... 


# =============== INFILLING: imputeTS ===============

# Notes from https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf: 
  # "In general, for most time series one algorithm out of na_kalman, na_interpolation and na_seadec will yield the best results. Meanwhile, na_random, na_mean, 
  # na_locf will be at the lower end accuracy wise for the majority of input time series."




# Create timeseries ------------
ts_CNfry_COMPLETE <- ts(eventMeta_totals[eventMeta_totals$year==2024,]$`chinook fry`)
ts_CNfry_TEST <- ts(eventMeta_totals[eventMeta_totals$year==2024,]$`chinook fry IMPTEST`)


# Visualilze imputeTS options ------------

# Imputation by interpolation: linear, stineman  (spline doesn't floor at zero and predicts negatives)
imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_interpolation(ts_CNfry, option="linear"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_interpolation(ts_CNfry_TEST, option="linear"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
                                ) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))


imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_interpolation(ts_CNfry, option="stine"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_interpolation(ts_CNfry_TEST, option="stine"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))





# Imputation by Kalman: by Structural model & kalman smoothing (ARIMA model predicts negative values so was removed)
imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_kalman(ts_CNfry, model="StructTS"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_kalman(ts_CNfry_TEST, model="StructTS"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))


# imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_kalman(ts_CNfry, model="auto.arima"))
# imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
#                                 x_with_imputations = imputeTS::na_kalman(ts_CNfry_TEST, model="auto.arima"),
#                                 x_with_truth = ts_CNfry_COMPLETE,
#                                 
#                                 color_points="gray",
#                                 color_imputations = "orange",
#                                 color_truth = "green",
#                                 
#                                 size_points=4,
#                                 size_imputations=4,
#                                 size_truth=4,
#                                 
#                                 connect_na = F,
#                                 #color_lines = "transparent"
# ) +
#   scale_x_continuous(breaks=seq(0,1000,by=1)) +
#   theme(panel.grid.major.x = element_line(colour="gray70"),
#         panel.grid.minor.x = element_line(colour="gray90"),
#         panel.grid.major.y = element_line(colour="gray70"),
#         panel.grid.minor.y = element_line(colour="gray70"))


# Imputation by mean/median/mode (none are great)
  #imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_mean(ts_CNfry))

# Imputation by last observation carried forward/next observation carried backward (neither are great)
  #imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_locf(ts_CNfry, option="locf"))
  #imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_locf(ts_CNfry, option="nocb"))

# Imputation by moving average: simple, linear weighted moving average, exponential weighted moving average 
imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_ma(ts_CNfry, weighting="simple"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_ma(ts_CNfry_TEST, weighting="simple"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))

imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_ma(ts_CNfry, weighting="linear"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_ma(ts_CNfry_TEST, weighting="linear"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))

imputeTS::ggplot_na_imputations(x_with_na=ts_CNfry, x_with_imputations=imputeTS::na_ma(ts_CNfry, k=4, weighting="exponential"))
imputeTS::ggplot_na_imputations(x_with_na = ts_CNfry_TEST, 
                                x_with_imputations = imputeTS::na_ma(ts_CNfry_TEST, weighting="linear"),
                                x_with_truth = ts_CNfry_COMPLETE,
                                
                                color_points="gray",
                                color_imputations = "orange",
                                color_truth = "green",
                                
                                size_points=4,
                                size_imputations=4,
                                size_truth=4,
                                
                                connect_na = F,
                                #color_lines = "transparent"
) +
  scale_x_continuous(breaks=seq(0,1000,by=1)) +
  theme(panel.grid.major.x = element_line(colour="gray70"),
        panel.grid.minor.x = element_line(colour="gray90"),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_line(colour="gray70"))






imputeTS::statsNA(ts_CNfry)





