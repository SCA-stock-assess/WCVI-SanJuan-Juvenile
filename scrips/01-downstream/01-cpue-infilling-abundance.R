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

# Link set totals to events 
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
         across(c(`chinook fry`:`chinook smolt (hatchery)`), ~case_when(!is.na(usid) & is.na(.) ~ 0,
                                                                        TRUE ~ .)),
         chinook_fry_IMPTEST = `chinook fry`) %>%
  janitor::clean_names() %>%
  ungroup() %>%
  print()


# =============== CREATE IMPUTATION VALIDATION COLUMNS ===============

# Identify sample sizes for each year: 20% of non-NA days each year ------------
random.sample.sizes <- data.frame(year=c(2023,2024,2025),
                                  sample_size = c(
                                    eventMeta_totals %>% 
                                      filter(year==2023 & !is.na(chinook_fry)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n),
                                    eventMeta_totals %>% 
                                      filter(year==2024 & !is.na(chinook_fry)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n),
                                    eventMeta_totals %>% 
                                      filter(year==2025 & !is.na(chinook_fry)) %>%
                                      summarize(n=round(n()*0.2, 0)) %>%
                                      pull(n)))


# Random selection of values to replace per year
set.seed(3)
random.selection <- eventMeta_totals %>%
  group_split(year) %>%
  map2_dfr(random.sample.sizes$sample_size, ~slice_sample(.x[.x$estimate_type=="observed",], n=.y)) %>%
  mutate(chinook_fry_imptest = NA)


# Rejoin:
eventMeta_totals_testing <- full_join(eventMeta_totals %>%
                                filter(date_stop %notin% random.selection$date_stop),
                              
                              random.selection) %>%
  arrange(date_stop)



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

imputeTS::statsNA(ts(eventMeta_totals_testing.interp[eventMeta_totals_testing.interp$year==2024,]$chinook_fry))


# Create new imputation testing variables ------------
eventMeta_totals_testing.interp <- eventMeta_totals_testing %>%
  filter(year==2024) %>%
  mutate(chinook_fry_interp.linear = imputeTS::na_interpolation(ts(chinook_fry_imptest), option="linear"),
         chinook_fry_interp.stine = imputeTS::na_interpolation(ts(chinook_fry_imptest), option="stine"),
         chinook_fry_kal.structs = imputeTS::na_kalman(ts(chinook_fry_imptest), model="StructTS"),
         chinook_fry_kal.arima = imputeTS::na_kalman(ts(chinook_fry_imptest), model="auto.arima"),
         chinook_fry_MA.simp = imputeTS::na_ma(ts(chinook_fry_imptest), weighting="simple"),
         chinook_fry_MA.linear = imputeTS::na_ma(ts(chinook_fry_imptest), weighting="linear"),
         chinook_fry_MA.exp = imputeTS::na_ma(ts(chinook_fry_imptest), weighting="exponential"),
         infill_type = case_when(is.na(chinook_fry_imptest) & !is.na(chinook_fry) ~ "ground truth",
                                 TRUE ~ "known value")) 


  
# Visualize imputeTS options ------------

ggplot() +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024), 
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry, fill=infill_type, colour=infill_type, size=infill_type), shape=21) +
  
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_interp.linear), colour="green", fill="green", shape=23, size=5, alpha=0.4) +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_interp.stine), colour="red", fill="red", shape=23, size=5, alpha=0.4) +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_kal.structs), colour="blue", fill="blue", shape=23, size=5, alpha=0.4) +
  #geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
  #           aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_kal.arima), colour="turquoise", fill="turquoise", shape=23, size=5, alpha=0.4) +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.simp), colour="orange", fill="orange", shape=23, size=5, alpha=0.4) +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.linear), colour="purple", fill="purple", shape=23, size=5, alpha=0.4) +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.exp), colour="magenta", fill="magenta", shape=23, size=5, alpha=0.4) +
  
  scale_x_date(date_breaks="1 day", date_labels="%b %d") +
  scale_size_manual(breaks=waiver(), values = c(5,3)) +
  scale_fill_manual(breaks=waiver(), values=c("black", "gray70")) +
  scale_colour_manual(breaks=waiver(), values=c("black", "gray70")) +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major.x = element_line(colour="gray80"),
        panel.grid.minor.x = element_blank())


# Calculate overall magnitude of differences between estimates
eventMeta_totals_testing.interp %>% 
  filter(year==2024 & infill_type=="ground truth") %>% 
  mutate(dif_interp.linear = chinook_fry_interp.linear - chinook_fry,
         dif_interp.stine = chinook_fry_interp.stine - chinook_fry,
         dif_kal.structs = chinook_fry_kal.structs - chinook_fry,
         dif_kal.arima = chinook_fry_kal.arima - chinook_fry,
         dif_MA.simp = chinook_fry_MA.simp - chinook_fry,
         dif_MA.linear = chinook_fry_MA.linear - chinook_fry,
         dif_MA.exp = chinook_fry_MA.exp - chinook_fry) %>%
  select(date_stop, chinook_fry, dif_interp.linear:dif_MA.exp) %>%
  pivot_longer(cols=c(dif_interp.linear:dif_MA.exp), names_to = "infill_method", values_to = "infill_value") %>%
  group_by(infill_method) %>%
  summarize(min=min(infill_value),
            max=max(infill_value),
            mean=mean(infill_value))



