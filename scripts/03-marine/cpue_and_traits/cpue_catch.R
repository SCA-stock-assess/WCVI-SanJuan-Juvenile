# Purse seine catch
# Feb 2026 



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Load catch data -----------------
setTotals <-  readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                   pattern = "^R_OUT - San Juan PSSI master database",
                                                   full.names = T),
                                 sheet = "set_totals") %>% 
  filter(grepl("mini purse seine", gear, ignore.case=T)) %>%
  mutate(species_simple = case_when(grepl("crab|dogfish|rockfish|flounder|sandfish|greenling", species, ignore.case=T) ~ "Other benthic species/groundfish",
                                    TRUE ~ species)) %>%
  janitor::clean_names() 


# Load set metadata -----------------
eventMeta <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                  pattern = "^R_OUT - San Juan PSSI master database",
                                                  full.names = T),
                                sheet = "sample_event_meta") %>% 
  filter(grepl("mini purse seine", gear, ignore.case=T), crew != "Huu-ay-aht") %>%
  janitor::clean_names() %>%
  mutate_at("date_start", as.Date)


# Join catch, set and statweek data -----------------
setTotals.Meta <- left_join(setTotals, 
                            eventMeta %>%
                              select(usid, site_name_clean, lat_dd, long_dd)) %>% 
  left_join(.,
            readxl::read_excel(here::here("data", "stat_weeks.xlsx")) %>%
              mutate_at("statWeek", as.character)) 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ===================== CALCULATE DAILY/SITE CPUE ===================== 

# Examine daily site effort ------------
day_site_sets <- eventMeta %>% 
  group_by(date_start, site_name_clean) %>% 
  summarize(n=n()) %>%
  print()

# Look at sites with >1 set
day_site_sets %>%
  filter(n>1)

# Botched sets: 
# - 07Jul23-PRS-Gordon R-1
# Other set was fine so need to sum and then need to divide by effort for each day/site


# Calculate Effort -------------
# Stat week and site
effort <- setTotals.Meta %>% 
  filter(usid !="07Jul23-PRS-Gordon R-1") %>% 
  group_by(year, statWeek, site_name_clean) %>% 
  summarize(usid = unique(usid)) %>%
  group_by(year, statWeek, site_name_clean) %>%
  summarize(effort_sets=n(), usid=usid) %>%
  print()


# Calculate CPUE -------------
CPUE <- setTotals.Meta %>% 
  group_by(year, statWeek, site_name_clean, species_simple) %>% 
  summarize(usid = unique(usid),
            total_catch = sum(total_caught_excl_recaps, na.rm=T)) %>%
  left_join(.,
            effort) %>%
  mutate(CPUE = total_catch/effort_sets) %>%
  print()


## Plot -----

CPUE$statWeek <- factor(CPUE$statWeek, levels=c("05-1", "05-2", "05-3", "05-4", "05-5", "06-1", "06-2", "06-3", "06-4", "07-1", "07-2", "07-3",
                                                "07-4", "08-1", "08-2", "08-3", "08-4", "08-5", "09-1", "09-2", "09-3", "09-4", "10-1", "10-2",
                                                "10-3", "10-4", "10-5"), ordered=T)

ggplot() +
  geom_bar(data = CPUE %>%
             filter(!is.na(species_simple), CPUE>0) %>%
             group_by(site_name_clean, statWeek, species_simple) %>%
             summarize(mean_CPUE = mean(CPUE, na.rm=T)),
           aes(x=statWeek, y=mean_CPUE, fill=species_simple, colour=species_simple), stat="identity", position="stack", alpha=0.8, linewidth=1) +
  scale_fill_manual(values=c("Anchovy" = "#ff3366",
                             "Chinook" = "#91ebec",
                             #"Chinook (natural)" = "#7caed1",
                             "Chum" = "#b3de69",
                             #"Coho alevin" = "#ffffb3",
                             #"Coho fry (sub-yearling)" = "#fdb462",
                             "Coho" = "#fb8072",
                             "Herring" = "#940317",
                             "Other benthic species/groundfish" = "gray90",
                             "Perch" = "#955230",
                             "Sandlance" = "#cdc1ff",
                             "Sardine" = "#2e5aac",
                             "Smelts" = "#e57a00",
                             "Sockeye" = "#f9d251",
                             "Steelhead" = "#ccff00"
                             )) +
  scale_colour_manual(values=c("Anchovy" = "#ff3366",
                             "Chinook" = "#91ebec",
                             #"Chinook (natural)" = "#7caed1",
                             "Chum" = "#b3de69",
                             #"Coho alevin" = "#ffffb3",
                             #"Coho fry (sub-yearling)" = "#fdb462",
                             "Coho" = "#fb8072",
                             "Herring" = "#940317",
                             "Other benthic species/groundfish" = "gray90",
                             "Perch" = "#955230",
                             "Sandlance" = "#cdc1ff",
                             "Sardine" = "#2e5aac",
                             "Smelts" = "#e57a00",
                             "Sockeye" = "#f9d251",
                             "Steelhead" = "#ccff00"
  )) +
  labs(x="Stat week (month-week)", y="Mean CPUE (catch per set)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=21),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=25),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = c(0.72, 0.1)) +
  guides(fill=guide_legend(ncol=2)) +
  facet_wrap(~site_name_clean, scales="free_y")



#
















# ** HERE NEXT DAY
# - calculate CPUE by stat week/site
# - make CPUE catch plot (below)




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


