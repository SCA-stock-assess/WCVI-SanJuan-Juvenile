# Purse seine catch
# Feb 2026 



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
setTotals <-  readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                   pattern = "^R_OUT - San Juan PSSI master database",
                                                   full.names = T),
                                 sheet = "set_totals") %>% 
  filter(grepl("mini purse seine", gear)) %>%
  janitor::clean_names()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ===================== CALCULATE DAILY/SITE CPUE ===================== 

# ** HERE NEXT DAY
# - probably need to load in event_meta and join to setTotals for site names etc.
# - also link to stat weeks
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


