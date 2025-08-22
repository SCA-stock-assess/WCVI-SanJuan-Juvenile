# San Juan 2024 RST 
# 2023-2025


# ========================= SET UP =========================

# Load libraries -------------------
library(tidyverse)

# Load helpers -------------------
"%notin%" <- Negate("%in%")

# Run source() if new genetic results -------------------
source(here::here("scripts", "misc-helpers", "gsiCompile.R"))




# ========================= LOAD JUVENILE DATA =========================

# Sample events ----------------- 
eventMeta <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/San Juan PSSI master database.xlsx",
  #                  sheet="sample_event_meta")
  readxl::read_excel(path=here::here("data", "juvenile", "R_OUT - San Juan PSSI master database LINKED.xlsx"),
                     sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear))


# Environmentals ----------------- 
enviros <- readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/San Juan PSSI master database.xlsx",
                    sheet="enviro") %>% 
  filter(grepl("RST|IPT", usid, ignore.case=T))
  
  
# Catch totals ----------------- 
setTotals <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/San Juan PSSI master database.xlsx",
  #                    sheet="set_totals")
  readxl::read_excel(path=here::here("data", "juvenile", "R_OUT - San Juan PSSI master database LINKED.xlsx"),
                     sheet="set_totals") %>% 
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
release <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/San Juan PSSI master database.xlsx",
  #                    sheet="mark-release")
  readxl::read_excel(path=here::here("data", "juvenile", "R_OUT - San Juan PSSI master database LINKED.xlsx"),
                     sheet="mark-release")



# Biosampling ----------------- 
biosamp.LINKED <- #readxl::read_excel(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/San Juan PSSI master database.xlsx",
  #                  sheet="biosampling")
  readxl::read_excel(path=here::here("data", "juvenile", "R_OUT - San Juan PSSI master database LINKED.xlsx"),
                     sheet="biosampling-LINKED") %>% 
  filter(grepl("RST|IPT", gear)) %>%
  mutate(species = stringr::str_to_lower(species),
         life_stage = stringr::str_to_lower(life_stage))




# ========================= LOAD HYDROMET DATA =========================
# Load hydromet data eventually  - can i call in existing code from the compendium?


# ========================= LOAD HATCHERY DATA =========================
# Load hatchery releases - can i call in existing code from the compendium?




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ========================= RST OPERATIONS =========================

# RPMs ---------------------
# eventMeta %>%
#   group_by(year) %>% 
#   summarize(minRPM = min())

# ** TO DO: still need to copy enviro info into "db"








         
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                 


# ======================= CATCH: ALL SPECIES, ALL YEARS =======================

# Species slightly simplified ---------------------
plot.rstCatchFig <- 
ggplot() +
  geom_bar(data=setTotals %>%
             filter(!is.na(species) & species!="unknown") %>%
             group_by(year, DOY, #species_stage_simple
                      species_simple) %>%
             summarize(n=sum(total_caught_excl_recaps, na.rm=T)) ,
           aes(x=as.Date(DOY, origin="2023-12-31"), y=n,
               fill=stringr::str_to_sentence(species_simple), colour=stringr::str_to_sentence(species_simple)),
           stat="identity", alpha=0.7,
           position=position_stack(vjust=0.5)) +
  # ggrepel::geom_text_repel(data=setTotals %>%
  #                            filter(!is.na(species) & species!="unknown") %>%
  #                            group_by(year, DOY, species_stage_simple) %>%
  #                            summarize(n=sum(total_caught_excl_recaps, na.rm=T)) %>%
  #                            filter(grepl("coho|chum|chinook", species_stage_simple, ignore.case=T), n<10),
  #                          aes(label=stringr::str_to_sentence(paste0(species_stage_simple, " (", n, ")")),
  #                              x=as.Date(DOY, origin="2023-12-31"),
  #                              y=n-10,
  #                              colour=species_stage_simple),
  #                          position=position_stack(vjust=1),
  #                          size=2.5,
  #                          direction="y",
  #                          hjust=-0.5,
  #                          segment.size=1, segment.curvature=-0.1, segment.ncp=3, segment.angle=45, min.segment.length=unit(0, 'lines'),
  #                          box.padding=2,
  #                          force=0.5, force_pull=1,
  #                          na.rm=T, show.legend=F) +
  scale_x_date(date_labels="%b %d", date_breaks="3 day") +
  labs(x="", y="Total catch") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=8),
        axis.text = element_text(colour="black", size=8),
        axis.title = element_text(face="bold", size=9),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(2.5, "mm"), 
        legend.title = element_blank(),
        legend.position = c(0.8, 0.3),
        legend.background = element_rect(colour="black"),
        strip.text = element_text(size=8)) +
  guides(fill=guide_legend(ncol=2)) +
  facet_wrap(~year, ncol=1, scales="free_y")
  

# Save as PDF 
# pdf(file = here::here("outputs", "figures", "juvenile", "RST raw catch (bars).pdf"),   
#     width = 14, # The width of the plot in inches
#     height = 8.5) # The height of the plot in inches
# 
# plot.rstCatchFig
# 
# dev.off()






# ========================= MORTS =========================

# Range of mort rates by date+species/stage -------------------
setTotals %>%
  filter(!is.na(species_stage_simple)) %>%
  group_by(year, DOY, species_stage_simple) %>%
  summarize(total = sum(total_caught_excl_recaps, na.rm=T),
            morts = sum(`#_morts`, na.rm=T)) %>%
  mutate(mort_rate = morts/total) %>%
  filter(mort_rate > 0) 


# Range of mort rates by day  -------------------
setTotals %>%
  filter(!is.na(species_stage_simple)) %>%
  group_by(year, DOY) %>%
  summarize(total = sum(total_caught_excl_recaps, na.rm=T),
            morts = sum(`#_morts`, na.rm=T)) %>%
  mutate(mort_rate = morts/total) %>%
  filter(mort_rate > 0) %>%
  arrange(year, DOY, mort_rate) 


# Max, min, median mort rate -------------------
mortRateSummary <- setTotals %>%
  filter(!is.na(species_stage_simple), !is.na(DOY)) %>%
  group_by(year, DOY) %>%
  summarize(total = sum(total_caught_excl_recaps, na.rm=T),
            morts = sum(`#_morts`, na.rm=T)) %>%
  mutate(mort_rate = morts/total) %>%
  #filter(mort_rate > 0) %>%
  arrange(year, DOY, mort_rate) %>%
  group_by(year) %>%
  summarize(max_mort_rate = max(mort_rate),
            min_mort_rate = min(mort_rate),
            median_mort_rate = median(mort_rate),
            mean_mort_rate = mean(mort_rate)) %>%
  print()






# ========================= CORRECTING FOR MIS-ID CHINOOK/COHO?? =========================
misID <- biosamp.LINKED %>% 
  filter(grepl("F", DNA_vial)) %>%
  select(species, life_stage, length, weight, genetic_species) %>% 
  print()

# Maybe later when genetics return... 




# ========================= CHINOOK, COHO CUMULATIVE PROBABILITY FIGS  =========================
cumlSum <- setTotals %>%
  filter(grepl("chinook|chum|coho", species_simple, ignore.case=T), !grepl("hatchery", species_simple), !is.na(DOY)) %>%
  group_by(year, DOY, species_simple) %>%
  summarize(n = sum(total_caught_excl_recaps, na.rm=T)) %>%
  group_by(year, species_simple) %>%
  mutate(annual_spp_total = sum(n)) %>%
  arrange(year, DOY, species_simple) %>%
  group_by(year, species_simple) %>%
  mutate(cumlsum = cumsum(n),
         propn_of_cuml = cumlsum/annual_spp_total) %>%
  print()


plot.rstcumlOutmigration <-
ggplot() +
  geom_line(data=cumlSum,
            aes(x=as.Date(DOY, origin="2023-12-31"), y=propn_of_cuml, 
                colour=species_simple, linetype=as.factor(year)), size=1, alpha=0.8) +
  geom_point(data=cumlSum,
             aes(x=as.Date(DOY, origin="2023-12-31"), y=propn_of_cuml, 
                 colour=species_simple, fill=species_simple,  shape=as.factor(year)), size=3, alpha=0.8) +
  scale_x_date(date_breaks = "5 day", date_labels = "%b %d") +
  scale_shape_manual(values=c(22, 21, 25)) +
  scale_linetype_manual(values=c("dotted", "solid")) +
  labs(y = "Proportion of outmigration") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, colour="black", size=7),
        axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(6, "mm"), 
        legend.background = element_rect(colour="black"))


# ~ temp?? 








# ========================= CHINOOK ONLY GRAPH? =========================

# Smolts & fry & Hatchery releases? -----------------------

ggplot() +
  geom_line(data=setTotals %>% filter(species=="chinook"),
            aes(x=as.Date(DOY, origin="2023-12-31"), y=total_caught_excl_recaps, group=as.factor(year), colour=as.factor(year)), size=1, na.rm = T) +
  scale_x_date(date_labels = "%b %d", date_breaks="5 day") +
  labs(y="Total caught", x="") +
  theme_bw() 





############################################################################################################################################################

#                                             INFILLING FOR UNFISHED DAYS (2023-2025) & MARK RECAPTURE ESTIMATE 


# ========================= INFILLING =========================
# Do for chum, coho, chinook only (?)




# ========================= MARK-RECAP ESTIMATE (2024, 2025) =========================
# Do for chum, coho, chinook only (?)



############################################################################################################################################################

#                                                                     BIOSAMPLING



# ========================= SUMMARY STATS, ALL SPECIES, ALL YEARS =========================

# Body size histogram ------------------ 
ggplot() +
  geom_histogram(data=biosamp.LINKED %>%
                   filter(grepl("chinook|chum|coho", species, ignore.case=T)), 
                 aes(length), na.rm=T, bins=50, alpha=0.7) +
  theme_bw() +
  facet_wrap(~species, nrow=3)


plot.lengthDensity <- 
ggplot() +
  geom_density(data=biosamp.LINKED %>%
                 filter(grepl("chinook|chum|coho", species, ignore.case=T)), 
               aes(length, group=stringr::str_to_sentence(species), fill=stringr::str_to_sentence(species), 
                   colour=stringr::str_to_sentence(species)), 
               alpha=0.3, size=0.1) +
  scale_x_continuous(breaks=seq(20,150,by=10)) +
  labs(x="Field fork Length (mm)", y="Density") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size=6.5),
        legend.key.size = unit(2.5, "mm"), 
        legend.background = element_rect(colour="black"))


# Body size ~ time ------------------
plot.lengthTime <- 
ggplot() +
  stat_smooth(data=biosamp.LINKED %>%
               filter(grepl("chinook|chum|coho", species, ignore.case=T)) %>%
               group_by(#year, 
                 DOY, species) %>%
               summarize(meanFL = mean(length, na.rm=T),
                         sdFL = sd(length, na.rm=T)), 
             aes(x=as.Date(DOY, origin="2023-12-31"), y=meanFL, 
                 fill=stringr::str_to_sentence(species), colour=stringr::str_to_sentence(species)), 
             alpha=0.2, size=1) +
  geom_point(data=biosamp.LINKED %>%
               filter(grepl("chinook|chum|coho", species, ignore.case=T)) %>%
               group_by(#year, 
                 DOY, species) %>%
               summarize(meanFL = mean(length, na.rm=T),
                         sdFL = sd(length, na.rm=T)), 
             aes(x=as.Date(DOY, origin="2023-12-31"), y=meanFL, 
                 fill=stringr::str_to_sentence(species), colour=stringr::str_to_sentence(species)), 
             alpha=0.8, size=2, shape=21) +
  scale_x_date(date_breaks = "5 day", date_labels = "%b %d") +
  labs(x="", y="Field fork length (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, colour="black", size=7),
        axis.text = element_text(colour="black", size=7),
        axis.title = element_text(face="bold", size=8),
        #legend.title = element_blank(),
        #legend.text = element_text(size=6.5),
        #legend.key.size = unit(2.5, "mm"), 
        legend.position = "none"
        #legend.background = element_rect(colour="black")
        ) #+
  #facet_wrap(~year)








# Length x weight x condition factor ------------------ 






############################################################################################################################################################














