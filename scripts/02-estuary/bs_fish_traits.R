# RST fish data
# Feb 2026



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")
options(scipen=9999)


# Read in data -----------------
bs.biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling", guess_max=3000) %>%
  filter(grepl("beach seine", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv")) %>%
              mutate_at("statWeek", as.character)) %>%
  mutate(species_stage_simple = case_when(grepl("coho", species, ignore.case=T) ~ stringr::str_to_title(paste0(species, " ", life_stage)),
                                          grepl("chum", species, ignore.case=T) ~ "Chum",
                                          grepl("chinook", species, ignore.case=T) ~ stringr::str_to_title(paste0(ad_clip, " ", species)),
                                          TRUE ~ species)) 


bs.biodat.fish <- bs.biodat.fish %>% 
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("beach seine", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid") %>%
  relocate(site_name_clean, .after=usid) %>%
  print()

#rst.biodat.fish$statWeek <- factor(rst.biodat.fish$statWeek, levels=c("2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2", "4-3", "4-4",
#                                                                     "5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4"), ordered=T)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============== SAMPLE SUMMARY ===============

## Number of Chinook touched --------------
bs.biodat.fish %>%
  group_by(year) %>%
  summarize(n=n())

## Number of genetic samples --------------
# Collected
bs.biodat.fish %>%
  filter(!is.na(dna_vial)) %>%
  group_by(year) %>%
  summarize(n())

# Analyzed with results
bs.biodat.fish %>%
  filter(!is.na(dna_vial), !is.na(mgl_id_source), !grepl("no sample", mgl_notes, ignore.case=T)) %>%
  group_by(mgl_id_source) %>%
  summarize(n())


# =============== % AGREEMENT SPECIES ID ===============
bs.biodat.fish %>%
  filter(!is.na(dna_vial), !is.na(mgl_species)) %>%
  mutate(mgl_species2 = case_when(mgl_species=="chinook" | grepl("assume chinook", mgl_notes, ignore.case=T) ~ "chinook",
                                  mgl_species == "coho" ~ "Coho",
                                  mgl_species == "coho; chinook" | mgl_notes == "Species ID ambiguous, use caution with result" ~ paste0(mgl_species, " (genetics ambiguous)"),
                                  mgl_species == "chinook; pink" & mgl_notes == "Species ID ambiguous, use caution with result" ~ "Chinook (assumed)",
                                  TRUE ~ "FLAG")) %>%
  group_by(mgl_species2) %>%
  summarize(n=n())



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== AVG BODY TRAITS ===============
# All Chinook

write.csv(
  x=bs.biodat.fish %>% 
    filter(grepl("chinook", resolved_species, ignore.case=T)) %>%
    mutate_at(c("resolved_weight_g", "fork_length_mm"), as.numeric) %>%
    group_by(year, hatchery_origin) %>%
    #group_by(year) %>%
    summarize(meanL = round(mean(fork_length_mm, na.rm=T),2),
              seL = round(sd(fork_length_mm, na.rm=T) / sqrt(length(fork_length_mm)),2),
              mseL = paste0(meanL, " (", seL, ")"),
              
              meanH = round(mean(height_mm, na.rm=T),2),
              seH = round(sd(height_mm, na.rm=T) / sqrt(length(height_mm)),2),
              mseH = paste0(meanH, " (", seH, ")"),
              
              meanW = round(mean(resolved_weight_g, na.rm=T),2),
              seW = round(sd(resolved_weight_g, na.rm=T) / sqrt(length(resolved_weight_g)),2),
              mseW = paste0(meanW, " (", seW, ")"),
              
              meanK = round(mean(cond_k, na.rm=T),2),
              seK = round(sd(cond_k, na.rm=T) / sqrt(length(cond_k)),2),
              mseK = paste0(meanK, " (", seK, ")"),
              
              N = n()),
  
  file=here::here("outputs", "R_OUT - Beach seine length, weight, height, condition table.csv"),
  row.names=F)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============== STOCK COMPOSITION ===============

## All ------------ 
bs.biodat.fish %>%
  group_by(resolved_stock_id) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(total=sum(n))

## Identifiable ------------ 
bs.biodat.fish %>%
  filter(resolved_stock_id!="Unknown") %>%
  group_by(resolved_stock_id) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(total=sum(n))

## Identifiable ROLLUP ------------ 
bs.biodat.fish %>%
  filter(resolved_stock_id!="Unknown") %>%
  group_by(year, resolved_stock_origin_rollup) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(total=sum(n))


### Plot ---- 
pdf(file = here::here("outputs", "figures", "stock comps", "Estuary stock composition (by SITE).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data = bs.biodat.fish %>%
             filter(resolved_stock_id!="Unknown") %>%
             mutate(resolved_stock_origin_rollup2 = case_when(resolved_stock_origin_rollup %in% c("Natural San Juan River", "Natural (assumed) San Juan River") ~ "Natural San Juan",
                                                              resolved_stock_origin_rollup %in% c("Hatchery San Juan (River)", "Hatchery San Juan River") ~ "Hatchery San Juan",
                                                              resolved_stock_origin_rollup %in% c("Natural Sooke/Nitinat", "Natural (assumed) Sooke/Nitinat") ~ "Natural Sooke/Nitinat",
                                                              TRUE ~ resolved_stock_origin_rollup),
                    arm = case_when(site_name_clean %in% c("BS03", "BS14", "BS03B", "BS17", "BS06", "BS07", "BS24") ~ "North arm",
                                    TRUE ~ "South arm")) %>%
             group_by(site_name_clean, arm, resolved_stock_origin_rollup2) %>%
             summarize(n=n()) %>% 
             group_by(site_name_clean, arm) %>% 
             mutate(total=sum(n),
                    propn = n/total), 
           aes(x=site_name_clean, y=propn, fill=resolved_stock_origin_rollup2, colour=resolved_stock_origin_rollup2), 
           stat="identity", alpha=0.8, linewidth=1, position="stack") +
  geom_label(data = bs.biodat.fish %>%
               filter(resolved_stock_id!="Unknown") %>%
               mutate(arm = case_when(site_name_clean %in% c("BS03", "BS14", "BS03B", "BS17", "BS06", "BS07", "BS24") ~ "North arm",
                                      TRUE ~ "South arm")) %>%
               group_by(arm, site_name_clean) %>%
               summarize(n=n()),
             aes(x=site_name_clean, y=-0.02, label=n), size=4.5) +
  scale_fill_manual(breaks=c("Hatchery San Juan", "Natural San Juan",
                             "Natural Sooke/Nitinat", "Natural Inner Barkley",
                             "Natural SWVI", "Natural US"), 
                    values=c("#66b466", "#99f299", 
                             "#f8ccf8", "#8080ef", "#ef8080", "#ffb142")) +
  scale_colour_manual(breaks=c("Hatchery San Juan", "Natural San Juan",
                               "Natural Sooke/Nitinat", "Natural Inner Barkley",
                               "Natural SWVI", "Natural US"), 
                      values=c("#66b466", "#99f299", 
                               "#f8ccf8", "#8080ef", "#ef8080", "#ffb142")) +  
  scale_y_continuous(labels=scales::percent_format()) +
  labs(x="Site name", y="Proportion of genetic samples", fill="Stock ID", colour="Stock ID") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="white", fill=alpha("white", 0.7)),
        plot.margin = unit(c(t=0.5, r=0.5, b=0, l=1),"cm"),
        strip.text = element_text(size=18))  +
  facet_wrap(~arm, scales="free_x")
  
dev.off()
