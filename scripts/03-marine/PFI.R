# Partial fullness index
# Nov 2025





# Set up -----------------
library(tidyverse)
library(leaflet)
"%notin%" <- Negate("%in%")
options(scipen = 9999999)


# Beach and purse seine data -----------------
all.biodat.diet <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                     pattern="^R_OUT - San Juan PSSI master database",
                                                     full.names = T),
                                     sheet="biosampling core results") %>%
  filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(MT_status = case_when(taxonomy_simple=="Empty" ~ "Empty",
                               grepl("trace|bdl|below detectable", diet_comments, ignore.case=T) ~ "Trace",
                               taxonomy_simple %notin% c("Empty", "Non-food") ~ "Functional prey items",
                               taxonomy_simple == "Non-food" ~ "Not prey",
                               TRUE ~ "FLAG"),
         month = lubridate::month(date, label=T, abbr=T),
         total_ww_g = case_when(lowest_taxon_final=="Empty" ~ 0,
                                TRUE ~ total_ww_g)) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  group_by(lethal_tag_no) %>%
  mutate(MT_status_fish = case_when(!is.na(lethal_tag_no) & MT_status=="Empty" ~ "Empty",
                                    !is.na(lethal_tag_no) & MT_status!="Empty" ~ "Not empty",
                                    TRUE ~ NA),
         total_stomach_contents_g = sum(total_ww_g, na.rm=T)) %>%
  ungroup() %>%
  mutate(fish_weight_excl_stomCont = as.numeric(resolved_weight_g) - total_stomach_contents_g,
         PFI = total_ww_g/fish_weight_excl_stomCont) %>%
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid") %>%
  mutate(PFI_group = case_when(grepl("flies|beetles|butterflies|insect|midge|bug|wasp", taxonomy_simple, ignore.case=T) ~ "Insects",
                               grepl("arachnids|lice|centipede", taxonomy_simple, ignore.case=T) ~ "Other terrestrial invertebrates",
                               grepl("worm", taxonomy_simple, ignore.case=T) ~ "Worms (incl polychaete)",
                               grepl("crustaceans", taxonomy_simple, ignore.case=T) ~ "Other crustaceans",
                               grepl("arthropod", taxonomy_simple, ignore.case=T) ~ "Other arthropods",
                               grepl("Invertebrates (unspecified)", taxonomy_simple, ignore.case=T) ~ "Other invertebrates",
                               TRUE ~ taxonomy_simple)) %>%
  relocate(site_name_clean, .after=usid) %>%
  ungroup()




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PARTIAL FULLNESS INDEX: ALL FISH ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_ALL <- all.biodat.diet %>%
  filter(lethal_tag_no != "SJ25-071") %>%          # this sample is the one that had 2.2g of barnacles. 
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 grepl("purse seine", gear, ignore.case=T) ~ "Purse seine",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", !is.na(month),
         grepl("chinook", species, ignore.case=T)) %>%  #MT_status!="Empty",
  filter(taxonomy_simple %notin% c("Plastic", "Non-food")) %>%     #"Plant/seaweed", 
  group_by(lethal_tag_no, PFI_group) %>%
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%
  print()


## Sample size of fish for the label -----------------
meanPFI_by_month_ALLn <- all.biodat.diet %>%
   filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", !is.na(month), 
          grepl("chinook", species, ignore.case=T)) %>%  #MT_status!="Empty", 
   filter(taxonomy_simple %notin% c("Plastic", "Non-food")) %>%   # "Plant/seaweed",
  group_by(hatchery_origin, lethal_tag_no) %>%
  summarize(n=n()) %>%
  group_by(hatchery_origin) %>%
  summarize(n=n())



### PLOT All PFI ~ hatchery origin, month, gear -----------------
meanPFI_by_month_ALL$gear_simple <- factor(meanPFI_by_month_ALL$gear_simple, levels=c("RST", "Beach seine", "Purse seine"), ordered=T)





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PARTIAL FULLNESS INDEX: SAN JUAN ONLY ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_SJ <- all.biodat.diet %>%
  filter(stray_status=="local") %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", !is.na(month), 
         grepl("chinook", species, ignore.case=T)) %>%  #MT_status!="Empty", 
  filter(taxonomy_simple %notin% c("Plastic",  "Non-food")) %>%
  group_by(lethal_tag_no, PFI_group) %>%
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%
  print()


## Sample size of fish for the label -----------------
meanPFI_by_month_SJn <- all.biodat.diet %>%
  filter(stray_status=="local", !is.na(lethal_tag_no)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", !is.na(month)) %>%  #MT_status!="Empty", 
  filter(taxonomy_simple %notin% c("Plastic", "Non-food")) %>%
  group_by(hatchery_origin, lethal_tag_no) %>%
  summarize(n=n()) %>%
  group_by(hatchery_origin) %>%
  summarize(n=n()) %>%
  print()




### PLOT San Juan PFI ~ origin, month, gear -----------------
meanPFI_by_month_SJ$gear_simple <- factor(meanPFI_by_month_SJ$gear_simple, levels=c("RST", "Beach seine", "Mini purse seine"), ordered=T)

ggpubr::ggarrange(
  ggplot() +
    geom_bar(data=meanPFI_by_month_SJ %>% 
               filter(hatchery_origin=="Y"),
             aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group, group=PFI_group), stat="identity", position="stack", alpha=0.8,
             linewidth=1) +
    scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                 "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                 "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                 "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                 "Plant/seaweed",  "Unidentified remains"),
                      values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                               "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                               "#9acda0", "#9fe375", "#fff700", "#ffa900",
                               "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                               "gray80",  "gray20")) +
    scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                   "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                   "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                   "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                   "Plant/seaweed",  "Unidentified remains"),
                        values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                                 "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                                 "#9acda0", "#9fe375", "#fff700", "#ffa900",
                                 "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                                 "gray80",  "gray20")) +
    labs(title = paste0("Hatchery-origin Chinook (San Juan), n=", meanPFI_by_month_SJn[meanPFI_by_month_SJn$hatchery_origin=="Y",]$n), 
         y="Mean Partial Fullness Index") +  theme_bw() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(face="bold"), 
          axis.text = element_text(colour="black") ,
          strip.background = element_rect(colour="transparent")) +
    facet_wrap(~gear_simple, nrow=2, strip.position = "right"),
  
  
  ggplot(data=meanPFI_by_month_SJ %>% 
           filter(hatchery_origin=="N")) +
    geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack") +
    scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                 "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                 "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                 "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                 "Plant/seaweed",  "Unidentified remains"),
                      values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                               "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                               "#9acda0", "#9fe375", "#fff700", "#ffa900",
                               "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                               "gray80",  "gray20")) +
    scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                   "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                   "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                   "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                   "Plant/seaweed",  "Unidentified remains"),
                        values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                                 "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                                 "#9acda0", "#9fe375", "#fff700", "#ffa900",
                                 "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                                 "gray80",  "gray20")) +
    labs(title = paste0("Natural-origin Chinook (San Juan), n=", meanPFI_by_month_SJn[meanPFI_by_month_SJn$hatchery_origin=="N",]$n), 
         y="Mean Partial Fullness Index") +  theme_bw() +    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(face="bold"), 
          axis.text = element_text(colour="black") ,
          strip.background = element_rect(colour="transparent")) +
    facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
  
  nrow = 2, common.legend = T,
  legend = "right"
)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PARTIAL FULLNESS INDEX: STRAYS ONLY ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_stray <- all.biodat.diet %>%
  filter(stray_status=="stray", !is.na(resolved_stock_id)) %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", MT_status!="Empty", !is.na(month),
         grepl("chinook", species, ignore.case=T)) %>%
  filter(taxonomy_simple %notin% c("Plastic", "Non-food")) %>%
  group_by(lethal_tag_no, PFI_group) %>%
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%
  print()


## Sample size of fish for the label -----------------
meanPFI_by_month_strayn <- all.biodat.diet %>%
  filter(stray_status=="stray", !is.na(lethal_tag_no), !is.na(resolved_stock_id)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", !is.na(month), 
         grepl("chinook", species, ignore.case=T)) %>%  #MT_status!="Empty", 
  filter(taxonomy_simple %notin% c("Plastic", "Non-food")) %>%
  group_by(hatchery_origin, lethal_tag_no) %>%
  summarize(n=n()) %>%
  group_by(hatchery_origin) %>%
  summarize(n=n()) %>%
  print()



### PLOT Stray PFI ~ origin, month, gear -----------------
meanPFI_by_month_stray$gear_simple <- factor(meanPFI_by_month_stray$gear_simple, levels=c("RST", "Beach seine", "Mini purse seine"), ordered=T)

ggpubr::ggarrange(
  # ggplot() +
  #   geom_bar(data=meanPFI_by_month_stray %>% 
  #              filter(hatchery_origin=="Y"),
  #            aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group, group=PFI_group), stat="identity", position="stack", alpha=0.8,
  #            linewidth=1) +
  #   scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
  #                                "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
  #                                "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
  #                                "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
  #                                "Plant/seaweed",  "Unidentified remains"),
  #                     values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
  #                              "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
  #                              "#9acda0", "#9fe375", "#fff700", "#ffa900",
  #                              "#f57407", "#ce2000", "#ff827b", "#ffcdff",
  #                              "gray80",  "gray20")) +
  #   scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
  #                                  "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
  #                                  "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
  #                                  "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
  #                                  "Plant/seaweed",  "Unidentified remains"),
  #                       values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
  #                                "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
  #                                "#9acda0", "#9fe375", "#fff700", "#ffa900",
  #                                "#f57407", "#ce2000", "#ff827b", "#ffcdff",
  #                                "gray80",  "gray20")) +
  #   labs(title = paste0("Hatchery-origin Chinook (strays), n=", meanPFI_by_month_strayn[meanPFI_by_month_strayn$hatchery_origin=="Y",]$n), 
  #        y="Mean Partial Fullness Index (g)") +  theme_bw() +
  #   theme_bw() +
  #   theme(axis.title.x = element_blank(),
  #         axis.title = element_text(face="bold"), 
  #         axis.text = element_text(colour="black") ,
  #         strip.background = element_rect(colour="transparent")) +
  #   facet_wrap(~gear_simple, nrow=2, strip.position = "right"),
  
  
  ggplot(data=meanPFI_by_month_stray %>% 
           filter(hatchery_origin=="N")) +
    geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack") +
    scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                 "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                 "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                 "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                 "Plant/seaweed",  "Unidentified remains"),
                      values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                               "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                               "#9acda0", "#9fe375", "#fff700", "#ffa900",
                               "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                               "gray80",  "gray20")) +
    scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
                                   "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
                                   "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
                                   "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
                                   "Plant/seaweed",  "Unidentified remains"),
                        values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
                                 "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
                                 "#9acda0", "#9fe375", "#fff700", "#ffa900",
                                 "#f57407", "#ce2000", "#ff827b", "#ffcdff",
                                 "gray80",  "gray20")) +
    labs(title = paste0("Natural-origin Chinook (strays), n=", meanPFI_by_month_strayn[meanPFI_by_month_strayn$hatchery_origin=="N",]$n), 
         y="Mean Partial Fullness Index") +  theme_bw() +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(face="bold"), 
          axis.text = element_text(colour="black") ,
          strip.background = element_rect(colour="transparent")) +
    facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
  
  
  # ggplot(data=mean_by_month %>% 
  #          filter(hatchery_origin=="U")) +
  #   geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack") +
  #   scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
  #                                "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
  #                                "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
  #                                "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
  #                                "Unidentified remains"),
  #                     values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
  #                              "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
  #                              "#9acda0", "#9fe375", "#fff700", "#ffa900",
  #                              "#f57407", "#ce2000", "#ff827b", "#ffcdff",
  #                              "gray20")) +
  #   scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
  #                                  "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
  #                                  "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
  #                                  "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
  #                                  "Unidentified remains"),
  #                       values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
  #                                "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
  #                                "#9acda0", "#9fe375", "#fff700", "#ffa900",
  #                                "#f57407", "#ce2000", "#ff827b", "#ffcdff",
  #                                "gray20")) +
  #   labs(title = "Unknown-origin Chinook, n=63", y="Mean Partial Fullness Index (g)") +
  #   theme_bw() +
  #   theme(axis.title.x = element_blank(),
  #         axis.title = element_text(face="bold"), 
  #         axis.text = element_text(colour="black") ,
  #         strip.background = element_rect(colour="transparent")) +
  #   facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
  
  nrow = 2, common.legend = T,
  legend = "right"
)





#******** NEXT DAY WORKING ON: 
# - PFI - refine calculation
# - want to look at BS taxa graph facet by hat/nat
# - want to look at PRS taxa graph facet by hat/nat
