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
                                     sheet="biosampling long w diet") %>%
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
                               grepl("arachnids|lice|centipede", taxonomy_simple, ignore.case=T) ~ "Other terrestrial arthropods",
                               grepl("worm", taxonomy_simple, ignore.case=T) ~ "Worms (incl polychaete)",
                               grepl("crustaceans", taxonomy_simple, ignore.case=T) ~ "Crustaceans (unspecified)",
                               grepl("arthropod", taxonomy_simple, ignore.case=T) ~ "Arthropods (unspecified)",
                               grepl("Invertebrates (unspecified)", taxonomy_simple, ignore.case=T) ~ "Invertebrates (unspecified)",
                               TRUE ~ taxonomy_simple)) %>%
  relocate(site_name_clean, .after=usid) %>%
  ungroup()




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PFI: ALL CHINOOK ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_ALL <- all.biodat.diet %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 grepl("mini purse seine", gear, ignore.case=T) ~ "Port Renfrew purse seine",
                                 grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
         grepl("chinook", resolved_species, ignore.case=T), 
         !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      # limit PFI to only "logical" prey items
         lethal_tag_no != "SJ25-071") %>%                                                    # this sample is the one that had 2.2g of barnacles - typo from analyst. have to discard as unreliable data.           
  group_by(lethal_tag_no, PFI_group) %>%                                                     # group data by individual fish and diet group and summarize the data to extract unique months, gear type, hatchery origin, and total PFI per fish
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  mutate(PFI_category = "obs/trace") %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%                           # fill out the dataframe for all combinations of individual fish and diet group (needed to calculate average below). For any diet group that wasn't present, give it a value of 0 for the fullness index
  group_by(lethal_tag_no) %>%                                                                # group by individual fish
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%                    # fill out other missing variables that are currently NA since using complete() above
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%                               # group by hatchery origin, month, gear, and diet group
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%                                        # calculate the mean PFI across individuals (but within above groups)
  full_join(.,
            data.frame(month = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")),   # join to a list of all months for plotting
            by="month") %>%
  left_join(.,
            all.biodat.diet %>%                                                              # Section below simply calculates the number of samples for each group so that we can plot sample size labels (and re-joins it to the main table for ease of plotting)
              mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                             grepl("mini purse seine", gear, ignore.case=T) ~ "Port Renfrew purse seine",
                                             grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                             TRUE ~ gear)) %>%
              filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
                     grepl("chinook", resolved_species, ignore.case=T), 
                     !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      
                     lethal_tag_no != "SJ25-071") %>%
              group_by(month, gear_simple, hatchery_origin, lethal_tag_no) %>%
              summarize(n=n()) %>%
              group_by(month, gear_simple, hatchery_origin) %>%
              summarize(n=n()) %>%
              ungroup()) %>%
  print()




### Plot -----------------
meanPFI_by_month_ALL$gear_simple <- factor(meanPFI_by_month_ALL$gear_simple, levels=c("RST", "Beach seine", "Port Renfrew purse seine",
                                                                                      "Barkley Sound purse seine"), ordered=T)

meanPFI_by_month_ALL$month <- factor(meanPFI_by_month_ALL$month, levels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), 
                                    ordered=T)


pdf(file = here::here("outputs", "figures", "diet", "PFI - All Chinook (new).pdf"),   
    width = 18, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot() +
  geom_bar(data=meanPFI_by_month_ALL %>%
             filter(hatchery_origin != "Unknown", mean_PFI > 0), 
           aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group, group=PFI_group), stat="identity", position="stack", 
           alpha=0.7, linewidth=1) +
  geom_label(data = meanPFI_by_month_ALL %>%
               filter(hatchery_origin != "Unknown", mean_PFI >0) %>%
               group_by(gear_simple, month, hatchery_origin) %>%
               summarize(total_PFI = sum(mean_PFI, na.rm=T),
                         n=n) %>%
               group_by(gear_simple, month, hatchery_origin) %>%
               summarize(total_PFI = unique(total_PFI),
                         n= unique(n)),
             aes(x=month, y=total_PFI, label=n), size=4, vjust = -0.3) +
  scale_fill_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                               "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                               "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                               "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                               "Shrimps", "Worms (incl polychaete)", "Unidentified remains"),
                    values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                             "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                             "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                             "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                             "black",  "#ad00ff",
                             "#cecfd3")) +
  scale_colour_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                                 "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                                 "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                                 "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                                 "Shrimps", "Worms (incl polychaete)", "Unidentified remains"),
                      values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                               "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                               "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                               "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                               "black",  "#ad00ff",
                               "#cecfd3")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3)))+ 
  labs(y="Mean Partial Fullness Index") +  
  theme_bw() +
  ggh4x::facet_nested(rows=vars(gear_simple, hatchery_origin),
                      nest_line = TRUE,
                      strip = ggh4x::strip_nested(text_y = list(element_text(face="bold", color="black", size=13),
                                                                element_text(size=11)),
                                                  background_y = list(element_rect(fill="gray90"),
                                                                      element_rect(fill="white")),
                                                  by_layer_y = T)) +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face="bold",size=14), 
        axis.text = element_text(colour="black", size=12),
        strip.placement = "outside",                     
        strip.background = element_rect(fill="white", colour="white", linewidth=0),
        legend.text = element_text(size=12),
        legend.title = element_blank()) 

dev.off()








# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PFI: SAN JUAN ONLY ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_SJ <- all.biodat.diet %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 grepl("mini purse seine", gear, ignore.case=T) ~ "Port San Juan purse seine",
                                 grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
         grepl("chinook", resolved_species, ignore.case=T), stray_status=="local",           # only keeping LOCAL (SJ) fish this time
         !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      # limit PFI to only "logical" prey items
         lethal_tag_no != "SJ25-071") %>%                                                    # this sample is the one that had 2.2g of barnacles - typo from analyst. have to discard as unreliable data.           
  group_by(lethal_tag_no, PFI_group) %>%                                                     # group data by individual fish and diet group and summarize the data to extract unique months, gear type, hatchery origin, and total PFI per fish
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  mutate(PFI_category = "obs/trace") %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%                           # fill out the dataframe for all combinations of individual fish and diet group (needed to calculate average below). For any diet group that wasn't present, give it a value of 0 for the fullness index
  group_by(lethal_tag_no) %>%                                                                # group by individual fish
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%                    # fill out other missing variables that are currently NA since using complete() above
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%                               # group by hatchery origin, month, gear, and diet group
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%                                        # calculate the mean PFI across individuals (but within above groups)
  full_join(.,
            data.frame(month = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")),   # join to a list of all months for plotting
            by="month") %>%
  left_join(., 
            all.biodat.diet %>%                                                              # Section below simply calculates the number of samples for each group so that we can plot sample size labels (and re-joins it to the main table for ease of plotting)
              mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                             grepl("mini purse seine", gear, ignore.case=T) ~ "Port San Juan purse seine",
                                             grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                             TRUE ~ gear)) %>%
              filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
                     grepl("chinook", resolved_species, ignore.case=T), stray_status=="local",           # only keeping LOCAL (SJ) fish this time
                     !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      
                     lethal_tag_no != "SJ25-071") %>%
              group_by(month, gear_simple, hatchery_origin, lethal_tag_no) %>%
              summarize(n=n()) %>%
              group_by(month, gear_simple, hatchery_origin) %>%
              summarize(n=n()) %>%
              ungroup()) %>%  
  print()






### Plot (Figure xx) -----------------
meanPFI_by_month_SJ$gear_simple <- factor(meanPFI_by_month_SJ$gear_simple, levels=c("RST", "Beach seine", "Port San Juan purse seine",
                                                                                    "Barkley Sound purse seine"), ordered=T)

meanPFI_by_month_SJ$month <- factor(meanPFI_by_month_SJ$month, levels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), 
                                    ordered=T)


pdf(file = here::here("outputs", "figures", "diet", "PFI - San Juan (new).pdf"),   
    width = 12, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot() +
  geom_bar(data=meanPFI_by_month_SJ %>%
             filter(hatchery_origin!="Unknown", mean_PFI > 0), 
           aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group, group=PFI_group), stat="identity", position="stack",  
           alpha=0.7, linewidth=0.5, width=0.8) +
  geom_label(data = meanPFI_by_month_SJ %>%
              filter(hatchery_origin != "Unknown", mean_PFI > 0) %>%
              group_by(gear_simple, month, hatchery_origin) %>%
              summarize(total_PFI = sum(mean_PFI, na.rm=T),
                        n=n) %>%
               group_by(gear_simple, month, hatchery_origin) %>%
               summarize(total_PFI = unique(total_PFI),
                         n= unique(n)),
             aes(x=month, y=total_PFI, label=n), size=4.3, vjust = -0.2) +
  scale_fill_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                               "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                               "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                               "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                               "Shrimps",   "Worms (incl polychaete)", "Unidentified remains"),
                    values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                             "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                             "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                             "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                             "black",  "#ad00ff",
                             "#cecfd3")) +
  scale_colour_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                                 "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                                 "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                                 "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                                 "Shrimps",   "Worms (incl polychaete)", "Unidentified remains"),
                      values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                               "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                               "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                               "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                               "black",  "#ad00ff",
                               "#cecfd3")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) + 
  labs(y="Mean Partial Fullness Index") +  
  theme_bw() +
  ggh4x::facet_nested(rows=vars(gear_simple, hatchery_origin),
                      nest_line = TRUE,
                      strip = ggh4x::strip_nested(text_y = list(element_text(face="bold", color="black", size=15),
                                                                element_text(size=15)),
                                                  background_y = list(element_rect(fill="gray90"),
                                                                      element_rect(fill="white")),
                                                  by_layer_y = T)) +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face="bold", size=17), 
        axis.text = element_text(colour="black", size=16),
        strip.placement = "outside",                     
        strip.background = element_rect(fill="white", colour="white", linewidth=0),
        legend.text = element_text(size=15),
        legend.title = element_blank()) 
  
dev.off()







# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== PFI: STRAYS ONLY ===============================================


## Calculate mean PFI by gear, month, hatchery origin -----------------
meanPFI_by_month_stray <- all.biodat.diet %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 grepl("mini purse seine", gear, ignore.case=T) ~ "Port Renfrew purse seine",
                                 grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
         grepl("chinook", resolved_species, ignore.case=T), stray_status=="stray",           # only keeping STRAY fish this time
         !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      # limit PFI to only "logical" prey items
         lethal_tag_no != "SJ25-071") %>%                                                    # this sample is the one that had 2.2g of barnacles - typo from analyst. have to discard as unreliable data.           
  group_by(lethal_tag_no, PFI_group) %>%                                                     # group data by individual fish and diet group and summarize the data to extract unique months, gear type, hatchery origin, and total PFI per fish
  summarize(month = unique(month),
            gear_simple = unique(gear_simple), 
            hatchery_origin = unique(hatchery_origin), 
            prey_PFI = sum(PFI, na.rm=T)) %>%
  mutate(PFI_category = "obs/trace") %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%                           # fill out the dataframe for all combinations of individual fish and diet group (needed to calculate average below). For any diet group that wasn't present, give it a value of 0 for the fullness index
  group_by(lethal_tag_no) %>%                                                                # group by individual fish
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%                    # fill out other missing variables that are currently NA since using complete() above
  ungroup() %>%
  group_by(hatchery_origin, month, gear_simple, PFI_group) %>%                               # group by hatchery origin, month, gear, and diet group
  summarize(mean_PFI = mean(prey_PFI, na.rm = T)) %>%                                        # calculate the mean PFI across individuals (but within above groups)
  full_join(.,
            data.frame(month = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")),   # join to a list of all months for plotting
            by="month") %>%
  left_join(., 
            all.biodat.diet %>%                                                              # Section below simply calculates the number of samples for each group so that we can plot sample size labels (and re-joins it to the main table for ease of plotting)
              mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                             grepl("mini purse seine", gear, ignore.case=T) ~ "Port Renfrew purse seine",
                                             grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                             TRUE ~ gear)) %>%
              filter(!is.na(source1), !is.na(taxonomy_simple), !is.na(month),
                     grepl("chinook", resolved_species, ignore.case=T), stray_status=="stray",           # only keeping STRAY fish this time
                     !grepl("No sample|Plastic|Non-food|parasite", taxonomy_simple, ignore.case=T),      
                     lethal_tag_no != "SJ25-071") %>%
              group_by(month, gear_simple, hatchery_origin, lethal_tag_no) %>%
              summarize(n=n()) %>%
              group_by(month, gear_simple, hatchery_origin) %>%
              summarize(n=n()) %>%
              ungroup()) %>%  
  print()




### Plot -----------------
meanPFI_by_month_stray$gear_simple <- factor(meanPFI_by_month_stray$gear_simple, 
                                             levels=c("RST", "Beach seine", "Port Renfrew purse seine",
                                                      "Barkley Sound purse seine"), ordered=T)

meanPFI_by_month_stray$month <- factor(meanPFI_by_month_stray$month, levels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), 
                                    ordered=T)


pdf(file = here::here("outputs", "figures", "diet", "PFI - Strays (new).pdf"),   
    width = 18, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot() +
  geom_bar(data=meanPFI_by_month_stray %>%
             filter(hatchery_origin != "Unknown", mean_PFI > 0), 
           aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group, group=PFI_group), stat="identity", position="stack", 
           alpha=0.7, linewidth=1) +
  geom_label(data = meanPFI_by_month_stray %>%
               filter(hatchery_origin != "Unknown", mean_PFI > 0) %>%
               group_by(gear_simple, month, hatchery_origin) %>%
               summarize(total_PFI = sum(mean_PFI, na.rm=T),
                         n = n) %>%
               group_by(gear_simple, month, hatchery_origin) %>%
               summarize(total_PFI = unique(total_PFI),
                         n = unique(n)),
             aes(x=month, y=total_PFI, label=n), size=4, vjust=-0.3) +
  scale_fill_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                               "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                               "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                               "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                               "Shrimps",   "Worms (incl polychaete)", "Unidentified remains"),
                    values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                             "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                             "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                             "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                             "black",  "#ad00ff",
                             "#cecfd3")) +
  scale_colour_manual(breaks = c("Amphipods", "Arthropods (unspecified)", "Barnacles", "Collembola",
                                 "Copepods (non-parasitic)", "Crustaceans (unspecified)", "Decapods", "Fish",
                                 "Insects", "Invertebrates (unspecified)", "Isopods", "Non-food (plants, seaweed, rocks, feathers)",
                                 "Octopus (larvae)", "Ostracods",  "Other terrestrial invertebrates", "Parasites*", 
                                 "Shrimps",   "Worms (incl polychaete)", "Unidentified remains"),
                      values=c("#14c8aa", "#00bce4", "#9f204f", "#8b6b5a",
                               "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                               "#9fe375", "#f57407", "#1ba831", "#6b7280", 
                               "#e3faff", "#ffea3b", "#ff006f", "#f73921", 
                               "black",  "#ad00ff",
                               "#cecfd3")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3)))+ 
  labs(y="Mean Partial Fullness Index") +  
  theme_bw() +
  ggh4x::facet_nested(rows=vars(gear_simple, hatchery_origin),
                      nest_line = TRUE,
                      strip = ggh4x::strip_nested(text_y = list(element_text(face="bold", color="black", size=13),
                                                                element_text(size=11)),
                                                  background_y = list(element_rect(fill="gray90"),
                                                                      element_rect(fill="white")),
                                                  by_layer_y = T)) +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face="bold",size=14), 
        axis.text = element_text(colour="black", size=12),
        strip.placement = "outside",                     
        strip.background = element_rect(fill="white", colour="white", linewidth=0),
        legend.text = element_text(size=12),
        legend.title = element_blank()) 

dev.off()















# ggpubr::ggarrange(
#   ggplot(data=meanPFI_by_month_stray %>% 
#            filter(hatchery_origin=="Hatchery")) +
#     geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack", alpha=0.7) +
#     scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                  "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                  "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                  "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                  "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                       values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                "#6b7280", "#cecfd3")) +
#     scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                    "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                    "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                    "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                    "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                         values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                  "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                  "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                  "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                  "#6b7280", "#cecfd3")) +
#     labs(title = paste0("Hatchery-origin Chinook (strays), n=", 
#                         meanPFI_by_month_strayn[meanPFI_by_month_strayn$hatchery_origin=="Hatchery",]$n), 
#          y="Mean Partial Fullness Index") +  theme_bw() +
#     theme_bw() +
#     theme(axis.title.x = element_blank(),
#           axis.title = element_text(face="bold"), 
#           axis.text = element_text(colour="black") ,
#           strip.background = element_rect(colour="transparent")) +
#     facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
#   
#   
#   ggplot(data=meanPFI_by_month_stray %>% 
#            filter(hatchery_origin=="Natural")) +
#     geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack", alpha=0.7) +
#     scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                  "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                  "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                  "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                  "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                       values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                "#6b7280", "#cecfd3")) +
#     scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                    "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                    "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                    "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                    "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                         values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                  "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                  "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                  "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                  "#6b7280", "#cecfd3")) +
#     labs(title = paste0("Natural-origin Chinook (strays), n=", 
#                         meanPFI_by_month_strayn[meanPFI_by_month_strayn$hatchery_origin=="Natural",]$n), 
#          y="Mean Partial Fullness Index") +  theme_bw() +
#     theme_bw() +
#     theme(axis.title.x = element_blank(),
#           axis.title = element_text(face="bold"), 
#           axis.text = element_text(colour="black") ,
#           strip.background = element_rect(colour="transparent")) +
#     facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
#   
#   
#   ggplot(data=meanPFI_by_month_stray %>% 
#            filter(hatchery_origin=="Unknown")) +
#     geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack", alpha=0.7) +
#     scale_fill_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                  "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                  "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                  "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                  "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                       values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                "#6b7280", "#cecfd3")) +
#     scale_colour_manual(breaks = c("Barnacles", "Copepods", "Ostracods", "Decapods", 
#                                    "Octopus (larvae)", "Shrimps", "Amphipods", "Isopods", 
#                                    "Other crustaceans", "Worms (incl polychaete)", "Parasites*", "Fish",
#                                    "Insects", "Other terrestrial invertebrates", "Other arthropods",  "Other invertebrates", 
#                                    "Non-food (plants, seaweed, rocks, feathers)",  "Unidentified remains"),
#                         values=c("#2a4b7c", "#0000ff", "#039be5", "#a4efff",
#                                  "#e3faff", "#7fffd4", "#14c8aa", "#3d6643",
#                                  "#9acda0", "#9fe375", "#fff700", "#ffa900",
#                                  "#f57407", "#ce2000", "#ff827b", "#ffcdff",
#                                  "#6b7280", "#cecfd3")) +
#     labs(title = paste0("Unknown-origin Chinook (strays), n=", 
#                         meanPFI_by_month_strayn[meanPFI_by_month_strayn$hatchery_origin=="Unknown",]$n), 
#          y="Mean Partial Fullness Index") +  theme_bw() +
#     theme_bw() +
#     theme(axis.title.x = element_blank(),
#           axis.title = element_text(face="bold"), 
#           axis.text = element_text(colour="black"),
#           strip.background = element_rect(colour="transparent")) +
#     facet_wrap(~gear_simple, nrow=3, strip.position = "right"),
#   
#   nrow = 3, common.legend = T,
#   legend = "right"
# )


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============================================== NDMS PFI: SAN JUAN only ===============================================

## Set up ---------------
# Re-format
meanPFI_by_month_SJ.wide <- meanPFI_by_month_SJ %>%
  filter(hatchery_origin != "Unknown") %>%
  mutate(sqrt_PFI = sqrt(mean_PFI)) %>%
  select(-c(mean_PFI)) %>%
  pivot_wider(names_from = PFI_group, values_from = sqrt_PFI) %>%
  select(-c(n)) %>%
  print()

# Make a matrix
PFI_matrix <- data.matrix(meanPFI_by_month_SJ.wide)

# Run NMDS & assess
set.seed(33)
PFI.NMDS <- vegan::metaMDS(sqrt(PFI_matrix), k=2, trymax=100, distance="bray")  

vegan::stressplot(PFI.NMDS)
PFI.NMDS$stress


## Extract NMDS scores and plot ---------------
data.scores = as.data.frame(vegan::scores(PFI.NMDS)$sites)

# add columns to data frame: 
data.scores$hatchery_origin = meanPFI_by_month_SJ.wide$hatchery_origin
data.scores$gear_simple = meanPFI_by_month_SJ.wide$gear_simple
data.scores$month = meanPFI_by_month_SJ.wide$month

# Extract centroids
# origin_centroid <- 
#   data.scores %>% 
#   group_by(hatchery_origin) %>% 
#   summarise(axis1 = mean(NMDS1),
#             axis2 = mean(NMDS2)) %>% 
#   ungroup()

# Extract polygons
origin_hull <- 
  data.scores %>% 
  group_by(hatchery_origin) %>%
  slice(chull(NMDS1, NMDS2))

month_hull <- 
  data.scores %>% 
  group_by(month) %>%
  slice(chull(NMDS1, NMDS2))

gear_hull <- 
  data.scores %>% 
  group_by(gear_simple) %>%
  slice(chull(NMDS1, NMDS2))



### Plot (Figure xx) --------
data.scores$month <- factor(data.scores$month, levels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), ordered=T)
data.scores$gear_simple <- factor(data.scores$gear_simple, levels=c("RST", "Beach seine", "Port Renfrew purse seine", 
                                                                    "Barkley Sound purse seine", ordered=T))

pdf(file = here::here("outputs", "figures", "diet", "NMDS multi-plot.pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggpubr::ggarrange(
  ggplot() + 
    geom_polygon(data=origin_hull, 
                 aes(x=NMDS1, y=NMDS2, fill=hatchery_origin, group=hatchery_origin), 
                 alpha=0.20) +
    geom_point(data=data.scores, 
               aes(x=NMDS1, y=NMDS2, colour=hatchery_origin, fill=hatchery_origin), size=3, shape=21, alpha=0.8, stroke=1.5) +
    annotate("text", x=-0.32, y=0.28, label="A", fontface="bold", size=7) +
    annotate("text", x=0.35, y=0.28, label = paste0("2D Stress: ", round(PFI.NMDS$stress, 2)), size=6) +
    # geom_point(data = origin_centroid, 
    #            aes(x = axis1, y = axis2, color = hatchery_origin), 
    #            size = 5, shape = 17) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x="NMDS1", y="NMDS2")  + 
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=18), 
          legend.text = element_text(size=17), 
          legend.title = element_blank(),
          legend.key.spacing.y = unit(2, "mm")),
  
  ggplot() + 
    geom_point(data=data.scores, 
               aes(x=NMDS1, y=NMDS2, colour=str_wrap(gear_simple, width=14), fill=str_wrap(gear_simple, width=14)), 
               size=3, shape=21, alpha=0.8, stroke=1.5) + 
    geom_polygon(data=gear_hull, 
                 aes(x=NMDS1, y=NMDS2, fill=str_wrap(gear_simple, width=14), group=gear_simple), alpha=0.2) +
    annotate("text", x=-0.32, y=0.28, label="B", fontface="bold", size=7) +
    annotate("text", x=0.32, y=0.28, label = paste0("2D Stress: ", round(PFI.NMDS$stress, 2)), size=6) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_discrete(labels = c("Freshwater", "San Juan \nestuary", "Port San Juan \n(marine)", "Barkley Sound \n(marine)")) +
    scale_colour_discrete(labels = c("Freshwater", "San Juan \nestuary", "Port San Juan \n(marine)", "Barkley Sound \n(marine)")) +
    labs(x="NMDS1", y="NMDS2")  +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=18), 
          legend.text = element_text(size=16), 
          legend.title = element_blank(),
          legend.key.spacing.y = unit(3, "mm"),
          plot.margin = margin(5, 0, 5, 5)    #t, r, b, l
          ),
  
  
  ggplot() + 
    geom_point(data=data.scores, 
               aes(x=NMDS1, y=NMDS2, colour=month, fill=month), size=3, shape=21, alpha=0.8, stroke=1.5) + 
    geom_polygon(data=month_hull, 
                 aes(x=NMDS1, y=NMDS2, fill=month, group=month), alpha=0.2) +
    annotate("text", x=-0.32, y=0.28, label="C", fontface="bold", size=7) +
    annotate("text", x=0.35, y=0.28, label = paste0("2D Stress: ", round(PFI.NMDS$stress, 2)), size=6) +
    scale_fill_discrete (breaks=waiver()) +
    scale_colour_discrete (breaks=waiver()) +  
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    labs(x="NMDS1", y="NMDS2") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_text(size=18), 
          legend.text = element_text(size=17), 
          legend.title = element_blank(),
          plot.margin = margin(10, 41, 10, 5),    #t, r, b, l
          legend.key.spacing.y = unit(2, "mm"))  
  
)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ==== condition ~ prey weight ====

ggplot(data = all.biodat.diet %>%
         filter(stray_status=="local") %>%
         group_by(lethal_tag_no) %>%
         summarize(total_PFI = sum(PFI, na.rm=T),
                   total_prey_w = sum(total_ww_g),
                   cond_k = unique(cond_k))) +
  geom_point(aes(x=total_prey_w, y=cond_k))
  