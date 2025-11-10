# Partial fullness index
# Nov 2025





# Set up -----------------
library(tidyverse)
library(leaflet)
"%notin%" <- Negate("%in%")


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
         condK = (as.numeric(weight)/(as.numeric(length)^3))*100000,
         month = lubridate::month(date, label=T, abbr=T),
         total_ww_g = case_when(lowest_taxon_final=="Empty" ~ 0,
                                TRUE ~ total_ww_g)) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  group_by(lethal_tag_no) %>%
  mutate(MT_status_fish = case_when(!is.na(lethal_tag_no) & MT_status=="Empty" ~ "Empty",
                                    !is.na(lethal_tag_no) & MT_status!="Empty" ~ "Not empty",
                                    TRUE ~ NA),
         total_ww_contents = sum(total_ww_g, na.rm=T)) %>%
  ungroup() %>%
  mutate(weight_no_contents = as.numeric(weight) - total_ww_contents,
         PFI = total_ww_g/weight_no_contents) %>%
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
                               TRUE ~ taxonomy_simple)) %>%
  relocate(site_name_clean, .after=usid)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


mean_by_month <- all.biodat.diet %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 TRUE ~ gear)) %>%
  filter(!is.na(source1), !is.na(taxonomy_simple), taxonomy_simple!="No sample", MT_status!="Empty", !is.na(month)) %>%
  filter(taxonomy_simple %notin% c("Plastic", "Plant/seaweed", "Non-food")) %>%
  group_by(gear_simple, hatchery_origin, lethal_tag_no, PFI_group) %>%
  summarize(month = unique(month),
            prey_PFI = sum(PFI, na.rm=T)) %>%
  ungroup() %>%
  complete(., lethal_tag_no, PFI_group, fill=list(prey_PFI=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(gear_simple, month, hatchery_origin), .direction = "updown") %>%
  group_by(hatchery_origin, PFI_group) %>%
  summarize(mean_PFI = mean(prey_PFI, na.rm = T),
            month=month,
            gear_simple=gear_simple)


ggplot() +
  geom_bar(data=mean_by_month %>% 
             filter(hatchery_origin=="Y"),
           aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack", alpha=0.8) +
  labs(title = "Hatchery-origin Chinook") +
  scale_fill_manual(breaks = waiver(), values=c("salmon", "orange", "light green", "teal", "#6fb878"))
  theme_bw() +
  facet_wrap(~gear_simple)


mean_by_month$gear_simple <- factor(mean_by_month$gear_simple, levels=c("RST", "Beach seine", "Mini purse seine"), ordered=T)

ggplot(data=mean_by_month %>% 
         filter(hatchery_origin=="N")) +
  geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack") +
  labs(title = "Natural-origin Chinook") +
  theme_bw() +
  facet_wrap(~gear_simple)


ggplot(data=mean_by_month %>% 
         filter(hatchery_origin=="U")) +
  geom_bar(aes(x=month, y=mean_PFI, fill=PFI_group, colour=PFI_group), stat="identity", position="stack") +
  labs(title = "Unknown-origin Chinook") +
  theme_bw() +
  facet_wrap(~gear_simple)









#******** NEXT DAY WORKING ON: 
# - PFI - refine calculation
# - want to look at BS taxa graph facet by hat/nat
# - want to look at PRS taxa graph facet by hat/nat
