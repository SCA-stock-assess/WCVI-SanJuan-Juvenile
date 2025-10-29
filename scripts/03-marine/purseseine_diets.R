# PURSE SEINE diet workup
# Oct 2025



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
prs.biodat.diet <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                     pattern="^R_OUT - San Juan PSSI master database",
                                                     full.names = T),
                                     sheet="biosampling core results") %>%
  filter(grepl("purse", gear, ignore.case=T)) %>%
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
         total_ww_contents = sum(total_ww_g)) %>%
  ungroup() %>%
  mutate(weight_no_contents = as.numeric(weight) - total_ww_contents,
         PFI = total_ww_g/weight_no_contents) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                                        Data exploration 

# Lethal sample sizes by year --------------
prs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple)) %>%
  group_by(year, lethal_tag_no) %>%
  summarize(n=n()) %>%
  group_by(year) %>%
  summarize(n=n())


# stomach content groups --------------
prs.biodat.diet %>%
  filter(!is.na(lethal_tag_no)) %>%
  group_by(taxonomy_simple) %>% 
  summarize(n=n())

View(prs.biodat.diet %>% filter(!is.na(lethal_tag_no), is.na(taxonomy_simple)))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                         Fullness

# % empty stomachs vs some contents --------------
# Total inventory of empty/prey/non-food 
prs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample") %>%
  group_by(year, MT_status) %>% 
  summarize(MT_status = unique(MT_status))  


# Exclude non-food and calculate % empty/not empty --------------
prs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample", MT_status!="Not prey") %>%
  group_by(year, lethal_tag_no) %>% 
  summarize(MT_status_fish=unique(MT_status_fish)) %>% 
  group_by(year, MT_status_fish) %>%
  summarize(n = n()) %>%
  group_by(year) %>%
  mutate(year_total=sum(n),
         propn=n/year_total)


# Do we know anything about the fish with empty stomachs --------------
prs.biodat.diet %>%
  filter(MT_status_fish=="Empty") %>%
  select(mgl_id_source, mgl_top_collection)



# ================== PLOTS ================== 
# Not going to bother plotting because there were almost no empty stomachs and obviously the rest will have a mix of detectable and non-detectable parts. 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                                     Prey SOURCES 

# Prey sources as a % of total prey weight ------------
prs.biodat.diet %>% 
  filter(!is.na(source1)) %>%
  group_by(year, month, source1) %>%
  summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
  group_by(year, month) %>%
  mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T))


# Prey sources POOLED ------------
prs.biodat.diet$source1 <- factor(prs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", "Human", ordered=T))

pdf(file = here::here("outputs", "figures", "Marine diets (yearly).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data=prs.biodat.diet %>% 
             filter(!is.na(source1)) %>%
             group_by(year, source1) %>%
             summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
             group_by(year) %>%
             mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T)),
           aes(x=as.factor(year), y=propn, fill=source1, colour=source1), stat="identity", postion="stack", alpha=0.8, linewidth =1) +
  geom_text(data=prs.biodat.diet %>% 
              filter(!is.na(source1), !is.na(lethal_tag_no)) %>%
              group_by(year, lethal_tag_no) %>%
              summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
              group_by(year) %>%
              summarize(n = n()), 
            aes(x=as.factor(year), y=1.05, label=n), size=5) +
  scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "black")) +
  scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "black")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,1,by=0.1)) +
  labs(y="Proportion of all prey items (by weight)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face='bold', size=19),
        axis.text = element_text(colour="black", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.position = c(0.85, 0.7),
        legend.background = element_rect(fill=alpha("white", alpha=0.9))) 

dev.off()




# Prey sources by MONTH ------------
# Not great because such small sample sizes, but made and will save in case there is interest. 
prs.biodat.diet$source1 <- factor(prs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", "Human", ordered=T))

pdf(file = here::here("outputs", "figures", "Estuary diets (monthly).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data=prs.biodat.diet %>% 
             filter(!is.na(source1)) %>%
             group_by(month, source1) %>%
             summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
             group_by(month) %>%
             mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T)),
           aes(x=month, y=propn, fill=source1, colour=source1), stat="identity", postion="stack", alpha=0.8, linewidth =1) +
  geom_text(data=prs.biodat.diet %>% 
              filter(!is.na(source1), !is.na(lethal_tag_no)) %>%
              group_by(month, lethal_tag_no) %>%
              summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
              group_by(month) %>%
              summarize(n = n()), 
            aes(x=month, y=1.05, label=n), size=5) +
  scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "black")) +
  scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "black")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,1,by=0.1)) +
  labs(y="Proportion of all prey items (by weight)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face='bold', size=19),
        axis.text = element_text(colour="black", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.position = c(0.17, 0.75),
        legend.background = element_rect(fill=alpha("white", alpha=0.9))) 

dev.off()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                            Prey TAXA

# Prey TAXA as a % of total prey weight ------------
prs.biodat.diet %>% 
  filter(!is.na(taxonomy_stage_simple), taxonomy_stage_simple!="Empty") %>%
  group_by(taxonomy_stage_simple) %>%
  summarize(weight_by_taxa = sum(total_ww_g, na.rm=T)) %>%
  ungroup() %>%
  mutate(propn = weight_by_taxa/sum(weight_by_taxa, na.rm=T))


pdf(file = here::here("outputs", "figures", "Marine diets (by taxonomy simplified - no stage).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data=prs.biodat.diet %>% 
             filter(!is.na(taxonomy_simple), taxonomy_simple %notin% c("Empty", "No sample", "Plastic")) %>%
             mutate(source_simple = case_when(taxonomy_simple=="Non-food" ~ "Non-food",
                                              source1=="Marine" ~ "Marine",
                                              source1 %in% c("Terrestrial", "Freshwater", "Terrestrial/Freshwater", "Aquatic") ~ "Terrestrial/Freshwater",
                                              
                                              source1=="Undetermined" ~ "Undetermined",
                                              source1=="Human" ~ "Human",
                                              TRUE ~ NA)) %>%
             group_by(taxonomy_simple) %>%
             summarize(source_simple = unique(source_simple), weight_by_taxa = sum(total_ww_g, na.rm=T)) %>%
             ungroup() %>%
             mutate(propn = weight_by_taxa/sum(weight_by_taxa, na.rm=T)),
           aes(x=fct_reorder(taxonomy_simple, propn, .desc = TRUE), y=propn, 
               fill=factor(source_simple, levels=c("Marine", "Terrestrial/Freshwater", "Non-food", "Undetermined", "Human", ordered=T)), 
               colour=factor(source_simple, levels=c("Marine", "Terrestrial/Freshwater", "Non-food", "Undetermined", "Human", ordered=T))), 
           stat="identity", position="dodge", alpha=0.7, linewidth=1) +
  scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "gray80",  "gray40", "black")) +
  scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "gray80", "gray40", "black")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,1,by=0.1)) +
  labs(x="Lowest identifiable prey group and life stage (if known)", y="Proportion of all prey items (by weight)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.position=c(0.8,0.8),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.7)))  

dev.off()

# **** FIX NEXT DAY: in purse seine samples, decapods should always be marine source! 
  # need to change this in the resultsCompile or maybe the diet file even?
# also consider amending (maybe just in plot above) to have insects, true flies == terr/fw; and amphipods, crustaceans==marine. 