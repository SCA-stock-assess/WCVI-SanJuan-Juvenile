# BEACH SEINE Diet work-up 
# Oct 2025



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
bs.biodat.diet <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling core results") %>%
  filter(grepl("beach", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(MT_status = case_when(taxonomy_simple=="Empty" ~ "Empty",
                               grepl("trace|bdl|below detectable", diet_comments, ignore.case=T) ~ "Trace",
                               taxonomy_simple %notin% c("Empty", "Non-food") ~ "Functional prey items",
                               taxonomy_simple == "Non-food" ~ "Not prey",
                               TRUE ~ "FLAG"),
         condK = (as.numeric(weight)/(as.numeric(length)^3))*100000,
         month = lubridate::month(date, label=T, abbr=T)) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  group_by(lethal_tag_no) %>%
  mutate(MT_status_fish = case_when(!is.na(lethal_tag_no) & MT_status=="Empty" ~ "Empty",
                                    !is.na(lethal_tag_no) & MT_status!="Empty" ~ "Not empty",
                                    TRUE ~ NA)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                        Data exploration 

# Lethal sample sizes by year --------------
bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple)) %>%
  group_by(year, lethal_tag_no) %>%
  summarize(n=n()) %>%
  group_by(year) %>%
  summarize(n=n())


# stomach content groups --------------
bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no)) %>%
  group_by(taxonomy_simple) %>% 
  summarize(n=n())

View(bs.biodat.diet %>% filter(!is.na(lethal_tag_no), is.na(taxonomy_simple)))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                         Fullness

# % empty stomachs vs some contents --------------
# Total inventory of empty/prey/non-food 
bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample") %>%
  group_by(year, MT_status) %>% 
  summarize(MT_status = unique(MT_status))  


# Exclude non-food and calculate % empty/not empty --------------
bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample", MT_status!="Not prey") %>%
  group_by(year, lethal_tag_no) %>% 
  summarize(MT_status_fish=unique(MT_status_fish)) %>% 
  group_by(year, MT_status_fish) %>%
  summarize(n = n()) %>%
  group_by(year) %>%
    mutate(year_total=sum(n),
           propn=n/year_total)


# Do we know anything about the fish with empty stomachs --------------
bs.biodat.diet %>%
  filter(MT_status_fish=="Empty") %>%
  select(mgl_id_source, mgl_top_collection)



# ================== PLOTS ================== 
# Not going to bother plotting because there were almost no empty stomachs and obviously the rest will have a mix of detectable and non-detectable parts. 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                                     Prey SOURCES 

# Prey sources as a % of total prey weight ------------
bs.biodat.diet %>% 
  filter(!is.na(source1)) %>%
  group_by(year, month, source1) %>%
  summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
  group_by(year, month) %>%
  mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T))


# Prey sources POOLED ------------
bs.biodat.diet$source1 <- factor(bs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", ordered=T))

pdf(file = here::here("outputs", "figures", "Estuary diets (yearly).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data=bs.biodat.diet %>% 
             filter(!is.na(source1)) %>%
             group_by(year, source1) %>%
             summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
             group_by(year) %>%
             mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T)),
           aes(x=as.factor(year), y=propn, fill=source1, colour=source1), stat="identity", postion="stack", alpha=0.8, linewidth =1) +
  geom_text(data=bs.biodat.diet %>% 
              filter(!is.na(source1), !is.na(lethal_tag_no)) %>%
              group_by(year, lethal_tag_no) %>%
              summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
              group_by(year) %>%
              summarize(n = n()), 
            aes(x=as.factor(year), y=1.05, label=n), size=5) +
  scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "#8bc7e1", "gray80",  "gray40")) +
  scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "#8bc7e1", "gray80",  "gray40")) +
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
bs.biodat.diet$source1 <- factor(bs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", ordered=T))

ggplot() +
  geom_bar(data=bs.biodat.diet %>% 
             filter(!is.na(source1)) %>%
             group_by(year, month, source1) %>%
             summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
             group_by(year, month) %>%
             mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T)),
           aes(x=month, y=propn, fill=source1, colour=source1), stat="identity", postion="stack", alpha=0.8, linewidth =1) +
  geom_text(data=bs.biodat.diet %>% 
              filter(!is.na(source1), !is.na(lethal_tag_no)) %>%
              group_by(year, month, lethal_tag_no) %>%
              summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
              group_by(year, month) %>%
              summarize(n = n()), 
            aes(x=month, y=1.1, label=n)) +
  scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "#8bc7e1", "gray80",  "gray40")) +
  scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "#8bc7e1", "gray80",  "gray40")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,1,by=0.1)) +
  labs(y="Proportion of all prey items", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face='bold'),
        axis.text = element_text(colour="black")) +
  facet_wrap(~year)





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# Prey TAXA

# taxon simple, colour groups by mar/terr still?? 
