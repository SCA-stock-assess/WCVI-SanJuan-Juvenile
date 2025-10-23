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
  summarize(n=n())  

# Exclude non-food and calculate % empty/not empty
bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample", MT_status!="Not prey") %>%
  group_by(year, MT_status) %>% 
  summarize(n=n()) %>% 
  group_by(year) %>%
  mutate(year_total = sum(n),
         propn = n/year_total) 



# Fullness plot (mirror Sarita) --------------
bs.fullness <- bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample", MT_status!="Not prey") %>%
  group_by(year, MT_status) %>% 
  summarize(n=n()) %>% 
  group_by(year) %>%
  mutate(year_total = sum(n),
         propn = n/year_total) %>%
  print()
bs.fullness$MT_status <- factor(bs.fullness$MT_status, levels=c("Functional prey items", "Trace", "Empty"), ordered=T)

pdf(file = here::here("outputs", "figures", "Beach seine stomach fullness (pooled).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=bs.fullness, aes(x=year, y=propn, fill=MT_status)) +
  geom_bar(stat="identity", position="stack", alpha=0.8) +
  geom_text(aes(x=year, y=1.03, label=year_total), inherit.aes=F, size=5) +
  scale_fill_manual(values=c("Functional prey items"="green", "Trace"="#FDB100", "Empty"="#ff7100")) +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(x="", y="Proportion of stomachs (%)", fill="Fullness status") +
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=17),
        axis.text = element_text(colour="black", size=15),
        legend.title = element_text(face="bold", size=13),
        legend.text = element_text(size=13),
        legend.position = c(0.85, 0.6),
        legend.background = element_rect(fill=alpha('white', 0.9))
        #legend.direction = "horizontal"
  )

dev.off()


bs.fullness_month <- bs.biodat.diet %>%
  filter(!is.na(lethal_tag_no), !is.na(taxonomy_simple), lethal_tag_no!="P9629", taxonomy_simple!="No sample", MT_status!="Not prey") %>%
  group_by(year, lubridate::month(date, label=T, abbr=T), MT_status) %>% 
  summarize(n=n()) %>% 
  rename(month=`lubridate::month(date, label = T, abbr = T)`) %>%
  group_by(year, month) %>%
  mutate(month_total = sum(n),
         propn = n/month_total)
bs.fullness_month$MT_status <- factor(bs.fullness_month$MT_status, levels=c("Functional prey items", "Trace", "Empty"), ordered=T)


pdf(file = here::here("outputs", "figures", "RST stomach fullness (monthly).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=bs.fullness_month, aes(x=month, y=propn, fill=MT_status)) +
  geom_bar(stat="identity", position="stack", alpha=0.8) +
  geom_text(aes(x=month, y=1.05, label=month_total), inherit.aes=F) +
  scale_fill_manual(values=c("Functional prey items"="green", "Trace"="#FDB100", "Empty"="#ff7100")) +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(x="", y="Proportion of stomachs (%)", fill="Fullness status") +
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=17),
        axis.text = element_text(colour="black", size=15),
        legend.title = element_text(face="bold", size=13),
        legend.text = element_text(size=13),
        legend.background = element_rect(fill=alpha('white', 0.9)),
        strip.text = element_text(size=15)
  ) +
  facet_wrap(~year)

dev.off()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# Prey SOURCES 
bs.biodat.diet$source1 <- factor(bs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", ordered=T))

ggplot(data=bs.biodat.diet %>% 
         filter(!is.na(source1)) %>%
         group_by(year, month, source1) %>%
         summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
         group_by(year, month) %>%
         mutate(propn=weight_by_source1/sum(weight_by_source1, na.rm=T))) +
  geom_bar(aes(x=month, y=propn, fill=source1, colour=source1), stat="identity", postion="stack") +
  scale_fill_manual(breaks=waiver(), values=c("dodger blue", "light green", "green", "dark green", "gray80",  "gray40")) +
  scale_colour_manual(breaks=waiver(), values=c("dodger blue", "light green", "green", "dark green", "gray80",  "gray40")) +
  theme_bw() +
  facet_wrap(~year)
  




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# Prey TAXA

# taxon simple, colour groups by mar/terr still?? 
