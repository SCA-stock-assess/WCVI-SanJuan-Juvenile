# RST fish data
# Oct 2025



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
rst.biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                 pattern="^R_OUT - San Juan PSSI master database",
                                                 full.names = T),
                                 sheet="biosampling detailed w GSI") %>%
  filter(grepl("RST|IPT", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(condK = (as.numeric(weight)/(as.numeric(length)^3))*100000) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  print()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# Average body traits --------------- 
rst.biodat.fish %>% 
  filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
  mutate_at(c("weight", "length"), as.numeric) %>%
  group_by(year) %>%
  summarize(meanW = mean(weight, na.rm=T),
            seW = sd(weight, na.rm=T) / sqrt(length(weight)),
            meanL = mean(length, na.rm=T),
            seL = sd(length, na.rm=T) / sqrt(length(length)),
            meanK = mean(condK, na.rm=T),
            seK = sd(condK, na.rm=T) / sqrt(length(condK)))


# Weight ---------------  
pdf(file = here::here("outputs", "figures", "RST Chinook by statweek - weight.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=rst.biodat.fish %>% 
         mutate_at("weight", as.numeric) %>%
         filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
         group_by(year, statWeek) %>%
         summarize(meanW = mean(weight, na.rm=T),
                   seW = sd(weight, na.rm=T) / sqrt(length(weight)),
                   n=n()) %>%
         group_by(year) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))) +
  geom_errorbar(aes(x=statWeek, ymin=meanW-seW, ymax=meanW+seW, colour=as.factor(label)), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanW), fill=as.factor(label), colour=as.factor(label)), shape=21, size=7, alpha=0.8) +
  scale_y_continuous(limits=c(0,5)) +
  labs(y="Mean weight \u00B1 SE (g)", x="Month - week") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "top",
        legend.key.spacing.x = unit(2, "cm"))

dev.off()


# Length ---------------  
pdf(file = here::here("outputs", "figures", "RST Chinook by statweek - length.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=rst.biodat.fish %>% 
         mutate_at("length", as.numeric) %>%
         filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
         group_by(year, statWeek) %>%
         summarize(meanL = mean(length, na.rm=T),
                   seL = sd(length, na.rm=T) / sqrt(length(length)),
                   n=n()) %>%
         group_by(year) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))
       ) +
  geom_errorbar(aes(x=statWeek, ymin=meanL-seL, ymax=meanL+seL, colour=as.factor(label)), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanL), fill=as.factor(label), colour=as.factor(label)), shape=21, size=7, alpha=0.8) +
  scale_y_continuous(limits=c(0,80)) +
  labs(y="Mean fork length \u00B1 SE (mm)", x="Month - week") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "top",
        legend.key.spacing.x = unit(2, "cm")
        ) +
  guides(fill = guide_legend(bycol = TRUE), colour=guide_legend(bycol = TRUE))

dev.off()



# Condition factor ---------------  
pdf(file = here::here("outputs", "figures", "RST Chinook by statweek - condition.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=rst.biodat.fish %>% 
         filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
         group_by(year, statWeek) %>%
         summarize(meanK = mean(condK, na.rm=T),
                   seK = sd(condK, na.rm=T) / sqrt(length(condK)),
                   n=n()) %>%
         group_by(year) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))) +
  geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=as.factor(label)), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanK), fill=as.factor(label), colour=as.factor(label)), shape=21, size=7, alpha=0.8) +
  scale_y_continuous(limits=c(0.5, 1.75)) +
  labs(y="Mean condition factor 'K' \u00B1 SE", x="Month - week") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "top",
        legend.key.spacing.x = unit(2, "cm"))

dev.off()
