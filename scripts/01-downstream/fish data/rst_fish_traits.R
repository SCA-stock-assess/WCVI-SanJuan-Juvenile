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
  #mutate(condK = (as.numeric(weight)/(as.numeric(length)^3))*100000) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  mutate(species_stage_simple = case_when(grepl("coho", species, ignore.case=T) ~ stringr::str_to_title(paste0(species, " ", life_stage)),
                                          grepl("chum", species, ignore.case=T) ~ "Chum",
                                          grepl("chinook", species, ignore.case=T) ~ stringr::str_to_title(paste0(ad_clip, " ", species)),
                                          TRUE ~ species)) %>%
  print()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# Average body traits (all salmon) --------------- 
write.csv(
  x=rst.biodat.fish %>% 
    #mutate(spp_stage = ) %>%
    filter(grepl("chinook|chum|coho|pink|sockeye", species, ignore.case=T), !is.na(ad_clip)) %>%
    mutate_at(c("resolved_weight_g", "length_mm"), as.numeric) %>%
    group_by(year, species_stage_simple) %>%
    #group_by(year) %>%
    summarize(meanL = round(mean(length_mm, na.rm=T),2),
              seL = round(sd(length_mm, na.rm=T) / sqrt(length(length_mm)),2),
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
  
  file=here::here("outputs", "R_OUT - RST length, weight, height, condition table.csv"),
  row.names=F)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# --------------- CHINOOK by STAT WEEK ---------------  

## Weight ---------------  
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


## Length ---------------  
pdf(file = here::here("outputs", "figures", "fish traits", "RST Chinook by statweek - length.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=rst.biodat.fish %>% 
         mutate_at("length_mm", as.numeric) %>%
         filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
         mutate(statWeek = paste0(0, statWeek)) %>%
         group_by(year, statWeek) %>%
         summarize(meanL = mean(length_mm, na.rm=T),
                   seL = sd(length_mm, na.rm=T) / sqrt(length(length_mm)),
                   n=n()) %>%
         group_by(year) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))) +
  geom_errorbar(aes(x=statWeek, ymin=meanL-seL, ymax=meanL+seL, colour=as.factor(label)), width=0.2, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanL), fill=as.factor(label)), shape=21, size=7, alpha=0.8, colour="black", linewidth=1) +
  scale_y_continuous(limits=c(0,80)) +
  scale_fill_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
  scale_colour_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
  labs(y="Mean fork length \u00B1 SE (mm)", x="Stat week (month-week)") +
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



## Condition factor ---------------  
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


## Length + Condition factor combo plot ---------------  

pdf(file = here::here("outputs", "figures", "fish traits", "RST Chinook by statweek - length AND condition.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggpubr::ggarrange(
  ggplot(data=rst.biodat.fish %>% 
           mutate_at("length_mm", as.numeric) %>%
           filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
           mutate(statWeek = paste0(0, statWeek)) %>%
           group_by(year, statWeek) %>%
           summarize(meanL = mean(length_mm, na.rm=T),
                     seL = sd(length_mm, na.rm=T) / sqrt(length(length_mm)),
                     n=n()) %>%
           group_by(year) %>%
           mutate(n=sum(n),
                  label = paste0(year, " (n=", n, ")"))) +
    geom_errorbar(aes(x=statWeek, ymin=meanL-seL, ymax=meanL+seL, colour=as.factor(label)), width=0.1, size=1, alpha=0.8, show.legend=F) +
    geom_point(aes(x=statWeek, y=as.numeric(meanL), fill=as.factor(label)), shape=21, size=7, alpha=0.8, colour="black", linewidth=1) +
    geom_text(x=1, y=80, label="(A)", size=5) +
    scale_y_continuous(limits=c(0,80)) +
    scale_fill_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
    scale_colour_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
    labs(y="Mean fork length (mm)", x="Stat week (month-week)", fill="Year (total sample size)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=18),
          #axis.text.x = element_text(angle=45, hjust=1),
          axis.text.x = element_blank(),
          axis.title = element_text(face="bold", size=20),
          axis.title.x = element_blank(),
          legend.title = element_text(size=17, face="bold"),
          legend.text = element_text(size=16),
          legend.position = "right",
          legend.key.spacing.x = unit(3, "cm"),
          legend.key.spacing.y = unit(2, "mm")) +
    guides(fill = guide_legend(bycol = TRUE), colour=guide_legend(bycol = F)),
  #\u00B1 SE 
  
  ggplot(data=rst.biodat.fish %>% 
           filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
           mutate(statWeek = paste0(0, statWeek)) %>%
           group_by(year, statWeek) %>%
           summarize(meanK = round(mean(cond_k, na.rm=T),1),
                     seK = sd(cond_k, na.rm=T) / sqrt(length(cond_k)),
                     n=n()) %>%
           group_by(year) %>%
           mutate(n=sum(n),
                  label = paste0(year, " (n=", n, ")"))) +
    geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=as.factor(label)), width=0.1, size=1, alpha=0.8, show.legend=F) +
    geom_point(aes(x=statWeek, y=as.numeric(meanK), fill=as.factor(label)), colour='black', shape=21, size=7, alpha=0.8) +
    geom_text(x=1, y=1.7, label="(B)", size=5) +
    scale_y_continuous(limits=c(0.5, 1.8)) +
    scale_fill_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
    scale_colour_manual(values=c("#70b985", "#6ac7ff", "#7d023c")) +
    labs(y="Mean condition factor", x="Stat week (month-week)", fill="Year (total sample size)") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=18),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title = element_text(face="bold", size=20),
          legend.title = element_text(size=17, face="bold"),
          legend.text = element_text(size=16),
          legend.position = "right",
          legend.key.spacing.x = unit(3, "cm"),
          legend.key.spacing.y = unit(2, "mm")) +
    guides(fill = guide_legend(bycol = TRUE), colour=guide_legend(bycol = F)),
  
  nrow=2, common.legend=T, legend="right"
)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ============= COHO =============

pdf(file = here::here("outputs", "figures", "fish traits", "RST Coho length density plot.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_density(data = rst.biodat.fish %>%
                 filter(grepl("coho", species, ignore.case=T)),
               aes(x=as.numeric(length_mm), fill=species_stage_simple), alpha=0.7, colour="black", linewidth=0.5)  +
  geom_vline(data = rst.biodat.fish %>%
               filter(grepl("coho", species, ignore.case=T)) %>%
               group_by(species_stage_simple) %>%
               summarize(meanFL = mean(as.numeric(length_mm), na.rm = T)),
             aes(xintercept=meanFL, color=species_stage_simple), linetype="dashed", linewidth=1) +
  scale_fill_manual(values=c("#fad875", "#ec7031", "#801e26"),
                    labels=c("Coho alevin", "Coho fry (sub-yearling)", "Coho smolt (yearling)")) +
  scale_colour_manual(values=c("#fad875", "#ec7031", "#801e26"),
                      labels=c("Coho alevin", "Coho fry (sub-yearling)", "Coho smolt (yearling)")) +
  
  
  scale_x_continuous(breaks=seq(20, 200, by=10)) +
  labs(x="Fork length (mm)", y="Density") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "right",
        legend.key.spacing.x = unit(2, "cm"),
        strip.text = element_text(size=20)) 

dev.off()




