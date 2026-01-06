# Purse seine catch composition
# Oct 2025



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
prs.biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling detailed w GSI") %>%
  filter(grepl("purse", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  full_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  filter(statWeek %notin% c("2-3", "2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2","4-3", "4-4")) %>%
  print()




# # Plot: Weight ~ Height ---------------  
# # Plot to prove that height is a good proxy for modelling weight 
# 
# # Raw:
# ggplot(data=prs.biodat.fish %>% 
#          filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
#   geom_point(aes(x=height, y=lab_weight_g, fill=hatchery_origin), shape=21, size=3) +
#   theme_bw()
# 
# 
# 
# # Log-transformed
# ggplot(data=prs.biodat.fish %>% 
#          filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
#   geom_point(aes(x=log(height), y=log(lab_weight_g), fill=hatchery_origin), shape=21, size=3) +
#   geom_smooth(aes(x=log(height), y=log(lab_weight_g)), method = "lm", se = T, colour="black") +
#   ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..eq.label.., sep = "~~~")),
#                                 label.x.npc=0.04, label.y.npc = 1) +
#   ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..rr.label.., sep = "~~~")),
#                                 label.x.npc=0.07, label.y.npc=0.9) +
#   labs(x="Log ( Fish field height )", y="Log ( Fish lab weight )", fill="Hatchery origin") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=45, hjust=1),
#         axis.text = element_text(colour="black", size=15),
#         axis.title = element_text(face="bold", size=17),
#         legend.position=c(0.8,0.2),
#         legend.title = element_text(face="bold", size=17),
#         legend.text = element_text(size=15),
#         legend.background = element_rect(colour="black", fill=alpha("white", 0.7)))  
# 
# 
# # Create dataset to fit model
# prs.HW.data <- prs.biodat.fish %>% 
#   filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1, !is.na(height))
# 
# linear_log_HW_model <- lm(log(prs.HW.data$lab_weight_g) ~ log(prs.HW.data$height))
# 
# log_a <- coef(linear_log_HW_model)[1] # Intercept corresponds to log(a)
# b <- coef(linear_log_HW_model)[2]    # Slope corresponds to b
# a <- exp(log_a)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================================================== DATA SUMMARY ==========================================================

## All average body traits --------------- 
prs.biodat.fish %>% 
  filter(grepl("chinook", species, ignore.case=T)) %>%
  mutate_at(c("resolved_weight_g", "length_mm"), as.numeric) %>%
  group_by(hatchery_origin) %>%
  summarize(meanL = mean(length_mm, na.rm=T),
            seL = sd(length_mm, na.rm=T) / sqrt(length(length_mm)),
            meanLabW = mean(resolved_weight_g, na.rm=T),
            seLabW = sd(resolved_weight_g, na.rm=T) / sqrt(length(resolved_weight_g)),
            meanH = mean(height_mm, na.rm=T),
            seH = sd(height_mm, na.rm=T) / sqrt(length(height_mm)))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== BIODATA OVER TIME ==========================================================


## Length ---------------  
pdf(file = here::here("outputs", "figures", "fish traits", "Purse seine Chinook by statweek - length.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=prs.biodat.fish %>% 
         mutate_at("length_mm", as.numeric) %>%
         filter(grepl("chinook", species, ignore.case=T), ad_clip=="N") %>%
         group_by(year, statWeek, hatchery_origin) %>%
         summarize(meanL = mean(length_mm, na.rm=T),
                   seL = sd(length_mm, na.rm=T) / sqrt(length(length_mm)),
                   n=n()) %>%
         group_by(year,hatchery_origin) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))) +
  geom_errorbar(aes(x=statWeek, ymin=meanL-seL, ymax=meanL+seL, colour=hatchery_origin), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanL), fill=hatchery_origin, colour=hatchery_origin), shape=21, size=7, alpha=0.8) +
  scale_fill_manual(values=c("N" = "blue",
                             "Y" = "orange",
                             "U" = "gray70")) +
  scale_colour_manual(values=c("N" = "blue",
                               "Y" = "orange",
                               "U" = "gray70")) +  
  scale_y_continuous(breaks=seq(90, 350, by=10)) +
  labs(y="Mean fork length \u00B1 SE (mm)", x="Month - week", fill="Hatchery origin?", colour="Hatchery origin?") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        #legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.position = "right",
        legend.key.spacing.x = unit(2, "cm")
  ) #+
 # guides(fill = guide_legend(bycol = TRUE), colour=guide_legend(bycol = TRUE))

dev.off()



## Condition factor ---------------  

prs.biodat.fish$statWeek <- factor(prs.biodat.fish$statWeek, levels=c("5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", "7-1", "7-2", "7-3", "7-4", "8-1", 
                                                                      "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4", ordered=T))

pdf(file = here::here("outputs", "figures", "fish traits", "Purse seine Chinook by statweek - condition (facet year).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=prs.biodat.fish %>% 
         filter(grepl("chinook", species, ignore.case=T), grepl("smolt", life_stage, ignore.case=T)) %>%
         group_by(year, statWeek, hatchery_origin) %>%
         summarize(meanK = mean(cond_k, na.rm=T),
                   seK = sd(cond_k, na.rm=T) / sqrt(length(cond_k)),
                   n=n()) %>%
         group_by(year, statWeek) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")")
                )) +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0.9, alpha=0.1, fill="red") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=0.9, ymax=1, alpha=0.2, fill="yellow") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1, ymax=1.1, alpha=0.1, fill="green") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1.1, ymax=Inf, alpha=0.2, fill="light blue") +
  geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=hatchery_origin), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanK), fill=hatchery_origin, colour=hatchery_origin), shape=21, size=7, alpha=0.8) +
  scale_fill_manual(values=c("N" = "blue",
                             "Y" = "orange",
                             "U" = "gray70")) +
  scale_colour_manual(values=c("N" = "blue",
                             "Y" = "orange",
                             "U" = "gray70")) +
  scale_y_continuous(breaks=seq(0.3, 1.7, by=0.2)) +
  scale_x_discrete(limits=c("5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", "7-1", "7-2", "7-3", "7-4", "8-1", 
                            "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4")) +
  labs(y="Mean condition factor 'K' \u00B1 SE", x="Month - week", fill="Hatchery origin?:   ", colour="Hatchery origin?:   ") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_text(size=17, face="bold"),
        legend.text = element_text(size=15),
        legend.position = "top",
        legend.key.spacing.x = unit(1, "cm"),
        strip.text = element_text(size=14))+
  facet_wrap(~year , nrow = 3, strip.position = "right")

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== STRAYS/HATCHERY/NATURAL ==========================================================

prs.biodat.fish %>% 
  group_by(year, site)












# Davidson, Katie (DFO/MPO)
# maybe?? im so rusty on stats! i also don't totally know how to boil down all the dietary component data best to do comparisons. like if it's % of most common prey type or total weight of prey or what
# oh I am so rusty too lol I have just read enough now that for the ordination plots people all do PERMANOVAs afterwards to test for differences between the elipses
# 
# Davidson, Katie (DFO/MPO)
# maybe?? im so rusty on stats! i also don't totally know how to boil down all the dietary component data best to do comparisons. like if it's % of most common prey type or total weight of prey or what
# we can show you what we are doing for this if you want!
#   
#   We are doing individual mean proportions (individual prey weight/total stomach weight for each prey item averaged across individuals by month) and we are doing the relative fullness index (prey item/predator weight) for visual graphs. 
# 
# I think in text when explaining results you can use those figures to discuss what the largest contributor to diet was and then most further statistics I have seen use the NMDS plot
