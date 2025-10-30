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
  mutate(weight_source = case_when(!is.na(weight) & is.na(lab_weight_g) ~ "field",
                                   is.na(weight) & !is.na(lab_weight_g) ~ "lab",
                                   TRUE ~ "modelled"),
         weight = case_when(weight_source=="lab" ~ as.numeric(lab_weight_g),
                            weight_source=="modelled" & grepl("chinook", species, ignore.case=T) ~ as.numeric(exp(-6.5 + (2.9*log(height)))),
                            TRUE ~ as.numeric(weight)), 
         condK = case_when(weight_source=="lab" ~ (as.numeric(lab_weight_g)/(as.numeric(length)^3))*100000,
                           weight_source=="modelled" ~ (as.numeric(weight)/(as.numeric(length)^3))*100000)) %>%
  left_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  relocate(lab_weight_g, .after=weight) %>%
  relocate(weight_source, .after=lab_weight_g) %>%
  print()




# Plot: Weight ~ Height ---------------  
# Plot to prove that height is a good proxy for modelling weight 

# Raw:
ggplot(data=prs.biodat.fish %>% 
         filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
  geom_point(aes(x=height, y=lab_weight_g, fill=hatchery_origin), shape=21, size=3) +
  theme_bw()



# Log-transformed
ggplot(data=prs.biodat.fish %>% 
         filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
  geom_point(aes(x=log(height), y=log(lab_weight_g), fill=hatchery_origin), shape=21, size=3) +
  geom_smooth(aes(x=log(height), y=log(lab_weight_g)), method = "lm", se = T, colour="black") +
  ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..eq.label.., sep = "~~~")),
                                label.x.npc=0.04, label.y.npc = 1) +
  ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..rr.label.., sep = "~~~")),
                                label.x.npc=0.07, label.y.npc=0.9) +
  labs(x="Log ( Fish field height )", y="Log ( Fish lab weight )", fill="Hatchery origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.position=c(0.8,0.2),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.7)))  


# Create dataset to fit model
prs.HW.data <- prs.biodat.fish %>% 
  filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1, !is.na(height))

linear_log_HW_model <- lm(log(prs.HW.data$lab_weight_g) ~ log(prs.HW.data$height))

log_a <- coef(linear_log_HW_model)[1] # Intercept corresponds to log(a)
b <- coef(linear_log_HW_model)[2]    # Slope corresponds to b
a <- exp(log_a)



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# Average body traits --------------- 
prs.biodat.fish %>% 
  filter(grepl("chinook", species, ignore.case=T)) %>%
  mutate_at(c("weight", "length"), as.numeric) %>%
  group_by(year, hatchery_origin) %>%
  summarize(meanL = mean(length, na.rm=T),
            seL = sd(length, na.rm=T) / sqrt(length(length)),
            meanLabW = mean(lab_weight_g, na.rm=T),
            seLabW = sd(lab_weight_g, na.rm=T) / sqrt(length(lab_weight_g)),
            meanH = mean(height, na.rm=T),
            seH = sd(height, na.rm=T) / sqrt(length(height)))








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
prs.biodat.fish$statWeek <- factor(prs.biodat.fish$statWeek, levels=c("5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", "7-1", "7-2", "7-3", "7-4", "8-1", 
                                                                      "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4", ordered=T))

pdf(file = here::here("outputs", "figures", "RST Chinook by statweek - condition.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=prs.biodat.fish %>% 
         filter(grepl("chinook", species, ignore.case=T), grepl("smolt", life_stage, ignore.case=T)) %>%
         group_by(year, statWeek, weight_source) %>%
         summarize(meanK = mean(condK, na.rm=T),
                   seK = sd(condK, na.rm=T) / sqrt(length(condK)),
                   n=n()) %>%
         group_by(year) %>%
         mutate(n=sum(n),
                label = paste0(year, " (n=", n, ")"))) +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0.9, alpha=0.2, fill="red") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=0.9, ymax=1, alpha=0.2, fill="yellow") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1, ymax=1.1, alpha=0.1, fill="green") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1.1, ymax=Inf, alpha=0.2, fill="dodger blue") +
  
  geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=as.factor(weight_source)), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(meanK), fill=as.factor(weight_source), colour=as.factor(weight_source)), shape=21, size=7, alpha=0.8) +
  #scale_y_continuous(limits=c(0.5, 1.75)) +
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

