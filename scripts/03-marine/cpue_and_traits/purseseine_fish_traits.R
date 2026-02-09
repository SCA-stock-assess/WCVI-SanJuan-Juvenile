# Purse seine catch composition
# Oct 2025



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")


# Read in data -----------------
prs.biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling") %>%
  filter(grepl("Mini purse seine", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  full_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  filter(statWeek %notin% c("2-3", "2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2","4-3", "4-4")) %>%
  print()



prs.biodat.fish <- prs.biodat.fish %>% 
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("purse", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid") %>%
  relocate(site_name_clean, .after=usid) %>%
  print()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============== SAMPLE SUMMARY ===============

## Number of Chinook touched --------------
# prs.biodat.fish %>%
#   filter(grepl("chinook", resolved_species, ignore.case=T)) %>%
#   group_by(year) %>%
#   summarize(n=n())

## Number of genetic samples --------------
# Collected
prs.biodat.fish %>%
  filter(!is.na(dna_vial)) %>%
  group_by(year) %>%
  summarize(n())

prs.biodat.fish %>%
  filter(!is.na(dna_vial)) %>%
  group_by(species) %>%
  summarize(n())


# Analyzed with results
prs.biodat.fish %>%
  filter(#grepl("chinook", resolved_species, ignore.case=T), 
    !is.na(dna_vial), !is.na(mgl_id_source), !grepl("no sample", mgl_notes, ignore.case=T)) %>%
  group_by(year) %>%
  summarize(n())

prs.biodat.fish %>%
  filter(#grepl("chinook", resolved_species, ignore.case=T), 
    !is.na(dna_vial), !is.na(mgl_id_source), !grepl("no sample", mgl_notes, ignore.case=T)) %>%
  group_by(species) %>%
  summarize(n())


prs.biodat.fish %>%
  filter(#grepl("chinook", resolved_species, ignore.case=T), 
         !is.na(dna_vial), !is.na(mgl_id_source), !grepl("no sample", mgl_notes, ignore.case=T)) %>%
  group_by(mgl_id_source) %>%
  summarize(n())


# =============== % AGREEMENT SPECIES ID ===============
prs.biodat.fish %>%
  filter(!is.na(dna_vial), !is.na(mgl_species)) %>%
  # mutate(mgl_species2 = case_when(mgl_species=="chinook" | grepl("assume chinook", mgl_notes, ignore.case=T) ~ "chinook",
  #                                 mgl_species == "coho" ~ "Coho",
  #                                 mgl_species == "coho; chinook" | mgl_notes == "Species ID ambiguous, use caution with result" ~ paste0(mgl_species, " (genetics ambiguous)"),
  #                                 mgl_species == "chinook; pink" & mgl_notes == "Species ID ambiguous, use caution with result" ~ "Chinook (assumed)",
  #                                 TRUE ~ "FLAG")) %>%
  group_by(species, resolved_species) %>%
  summarize(n=n())



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== DATA SUMMARY ==========================================================

# =============== AVG BODY TRAITS ===============
# All salmon
write.csv(
  x=prs.biodat.fish %>% 
    filter(grepl("chinook|coho|chum|sockeye|pink", resolved_species, ignore.case=T)) %>%
    mutate_at(c("resolved_weight_g", "fork_length_mm"), as.numeric) %>%
    group_by(year, hatchery_origin, resolved_species) %>%
    summarize(meanL = round(mean(fork_length_mm, na.rm=T),2),
              seL = round(sd(fork_length_mm, na.rm=T) / sqrt(length(fork_length_mm)),2),
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
              
              N = n()) %>%
    arrange(year, resolved_species, hatchery_origin),
  
  file=here::here("outputs", "R_OUT - Purse seine length, weight, height, condition table (SALMON).csv"),
  row.names=F)


# By-catch
write.csv(
  x=prs.biodat.fish %>% 
    filter(!grepl("chinook|coho|chum|sockeye|pink|unknown", resolved_species, ignore.case=T), !is.na(resolved_species)) %>%
    mutate_at(c("resolved_weight_g", "fork_length_mm"), as.numeric) %>%
    group_by(year, resolved_species) %>%
    summarize(meanL = round(mean(fork_length_mm, na.rm=T),2),
              seL = round(sd(fork_length_mm, na.rm=T) / sqrt(length(fork_length_mm)),2),
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
  
  file=here::here("outputs", "R_OUT - Purse seine length, weight, height, condition table (NON-SALMON).csv"),
  row.names=F)



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


# ======================================================== STOCK COMPOSITION ==========================================================


## All ------------ 
prs.biodat.fish %>%
  group_by(resolved_stock_id) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(total=sum(n))

## Identifiable ------------ 
prs.biodat.fish %>%
  filter(resolved_stock_id!="Unknown") %>%
  group_by(resolved_stock_id) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(total=sum(n))

## Identifiable ROLLUP ------------ 
prs.biodat.fish %>%
  filter(resolved_species=="Chinook") %>%
  group_by(year, resolved_stock_origin_rollup) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total=sum(n))



### Plot ---- 
prs.biodat.fish$site_name_clean <- factor(prs.biodat.fish$site_name_clean, levels=c("Gordon R", "PGM", "PGM \"Mouth\"", "Nearshore",
                                                                                    "PRCD", "Offshore A", 
                                                                                    "Jap Rock", "Mill Bay", "Offshore B", "Thrasher",
                                                                                    "Numukamis Bay North",
                                                                                    ordered=T))

pdf(file = here::here("outputs", "figures", "stock comps", "Marine stock composition (by SITE).pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data = prs.biodat.fish %>%
             filter(resolved_stock_id!="Unknown", resolved_species=="Chinook", !grepl("GPS", site_name_clean),
                    site_name_clean != "Numukamis Bay North") %>%
              mutate(resolved_stock_origin_rollup2 = gsub(" River", "", resolved_stock_origin_rollup)) %>%
             group_by(year, site_name_clean, resolved_stock_origin_rollup2) %>%
             summarize(n=n()) %>% 
             group_by(year, site_name_clean) %>% 
             mutate(total=sum(n),
                    propn = n/total), 
           aes(x=site_name_clean, y=propn, fill=resolved_stock_origin_rollup2, colour=resolved_stock_origin_rollup2), 
           stat="identity", alpha=0.8, linewidth=1, position="stack") +
  geom_label(data = prs.biodat.fish %>%
               filter(resolved_stock_id!="Unknown", resolved_species=="Chinook", !grepl("GPS", site_name_clean), 
                      site_name_clean != "Numukamis Bay North") %>%
               group_by(year, site_name_clean) %>%
               summarize(n=n()),
             aes(x=site_name_clean, y=-0.03, label=n), size=4.5) +
  scale_fill_manual(breaks = c("Hatchery San Juan (Port Renfrew Seapen)", "Hatchery San Juan", "Natural San Juan",
                               "Hatchery Nitinat (Sooke Harbour Release)", "Hatchery Sooke", "Natural Sooke/Nitinat",
                               "Hatchery US", "Natural US",
                               "Natural Inner Barkley", "Natural Outer Barkley",
                               "Natural Clayoquot", "Natural SWVI" ),

                    values=c("#3d6c3d", "#66b466", "#99f299",
                             "#a759a7", "#ef80ef", "#f8ccf8",
                             "#b27b2e", "#ffb142",
                             "#8080ef", "#80b8ef",
                             "#9b3a5e", "#174950")) +
  scale_colour_manual(breaks = c("Hatchery San Juan (Port Renfrew Seapen)", "Hatchery San Juan", "Natural San Juan",
                                 "Hatchery Nitinat (Sooke Harbour Release)", "Hatchery Sooke", "Natural Sooke/Nitinat",
                                 "Hatchery US", "Natural US",
                                 "Natural Inner Barkley", "Natural Outer Barkley",
                                 "Natural Clayoquot", "Natural SWVI" ),
                      
                      values=c("#3d6c3d", "#66b466", "#99f299",
                               "#a759a7", "#ef80ef", "#f8ccf8",
                               "#b27b2e", "#ffb142",
                               "#8080ef", "#80b8ef",
                               "#9b3a5e", "#174950")) +
  
  scale_y_continuous(labels=scales::percent_format()) +
  labs(x="Site name", y="Proportion of genetic samples", fill="Stock ID", colour="Stock ID") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="white", fill=alpha("white", 0.7)),
        #legend.position = c(0.75,0.2),
        plot.margin = unit(c(t=0.5, r=0.5, b=0, l=1),"cm"),
        strip.text = element_text(size=18))  +
  facet_wrap(~year, scales="free_x", nrow=1)

dev.off()
  












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
