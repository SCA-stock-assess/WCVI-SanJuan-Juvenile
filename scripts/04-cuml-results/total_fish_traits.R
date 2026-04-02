# all condition factor
# Nov 2025



# Set up -----------------
library(tidyverse)
library(leaflet)
"%notin%" <- Negate("%in%")
options(scipen = 9999999)


# Read in all biodata -----------------
all.biodat <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling") %>%
  filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(month = lubridate::month(date, label=T, abbr=T)) %>%
  full_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid") %>%
  mutate(gear_simple = case_when(gear%in% c("6' RST", "IPT") ~ "RST",
                                 grepl("mini purse seine", gear, ignore.case=T) ~ "Port San Juan purse seine",
                                 grepl("large purse seine", gear, ignore.case=T) ~ "Barkley Sound purse seine",
                                 TRUE ~ gear))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================================================== IDENTIFY OUTLIERS ==========================================================

## Condition factor -----------------
### RST -----
rst.nat.k <- all.biodat %>% 
  filter(gear_simple=="RST", resolved_species=="Chinook", hatchery_origin=="Natural") %>% 
  pull(cond_k)

# Get the outlier values and their indices using boxplot.stats()
rst.nat.k.out <- boxplot.stats(rst.nat.k)$out
rst.nat.k.ind <- which(rst.nat.k %in% rst.nat.k.out)

# Print the outliers and their indices
print(rst.nat.k.out)
print(rst.nat.k.ind)

# Z-score
z_scores <- scale(rst.nat.k)

# Identify indices of outliers (e.g., Z-score > 3 or < -3)
outlier_indices <- which(abs(z_scores) > 3)

# Print the outliers
print(rst.nat.k[outlier_indices])


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================================================== LENGTH ==========================================================


all.FL <- all.biodat %>% 
  filter(grepl("chinook", resolved_species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T),
         grepl("san juan", resolved_stock_origin, ignore.case=T), 
         !is.na(resolved_fork_length_mm), hatchery_origin!="Unknown") %>%
  group_by(month, gear_simple, hatchery_origin) %>%
  summarize(n = length(resolved_fork_length_mm),
            meanFL = mean(resolved_fork_length_mm),
            sdFL = sd(resolved_fork_length_mm),
            seFL = sdFL/sqrt(length(resolved_fork_length_mm)),
            CIFL = case_when(n>5 ~ qt(0.975, df=length(resolved_fork_length_mm)-1)*sdFL/sqrt(length(resolved_fork_length_mm)),    #formula to get to 0.975:  1-(1-conf.level)/2
                             TRUE ~ NA),     
            CI_flag = case_when(is.na(seFL) ~ "grp1",
                                TRUE ~ "grp2")) %>%  
  mutate(gear_simple = case_when(gear_simple=="RST" ~ "Freshwater",
                                 grepl("beach seine", gear_simple, ignore.case=T) ~ "San Juan estuary",
                                 grepl("port san juan", gear_simple, ignore.case=T) ~ "Early marine (Port San Jan)",
                                 grepl("barkley", gear_simple, ignore.case=T) ~ "Early marine (Barkley Sound)")) %>%
  print()


## Length by month (Figure xx) ----------------- 
all.FL$gear_simple <- factor(all.FL$gear_simple, levels=c("Freshwater", "San Juan estuary", "Early marine (Port San Jan)",
                                                          "Early marine (Barkley Sound)", ordered=T))


pdf(file = here::here("outputs", "figures", "fish traits", "SJ Chinook by month - length.pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data = all.FL) +
  geom_errorbar(aes(x=month, ymin=meanFL-CIFL, ymax=meanFL+CIFL, 
                    group=interaction(gear_simple, hatchery_origin), colour=gear_simple), 
                width=0.2, size=0.8, alpha=0.4, position=position_dodge(width=0.1)) +
  geom_point(aes(x=month, y=meanFL, shape=hatchery_origin, colour=gear_simple, fill=gear_simple), 
             stroke=1, size=4, position=position_dodge(width=0.1)) +
  geom_line(aes(x=month, y=meanFL, group=interaction(hatchery_origin, gear_simple), 
                colour=gear_simple, linetype=hatchery_origin),
            size=1.1, alpha=1, position=position_dodge(width=0)) +
  
  scale_shape_manual(breaks=c("Hatchery", "Natural"), values=c(24, 21)) +
  scale_linetype_manual(breaks=c("Hatchery", "Natural"), values=c("dashed", "solid")) +
  scale_colour_hue() +
  scale_fill_hue() +
  
  scale_y_continuous(breaks=seq(0, 200, by=25)) +
  labs(y="Fork length (mm)", fill="Life stage", colour="Life stage", shape = "Hatchery origin", 
       linetype = "Hatchery origin") +
  
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.x = element_blank(),
        legend.title = element_text(size=19, face="bold"),
        legend.text = element_text(size=17),
        legend.key.width = unit(10, "mm"),
        plot.margin = margin(t=0.5, b=0.5, l=0.5, r=0.5, unit="cm"))  +
  guides(fill = guide_legend(override.aes = list(shape=32)),
         shape = guide_legend(override.aes = list(size=4, stroke=1)),
         linetype = guide_legend(override.aes = list(linetype=c("dashed", "solid"), linewidth=c(0.8,0.8))))

dev.off()






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== CONDITION FACTOR ==========================================================


## CF by statweek (boxplot) -----------------
all.biodat$gear_simple <- factor(all.biodat$gear_simple, levels=c("RST", "Beach seine", "Purse seine", ordered=T))


pdf(file = here::here("outputs", "figures", "fish traits", "Chinook by statweek - condition (facet gear).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=all.biodat %>% 
         filter(grepl("chinook", resolved_species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T), 
                grepl("san juan", resolved_stock_origin, ignore.case=T), !is.na(cond_k)
                ) #%>%
         # group_by(gear_simple, statWeek, hatchery_origin) %>%
         # summarize(meanK = mean(cond_k, na.rm=T),
         #           seK = sd(cond_k, na.rm=T) / sqrt(length(cond_k)),
         #           n=n()) %>%
         # group_by(gear_simple, statWeek) %>%
         # mutate(n=sum(n) 
         #        #label = paste0(year, " (n=", n, ")")
         # )
       ) +
   # annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=1, alpha=0.1, fill="red") +
   # annotate('rect', xmin=-Inf, xmax=Inf, ymin=1, ymax=1.2, alpha=0.2, fill="yellow") +
   # annotate('rect', xmin=-Inf, xmax=Inf, ymin=1.2, ymax=Inf, alpha=0.1, fill="green") +
  geom_hline(aes(yintercept = 1), colour='red', linetype="dashed", size=1) +
  geom_hline(aes(yintercept = 1.2), colour="green", linetype="dashed", size=1) +
  # geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=hatchery_origin), width=0.1, size=1, alpha=0.8) +
  # geom_point(aes(x=statWeek, y=as.numeric(meanK), fill=hatchery_origin, colour=hatchery_origin, shape=gear_simple), size=7, alpha=0.7,
  #            stroke=1.5) +
  geom_boxplot(aes(x=as.factor(statWeek), y=as.numeric(cond_k), fill=hatchery_origin, linetype=gear_simple), 
               alpha=0.7, size=1) +
  # scale_fill_manual(values=c("Natural" = "blue",
  #                            "Hatchery" = "orange",
  #                            "Unknown" = "gray70")) +
  # scale_colour_manual(values=c("Natural" = "blue",
  #                              "Hatchery" = "orange",
  #                              "Unknown" = "gray70")) +
  # scale_shape_manual(labels = c("Freshwater",
  #                                 "Estuary",
  #                                 "Early marine"),
  #                      values=c(21, 24, 4),
  #                      breaks=c("RST", "Beach seine", "Purse seine")) +
  scale_y_continuous(breaks=seq(0.3, 1.7, by=0.2)) +
  #scale_x_discrete(limits=c("2-3", "2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2", "4-3", "4-4", "5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", 
  #                         "7-1", "7-2", "7-3", "7-4", "8-1", "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4")) +
  labs(y="Mean condition factor 'K' \u00B1 SE", x="Month - week", fill="Hatchery origin", colour="Hatchery origin", shape="Life stage") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        legend.title = element_text(size=17, face="bold"),
        legend.text = element_text(size=15),
        #legend.position = "top",
        #legend.key.spacing.x = unit(1, "cm"),
        strip.text = element_text(size=14)) #+
  #facet_wrap(~gear_simple , nrow = 3, strip.position = "right")

dev.off()



### Scatterplot ----
ggplot(data=all.biodat %>% 
         filter(grepl("chinook", resolved_species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T), 
                grepl("san juan", resolved_stock_origin, ignore.case=T), !is.na(cond_k))) +
  geom_hline(aes(yintercept = 1), colour='red', linetype="dashed", size=1) +
  geom_hline(aes(yintercept = 1.2), colour="green", linetype="dashed", size=1) +
  # geom_errorbar(aes(x=statWeek, ymin=meanK-seK, ymax=meanK+seK, colour=hatchery_origin), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=statWeek, y=as.numeric(cond_k), fill=hatchery_origin, colour=hatchery_origin, shape=gear_simple), size=7, alpha=0.7,
             stroke=1.5)



## CF by month, facet gear (boxplot) ----------------- 

all.biodat$gear_simple <- factor(all.biodat$gear_simple, levels=c("RST", "Beach seine", "Port San Juan purse seine",
                                                                  "Barkley Sound purse seine", ordered=T))


pdf(file = here::here("outputs", "figures", "fish traits", "Chinook by month - condition (facet gear).pdf"),   
    width = 14, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot(data=all.biodat %>% 
         filter(grepl("chinook", resolved_species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T),
                grepl("san juan", resolved_stock_origin, ignore.case=T), !is.na(cond_k),
                (gear_simple=="RST" & hatchery_origin=="Natural" & cond_k<1.5) |
                  (gear_simple=="RST" & hatchery_origin=="Hatchery" & cond_k<1.75) |
                  (grepl("seine", gear_simple, ignore.case=T) & cond_k<3))) +
  geom_hline(aes(yintercept = 1), colour="#d91817", linetype="dashed", size=0.5, alpha=0.8) +
  geom_hline(aes(yintercept = 1.2), colour="#15c528", linetype="dashed", size=0.5, alpha=0.8) +
  geom_boxplot(aes(x=month, y=cond_k, fill=hatchery_origin, colour=hatchery_origin), linewidth=0.7, size=3, shape=21, alpha=0.7) +
  scale_y_continuous(breaks=seq(0, 2, by=0.4)) +
  labs(y="Condition factor", fill="Hatchery origin", colour="Hatchery origin") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.x = element_blank(),
        legend.title = element_text(size=19, face="bold"),
        legend.text = element_text(size=17),
        strip.text = element_text(size=17)) +
  facet_wrap(~gear_simple , nrow = 4, strip.position = "right", labeller = label_wrap_gen(width=14))

dev.off()





## CF by month (Figure xx) ----------------- 

all.K <- all.biodat %>% 
  filter(grepl("chinook", resolved_species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T),
         grepl("san juan", resolved_stock_origin, ignore.case=T), 
         !is.na(cond_k), hatchery_origin!="Unknown") %>%
  group_by(month, gear_simple, hatchery_origin) %>%
  summarize(n = length(cond_k),
            meanK = mean(cond_k),
            sdK = sd(cond_k),
            seK = sdK/sqrt(length(cond_k)),
            CIK = case_when(n>5 ~ qt(0.975, df=length(cond_k)-1)*sdK/sqrt(length(cond_k)),    #formula to get to 0.975:  1-(1-conf.level)/2
                             TRUE ~ NA),     
            CI_flag = case_when(is.na(seK) ~ "grp1",
                                TRUE ~ "grp2")) %>%  
  mutate(gear_simple = case_when(gear_simple=="RST" ~ "Freshwater",
                                 grepl("beach seine", gear_simple, ignore.case=T) ~ "San Juan estuary",
                                 grepl("port san juan", gear_simple, ignore.case=T) ~ "Early marine (Port San Jan)",
                                 grepl("barkley", gear_simple, ignore.case=T) ~ "Early marine (Barkley Sound)")) %>%
  print()


### Scatterplot w/ line -----

all.K$gear_simple <- factor(all.K$gear_simple, levels=c("Freshwater", "San Juan estuary", "Early marine (Port San Jan)",
                                                          "Early marine (Barkley Sound)", ordered=T))

pdf(file = here::here("outputs", "figures", "fish traits", "SJ Chinook by month - condition.pdf"),   
    width = 14, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches


ggplot(data = all.K) +  
  #annotate(geom="text", x="Feb", y=1.25, label="B", fontface="bold") +
  geom_hline(aes(yintercept = 1), colour="black", linetype="solid", size=1, alpha=0.8) +
  geom_hline(aes(yintercept = 1.2), colour="gray60", linetype="solid", size=1, alpha=0.8) +
  geom_line(aes(x=month, y=meanK, group=interaction(hatchery_origin, gear_simple), 
                colour=gear_simple, linetype=hatchery_origin),
            size=0.8, alpha=1, position=position_dodge(width=0.1)) +
  
  geom_errorbar(aes(x=month, ymin=meanK-CIK, ymax=meanK+CIK, 
                    group=interaction(gear_simple, hatchery_origin), colour=gear_simple), 
                width=0.2, size=0.8, alpha=0.4, position=position_dodge(width=0.1)) +
  geom_point(aes(x=month, y=meanK, shape=hatchery_origin, colour=gear_simple, fill=gear_simple), 
             linewidth=0.7, size=4, alpha=1, position = position_dodge(width=0.1)) +
  
  scale_shape_manual(breaks=c("Hatchery", "Natural"), values=c(24, 21)) +
  scale_linetype_manual(breaks=c("Hatchery", "Natural"), values=c("dashed", "solid")) +
  
  scale_y_continuous(breaks=seq(0, 2, by=0.1)) +
  labs(y="Fulton's condition factor", fill="Life stage", colour="Life stage", shape="Hatchery origin", linetype="Hatchery origin") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=20),
        axis.title = element_text(face="bold", size=22),
        axis.title.x = element_blank(),
        legend.title = element_text(size=19, face="bold"),
        legend.text = element_text(size=17),
        legend.key.width = unit(10, "mm"),
        plot.margin = margin(t=0.5, b=0.5, l=0.5, r=0.5, unit="cm"))  +
  guides(fill = guide_legend(override.aes = list(shape=32)),
         shape = guide_legend(override.aes = list(size=4, stroke=1)),
         linetype = guide_legend(override.aes = list(linetype=c("dashed", "solid"), linewidth=c(0.8,0.8))))

dev.off()





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== STOCK COMP ==========================================================


## By SITE (mapped) -----------------
stock_comp_site <- all.biodat %>%
  filter(gear_simple%notin%c("RST", "FLAG"), !is.na(site_name_clean), grepl("chinook", species, ignore.case=T), 
         !is.na(resolved_stock_id), resolved_stock_origin_rollup != "FLAG") %>%
  group_by(site_name_clean, resolved_stock_origin_rollup) %>%
  summarize(n=n(),
            lat=mean(lat_dd),
            long=mean(long_dd)) %>%
  group_by(site_name_clean) %>%
  mutate(total = sum(n),
         propn = n/total,
         lat=mean(lat),
         long=mean(long)) %>%
  ungroup() %>%
  filter(!grepl("confirm w GPS", site_name_clean)) %>%
  select(-c(n, total)) %>%
  mutate(site_name_clean = gsub(site_name_clean, pattern=" ", replacement="_")) %>%
  pivot_wider(names_from = resolved_stock_origin_rollup, values_from = propn) %>%
  nest_by(site_name_clean) %>%
  mutate(data = set_names(list(data), site_name_clean)) %>%
  pull(data) %>% 
  list2env(envir = globalenv()) %>%
  print()



# Define colour palette for miniplots: (17)
colours <- c("#ffa73d", "#ff2800", 
             "#0000ff", "#039be5", 
             "#444444", "#9fe375", 
             "#bcbcbc", "#a4efff",
             "#3d6643" 
             
             #"#ce2000",  "#ffcdff", "#ffa900",
)



# MAP: 
leaflet() %>% 
  # 1. Structure BASEMAP: 
  #addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
  #addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addProviderTiles(providers$Stamen.TerrainBackground) %>%   # doesn't render
  #addProviderTiles(providers$Esri.OceanBasemap) %>%   # too little detail
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  setView(lng=-124.441823, lat=48.556708, zoom=12) %>%
  hideGroup(c('Place names')) %>%


  # 2. Add PROPORTIONS
  leaflet.minicharts::addMinicharts(
    BS03$long, BS03$lat,
    type = "pie",
    chartdata = BS03[, c(3:11)], 
    colorPalette = colours,                                                                     
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS03B$long, BS03B$lat,
    type = "pie",
    chartdata = BS03B[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    BS06$long, BS06$lat,
    type = "pie",
    chartdata = BS06[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    BS07$long, BS07$lat,
    type = "pie",
    chartdata = BS07[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    BS08$long, BS08$lat,
    type = "pie",
    chartdata = BS08[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    BS09$long, BS09$lat,
    type = "pie",
    chartdata = BS09[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    BS13$long, BS13$lat,
    type = "pie",
    chartdata = BS13[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS14$long, BS14$lat,
    type = "pie",
    chartdata = BS14[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS17$long, BS17$lat,
    type = "pie",
    chartdata = BS17[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS21$long, BS21$lat,
    type = "pie",
    chartdata = BS21[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS23$long, BS23$lat,
    type = "pie",
    chartdata = BS23[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    BS24$long, BS24$lat,
    type = "pie",
    chartdata = BS24[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0)  %>%
  
  # --- Purse seine sites ---
  leaflet.minicharts::addMinicharts(
    Gordon_R$long, Gordon_R$lat,
    type = "pie",
    chartdata = Gordon_R[, c(3:11)], 
    colorPalette = colours,                                                                     
    width = 30, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    Jap_Rock$long, Jap_Rock$lat,
    type = "pie",
    chartdata = Jap_Rock[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Mill_Bay$long, Mill_Bay$lat,
    type = "pie",
    chartdata = Mill_Bay[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_A$long, Offshore_A$lat,
    type = "pie",
    chartdata = Offshore_A[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_B$long, Offshore_B$lat,
    type = "pie",
    chartdata = Offshore_B[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    PRCD$long, PRCD$lat,
    type = "pie",
    chartdata = PRCD[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Thrasher$long, Thrasher$lat,
    type = "pie",
    chartdata = Thrasher[, c(3:11)], 
    colorPalette = colours, 
    width = 30, transitionTime = 0)  


