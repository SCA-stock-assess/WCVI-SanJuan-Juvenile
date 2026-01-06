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
                                      sheet="biosampling detailed w GSI") %>%
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
  mutate(gear_simple = case_when(grepl("rst|ipt", gear, ignore.case=T) ~ "RST",
                                 grepl("purse seine", gear, ignore.case=T) ~ "Purse seine",
                                 grepl("beach seine", gear, ignore.case=T) ~ "Beach seine",
                                 TRUE ~ "FLAG"))




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== CONDITION FACTOR ==========================================================


## CF by statweek -----------------
all.biodat$gear_simple <- factor(all.biodat$gear_simple, levels=c("RST", "Beach seine", "Purse seine", ordered=T))


pdf(file = here::here("outputs", "figures", "fish traits", "Chinook by statweek - condition (facet gear).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=all.biodat %>% 
         filter(grepl("chinook", species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T)) %>%
         group_by(gear_simple, statWeek, hatchery_origin) %>%
         summarize(meanK = mean(cond_k, na.rm=T),
                   seK = sd(cond_k, na.rm=T) / sqrt(length(cond_k)),
                   n=n()) %>%
         group_by(gear_simple, statWeek) %>%
         mutate(n=sum(n) 
                #label = paste0(year, " (n=", n, ")")
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
  scale_x_discrete(limits=c("2-3", "2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2", "4-3", "4-4", "5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", 
                            "7-1", "7-2", "7-3", "7-4", "8-1", "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4")) +
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
  facet_wrap(~gear_simple , nrow = 3, strip.position = "right")

dev.off()



## CF by month -----------------

all.biodat$gear_simple <- factor(all.biodat$gear_simple, levels=c("RST", "Beach seine", "Purse seine", ordered=T))


pdf(file = here::here("outputs", "figures", "fish traits", "Chinook by month - condition (facet gear).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data=all.biodat %>% 
         filter(grepl("chinook", species, ignore.case=T), grepl("smolt|fry", life_stage, ignore.case=T)) %>%
         group_by(gear_simple, month, hatchery_origin) %>%
         summarize(meanK = mean(cond_k, na.rm=T),
                   seK = sd(cond_k, na.rm=T) / sqrt(length(cond_k)),
                   n=n()) %>%
         group_by(gear_simple, month) %>%
         mutate(n=sum(n) 
                #label = paste0(year, " (n=", n, ")")
         )) +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0.9, alpha=0.1, fill="red") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=0.9, ymax=1, alpha=0.2, fill="yellow") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1, ymax=1.1, alpha=0.1, fill="green") +
  annotate('rect', xmin=-Inf, xmax=Inf, ymin=1.1, ymax=Inf, alpha=0.2, fill="light blue") +
  geom_errorbar(aes(x=month, ymin=meanK-seK, ymax=meanK+seK, colour=hatchery_origin), width=0.1, size=1, alpha=0.8) +
  geom_point(aes(x=month, y=as.numeric(meanK), fill=hatchery_origin, colour=hatchery_origin), shape=21, size=7, alpha=0.8) +
  scale_fill_manual(values=c("N" = "blue",
                             "Y" = "orange",
                             "U" = "gray70")) +
  scale_colour_manual(values=c("N" = "blue",
                               "Y" = "orange",
                               "U" = "gray70")) +
  scale_y_continuous(breaks=seq(0.3, 1.7, by=0.2)) +
  # scale_x_discrete(limits=c("2-3", "2-4", "3-1", "3-2", "3-3", "3-4", "4-1", "4-2", "4-3", "4-4", "5-1", "5-2", "5-3", "5-4", "6-1", "6-2", "6-3", "6-4", 
  #                           "7-1", "7-2", "7-3", "7-4", "8-1", "8-2", "8-3", "8-4", "9-1", "9-2", "9-3", "9-4")) +
  labs(y="Mean condition factor 'K' \u00B1 SE", fill="Hatchery origin?:   ", colour="Hatchery origin?:   ") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=25),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=26),
        axis.title.x = element_blank(),
        legend.title = element_text(size=17, face="bold"),
        legend.text = element_text(size=15),
        legend.position = "top",
        legend.key.spacing.x = unit(1, "cm"),
        strip.text = element_text(size=14))+
  facet_wrap(~gear_simple , nrow = 3, strip.position = "right")

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ======================================================== STOCK COMP ==========================================================


# By SITE -----------------
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

