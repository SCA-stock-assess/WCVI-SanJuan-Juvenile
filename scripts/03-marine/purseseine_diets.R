# PURSE SEINE diet workup
# Oct 2025



# Set up -----------------
library(tidyverse)
library(leaflet)
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
         total_ww_contents = sum(total_ww_g, na.rm=T)) %>%
  ungroup() %>%
  mutate(weight_no_contents = as.numeric(weight) - total_ww_contents,
         PFI = total_ww_g/weight_no_contents) %>%
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("purse seine", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid") %>%
  relocate(site_name_clean, .after=usid) %>%
  filter(grepl("chinook", species, ignore.case=T)) %>%
  print()



# Load raster bathymetry image -------------
bathyE = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12450W.tiff"))
bathyW = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12460W.tiff"))

# bathyE = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12450W.tiff"))
# bathyW = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12460W.tiff"))

# Convert raster to data frames for ggplot -------------
bathy_dfE = raster::as.data.frame(bathyE, xy = TRUE)
bathy_dfW = raster::as.data.frame(bathyW, xy = TRUE)

colnames(bathy_dfE)[3] = "bathyE"
colnames(bathy_dfW)[3] = "bathyW"


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ========================================================= SAMPLE SUMMARY ========================================================== 

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

# ========================================================= FULLNESS ========================================================== 

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


# PLOTS
# Not going to bother plotting because there were almost no empty stomachs and obviously the rest will have a mix of detectable and non-detectable parts. 


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ========================================================= PREY SOURCES ========================================================== 

## =================== Prey SOURCES - POOLED =================== 

# Prey sources as a % of total prey weight
prs.biodat.diet %>% 
  filter(!is.na(source1)) %>%
  group_by(year, month, source1) %>%
  summarize(weight_by_source1 = sum(total_ww_g, na.rm=T)) %>%
  group_by(year, month) %>%
  mutate(propn = weight_by_source1/sum(weight_by_source1, na.rm=T))


### Source by YEAR ------------
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


### Source by MONTH ------------
# Not great because such small sample sizes, but made and will save in case there is interest. 
prs.biodat.diet$source1 <- factor(prs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", "Human", ordered=T))

pdf(file = here::here("outputs", "figures", "Marine diets (monthly).pdf"),   
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



## =============== Prey SOURCES - AVERAGED ===============

### Source by YEAR -------------------
# Calc % stomach weight for each prey type by Fish, and then average across fish (FtF matching analysis)

#prs.biodat.diet$source1 <- factor(prs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Human", "Undetermined", ordered=T))

pdf(file = here::here("outputs", "figures", "Marine diets (yearly, averaged).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data = prs.biodat.diet %>%
         filter(!is.na(source1), MT_status!="Empty") %>%
         # Add up prey source weights for each individual, and the total mass of all matter in the stomach
         group_by(lethal_tag_no, source1) %>%
         summarize(prey_source1_ww = sum(total_ww_g, na.rm=T),
                   total_prey_ww = unique(total_ww_contents, na.rm=T),
                   year=unique(year))   %>%
         # calculate the proportion of each prey source for each individual
         group_by(lethal_tag_no) %>%
         mutate(propn_prey_source = prey_source1_ww/total_prey_ww,
                year=unique(year)) %>%
         ungroup() %>%
         # Fill in for missing categories and fill the missing values: 
         complete(., lethal_tag_no, source1, fill=list(prey_source1_ww=0, propn_prey_source=0)) %>%
         filter(source1!='TRUE') %>%
         group_by(lethal_tag_no) %>%
         fill(year, .direction="updown") %>%
         fill(total_prey_ww, .direction = "updown") %>%
         # Calculate the average prey source for each year
         group_by(year, source1) %>%
         summarize(mean_source_ww_propn = mean(propn_prey_source, na.rm=T)) %>%
         mutate(check = sum(mean_source_ww_propn))# %>%
        # filter(mean_source_ww_propn>0)
) +
  geom_bar(aes(x=as.factor(year), y=mean_source_ww_propn, fill=source1, colour=source1), stat="identity", position="dodge", 
           alpha=0.8, linewidth=1, width=0.5) +
  geom_label(data=prs.biodat.diet %>% 
               filter(!is.na(source1), !is.na(lethal_tag_no)) %>%
               group_by(year, lethal_tag_no) %>%
               summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
               group_by(year) %>%
               summarize(n = n()), 
             aes(x=as.factor(year), y=-0.02, label=n), size=5) +
  #scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "gray40")) +
  scale_fill_manual(breaks=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Human", "Undetermined"), 
                    values=c("#0424D9", "#98dd0b", "#00ffc8", "gray20",  "gray80")) +
  #scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "gray40")) +.
  scale_colour_manual(breaks=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Human", "Undetermined"), 
                      values=c("#0424D9", "#98dd0b", "#00ffc8", "gray20", "gray80")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0, 1, by=0.1), limits=c(-0.02, 1)) +
  labs(y="Mean proportion in diet (g/g)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face='bold', size=19),
        axis.text = element_text(colour="black", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.85),
        legend.background = element_rect(fill=alpha("white", alpha=0.9), colour="black")) 

dev.off()


### Source by SITE (plot) -------------------

# Same as above but incorporate site as a variable - may explain diet choices especially with taxa below 

prs.biodat.diet$source1 <- factor(prs.biodat.diet$source1, levels=c("Marine", "Terrestrial", "Terrestrial/Freshwater", "Freshwater", "Non-food", "Undetermined", ordered=T))
prs.biodat.diet$site_name_clean <- factor(prs.biodat.diet$site_name_clean, levels=c("BS18", "BS14", "BS03", "BS03B", "BS17", "BS06", "BS07", "BS24",
                                                                                  "BS12", "BS09", "BS08", "BS23", "BS13", "BS21", "PS11-1", "FLBS01",
                                                                                  ordered=T))

pdf(file = here::here("outputs", "figures", "Marine diets (site, averaged - plot).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data = prs.biodat.diet %>%
         filter(!is.na(source1), MT_status!="Empty") %>%
         # Add up prey source weights for each individual, and the total mass of all matter in the stomach
         group_by(lethal_tag_no, source1) %>%
         summarize(prey_source1_ww = sum(total_ww_g, na.rm=T),
                   total_prey_ww = unique(total_ww_contents, na.rm=T),
                   site_name_clean=unique(site_name_clean))   %>%
         # calculate the proportion of each prey source for each individual
         group_by(lethal_tag_no) %>%
         mutate(propn_prey_source = prey_source1_ww/total_prey_ww,
                site_name_clean=unique(site_name_clean)) %>%
         ungroup() %>%
         # Fill in for missing categories and fill the missing values: 
         complete(., lethal_tag_no, source1, fill=list(prey_source1_ww=0, propn_prey_source=0)) %>%
         filter(source1!='TRUE') %>%
         group_by(lethal_tag_no) %>%
         fill(site_name_clean, .direction="updown") %>%
         fill(total_prey_ww, .direction = "updown") %>%
         # Calculate the average prey source for each year
         group_by(site_name_clean, source1) %>%
         summarize(mean_source_ww_propn = mean(propn_prey_source, na.rm=T)) %>%
         mutate(check = sum(mean_source_ww_propn))  %>%
         filter(source1 != "Freshwater")) +
  geom_bar(aes(x=as.factor(site_name_clean), y=mean_source_ww_propn, fill=source1, colour=source1), stat="identity", position="stack", #position=position_dodge(width=0.5), 
           alpha=0.8, linewidth=1, width=0.5) +
  # geom_errorbar(aes(x=as.factor(site_name_clean), ymin=pmax(mean_prey_ww_propn-sd_prey_ww_propn, 0), ymax=mean_prey_ww_propn+sd_prey_ww_propn, 
  #                   colour=source1), position=position_dodge(width=0.5), size=1, width = 0.1, show.legend=F) +
  geom_text(data=prs.biodat.diet %>% 
              filter(!is.na(source1), !is.na(lethal_tag_no), !is.na(site_name_clean)) %>%
              group_by(site_name_clean, lethal_tag_no) %>%
              summarize(lethal_tag_no = unique(lethal_tag_no)) %>%
              group_by(site_name_clean) %>%
              summarize(n = n()), 
            aes(x=as.factor(site_name_clean), y=-0.015, label=n), size=5) +
  #scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "gray40")) +
  scale_fill_manual(breaks=waiver(), values=c("#0424D9", "#98dd0b", "#00ffc8", "gray80",  "gray40")) +
  #scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "#569351", "#5db18b", "gray80",  "gray40")) +.
  scale_colour_manual(breaks=waiver(), values=c("#0424D9", "#98dd0b", "#00ffc8", "gray80",  "gray40")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0, 1, by=0.1), limits=c(-0.015,1)) +
  labs(y="Mean individual proportion (g/g)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(face='bold', size=19),
        axis.text = element_text(colour="black", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.position = "none",
        legend.background = element_rect(fill=alpha("white", alpha=0.9), colour="black")) 

dev.off()



### Source by SITE (mapped, no bathymetry) ----------------

# Create dataframes for each site to plot on the leaflet map: 
prs_mean_by_site <- prs.biodat.diet %>%
  filter(!is.na(source1), MT_status!="Empty", !is.na(site_name_clean), taxonomy_simple!="No sample") %>%
  # Add up prey source weights for each individual, and the total mass of all matter in the stomach
  group_by(lethal_tag_no, source1) %>%
  summarize(prey_source1_ww = sum(total_ww_g, na.rm=T),
            total_prey_ww = unique(total_ww_contents, na.rm=T),
            lat=unique(lat_dd),
            long=unique(long_dd),
            site_name_clean = unique(site_name_clean))   %>%
  # calculate the proportion of each prey source for each individual
  group_by(lethal_tag_no) %>%
  mutate(propn_prey_source = prey_source1_ww/total_prey_ww) %>%
  ungroup() %>%
  # Fill in for missing categories and fill the missing values: 
  complete(., lethal_tag_no, source1, fill=list(prey_source1_ww=0, propn_prey_source=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(site_name_clean, total_prey_ww, lat, long), .direction="updown") %>%
  # Calculate the average prey source for each year
  ungroup() %>%
  group_by(site_name_clean, source1) %>%
  summarize(mean_prey_ww_propn = mean(propn_prey_source, na.rm=T),
            lat=unique(lat),
            long=unique(long)  #,
            #n=unique(n)
  ) %>%
  mutate(site_name_clean = gsub(site_name_clean, pattern=" ", replacement="_")) %>%
  ungroup() %>%
  group_by(site_name_clean, source1) %>%
  summarize(mean_prey_ww_propn = mean(mean_prey_ww_propn),
            lat=mean(lat),
            long=mean(long)
            ) %>%
  ungroup() %>%
  #select(-c(sd_prey_ww_propn)) %>%
  pivot_wider(names_from = source1, values_from = mean_prey_ww_propn) %>%
  mutate(across(Marine:Undetermined, ~replace_na(., 0))) %>%
  nest_by(site_name_clean) %>%
  mutate(data = set_names(list(data), site_name_clean)) %>%
  pull(data) %>% 
  list2env(envir = globalenv())

# Sample sizes to adjust plot sizes based on samples:
prs.biodat.diet %>%
  group_by(site_name_clean, lethal_tag_no) %>%
  summarize() %>%
  group_by(site_name_clean) %>%
  summarize(n=n()) %>%
  arrange(desc(n))



# Define colour palette for miniplots: 
colours <- c("#5b5b5b", "#0424D9", "#98dd0b", "#00ffc8", "#ffffff")

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
  setView(lng=-124.452716, lat=48.552454, zoom=13) %>%
  hideGroup(c('Place names')) %>%
  
  # 2. Add PROPORTIONS
  leaflet.minicharts::addMinicharts(
    Gordon_R$long, Gordon_R$lat,
    type = "pie",
    chartdata = Gordon_R[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours,                                                                     
    width = 20, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    Jap_Rock$long, Jap_Rock$lat,
    type = "pie",
    chartdata = Jap_Rock[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 25, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Mill_Bay$long, Mill_Bay$lat,
    type = "pie",
    chartdata = Mill_Bay[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_A$long, Offshore_A$lat,
    type = "pie",
    chartdata = Offshore_A[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 15, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_B$long, Offshore_B$lat,
    type = "pie",
    chartdata = Offshore_B[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 20, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    PRCD$long, PRCD$lat,
    type = "pie",
    chartdata = PRCD[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Thrasher$long, Thrasher$lat,
    type = "pie",
    chartdata = Thrasher[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 25, transitionTime = 0)  




### Source by SITE (mapped, bathymetry) ----------------
palE <- colorNumeric(c("#02006f", "#0028c4", "#3492ff", "#90dcff", "#d2f1ff", '#e7f7ff'), raster::values(bathyE),
                     na.color = "transparent")
palW <- colorNumeric(c("#02006f", "#0028c4", "#3492ff", "#90dcff", "#d2f1ff", '#e7f7ff'), raster::values(bathyE),
                     na.color = "transparent")


leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(bathyW, colors=palW) %>%
  addRasterImage(bathyE, colors=palE) %>%
  setView(lng=-124.443899, lat=48.557208, zoom=12) %>%
  
  # 2. Add PROPORTIONS
  leaflet.minicharts::addMinicharts(
    Gordon_R$long, Gordon_R$lat,
    type = "pie",
    chartdata = Gordon_R[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours,                                                                     
    width = 20, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    Jap_Rock$long, Jap_Rock$lat,
    type = "pie",
    chartdata = Jap_Rock[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 25, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Mill_Bay$long, Mill_Bay$lat,
    type = "pie",
    chartdata = Mill_Bay[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_A$long, Offshore_A$lat,
    type = "pie",
    chartdata = Offshore_A[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 15, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Offshore_B$long, Offshore_B$lat,
    type = "pie",
    chartdata = Offshore_B[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 20, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    PRCD$long, PRCD$lat,
    type = "pie",
    chartdata = PRCD[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 30, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Thrasher$long, Thrasher$lat,
    type = "pie",
    chartdata = Thrasher[, c("Human", "Marine", "Terrestrial", "Terrestrial/Freshwater", "Undetermined")], 
    colorPalette = colours, 
    width = 25, transitionTime = 0)  




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ========================================================= PREY TAXA ========================================================== 


# Prey TAXA as a % of total prey weight
prs.biodat.diet %>% 
  filter(!is.na(taxonomy_stage_simple), taxonomy_stage_simple!="Empty") %>%
  group_by(taxonomy_stage_simple) %>%
  summarize(weight_by_taxa = sum(total_ww_g, na.rm=T)) %>%
  ungroup() %>%
  mutate(propn = weight_by_taxa/sum(weight_by_taxa, na.rm=T))


## =============== Prey TAXA - POOLED ===============

pdf(file = here::here("outputs", "figures", "Marine diets (by taxonomy simplified - pooled).pdf"),   
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



## =============== Prey TAXA - AVERAGED ===============

pdf(file = here::here("outputs", "figures", "Marine diets (by taxonomy simplified - averaged).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data = prs.biodat.diet %>%
             filter(!is.na(source1), MT_status!="Empty", taxonomy_simple!="No sample") %>%
             # -- Add up prey source weights for each individual, and the total mass of all matter in the stomach
             group_by(lethal_tag_no, taxonomy_simple) %>%
             summarize(weight_by_taxa = sum(total_ww_g, na.rm=T),
                       total_prey_ww = unique(total_ww_contents, na.rm=T))   %>%
             # -- Calculate the proportion of each prey source for each individual
             group_by(lethal_tag_no) %>%
             mutate(propn_prey_taxa = weight_by_taxa/total_prey_ww) %>%
             ungroup() %>%
             # -- Fill in for missing categories and fill the missing values: 
             complete(., lethal_tag_no, taxonomy_simple, fill=list(weight_by_taxa=0, propn_prey_taxa=0)) %>%
             group_by(lethal_tag_no) %>%
             fill(total_prey_ww, .direction = "updown") %>%
             # -- Calculate the average prey source for each year
             group_by(taxonomy_simple) %>%
             summarize(mean_taxa_ww_propn = mean(propn_prey_taxa, na.rm=T)) %>%
             mutate(check = sum(mean_taxa_ww_propn))  %>%
              mutate(source_simple = case_when(grepl("amphipods|barnacles|octopus|copepods|crustacean|decapods|fish|isopods|ostracods|parasites|polychaete", taxonomy_simple, ignore.case=T) ~ "Marine",
                                               grepl("flies|arachnid|beetle|moth|insect|lice|midges|bug|wasp", taxonomy_simple, ignore.case=T) ~ "Terrestrial/Freshwater",
                                               grepl("plant|seaweed", taxonomy_simple, ignore.case=T) ~ "Plant/seaweed",
                                               taxonomy_simple=="Non-food" ~ "Non-food",
                                               taxonomy_simple=="Plastic" ~ "Human",
                                               grepl("Arthropod|Unidentified remains|invert", taxonomy_simple, ignore.case=T) ~ "Undetermined",
                                               TRUE ~ "FLAG")),
           aes(x=fct_reorder(taxonomy_simple, mean_taxa_ww_propn, .desc = TRUE), y=mean_taxa_ww_propn, fill=source_simple, colour=source_simple), 
           stat="identity", position="dodge", alpha=0.7, linewidth=1) +
  # scale_fill_manual(breaks=waiver(), values=c("#00e7ff", "gray80", "#569351", "gray40")) +
  # scale_colour_manual(breaks=waiver(), values=c("#00e7ff", "gray80", "#569351", "gray40")) +
  scale_fill_manual(breaks=c("Marine", "Terrestrial/Freshwater", "Plant/seaweed", "Human", "Undetermined"), 
                     values=c("#0424D9", "#6be980", "#277f00", "gray20", "gray80")) +
  scale_colour_manual(breaks=c("Marine", "Terrestrial/Freshwater", "Plant/seaweed", "Human", "Undetermined"), 
                      values=c("#0424D9", "#6be980", "#277f00", "gray20", "gray80")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0, 1, by=0.1), limits=c(0, 0.5)) +
  labs(x="", y="Mean proportion in diet (g/g)", fill="Prey source", colour="Prey source") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.position=c(0.8,0.8),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.7)),
        plot.margin = unit(c(t=0.5, r=0.5, b=0, l=1),"cm"))  

dev.off()



## =============== Prey TAXA - AVERAGED (hat/nat) ===============

# *** HERE NEXT DAY! 
