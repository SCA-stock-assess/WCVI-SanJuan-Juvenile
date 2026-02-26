# San Juan MCT and NITA MARIA diet workup
# Oct 2025



# Set up -----------------
library(tidyverse)
library(leaflet)
"%notin%" <- Negate("%in%")


# Read in data -----------------
nmmct.biodat.diet <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling long w diet") %>%
  filter(grepl("large purse seine|microtroll", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(MT_status = case_when(taxonomy_simple=="Empty" ~ "Empty",
                               grepl("trace|bdl|below detectable", diet_comments, ignore.case=T) ~ "Trace",
                               taxonomy_simple %notin% c("Empty", "Non-food") ~ "Functional prey items",
                               taxonomy_simple == "Non-food" ~ "Not prey",
                               TRUE ~ "FLAG"),
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
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("purse seine|microtroll", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid, pfma),
            by="usid") %>%
  relocate(site_name_clean, .after=usid) %>%
  filter(grepl("chinook", species, ignore.case=T)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



## =============== Prey TAXA - AVERAGED ===============
# Summarize data: 
nm_meanTAXA_by_site <- nmmct.biodat.diet %>%
  filter(!is.na(taxonomy_simple), MT_status!="Empty", total_ww_contents>0, !is.na(site_name_clean), 
         taxonomy_simple%notin%c("Non-food", "No sample", "Plastic"), gear=="Large purse seine") %>%
   mutate(taxonomy_simple = case_when(grepl("non-food", taxonomy_simple, ignore.case=T) ~ "Non-food",
                                    TRUE ~ taxonomy_simple),
          tax_simp_plot = case_when(grepl("flies|beetles|insects|midges|bugs|wasps", taxonomy_simple, ignore.case=T) ~ "Insects",
                                    grepl("arachnids|lice|collembola", taxonomy_simple, ignore.case=T) ~ "Other terrestrial arthropods",
                                    TRUE ~ taxonomy_simple)) %>%
  # Add up prey source weights for each individual, and the total mass of all matter in the stomach
  group_by(lethal_tag_no, tax_simp_plot) %>%
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
  complete(., lethal_tag_no, tax_simp_plot, fill=list(prey_source1_ww=0, propn_prey_source=0)) %>%
  group_by(lethal_tag_no) %>%
  fill(c(site_name_clean, total_prey_ww, lat, long), .direction="updown") %>%
  group_by(site_name_clean, tax_simp_plot) %>%
  summarize(mean_prey_ww_propn = mean(propn_prey_source, na.rm=T),
            lat=mean(lat),
            long=mean(long))


### Taxa by SITE - Plot -------------------
nm_meanTAXA_by_site$site_name_clean <- factor(nm_meanTAXA_by_site$site_name_clean, levels=c("Nanat", "Sarita", "Sproat Narrows", "Kirby", "Village Reef",
                                                                                            "Turtle Island", "Hand Island", "Lyall Point", ordered=T))


pdf(file = here::here("outputs", "figures", "diet", "Barkley Sound Marine diets (by taxonomy simplified and SITE).pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_bar(data = nm_meanTAXA_by_site %>% 
             filter(!is.na(site_name_clean)),
           aes(x=site_name_clean, y=mean_prey_ww_propn, fill=tax_simp_plot, colour=tax_simp_plot), stat="identity", position="stack", alpha=0.7, linewidth=1) +
  geom_label(data=nmmct.biodat.diet %>%
               filter(!is.na(taxonomy_simple), MT_status!="Empty", total_ww_contents>0, !is.na(site_name_clean), 
                      taxonomy_simple%notin%c("Non-food", "No sample", "Plastic"), gear=="Large purse seine") %>%
               group_by(site_name_clean, lethal_tag_no) %>%
               summarize(n=n()) %>%
               group_by(site_name_clean) %>%
               summarize(n=n()),
             aes(x=site_name_clean, y=-0.03, label=n), size=4.5) +
   scale_fill_manual(breaks=waiver(), 
                     values=c("#14c8aa", #"#00bce4", "#9f204f", "#ff827b",
                              "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                              "#9fe375", #"#f57407", 
                              "#1ba831", "#bcbcbc",
                              #"#e3faff", "#ffea3b", 
                              "#ff006f", "#f73921", "#ad00ff",
                              "#997950",
                              "#444444")) +
   scale_colour_manual(breaks=waiver(), 
                       values=c("#14c8aa", #"#00bce4", "#9f204f", "#ff827b",
                                "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
                                "#9fe375", #"#f57407", 
                                "#1ba831", "#bcbcbc",
                                #"#e3faff", "#ffea3b", 
                                "#ff006f", "#f73921", "#ad00ff",
                                "#997950",
                                "#444444")) +
  scale_y_continuous(labels = scales::percent_format(), breaks=seq(0,1,by=0.1)) +
  labs(x="", y="Mean prey proprtion in diet (g/g)", fill="Diet item", colour="Diet item") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="white", fill=alpha("white", 0.7)),
        plot.margin = unit(c(t=0.5, r=0.5, b=0, l=1),"cm"),
        strip.text = element_text(size=18))  

dev.off()




### Taxa by SITE - Mapped -------------------

nm_meanTAXA_by_site_mapped <- nm_meanTAXA_by_site %>%
  ungroup() %>%
  pivot_wider(names_from = tax_simp_plot, values_from = mean_prey_ww_propn) %>%
  mutate(site_name_clean = gsub(site_name_clean, pattern=" ", replacement="_")) %>%
  nest_by(site_name_clean) %>%
  mutate(data = set_names(list(data), site_name_clean)) %>%
  pull(data) %>% 
  list2env(envir = globalenv())


# Define colour palette for miniplots:  
colours <- 
  c("#14c8aa", #"#00bce4", "#9f204f", "#ff827b",
    "#005cd0", "#abff00", "#ecbfc9", "#7fffd4",
    "#9fe375", #"#f57407", 
    "#1ba831", "#bcbcbc",
    #"#e3faff", "#ffea3b", 
    "#ff006f", "#f73921", "#ad00ff",
    "#997950",
    "#444444")


# MAP: 
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addRasterImage(bathyW, colors=palW) %>%
  #addRasterImage(bathyE, colors=palE) %>%
  setView(lng=-125.235539, lat=48.918130, zoom=10) %>%
  
  # 2. Add PROPORTIONS
  leaflet.minicharts::addMinicharts(
    Hand_Island$long, Hand_Island$lat,
    type = "pie",
    chartdata = Hand_Island[, c(3:15)], 
    colorPalette = colours,                                                                     
    width = 40, transitionTime = 0) %>%
  
  leaflet.minicharts::addMinicharts(
    Kirby$long, Kirby$lat,
    type = "pie",
    chartdata = Kirby[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Lyall_Point$long, Lyall_Point$lat,
    type = "pie",
    chartdata = Lyall_Point[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Nanat$long, Nanat$lat,
    type = "pie",
    chartdata = Nanat[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Sarita$long, Sarita$lat,
    type = "pie",
    chartdata = Sarita[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Sproat_Narrows$long, Sproat_Narrows$lat,
    type = "pie",
    chartdata = Sproat_Narrows[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0) %>% 
  
  leaflet.minicharts::addMinicharts(
    Turtle_Island$long, Turtle_Island$lat,
    type = "pie",
    chartdata = Turtle_Island[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0)  %>%
  
  leaflet.minicharts::addMinicharts(
    Village_Reef$long, Village_Reef$lat,
    type = "pie",
    chartdata = Village_Reef[, c(3:15)], 
    colorPalette = colours, 
    width = 40, transitionTime = 0)












