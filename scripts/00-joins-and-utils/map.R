# PORT RENFREW STUDY AREA MAP 
# Feb 2026
# S. Finnis / K. Davidson

# This script builds a study area map for the Port Renfrew area using a mix of 
# both vector (coastline, lakes, rivers, streams) and raster (bathymetry) data

## Data Sources:
# - Freshwater Atlas (FWA) vector laters: coastline, lakes, rivers, streams
# - These are all separate shapefiles, and you can obtain the data from ur study
# - area (and not the entire BC coast) from iMapBC: https://maps.gov.bc.ca/ess/hm/imap4m/

# - Canadian Hydrographic Service (CHS) bathymetry data: 
# - Downloadable from: https://data.chs-shc.ca/


# ====================== SET UP ======================
## Load packages ------------------
library(tidyverse)
library(ggspatial)

"%notin%" <- Negate("%in%")

## Set parent directory ------------------
# Set the directory containing all downloaded FWA/CHS data. This folder should include subfolders like FWA_LAKES_POLY, etc.
parent_dir = here::here("data", "spatial", "map-data")


# ====================== READ IN SHAPEFILES & MODIFY ======================
## Read in shapefiles ------------------
# Function to read in the shapefiles. This assumes there is only one shapefile per folder 
read_one_shp = function(dir_path) {
  shp = list.files(dir_path, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  sf::st_read(shp[1], quiet = TRUE)
}


# Create an sf object for each shapefile
coastline_poly = read_one_shp(file.path(parent_dir, "FWA_COASTLINE_POLY"))
lakes = read_one_shp(file.path(parent_dir, "FWA_LAKES_POLY"))
rivers = read_one_shp(file.path(parent_dir, "FWA_RIVERS_POLY"))
streams = read_one_shp(file.path(parent_dir, "FWA_STREAM_NETWORKS_SP"))

# Transform everything to BC Albers (EPSG:3005). Ppl usually recommend this for BC maps
to_3005 = function(x) sf::st_transform(x, 3005)

coastline_poly = to_3005(coastline_poly)
lakes = to_3005(lakes)
rivers = to_3005(rivers)
streams = to_3005(streams)


# ====================== READ IN BATHYMETRY & MODIFY ======================
## Read in bathymetry ------------------
bathy_path = file.path(parent_dir, "bathymetry/NONNA10_4850N12450W.tiff")
bathy = terra::rast(bathy_path)

# Reproject to BC Albers
bathy_3005 = terra::project(bathy, "EPSG:3005")

# Make depth positive
bathy_3005 = bathy_3005 * -1

# Get raster extent. This is used for setting min/max values for setting the map extents
b_ext = terra::ext(bathy_3005)

# Convert raster to data frame. ggplot needs it as a data frame
bathy_df = as.data.frame(bathy_3005, xy = TRUE) 

# Define colour scheme for bathymetry. Recommendation from Andrea
bathy_cols = c("white", pals::brewer.blues(9))
bathPal <- c("white", '#e7f7ff',"#d2f1ff","#90dcff","#3492ff", "#0028c4", "#02006f")
pathpal2 <- c("white", "#e5e9f9", "#ccd4f3", "#b2beed", "#99a9e7", "#7f93e1", "#667edb", "#4c68d5", "#3252cf", "#193dc9", "#0028c4", "#0024b0", "#00209c", "#001875")


# ====================== READ IN STUDY SITES ======================
sites <- readxl::read_excel(path = list.files(path = "//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                              pattern="^R_OUT - San Juan PSSI master database",
                                              full.names=T),
                            sheet = "sample_event_meta") 

sites2 <- sites %>% 
  filter(gear %in% c("6' RST", "PFN beach seine", "Mini purse seine"), PFMA==20) %>%
  mutate(site_name_CLEAN = case_when(gear=="6' RST" ~ "RST",
                                     TRUE ~ site_name_CLEAN)) %>%
  group_by(gear, site_name_CLEAN) %>%
  summarize(lat=mean(lat_DD), long=mean(long_DD)) %>%
  filter(site_name_CLEAN %notin% c("FLBS01", "BS12", "BS18", "San Juan @ Fairy Lake"), !is.na(lat) & !is.na(long)) %>%
  print()

points_sf <- sf::st_as_sf(sites2, coords = c("long", "lat"), crs = 4326)
points_bc_albers <- sf::st_transform(points_sf, 3005)


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


# Study site map -----------------
pdf(file = here::here("outputs", "figures", "Study area map.pdf"),   
    width = 16, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot() +
  # 
  # Add the bathymetry 
  geom_tile(data=bathy_df, aes(x=x, y=y, fill=Depth)) +
  scale_fill_gradientn(colours=pathpal2, name="Depth (m)") +
 
  # Add coastline
  geom_sf(data=coastline_poly, fill="#eaddc0", color="#e7d7b6", linewidth=1) +    # #ddc797 #d9c08a

  # Water shapefiles
  geom_sf(data=streams, color="#7fbbf7", linewidth=0.5) +
  geom_sf(data=lakes, color="#4ca0f4", fill="#4ca0f4", linewidth=0.5) +
  geom_sf(data=rivers, color="#4ca0f4", fill="#4ca0f4", linewidth=0.5) +
  
  # Add sites & labels
  geom_sf(data=points_bc_albers, aes(colour=gear, shape=gear), size=4, alpha=0.85, linewidth=1) +

  # --- Fairy Lake
  # geom_sf_label(data=points_bc_albers %>%
  #                 filter(gear %in% c("PFN beach seine", "Mini purse seine"),
  #                        site_name_CLEAN %in% c("Mill Bay")), 
  #               aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-120, nudge_y=-300, alpha=0.9) +
  
  # -- North arm sites 
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS03")),
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-100, nudge_y=380, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS03B")),
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-280, nudge_y=100, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS14")),
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=120, nudge_y=380, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS17")),
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=230, nudge_y=-20, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS07")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-140, nudge_y=40, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS24")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-150, nudge_y=-90, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS06")), 
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-20, nudge_y=-270, alpha=0.9) +
  
  # --- South arm
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS08")),
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=120, nudge_y=-60, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS09")),
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-40, nudge_y=280, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                 filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                        site_name_CLEAN %in% c("BS23")), 
               aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=280, nudge_y=300, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS13")), 
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=400, nudge_y=-50, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("BS21")), 
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=0, nudge_y=-250, alpha=0.9) +

  # --- Purse seine
  geom_sf_label(data=points_bc_albers %>%
                 filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                        site_name_CLEAN %in% c("Gordon R", "Nearshore")),
               aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-70, nudge_y=330, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("PGM")),
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-70, nudge_y=-250, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("PRCD")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=70, nudge_y=-260, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("PGM \"Mouth\"")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-300, nudge_y=-240, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("Offshore A")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-130, nudge_y=-90, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("Jap Rock")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=50, nudge_y=300, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("Thrasher", "Offshore B")), 
                aes(label=site_name_CLEAN), hjust=1, size=5, nudge_x=-150, nudge_y=40, alpha=0.9) +
  
  geom_sf_label(data=points_bc_albers %>%
                  filter(gear %in% c("PFN beach seine", "Mini purse seine"),
                         site_name_CLEAN %in% c("Mill Bay")), 
                aes(label=site_name_CLEAN), hjust=0, size=5, nudge_x=-120, nudge_y=-300, alpha=0.9) +
  
  scale_colour_manual(values=c("6' RST" = "#127901",
                               "PFN beach seine" = "#9a0446",  #ccff00
                               "Mini purse seine" = "#ff9300"),
                      labels=c("RST", "Purse seine", "Beach seine")) +
  
  scale_shape_manual(values=c("6' RST" = 15,
                              "PFN beach seine" = 19,
                              "Mini purse seine" = 17),
                     labels=c("RST", "Purse seine", "Beach seine")) +
  
  labs(colour="Sampling method", fill="Sampling method", shape="Sampling method") +
  
  # Set the extent of the map to be limits of the bathymetry layer (+/-)
  coord_sf(xlim=c(b_ext$xmin, b_ext$xmax+8000), ylim=c(b_ext$ymin, b_ext$ymax+1000), expand=F) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(colour="black", size=13),
        panel.background = element_rect(fill="#f4f6fc"),
        panel.grid.major = element_line(colour="gray95"),
        legend.title = element_text(face="bold", size=18),
        legend.text = element_text(size=15),
        legend.key = element_rect(fill="white")) +
  
  # Add scale bar to bottom right (br) of map 
  ggspatial::annotation_scale(location = "br", text_cex = 1.1) + 
  
  # Andrea's scalebar recommendation
  guides(fill = guide_colorbar(barheight=10, 
                               frame.colour="black", 
                               ticks.colour="black"))

dev.off()





# Inset map -----------------
canada_map <- rnaturalearth::ne_states(country = "canada", returnclass = "sf")
us_map <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")

pdf(file = here::here("outputs", "figures", "Study area map - inset.pdf"),   
    width = 16, # The width of the plot in inches
    height = 10) # The height of the plot in inches

ggplot() +
  geom_sf(data = canada_map, fill="#eaddc0", color="#d2c6ac", size = 0.2) +  
  geom_sf(data = us_map, fill="#eaddc0", color="#d2c6ac", size = 0.2) +  
  coord_sf(xlim = c(-136, -114), ylim = c(47, 57), expand = FALSE) +
  theme_void() +  
  theme(panel.background = element_rect(fill="#f4f6fc"))  

dev.off()