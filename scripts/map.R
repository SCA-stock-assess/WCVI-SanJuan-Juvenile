# Bathymetry mapping

# 1. Download tiff from: https://data.chs-shc.ca/dashboard/map
# 2. Find area/layer of interest, usually CHS NONNA - 100m  (uncheck any other layers/attributes)
# 3. Add to cart
# 4. Shopping cart > uncheck ass formats except GeoTIFF
# 5. Next > Download
# 6. Save to referenceable location

# Load packages -------------
library(tidyverse)

# Load raster bathymetry image -------------
bathyE = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12450W.tiff"))
bathyW = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12460W.tiff"))

# Convert raster to data frames for ggplot -------------
bathy_dfE = raster::as.data.frame(bathyE, xy = TRUE)
bathy_dfW = raster::as.data.frame(bathyW, xy = TRUE)

colnames(bathy_dfE)[3] = "bathyE"
colnames(bathy_dfW)[3] = "bathyW"


# Map -------------

ggplot() +
  geom_raster(data = bathy_dfE, aes(x=x, y=y, fill=bathyE)) +
  geom_raster(data = bathy_dfW, aes(x=x, y=y, fill=bathyW)) +
  cmocean::scale_fill_cmocean(name = "ice", na.value="black") +
  coord_cartesian(xlim = c(-124.2, -124), ylim = c(48.5, 48.6)) +
  labs(fill="Bathymetry (m)") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face="bold"),
        legend.position = "bottom")


# LEAFLET VERSION
palE <- colorNumeric(c("#02006f", "#0028c4", "#3492ff", "#90dcff", "#d2f1ff", '#e7f7ff'), raster::values(bathyE),
                    na.color = "transparent")
palE <- colorNumeric("Blues", raster::values(bathyE),
                     na.color = "transparent")
palW <- colorNumeric(c("#02006f", "#0028c4", "#3492ff", "#90dcff", "#d2f1ff", '#e7f7ff'), raster::values(bathyE),
                     na.color = "transparent")


bathyE = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12450W.tiff"))
bathyW = raster::raster(here::here("data", "spatial", "Bathymetry", "NONNA10_4850N12460W.tiff"))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addRasterImage(bathyW, colors=palW) %>%
  addRasterImage(bathyE, colors=palE) %>%
  setView(lng=-124.443899, lat=48.557208, zoom=12) 

