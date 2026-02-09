# Modelled weight assessments
# Feb 2026



# Set up -----------------
library(tidyverse)
"%notin%" <- Negate("%in%")

biodat <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                             pattern="^R_OUT - San Juan PSSI master database",
                                             full.names = T),
                             sheet="biosampling")


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# Compare lab/field weights -----------------
ggplot() +
  geom_point(data=biodat %>%
               filter(!is.na(field_weight_g) | !is.na(lab_weight_g)), 
             aes(x=field_weight_g, y=lab_weight_g, fill=gear, colour=gear), shape=21, size=3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(limits=c(0, 25)) +
  scale_x_continuous(limits=c(0, 15))


# Compare field and modelled weights -----------------
ggplot() +
  geom_point(data=biodat %>%
               filter(!is.na(field_weight_g) | !is.na(modelled_weight_g)), 
             aes(x=field_weight_g, y=modelled_weight_g, fill=gear, colour=gear), shape=21, size=3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(limits=c(0,30))


# Compare lab and modelled weights -----------------
ggplot() +
  geom_point(data=biodat %>%
               filter(!is.na(lab_weight_g) | !is.na(modelled_weight_g)), 
             aes(x=lab_weight_g, y=modelled_weight_g, fill=gear, colour=gear), shape=21, size=3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(limits=c(0, 150)) +
  scale_x_continuous(limits=c(0, 125))


# Examine condition factor by weight type 
ggplot() +
  geom_point(data=biodat, 
             aes(x=gear, y=condK, fill=resolved_weight_source, colour=resolved_weight_source), shape=21, size=3)  


# *** here next day - look at condition factor for outliers to remove *** 