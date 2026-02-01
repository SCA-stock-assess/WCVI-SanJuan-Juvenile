# Add lab results in to database 
# Feb 2024
# Only need to run if new lab results returned (e.g., GSI, stomachs, etc.) or if changes to underlying data file (San Juan PSSI master database...)



# Load libraries/set up ------------------------------------
library(tidyverse)
options(scipen = 99999999999999)
"%notin%" <- Negate("%in%")





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ========================= LOAD FIELD BIODATA =========================

## Load field biodata and add in some variables ------------------------------------
# (Doing this here so it is carried through the subsequent biodata tabs later)
  # See height_weight script for rationale on equation used to convert height to modelled weight
biodata <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                              pattern="^San Juan PSSI master database",
                                              full.names=T),
                              sheet="biosampling", guess_max = 5000) %>%
  mutate_at(c("weight", "length"), as.numeric) %>%
  mutate(resolved_weight_source = case_when(!is.na(weight) ~ "field",
                                            is.na(weight) & !is.na(lab_weight_g) ~ "lab",
                                            is.na(weight) & is.na(lab_weight_g) ~ "modelled"),
         modelled_weight_g = case_when(grepl("RST|IPT", gear, ignore.case=T) ~ 0.142*exp(0.229*weight),   #  weight models as of Jan 26
                                       grepl("seine", gear, ignore.case=T) ~ ((2.59*weight)-5.58)),
         resolved_weight_g = case_when(resolved_weight_source=="field" ~ as.numeric(weight),
                                       resolved_weight_source=="lab" ~ as.numeric(lab_weight_g),
                                       resolved_weight_source=="modelled" ~ as.numeric(modelled_weight_g)),
         condK = resolved_weight_g/(length^3)*100000) %>%
  relocate(c(lab_weight_g, modelled_weight_g), .after=weight) %>%
  relocate(c(resolved_weight_g, resolved_weight_source), .after=modelled_weight_g) %>%
  rename(fork_length_mm = length,
         height_mm = height,
         field_weight_g = weight) %>%
  print()




# ========================= LOAD MGL RESULTS =========================

## Load and compile repunit_table_ids tabs all years ------------------------------------
gsi.repunits_table_ids.LL <- c(
  # --- 2023:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                    pattern="new-format.xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         }),
  
  # --- 2024:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         }),
  
  # --- 2025:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         })
)

# Rename, convert to data frame: 
names(gsi.repunits_table_ids.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                                 pattern="new-format.xlsx", full.names=F),
                                      list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                                 pattern=".xlsx", full.names=F),
                                      list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.repunits_table_ids <- gsi.repunits_table_ids.LL %>%
  #do.call("rbind", gsi.repunits_table_ids.LL) %>%
  #tibble::rownames_to_column(var="file_source")
  reduce(full_join) 
remove(gsi.repunits_table_ids.LL)




## Load and compile extraction_sheet tabs all years ------------------------------------
gsi.extraction_sheets.LL <- c(
  # --- 2023:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                    pattern="new-format.xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet", col_types = "text")
         }),
  
  # --- 2024:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet", col_types = "text")
         }) #,
  
  # --- 2025:
  #  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
            #pattern=".xlsx", full.names=T), 
      #function(x) {
        #readxl::read_excel(x, sheet="extraction_sheet", col_types = "text")
        #})
)

# Rename, convert to data frame: 
names(gsi.extraction_sheets.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                                pattern="new-format.xlsx", full.names=F),
                                     list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                                pattern=".xlsx", full.names=F) #,
                                     #list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
                                     #    pattern=".xlsx", full.names=F)
)

# Convert the Large List into a useable R dataframe:
gsi.extraction_sheets <- gsi.extraction_sheets.LL %>%
  reduce(full_join) 
  #do.call("rbind", gsi.extraction_sheets.LL) %>%
  #tibble::rownames_to_column(var="file_source")
remove(gsi.extraction_sheets.LL)



## Load and compile species_ID tabs all years ------------------------------------
gsi.species_ID.LL <- c(
  # --- 2023:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                    pattern="new-format.xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID", col_types = "text")
         }),
  
  # --- 2024:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID", col_types = "text")
         }) #,
  
  # --- 2025:
  #lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
   #                 pattern=".xlsx", full.names=T), 
    #     function(x) {
     #      readxl::read_excel(x, sheet="species_ID", col_types = "text")
      #   })  
)

# Rename, convert to data frame: 
names(gsi.species_ID.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                         pattern="new-format.xlsx", full.names=F),
                              list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                         pattern=".xlsx", full.names=F) #,
                              #list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2025", 
                               #          pattern=".xlsx", full.names=F)  
)

# Convert the Large List into a useable R dataframe:
gsi.species_ID <- gsi.species_ID.LL %>%
  reduce(full_join) 
  #do.call("rbind", gsi.species_ID.LL) %>%
  #tibble::rownames_to_column(var="file_source")
remove(gsi.species_ID.LL)




## JOIN into a master GSI dataframe ------------------------------------
gsi.master <- full_join(
  gsi.repunits_table_ids %>% 
    select(-c(collection, mixture_collection)),
  gsi.extraction_sheets %>% 
    select(-c( SampleID, StockCode, Adipose.Status, Lane, Fish, Tray, PID, DigestionDate..YYYY.MM.DD., ExtractionTech, X16, X17)),
  by=c("indiv", "ID_Source")
) %>%
  relocate(c(Vial, CatchYear, CatchJulDate, CatchDate..YYYY.MM.DD.), .after=indiv) %>%
  relocate(SampleName, .before=indiv) %>%
  full_join(.,
            gsi.species_ID,
            by=c("indiv")) %>%
  rename_with(.cols=everything(), ~paste0("MGL_", .x)) %>%
  mutate(MGL_CatchDate = case_when(grepl("-", MGL_CatchDate..YYYY.MM.DD.) ~ lubridate::ymd(MGL_CatchDate..YYYY.MM.DD.),
                                   TRUE ~ janitor::excel_numeric_to_date(as.numeric(MGL_CatchDate..YYYY.MM.DD.)))) %>%
  select(-c(MGL_CatchDate..YYYY.MM.DD.)) %>%
  relocate(MGL_CatchDate, .before=MGL_ID_Source) %>%
  print()


### Export master GSI file in case needed ------------------------------------
writexl::write_xlsx(gsi.master, path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/", 
                                            "/San Juan MGL master file ",
                                            Sys.Date(),
                                            ".xlsx"))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ================================================================= JOIN: FIELD BIODATA + GSI RESULTS =================================================================

# Joining biosample sheet of field ata to the GSI master file created above 
# Add some new variables/clean up existing ones 
biosamp.gsi.linked <-  left_join(biodata,
                                 gsi.master,
                                 by=c("DNA_vial" = "MGL_Vial"),
                                 na_matches = "never") %>%
  mutate(origin_method = case_when(ad_clip=="Y" ~ "Ad clip (presence)",
                                   cwt=="Y" | !is.na(cwt_code) ~ "CWT",
                                   MGL_ID_Source=="PBT" ~ "PBT (presence/absence)",
                                   grepl("HATTK|HATSP|HATLP", usid) ~ "Hatchery sample",
                                   year=="2023" & gear %in% c("IPT", "6' RST") ~ "F/W sample, no hatchery releases",
                                   year=="2023" & gear%in%c("Beach seine", "Mini purse seine") & date < as.Date("2023-05-25") ~ "F/W sample before hatchery releases",
                                   year=="2024" & date < as.Date("2024-04-29") ~ "F/W sample before hatchery releases",
                                   year=="2025" & ad_clip=="N" ~ "Ad clip (absence in f/w)",
                                   year=="2025" & date < as.Date("2025-04-27") ~ "F/W sample before hatchery releases",
                                   MGL_ID_Source=="GSI" ~ "PBT (presence/absence)",
                                   gear %in% c("IPT", "6' RST") & species %in% c("chum", "coho", "trout") ~ "F/W sample of non-enhanced species",
                                   gear=="IPT" ~ "F/W sample before hatchery releases",
                                   TRUE ~ NA),
         hatchery_origin = case_when(ad_clip=="Y" ~ "Hatchery",
                                     cwt=="Y" | !is.na(cwt_code) ~ "Hatchery",
                                     MGL_ID_Source=="PBT" ~ "Hatchery",
                                     grepl("HATTK|HATSP|HATLP", usid) ~ "Hatchery",
                                     year=="2023" & gear %in% c("IPT", "6' RST") ~ "Natural",
                                     year=="2023" & gear%in%c("Beach seine", "Mini purse seine") & date < as.Date("2023-05-25") ~ "Natural",        # earliest hatchery release date 2023 (lakepen)
                                     year=="2024" & date < as.Date("2024-04-29") ~ "Natural",  # earliest hatchery release date 2024 (river)
                                     year=="2025" & ad_clip=="N" ~ "Natural",
                                     year=="2025" & date < as.Date("2025-04-27") ~ "Natural",  # earliest hatchery release date 2025 (river)
                                     MGL_ID_Source=="GSI" ~ "Natural (assumed)",
                                     gear %in% c("IPT", "6' RST") & species %in% c("chum", "coho", "trout") ~ "Natural",
                                     gear=="IPT" ~ "Natural", 
                                     TRUE ~ "Unknown"),
         resolved_stock_id_method = case_when(MGL_ID_Source=="PBT" ~ "PBT",
                                              !is.na(cwt_code) ~ "CWT",
                                              MGL_ID_Source=="GSI" & MGL_associated_collection_prob>80 ~ "GSI (>80%)",
                                              MGL_ID_Source=="GSI" & MGL_associated_collection_prob>70 & MGL_associated_collection_prob<80 ~ 
                                                "GSI (70-80%)",
                                              gear %in% c("IPT", "6' RST") ~ "F/W sample",
                                              TRUE ~ NA
                                              ),
         resolved_stock_id = case_when(MGL_ID_Source=="PBT" & !is.na(MGL_PBT_brood_group) ~ 
                                         paste0(stringr::str_to_title(gsub(MGL_PBT_brood_collection, pattern="_", replacement=" ")), 
                                                " (", 
                                                stringr::str_to_title(gsub(MGL_PBT_brood_group, pattern="_", replacement=" ")),
                                                ")"),
                                       
                                       MGL_ID_Source=="PBT" & is.na(MGL_PBT_brood_group) ~ 
                                         stringr::str_to_title(gsub(MGL_PBT_brood_collection, pattern="_", replacement=" ")),
                                       
                                       !is.na(cwt_code) ~ paste0(cwt_stock_ID, " (", cwt_release_group, ")"),
                                       
                                       MGL_ID_Source=="GSI" & MGL_associated_collection_prob>80 ~ 
                                         stringr::str_to_title(gsub(MGL_top_collection, pattern="_", replacement=" ")),
                                       
                                       MGL_ID_Source=="GSI" & MGL_associated_collection_prob>70 & MGL_associated_collection_prob<80  ~ 
                                         stringr::str_to_upper(gsub(MGL_repunit.1, pattern="_", replacement=" ")),
                                       
                                       gear %in% c("IPT", "6' RST") ~ "San Juan River",
                                       TRUE ~ "Unknown"),
         
         resolved_stock_origin = paste0(hatchery_origin, " ", resolved_stock_id),
         
         resolved_stock_origin_rollup = case_when(grepl("queets|forks creek|abernathy|hood|cwa|nooksack|hoh|quinault|sol|duc", 
                                                        resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " US"),
                                                  
                                                  grepl("harrison|fraser", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Fraser River"),

                                                  hatchery_origin=="Hatchery" & !grepl("queets|forks creek|abernathy|hood|cwa|nooksack|hoh|quinault|sol|duc", 
                                                                                       resolved_stock_id, ignore.case=T) ~ 
                                                    resolved_stock_origin,
                                                  
                                                  hatchery_origin=="Natural" & !grepl("GSI", resolved_stock_id_method) ~ resolved_stock_origin,
                                                  
                                                  grepl("GSI", resolved_stock_id_method) & grepl("san juan", resolved_stock_id, ignore.case=T) ~ resolved_stock_origin,
                                                  grepl("GSI", resolved_stock_id_method) & grepl("nitinat|sooke", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Sooke/Nitinat"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("sarita", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Sarita"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("toquart|thornton", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Outer Barkley"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("Nahmint", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Nahmint"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("stamp|robertson|gold", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Inner Barkley"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("tranquil|kennedy|cypre|bedwell", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Clayoquot"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("zeballos|tlupana|tahsis|tahsish|moyeha|megin|leiner|kaouk|conuma|burman|marble", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Nootka/Kyuquot"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("marble|colonial", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Quatsino"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("woss|salmon|quinsam|nimpkish", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Northeast VI"),
                                                  grepl("GSI", resolved_stock_id_method) & grepl("cowichan|nanaimo|qualicum|puntledge|englishman", resolved_stock_id, ignore.case=T) ~ paste0(hatchery_origin, " Strait of Georgia"),
                                                  
                                                  resolved_stock_id=="Unknown" ~ paste0(hatchery_origin, " Unknown"),

                                                  TRUE ~ "FLAG"), 
         stray_status = case_when(grepl("san juan", resolved_stock_id, ignore.case=T) ~ "local",
                                  !grepl("san juan", resolved_stock_id, ignore.case=T) ~ "stray",
                                  TRUE ~ NA)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ================================================================= LOAD MICROCHEM DATA =================================================================

## Load microchemistry raw results -------------------
otochem <- readxl::read_excel(path=here::here("data", "biosamples", "San Juan Otolith Microchemistry Data report.xlsx"),
                              sheet="radii") %>%
  mutate(otolith_box_vial = paste0(otolith_box, sep="-", otolith_vial)) 


### JOIN: Juvi field biodata+GSI + microchem (juveniles) -------------------
biosamp.gsi.otochem.linked <- left_join(biosamp.gsi.linked,
                                        otochem %>%
                                          select(c(`Sample ID`, `Estuary Days`, `Marine Duration time (s)`, `Zinc age (European Notation)`,
                                                   `Fairy Lake OR Estuary entry Back-calculated FL(mm)`, Comments)) %>%
                                          rename(otochem_comments = Comments),
                                        by=c("lethal_tag_no"="Sample ID"))



## Load EPRO All Adult Biosampling -------------------
# Might git an std::bad_alloc error message - close some windows to free up memory if this happens (esp Google Chrome)
sj.aab <- readxl::read_excel(path=list.files(path=here::here("data", "biosamples"),
                                             pattern="R_OUT - All Adult Biosampling ALL FACILITIES WITH RESULTS",
                                             full.names = T),
                             sheet=2) %>%
  janitor::clean_names() %>%
  filter(facility_name=="San Juan River H") %>%
  select(start_date, spawning_year, species, sex, poh_length_mm, external_marks,
         r_otolith_box_vial_concat, otolith_hatch_code, scale_part_age:scale_total_age_yrs,
         scale_gilbert_age, scale_european_age, r_resolved_total_age, r_resolved_total_age_method, r_resolved_brood_year,
         r_resolved_origin, r_resolved_origin_method, r_resolved_stock_id, r_resolved_stock_id_method, r_resolved_stock_origin)


### JOIN: All Adult Biosampling + microchem (adults) -------------------
aab.otochem.linked <- left_join(sj.aab,
                                otochem %>%
                                  select(c(`Sample ID`, otolith_box, otolith_vial, `Estuary Days`, `Marine Duration time (s)`, 
                                           `Zinc age (European Notation)`, `Fairy Lake OR Estuary entry Back-calculated FL(mm)`, 
                                           Comments, otolith_box_vial)) %>%
                                  rename(otochem_comments = Comments),
                                by=c("r_otolith_box_vial_concat" = "otolith_box_vial"))


## Reverse join for data sharing with MQ/NL -------------------
otochem.w.biodata <- left_join(otochem,
                               sj.aab %>%
                                 select(-c(spawning_year)),
                               by=c("otolith_box_vial" = "r_otolith_box_vial_concat")) %>%
  left_join(.,
            biosamp.gsi.linked %>%
              select(gear, usid, date, species, fork_length_mm, height_mm, field_weight_g, ad_clip, hatchery_origin,
                     lethal_tag_no, comments, condK, MGL_ID_Source, MGL_PBT_brood_collection, MGL_PBT_brood_group, MGL_top_collection,
                     MGL_associated_collection_prob, MGL_species, origin_method:stray_status),
            by=c("Sample ID" = "lethal_tag_no")) %>% 
  mutate(species = coalesce(species.x, species.y),
         sample_date = coalesce(start_date, date),
         adipose_clipped = coalesce(external_marks, ad_clip),
         resolved_origin = coalesce(r_resolved_origin, hatchery_origin),      
         resolved_origin_method = coalesce(r_resolved_origin_method, origin_method),
         resolved_stock_ID = coalesce(r_resolved_stock_id, resolved_stock_id),
         resolved_stock_ID_method = coalesce(r_resolved_stock_id_method, resolved_stock_id_method),
         resolved_stock_ID_origin = coalesce(r_resolved_stock_origin, resolved_stock_origin)
         ) %>%
  select(-c(species.x, species.y, start_date, date, external_marks, ad_clip, r_resolved_origin, hatchery_origin, r_resolved_origin_method,
            origin_method, r_resolved_stock_id, resolved_stock_id, r_resolved_stock_id_method, resolved_stock_id_method, 
            r_resolved_stock_origin, resolved_stock_origin, MGL_ID_Source, comments, MGL_PBT_brood_group)) %>%
  relocate(c(sample_date, gear, usid, species, MGL_species, sex, adipose_clipped, poh_length_mm, fork_length_mm, height_mm, field_weight_g, condK), 
           .before=otolith_box_vial) %>%
  relocate(c(resolved_origin, resolved_origin_method, resolved_stock_ID, resolved_stock_ID_method, resolved_stock_ID_origin,
             resolved_stock_origin_rollup, stray_status), .after = r_resolved_brood_year)



### Export for MQ/NL --------
writexl::write_xlsx(otochem.w.biodata, here::here("outputs", "R_OUT - Otolith microchemstry results with biodata.xlsx"))



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



# ================================================================= LOAD DIET DATA =================================================================

# Add some new groupings, clean up old ones
diet.results <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/",
                                                   pattern="^Stomach_Analyis_Master_cleaned.xlsx",
                                                   full.names=T, recursive=T),
                                   sheet="CLEANED_data", skip=2) %>%
  select(-c(Company_Doing_Analysis, Year, Date_Sampled, Biologica_Sample_ID)) %>% 
  rename(diet_comments = Comments) %>% 
  janitor::clean_names() %>%
  mutate(across(c(source1:total_ww_g), ~case_when(.%in%c("n/a", "NA")~NA, TRUE~.)),
         ww_note = case_when(total_ww_g=="TR" ~ "TRACE",
                             TRUE ~ NA),
         total_ww_g = case_when(total_ww_g=="TR" ~ 0,
                                TRUE ~ as.numeric(total_ww_g))) %>%
  mutate(lowest_taxon_final = case_when(!is.na(taxon) ~ taxon,
                                        is.na(taxon) & !is.na(order) ~ paste0("O. ", order),
                                        is.na(taxon) & is.na(order) & !is.na(class) ~ paste0("C. ", class),
                                        is.na(taxon) & is.na(order) & is.na(class) & !is.na(phylum) ~ paste0("Ph. ", phylum),
                                        is.na(phylum) & !is.na(category) ~ paste0("Unknown ", category),
                                        TRUE ~ "FLAG"),
         taxonomy_simple = case_when(grepl("Acari|arachnid|Araneae|pseudoscorpion", lowest_taxon_final, ignore.case=T) ~ "Arachnids",
                                     grepl("Psocodea", lowest_taxon_final, ignore.case=T) ~ "Lice (non-parasitic)",
                                     #grepl("formicidae", lowest_taxon_final, ignore.case=T) ~ "Ants", 
                                     grepl("staphylinid|Coleoptera", lowest_taxon_final, ignore.case=T) ~ "Beetles",
                                     grepl("hemiptera", lowest_taxon_final, ignore.case=T) ~ "True bugs",
                                     grepl("myriapod", lowest_taxon_final, ignore.case=T) ~ "Centipedes",
                                     grepl("annelid", lowest_taxon_final, ignore.case=T) ~ "Worms",
                                     grepl("chironomid", lowest_taxon_final, ignore.case=T) ~ "Midges",
                                     grepl("Diptera", lowest_taxon_final, ignore.case=T) ~ "'True' flies (unspecified)",
                                     grepl("caddisfly|trichoptera", lowest_taxon_final, ignore.case=T) ~ "Aquatic flies",
                                     grepl("Hymenoptera", lowest_taxon_final, ignore.case=T) ~ "Wasps/bees",
                                     grepl("Neuroptera", lowest_taxon_final, ignore.case=T) ~ "Lacewing flies",
                                     grepl("Lepidoptera", lowest_taxon_final, ignore.case=T) ~ "Butterflies/moths",

                                     grepl("Balanomorpha|Cirripedia", lowest_taxon_final, ignore.case=T) ~ "Barnacles",
                                     grepl("Copepod|Eucalanus|harpactacoid|harpacticoid", lowest_taxon_final, ignore.case=T) ~ "Copepods (non-parasitic)",
                                     grepl("Ostracod", lowest_taxon_final, ignore.case=T) ~ "Ostracods",
                                     grepl("Isopod|Gnorimosphaeroma", lowest_taxon_final, ignore.case=T) ~ "Isopods",
                                     grepl("Amphipod|Corophium|Gammaridae", lowest_taxon_final, ignore.case=T) ~ "Amphipods",
                                     grepl("Decapod|Anomura|porcellanidae", lowest_taxon_final, ignore.case=T) ~ "Decapods",
                                     grepl("campylaspis|cumacea|euphausiacea", lowest_taxon_final, ignore.case=T) ~ "Shrimps",
                                     grepl("podon", lowest_taxon_final, ignore.case=T) ~ "Branchiopods",
                                     grepl("Phyllodocida|Polychaet", lowest_taxon_final, ignore.case=T) ~ "Polychaete worms",
                                     grepl("cephalopod", lowest_taxon_final, ignore.case=T) ~ "Octopus (larvae)",
                                     
                                     grepl("Actinopterygii|Teleost|Osmeriformes|Clupeiformes|Ammodytidae|Perciformes", lowest_taxon_final, ignore.case=T) ~ "Fish",
                                     
                                     grepl("parasite|sea louse|nematod|trematod", lowest_taxon_final, ignore.case=T) ~ "Parasites*",
                                     
                                     grepl("Crustacea", lowest_taxon_final, ignore.case=T) ~ "Crustaceans (unspecified)",
                                     grepl("Arthropod", lowest_taxon_final, ignore.case=T) ~ "Arthropods (unspecified)",
                                     grepl("invertebrate", lowest_taxon_final, ignore.case=T) ~ "Invertebrates (unspecified)",
                                     grepl("insect|formicidae", lowest_taxon_final, ignore.case=T) ~ "Insects (unspecified)",
                                     
                                     grepl("Non-food", source1, ignore.case=T) ~ "Non-food (plants, seaweed, rocks, feathers)",
                                     
                                     TRUE ~ lowest_taxon_final),
         
         taxonomy_stage_simple = case_when(!is.na(stage_simple) ~ paste0(taxonomy_simple, " (", stage_simple, ")"),
                                           is.na(stage_simple) ~ taxonomy_simple)) %>%
  print()




                                                         
## JOIN: field biodata+GSI + diet results ----------------- 

biosample.long.diet <- full_join(biosamp.gsi.otochem.linked %>% 
                                     select(year, gear, usid, date, DOY, species, fork_length_mm, height_mm, field_weight_g, lab_weight_g,
                                            modelled_weight_g, resolved_weight_g, resolved_weight_source, 
                                            ad_clip, cwt, hatchery_origin, lethal_tag_no, DNA_vial, 
                                            MGL_ID_Source, MGL_PBT_brood_year, MGL_PBT_brood_collection, MGL_PBT_brood_group, MGL_top_collection,
                                            MGL_associated_collection_prob, MGL_species, MGL_notes,
                                            resolved_stock_id, resolved_stock_origin, resolved_stock_origin_rollup, stray_status,
                                            `Estuary Days`, `Fairy Lake OR Estuary entry Back-calculated FL(mm)`, otochem_comments),
                                   diet.results %>%
                                     select(-c(project)),
                                   by=c("lethal_tag_no" = "client_sample_id"),
                                   multiple = "all")



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ================= Create readme ================= 
readme <- data.frame(x1=c("last update:",
                       "",
                       "SHEET NAME",
                       "sample_event_meta",
                       "enviro",
                       "set_totals",
                       "mark-release",
                       "biosampling",
                       "biosampling long with diet",
                       "otolith microchem"
                       ),
                     
                     x2=c(as.character(Sys.Date()),
                       "",
                       "SHEET DESCRIPTION",
                       "Sampling events with associated capture metadata including lat/longs, effort, weather and other relevant notes.",
                       "Environmental data associated with field sampling events. For RST sampling this includes YSI DSS readings, RST RPMs, etc. but does not include HOBO data logger readings (see data files beginning 'SJHOBO' in https://github.com/SCA-stock-assess/WCVI-SanJuan-Juvenile/tree/main/data/enviro). Beach seine water quality data were collected by Pacheedaht First Nations and stored in their database. Purse seine YSI readings are included.",
                       "Total enumeration of all sampling events, for example each day of RST sampling or each seine set. Includes Bismarck Brown recaptures in the RST",
                       "Release dates and abundances for all fish marked with Bismarck Brown for RST trap efficiency trials.",
                       "All juvenile fish biosampling data collected from 2023-2025 along with associated lab dissection details. This sheet should be used for analysis such as calculating average fish length, weight, etc. Each row is a unique fish. Included here too are genetic stock ID results for those fish (predominantly Chinook) analyzed for genetics.",
                       "This is all fish biosampled, but joined to stomach contents results. As one fish can have >1 prey group in its stomach, data are in long-form, where each row represents a unique dietary group within an individual fish. Fish traits (e.g., length, weight) are duplicated across multiple rows, depending on how many diet items a fish had. Therefore this sheet is not appropriate for calculating pooled, average lengths, weights, etc. It is meant for analyzing diet results along with context on hatchery-origin, stock ID, size, etc.",
                       "Otolith microchemistry results joined to capture metadata for both adults and juveniles. Adult microchemistry results are used to report on size-specific survival while juvenile microchemistry informs estuary residency time."
                       )
)


# ================= EXPORT ================= 
# Create empty workbook ---------------------------
R_OUT_SJjuviDB <- openxlsx::createWorkbook()


# Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT_SJjuviDB, "readme")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "sample_event_meta")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "enviro")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "set_totals")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "mark-release")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "biosampling")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "biosampling long w diet")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "otolith microchem raw")


# Write data to tabs (read in data and then re-save to tabs in new workbook) ---------------------------
openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="readme", 
                    x = readme)

openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="sample_event_meta", 
                    x = readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                           pattern="^San Juan PSSI master database",
                                                           full.names=T),
                                           sheet="sample_event_meta"))
openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="enviro", 
                    x = readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                           pattern="^San Juan PSSI master database",
                                                           full.names=T),
                                           sheet="enviro"))
openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="set_totals", 
                    x = readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                           pattern="^San Juan PSSI master database",
                                                           full.names=T),
                                           sheet="set_totals"))
openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="mark-release", 
                    x = readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                           pattern="^San Juan PSSI master database",
                                                           full.names=T),
                                           sheet="mark-release"))
openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="biosampling", 
                    x = biosamp.gsi.otochem.linked)

openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="biosampling long w diet", 
                    x = biosample.long.diet)

openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="otolith microchem raw", 
                    x = otochem.w.biodata)




# Export to github ------------------------------------
openxlsx::saveWorkbook(wb = R_OUT_SJjuviDB, 
                       file = paste0(here::here("data", "juvenile"),
                                     "/R_OUT - San Juan PSSI master database ",
                                     stringr::str_sub(string=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                                        pattern="^San Juan PSSI master database"),
                                                      start=31, 
                                                      end=42),
                                     " WITH RESULTS.xlsx"),
                       overwrite = T)

# Export to Drive ------------------------------------
openxlsx::saveWorkbook(wb = R_OUT_SJjuviDB, 
                       file = paste0("//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                     "R_OUT - San Juan PSSI master database ",
                                     stringr::str_sub(string=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                                        pattern="^San Juan PSSI master database"),
                                                      start=31, 
                                                      end=42),
                                     " WITH RESULTS.xlsx"),
                       overwrite = T)






# Clear library for sake of running as source()
remove(list = ls())
