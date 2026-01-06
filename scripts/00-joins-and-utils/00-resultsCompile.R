# Add lab results in to database 
# Feb 2024
# Only need to run if new lab results returned (e.g., GSI, stomachs, etc.) or if changes to underlying data file (San Juan PSSI master database...)



# Load libraries ------------------------------------
library(tidyverse)
options(scipen = 99999999999999)
"%notin%" <- Negate("%in%")





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ================================================================= DATA LOAD =================================================================


## ========================= LOAD FIELD BIODATA =========================

### Load field biodata and add in some variables ------------------------------------
# (Doing this here so it is carried through the subsequent biodata tabs later)
  # See height_weight script for rationale on equation used to convert height to modelled weight
biodata <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                              pattern="^San Juan PSSI master database",
                                              full.names=T),
                              sheet="biosampling", guess_max = 5000) %>%
  mutate(resolved_weight_source = case_when(!is.na(weight) & is.na(lab_weight_g) ~ "field",
                                            is.na(weight) & !is.na(lab_weight_g) ~ "lab",
                                            is.na(weight) & is.na(lab_weight_g) ~ "modelled"),
         modelled_weight_g = as.numeric(exp(-6.5 + (2.9*log(height)))),
         resolved_weight_g = case_when(resolved_weight_source=="field" ~ as.numeric(weight),
                                       resolved_weight_source=="lab" ~ as.numeric(lab_weight_g),
                                       resolved_weight_source=="modelled" ~ as.numeric(modelled_weight_g)),
         condK = resolved_weight_g/(as.numeric(length)^3)*100000) %>%
  relocate(c(lab_weight_g, modelled_weight_g), .after=weight) %>%
  relocate(c(resolved_weight_g, resolved_weight_source), .after=modelled_weight_g) %>%
  rename(length_mm = length,
         height_mm = height,
         field_weight_g = weight) %>%
  print()




## ========================= LOAD MGL FILES =========================

### Load and compile repunit_table_ids tabs all years ------------------------------------
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
         })
)

# Rename, convert to data frame: 
names(gsi.repunits_table_ids.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                            pattern="new-format.xlsx", full.names=F),
                                   list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                              pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.repunits_table_ids <- do.call("rbind", gsi.repunits_table_ids.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.repunits_table_ids.LL)




### Load and compile extraction_sheet tabs all years ------------------------------------
gsi.extraction_sheets.LL <- c(
  # --- 2023:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                    pattern="new-format.xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         }),
  
  # --- 2024:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         })
)

# Rename, convert to data frame: 
names(gsi.extraction_sheets.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                                 pattern="new-format.xlsx", full.names=F),
                                      list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.extraction_sheets <- do.call("rbind", gsi.extraction_sheets.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.extraction_sheets.LL)



### Load and compile species_ID tabs all years ------------------------------------
gsi.species_ID.LL <- c(
  # --- 2023:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                    pattern="new-format.xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         }),
  
  # --- 2024:
  lapply(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         })
)

# Rename, convert to data frame: 
names(gsi.species_ID.LL) <- c(list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2023", 
                                                 pattern="new-format.xlsx", full.names=F),
                                      list.files("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/2024", 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.species_ID <- do.call("rbind", gsi.species_ID.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.species_ID.LL)




## ========================= JOIN MGL INTO 1 MASTER FILE =========================

### Join into a master GSI dataframe ------------------------------------
gsi.master <- full_join(
  gsi.repunits_table_ids %>% 
    select(-c(file_source, collection, mixture_collection)),
  gsi.extraction_sheets %>% 
    select(-c(file_source, SampleID, StockCode, Adipose.Status, Lane, Fish, Tray, PID, DigestionDate..YYYY.MM.DD., ExtractionTech, X16, X17)),
  by=c("indiv", "ID_Source")
) %>%
  relocate(c(Vial, CatchYear, CatchJulDate, CatchDate..YYYY.MM.DD.), .after=indiv) %>%
  relocate(SampleName, .before=indiv) %>%
  full_join(.,
            gsi.species_ID %>%
              select(-c(file_source)),
            by=c("indiv")) %>%
  rename_with(.cols=everything(), ~paste0("MGL_", .x)) %>%
  mutate(MGL_CatchDate = case_when(grepl("-", MGL_CatchDate..YYYY.MM.DD.) ~ lubridate::ymd(MGL_CatchDate..YYYY.MM.DD.),
                                   TRUE ~ janitor::excel_numeric_to_date(as.numeric(MGL_CatchDate..YYYY.MM.DD.)))) %>%
  select(-c(MGL_CatchDate..YYYY.MM.DD.)) %>%
  relocate(MGL_CatchDate, .before=MGL_ID_Source) %>%
  print()


### Export in case needed ------------------------------------
writexl::write_xlsx(gsi.master, path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/", 
                                            "/San Juan MGL master file ",
                                            Sys.Date(),
                                            ".xlsx"))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                           LINK GSI MASTER TO BIODATA 


# Load Biosampling sheet of juvenile database, Join to MGL Master file ----------------- 
biosamp.gsi.linked <-  left_join(biodata,
                                 gsi.master,
                                 by=c("DNA_vial" = "MGL_Vial"),
                                 na_matches = "never") %>%
  mutate(hatchery_origin = case_when(ad_clip=="Y" ~ "Y",
                                     MGL_ID_Source=="PBT" ~ "Y",
                                     grepl("HATTK", usid) ~ "Y",
                                     year=="2023" & gear %in% c("IPT", "6'RST") ~ "N",
                                     year=="2024" & date < as.Date("2024-04-29") ~ "N",  # hatchery river release date 2024
                                     year=="2025" & ad_clip=="N" ~ "N",
                                     year=="2025" & date < as.Date("2025-04-27") ~ "N",  # earliest hatchery river release date 2025
                                     MGL_ID_Source=="GSI" ~ "N",
                                     gear %in% c("IPT", "6'RST") & species %in% c("chum", "coho", "trout") ~ "N",
                                     gear=="IPT" ~ "N", 
                                     TRUE ~ "U"),
         resolved_stock_id = case_when(MGL_ID_Source=="PBT" & !is.na(MGL_PBT_brood_group) ~ 
                                         paste0(stringr::str_to_title(gsub(MGL_PBT_brood_collection, pattern="_", replacement=" ")), 
                                                " (", 
                                                stringr::str_to_title(gsub(MGL_PBT_brood_group, pattern="_", replacement=" ")),
                                                ")"),
                                       MGL_ID_Source=="PBT" & is.na(MGL_PBT_brood_group) ~ 
                                         stringr::str_to_title(gsub(MGL_PBT_brood_collection, pattern="_", replacement=" ")),
                                       MGL_ID_Source=="GSI" & MGL_associated_collection_prob>80 ~ 
                                         stringr::str_to_title(gsub(MGL_top_collection, pattern="_", replacement=" ")),
                                       MGL_ID_Source=="GSI" & MGL_associated_collection_prob>70 & MGL_associated_collection_prob<80  ~ 
                                         stringr::str_to_upper(gsub(MGL_repunit.1, pattern="_", replacement=" ")),
                                       gear %in% c("IPT", "6'RST") ~ "San Juan River*",
                                       TRUE ~ NA),
         resolved_stock_origin = case_when(hatchery_origin=="Y" & !is.na(resolved_stock_id) ~ paste0("Hatchery ", resolved_stock_id),
                                           hatchery_origin=="N" & !is.na(resolved_stock_id) ~ paste0("Natural ", resolved_stock_id),
                                           hatchery_origin=="U" & !is.na(resolved_stock_id) ~ paste0("Unknown ", resolved_stock_id),
                                           hatchery_origin=="U" & is.na(resolved_stock_id) ~ NA,
                                           TRUE ~ NA),
         resolved_stock_origin_rollup = case_when(hatchery_origin=="Y" & 
                                                    grepl("queets|forks creek|abernathy|HOOD|CWA|Nooksack", resolved_stock_id, 
                                                          ignore.case=T) ~ "Hatchery US",
                                                  hatchery_origin=="N" & 
                                                    grepl("queets|forks creek|abernathy|HOOD|CWA|Nooksack", resolved_stock_id, 
                                                          ignore.case=T) ~ "Natural US",
                                                  grepl("san juan|nitinat", resolved_stock_id, ignore.case=T) ~ resolved_stock_origin,
                                                  hatchery_origin=="N" & grepl("sooke", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Sooke/Nitinat",
                                                  hatchery_origin=="Y" & grepl("toquart", resolved_stock_id, ignore.case=T) ~ 
                                                    "Hatchery Outer Barkley",
                                                  hatchery_origin=="N" & grepl("toquart", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Outer Barkley",
                                                  hatchery_origin=="N" & grepl("bedwell", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Clayoquot",
                                                  hatchery_origin=="N" & grepl("stamp|robertson", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Inner Barkley",
                                                  hatchery_origin=="N" & grepl("megin", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Nootka-Kyuquot",
                                                  hatchery_origin=="Y" & grepl("SWVI", resolved_stock_id, ignore.case=T) ~ 
                                                    "Hatchery SWVI",
                                                  hatchery_origin=="N" & grepl("SWVI", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural SWVI",
                                                  hatchery_origin=="N" & grepl("harrison", resolved_stock_id, ignore.case=T) ~ 
                                                    "Natural Lower Fraser River",
                                                  TRUE ~ "FLAG"), 
         stray_status = case_when(!is.na(resolved_stock_id) & grepl("san juan", resolved_stock_id, ignore.case=T) ~ "local",
                                  !is.na(resolved_stock_id) & !grepl("san juan", resolved_stock_id, ignore.case=T) ~ "stray",
                                  TRUE ~ NA)) %>%
  print()






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                           LOAD & JOIN DIET DATA

# ================= MASTER DIET DATA ================= 
diet.results <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/",
                                                   pattern="^Stomach_Analyis_Master_cleaned.xlsx",
                                                   full.names=T, recursive=T),
                                   sheet=2, skip=5) %>%
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
                                     grepl("Diptera", lowest_taxon_final, ignore.case=T) ~ "'True' flies",
                                     grepl("caddisfly|trichoptera", lowest_taxon_final, ignore.case=T) ~ "Aquatic flies",
                                     grepl("Hymenoptera", lowest_taxon_final, ignore.case=T) ~ "Wasps/bees",
                                     grepl("Neuroptera", lowest_taxon_final, ignore.case=T) ~ "Lacewing flies",
                                     grepl("Lepidoptera", lowest_taxon_final, ignore.case=T) ~ "Butterflies/moths",

                                     grepl("Balanomorpha|Cirripedia", lowest_taxon_final, ignore.case=T) ~ "Barnacles",
                                     grepl("Copepod|Eucalanus|harpactacoid|harpacticoid", lowest_taxon_final, ignore.case=T) ~ "Copepods",
                                     grepl("Ostracod", lowest_taxon_final, ignore.case=T) ~ "Ostracods",
                                     grepl("Isopod|Gnorimosphaeroma", lowest_taxon_final, ignore.case=T) ~ "Isopods",
                                     grepl("Amphipod|Corophium|Gammaridae", lowest_taxon_final, ignore.case=T) ~ "Amphipods",
                                     grepl("Decapod|Anomura|porcellanidae", lowest_taxon_final, ignore.case=T) ~ "Decapods",
                                     grepl("campylaspis|podon|cumacea|euphausiacea", lowest_taxon_final, ignore.case=T) ~ "Shrimps",
                                     grepl("Phyllodocida|Polychaet", lowest_taxon_final, ignore.case=T) ~ "Polychaete worms",
                                     grepl("cephalopod", lowest_taxon_final, ignore.case=T) ~ "Octopus (larvae)",
                                     
                                     grepl("Actinopterygii|Teleost|Osmeriformes|Clupeiformes|Ammodytidae|Perciformes", lowest_taxon_final, ignore.case=T) ~ "Fish",
                                     
                                     grepl("parasite|sea louse|nematod|trematod", lowest_taxon_final, ignore.case=T) ~ "Parasites*",
                                     
                                     grepl("Crustacea", lowest_taxon_final, ignore.case=T) ~ "Crustaceans (unspecified)",
                                     grepl("Arthropod", lowest_taxon_final, ignore.case=T) ~ "Arthropods (unspecified)",
                                     grepl("invertebrate", lowest_taxon_final, ignore.case=T) ~ "Invertebrates (unspecified)",
                                     grepl("insect|formicidae", lowest_taxon_final, ignore.case=T) ~ "Insects (unspecified)",
                                     
                                     grepl("Rock|feather", lowest_taxon_final, ignore.case=T) ~ "Non-food",
                                     grepl("plant|Seaweed", lowest_taxon_final, ignore.case=T) ~ "Plant/seaweed",
                                     
                                     TRUE ~ lowest_taxon_final),
         
         taxonomy_stage_simple = case_when(!is.na(stage_simple) ~ paste0(taxonomy_simple, " (", stage_simple, ")"),
                                           is.na(stage_simple) ~ taxonomy_simple)) %>%
  print()




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                           LINK GSI + BIODATA TO STOMACH RESULTS


core.biosample.linked <- full_join(biosamp.gsi.linked %>%
                                     select(year, gear, usid, date, DOY, species, length_mm, height_mm, field_weight_g, lab_weight_g,
                                            modelled_weight_g, resolved_weight_g, resolved_weight_source, 
                                            ad_clip, cwt, hatchery_origin, lethal_tag_no, DNA_vial, 
                                            MGL_ID_Source, MGL_PBT_brood_year, MGL_PBT_brood_collection, MGL_PBT_brood_group, MGL_top_collection,
                                            MGL_associated_collection_prob, MGL_species, MGL_notes,
                                            resolved_stock_id, resolved_stock_origin, resolved_stock_origin_rollup, stray_status),
                                   diet.results %>%
                                     select(-c(project)),
                                   by=c("lethal_tag_no" = "client_sample_id"),
                                   multiple = "all")



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# ================= EXPORT ================= 
# Create empty workbook ---------------------------
R_OUT_SJjuviDB <- openxlsx::createWorkbook()


# Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT_SJjuviDB, "sample_event_meta")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "enviro")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "set_totals")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "mark-release")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "biosampling detailed w GSI")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "biosampling core results")


# Write data to tabs (read in data and then re-save to tabs in new workbook) ---------------------------
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
                    sheet="biosampling detailed w GSI", 
                    x = biosamp.gsi.linked)

openxlsx::writeData(R_OUT_SJjuviDB, 
                    sheet="biosampling core results", 
                    x = core.biosample.linked)




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
