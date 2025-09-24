# Add lab results in to database 
# Feb 2024
# Only need to run if new lab results returned (e.g., GSI, stomachs, etc.) or if changes to underlying data file (San Juan PSSI master database...)



# Load libraries ------------------------------------
library(tidyverse)
options(scipen = 99999999999999)


# ========================= LOAD MGL FILES =========================

# Load and compile repunit_table_ids tabs all years ------------------------------------
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







# Load and compile extraction_sheet tabs all years ------------------------------------
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






# Load and compile species_ID tabs all years ------------------------------------
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



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~




# ========================= JOIN MGL INTO 1 MASTER FILE =========================

# Join into a master dataframe ------------------------------------
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


# Export in case needed ------------------------------------
writexl::write_xlsx(gsi.master, path=paste0("//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/GSI to join/", 
                                            "/San Juan MGL master file ",
                                            Sys.Date(),
                                            ".xlsx"))





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                           LINK GSI MASTER TO BIODATA 


# Load Biosampling sheet of juvenile database, Join to MGL Master file ----------------- 
biosamp.gsi.linked <-  left_join(
  readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                     pattern="^San Juan PSSI master database",
                                     full.names=T),
                     sheet="biosampling", guess_max = 5000),
  gsi.master,
  by=c("DNA_vial" = "MGL_Vial"),
  na_matches = "never")




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#                                                           LOAD & JOIN DIET DATA

# ================= MASTER DIET DATA ================= 
diet.results <- readxl::read_excel(path=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/",
                                                   pattern="^Stomach_Analyis_Master.xlsx",
                                                   full.names=T, recursive=T),
                                   sheet=2, skip=5) %>%
  select(-c(Company_Doing_Analysis, Year, Project, Date_Sampled, Biologica_Sample_ID)) %>% 
  rename(biologica_comments = Comments) %>% 
  janitor::clean_names() %>%
  mutate(across(c(stage:total_ww_g), ~case_when(.=="n/a"~NA, TRUE~.))) %>%
  mutate(taxon_clean = stringr::str_remove(taxon, " indet."),
         taxon_clean = case_when(grepl("fish feed", biologica_comments) ~ "Fish feed",
                                  TRUE ~ taxon_clean),
         taxonomy_stage_simple_detail = case_when(
                                     source=="Benthic" & taxon_clean %in% c("Acari", "Diptera") ~ paste0(taxon_clean, " (larvae/pupae)"),
                                     source=="Benthic" & order=="Balanomorpha" ~ "Balanomorpha",
                                     source=="Benthic" & order=="Isopoda" ~ "Isopod",
                                     source=="Benthic" & taxon_clean=="Ostracoda" ~ "Ostracod",
                                     source=="Benthic" & taxon_clean=="Copepoda" ~ "Copepod (Harpacticoid)",
                                     source=="Benthic" & taxon_clean=="Phyllodocida" ~ "Phyllodocida",
                                     source=="Fish" & taxon_clean=="Actinopterygii" ~ "Fish (unknown)",
                                     source=="Fish" & taxon_clean=="Perciformes" ~ "Fish (likely sandlance)",
                                     source=="Fish" & taxon_clean=="Teleostei" & !grepl("Perciformes", biologica_comments) ~ "Fish (likely herring/sardine/anchovy)",
                                     source=="Fish" & taxon_clean=="Teleostei" & grepl("Perciformes", biologica_comments) ~ "Fish (likely sandlance)",
                                     source=="Parasite" & taxon_clean%in%c("Nematoda", "Trematoda") ~ "Internal parasite",
                                     source=="Planktonic" & taxon_clean=="Amphipoda" ~ "Amphipod (adult/juvenile)",
                                     source=="Planktonic" & taxon_clean=="Crustacean remains" ~ "Amphipod (adult/juvenile)",
                                     source=="Planktonic" & taxon_clean=="Decapoda" ~ "Decapod (zoae/megalopa)",
                                     source=="Planktonic" & taxon_clean=="Ostracoda" ~ "Ostracod", 
                                     source=="Terrestrial" ~ paste0(taxon_clean, " (adult)"),
                                     source=="Undetermined" & taxon_clean=="Amphipoda" ~ "Amphipod (adult/juvenile)",
                                     source=="Undetermined" & taxon_clean=="Crustacean remains" ~ "UnID crustacean",
                                     source=="Undetermined" & taxon_clean=="Decapoda" ~ "Decapod (zoae/megalopa)",
                                     source=="Undetermined" & taxon_clean=="Diptera" ~ "Diptera (larvae/pupae)",
                                     source=="Undetermined" & taxon_clean=="Insecta" ~ "UnID insect",
                                     source=="Undetermined" & taxon_clean=="Invertebrate remains" ~ "UnID invert",
                                     source=="Undetermined" & taxon_clean=="Lepidoptera" ~ "Lepidoptera (larvae)",
                                     source=="Undetermined" & taxon_clean=="UnID remains" ~ "Unknown"),
         taxonomy_simple = case_when(grepl("Acari", taxonomy_stage_simple_detail, ignore.case=T) ~ "Mites",
                                     grepl("Diptera", taxonomy_stage_simple_detail, ignore.case=T) ~ "Flies",
                                     grepl("Balanomorpha|Barnacle", taxonomy_stage_simple_detail, ignore.case=T) ~ "Barnacles",
                                     grepl("Isopod", taxonomy_stage_simple_detail, ignore.case=T) ~ "Isopods",
                                     grepl("Ostracod", taxonomy_stage_simple_detail, ignore.case=T) ~ "Ostracods",
                                     grepl("Copepod", taxonomy_stage_simple_detail, ignore.case=T) ~ "Copepods",
                                     grepl("Fish", taxonomy_stage_simple_detail, ignore.case=T) ~ "Fish",
                                     grepl("Phyllodocida", taxonomy_stage_simple_detail, ignore.case=T) ~ "Polychaete worms",
                                     grepl("parasite", taxonomy_stage_simple_detail, ignore.case=T) ~ "Internal parasites",
                                     grepl("Amphipod", taxonomy_stage_simple_detail, ignore.case=T) ~ "Amphipods",
                                     grepl("Crustacean", taxonomy_stage_simple_detail, ignore.case=T) ~ "UnID crustaceans",
                                     grepl("Decapod", taxonomy_stage_simple_detail, ignore.case=T) ~ "Crab larvae",
                                     grepl("Insect", taxonomy_stage_simple_detail, ignore.case=T) ~ "UnID insects",
                                     grepl("invert", taxonomy_stage_simple_detail, ignore.case=T) ~ "UnID inverts",
                                     grepl("Lepidoptera", taxonomy_stage_simple_detail, ignore.case=T) ~ "Butteryfly larvae",
                                     grepl("UnID remains", taxonomy_stage_simple_detail, ignore.case=T) ~ "Unknown",
                                     taxon_clean%in%c("Arachnida","Araneae") ~ "Spiders",
                                     taxon_clean=="Coleoptera" ~ "Beetles",
                                     taxon_clean=="Hemiptera" ~ "True bugs",
                                     taxon_clean=="Hymenoptera" ~ "Wasps/bees",
                                     taxon_clean=="Hemiptera" ~ "True bugs",
                                     taxon_clean=="Neuroptera" ~ "Lacewing flies",
                                     taxon_clean=="Perciformes" ~ "Fish",
                                     taxon_clean=="Psocodea" ~ "Lice (non-parasitic)",
                                     taxon_clean=="Trichoptera" ~ "Caddisflies",
                                     TRUE ~ taxon_clean
                                     )) %>%
  mutate_at("total_ww_g", as.numeric)





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


#                                                           LINK GSI + BIODATA TO STOMACH RESULTS


core.biosample.linked <- full_join(biosamp.gsi.linked %>%
                                     select(year, gear, usid, date, DOY, species, length, height, weight, ad_clip, cwt, hatchery_origin, lethal_tag_no,
                                            DNA_vial, MGL_ID_Source, MGL_PBT_brood_year, MGL_PBT_brood_collection, MGL_PBT_brood_group, MGL_top_collection,
                                            MGL_associated_collection_prob, MGL_species, MGL_notes),
                                   diet.results,
                                   by=c("DNA_vial" = "client_sample_id"),
                                   multiple = "all")


#write.csv(core.biosample.linked, "C:/Users/DAVIDSONKA/Desktop/biosample diet gsi linked.csv", row.names=F)


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
