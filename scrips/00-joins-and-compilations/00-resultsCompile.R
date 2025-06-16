# Add lab results in to database 
# Feb 2024
# Only need to run if new lab results returned (e.g., GSI, stomachs, etc.)



# Load libraries ------------------------------------
library(tidyverse)



# ========================= LOAD MGL FILES =========================

# Load and compile repunit_table_ids tabs all years ------------------------------------
gsi.repunits_table_ids.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="repunits_table_ids")
         })
)

# Rename, convert to data frame: 
names(gsi.repunits_table_ids.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                            pattern=".xlsx", full.names=F),
                                   list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                              pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.repunits_table_ids <- do.call("rbind", gsi.repunits_table_ids.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.repunits_table_ids.LL)



# Load and compile extraction_sheet tabs all years ------------------------------------
gsi.extraction_sheets.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="extraction_sheet")
         })
)

# Rename, convert to data frame: 
names(gsi.extraction_sheets.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                                 pattern=".xlsx", full.names=F),
                                      list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.extraction_sheets <- do.call("rbind", gsi.extraction_sheets.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.extraction_sheets.LL)


# Load and compile species_ID tabs all years ------------------------------------
gsi.species_ID.LL <- c(
  # --- 2023:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         }),
  
  # --- 2024:
  lapply(list.files(here::here("data", "juvenile", "GSI", "2024"), 
                    pattern=".xlsx", full.names=T), 
         function(x) {
           readxl::read_excel(x, sheet="species_ID")
         })
)

# Rename, convert to data frame: 
names(gsi.species_ID.LL) <- c(list.files(here::here("data", "juvenile", "GSI", "2023"), 
                                                 pattern=".xlsx", full.names=F),
                                      list.files(here::here("data", "juvenile", "GSI", "2024"), 
                                                 pattern=".xlsx", full.names=F))

# Convert the Large List into a useable R dataframe:
gsi.species_ID <- do.call("rbind", gsi.species_ID.LL) %>%
  tibble::rownames_to_column(var="file_source")
remove(gsi.species_ID.LL)




# ========================= JOIN MGL INTO 1 MASTER FILE =========================

# Join into a master dataframe ------------------------------------
gsi.master <- full_join(
  gsi.repunits_table_ids %>% 
    select(indiv, mixture_collection:prob.2, top_collection, associated_collection_prob),
  gsi.extraction_sheets %>% 
    select(indiv, CatchJulDate, Vial, Comments, ID_Source),
  by=c("indiv", "ID_Source")
) %>%
  full_join(.,
            gsi.species_ID %>%
              select(indiv:neg_sp_confirmed),
            by=c("indiv")) %>%
  print()


# Export in case needed ------------------------------------
writexl::write_xlsx(gsi.master, path=paste0(here::here("data", "juvenile", "GSI"), 
                                            "/San Juan MGL master file ",
                                            Sys.Date(),
                                            ".xlsx"))



############################################################################################################################################################

#                                                           LINK GSI MASTER TO BIODATA 


# Load Biosampling sheet of juvenile database, Join to MGL Master file ----------------- 
biosamp.linked <-  full_join(
  readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                     pattern="^San Juan PSSI master database",
                                     full.names=T),
                     sheet="biosampling"),
  gsi.master %>%
    select(ID_Source:Vial,species:neg_sp_confirmed) %>%
    rename(genetic_species = species),
  by=c("DNA_vial" = "Vial"),
  na_matches = "never")






# ================= EXPORT ================= 
# Create empty workbook ---------------------------
R_OUT_SJjuviDB <- openxlsx::createWorkbook()


# Add empty tabs to the workbook ---------------------------
openxlsx::addWorksheet(R_OUT_SJjuviDB, "sample_event_meta")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "enviro")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "set_totals")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "mark-release")
openxlsx::addWorksheet(R_OUT_SJjuviDB, "biosampling w RESULTS")


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
                    sheet="biosampling w RESULTS", 
                    x = biosamp.linked)



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
