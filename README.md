# WCVI San Juan River Juvenile Project

This repository is for data and analyses used in the San Juan downstream (RST), estuary, and nearshore marine (beach/purse seine) report published 2026 (links when available). 
The work is part of a project funded under Pacific Salmon Strategy Initiative (PSSI) Pillar 1.1, the San Juan Juvenile and Adult Assesment Program. The project is linked to and includes results from the Follow the Fish Program.

# Directory

## data
- archive --> delete
- biosample/GSI - raw DNA analysis files from Molecular Genetics Lab; joined to field data using 00-resultsCompile.R
- enviro - external environmental data (e.g., HOBO DO meter raw download files), Environment Canada hydromet station files downloaded from https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08HA010 (realtime) and https://wateroffice.ec.gc.ca/report/historical_e.html?stn=08HA010 (historical)
- hatchery_releases - summary of hatchery release dates/abundances relevant to RST ops. This level of information currently not available in SEP EPAD database
- Method of grouping WCVI Chinook based on GSI --> Natural WCVI GSI groupings - JBokvist-BHendriks 2025.png
- Natural WCVI GSI groupings - SPower 2025.png --> delete
- date - stat week lookup table joined to field data when needed --> stat_weeks.csv

## outputs
- figures output from scripts. Used in various sections of the report
- table outputs from scripts. Used in various sections of the report --> R_OUT... 

## report
copy of final report and DFO Manuscript and Tech Report templates for reference. 

## scripts

### 00-joins-and-utils
Contains:
- scripts used to join field data to results (otolith microchem, DNA, etc.) --> 00-resultsCompile.R
  + Must be re-run anytime new data available. Outputs to a file called "R_OUT -  San Juan PSSI master database..." that is required for all other scripts in the repo. 
- Also contains the weight ~ height model work used to predict missing fish weights (and subsequent model validation) --> height_weight_model.R and height_weight_model_validation.R
- Code for study map in Methods section 2.2 --> map.R
- source_RST.R --> delete 

### 01-downstream
Contains code within sub-folders all related to report Results section 3.1
- abundance
  + Initial script with plots for raw RST catch, prepping data frame for infilling, selecting validation data set, and then calculating abundance estimates --> 01-cpue-infilling-abundance.R. Note that infilling models and metrics were calculated in their life history specific scripts: 
  + 5 individual scripts related to infilling catch data for each life history type (NO Chinook, HO Chinook, sub-yearling Coho, yearling Coho, Chum). Only NO chinook and coho were reported on as other information was either unreliable (Chum) or unnececssary (HO Chinook)
- diet
  + RST diet analysis - primarily focused simply on fullness due to low prey resolution/largely empty stomachs --> rst_diets.R
- fish data
  + Applied size sub-sampling %s to days with unsampled catch. Expanded to total daily catch to provide MQ with size bin %s for otolith microchemistry summary statistics --> rst_fish_lengths_for_otochem.R
  + Summary and analyses of fish sizes, weights, etc. (essentially all biodata analysis) --> rst_fish_traits.R3
- Summary of environmental conditions in the RST program --> rst_enviros.R

### 02-estuary
Contains code related to report Results section 3.2
- Diet analysis of beach seine fish (Section 3.2.3) --> beachseine_diets.R
- Summary and analyses of fish biodata traits from beach seine fish (Section 3.2.1 - 3.2.2) --> bs_fish_traits.R

### 03-marine
Contains code related to report Results section 3.3 & 3.4
- cpue and traits (folder), specific to Port Renfrew purse seine
  + Catch and CPUE calculations for purse seine fish (Section 3.3.1) --> cpue_catch.R
  + Summary and analyses of fish biodata traits from purse seine fish (Section 3.3.1 - 3.3.2) --> bs_fish_traits.R
- diet (folder), includes both PR & Barkley Sound purse seine
  + Diet analysis of San Juan Chinook in Nita Maria (Barkley Sound) purse seine from JB/BH --> NMpurseseine_diets-SJonly.R
  + Partial Fullness Index (summary/plot) (Section 3.4) --> PFI.R
  + Other diet analysis specific to Port Renfrew purse seine (Section 3.3.4) --> purseseine_diets.R
 
map.R (loose script) --> delete


Maintainer: Katie Davidson
