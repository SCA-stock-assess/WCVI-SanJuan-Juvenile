# WCVI San Juan River Juvenile Project

This repository is for data and analyses used in the San Juan downstream (RST), estuary, and nearshore marine (beach/purse seine) report published 2026 (links when available). 
The work is part of a project funded under Pacific Salmon Strategy Initiative (PSSI) Pillar 1.1, the San Juan Juvenile and Adult Assesment Program. The project is linked to and includes results from the Follow the Fish Program.

# Directory

## Scripts

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
Contains code related to report Results section 3.3
- cpue and traits (folder), specific to Port Renfrew purse seine
  + Catch and CPUE calculations for purse seine fish (Section 3.3.1) --> cpue_catch.R
  + Summary and analyses of fish biodata traits from purse seine fish (Section 3.3.1 - 3.3.2) --> bs_fish_traits.R
  + 


Maintainer: Katie Davidson
