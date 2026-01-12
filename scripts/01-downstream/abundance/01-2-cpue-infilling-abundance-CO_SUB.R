
# I HAVE DECIDED TO MOVE INFILLING FOR EACH SPECIES-STAGE TO THEIR OWN SCRIPTS
# Want to do an in-depth evaluation for each species-stage thoroughly, and it is too bulky in the parent 01-cpue-infilling-abundance.R script
# Therefore, consider these child scripts of the parent script. 



# COHO SUBYEARLING
source(here::here("scripts", "01-downstream", "abundance", "01-cpue-infilling-abundance.R"))


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== IMPUTE VALUES ===============

# ----- Create a bunch of different imputed time series using various methods ------------
COSUB_impVal <- eventMeta_totals_impValFull %>%
  select(-c(chinook_natural_obs, chum_fry_obs, coho_yearling_obs:chinook_natural_obs_validation, coho_yearling_obs_validation:chum_fry_obs_validation)) %>%
  filter(year != 2023) %>%
  group_by(year) %>%
  mutate(coho_subyearling_interp.linear = imputeTS::na_interpolation(ts(coho_subyearling_obs_validation), option="linear"),
         coho_subyearling_interp.stine = imputeTS::na_interpolation(ts(coho_subyearling_obs_validation), option="stine"),
         coho_subyearling_kal.structs = imputeTS::na_kalman(ts(coho_subyearling_obs_validation), model="StructTS"),
         coho_subyearling_kal.arima = imputeTS::na_kalman(ts(coho_subyearling_obs_validation), model="auto.arima"),
         coho_subyearling_MA.simp2 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="simple", k=2),
         coho_subyearling_MA.simp3 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="simple", k=3),
         coho_subyearling_MA.linear2 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="linear", k=2),
         coho_subyearling_MA.linear3 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="linear", k=3),
         coho_subyearling_MA.exp2 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="exponential", k=2),
         coho_subyearling_MA.exp3 = imputeTS::na_ma(ts(coho_subyearling_obs_validation), weighting="exponential", k=3),
         # infill_type = case_when(is.na(coho_subyearling_obs_validation) & !is.na(coho_subyearling_obs) ~ "ground truth",
         #                         TRUE ~ "known value")
  )  %>%
  pivot_longer(cols=c(coho_subyearling_obs:coho_subyearling_MA.exp3), names_to = "data_series", values_to = "value") 



# ----- VISUALIZE ------------

# plot_cohsub_imputation_validation <- 
#   ggplot() +
#   geom_point(data=COSUB_impVal %>% 
#                filter(!is.na(value), estimate_type=="observed" & data_series=="coho_subyearling_obs"),
#              aes(x=as.Date(doy, origin="2024-12-31"), y=value, size=validation_type, shape=validation_type), colour="black", fill="black", alpha=1, stroke=1) +  #
#   geom_point(data=COSUB_impVal %>% 
#                filter(!is.na(value), estimate_type=="infill"),
#              aes(x=as.Date(doy, origin="2024-12-31"), y=value, fill=data_series), colour="transparent", shape=21, size=3, alpha=0.2) +
#   geom_jitter(data=COSUB_impVal %>% 
#                 filter(!is.na(value), validation_type=="validation" & data_series!="coho_subyearling_obs"),
#               aes(x=as.Date(doy, origin="2024-12-31"), y=value, colour=data_series), size=3, stroke=1, alpha=0.7, shape=4, width=0.1) +
#   scale_x_date(date_breaks="2 day", date_labels="%b %d") +
#   scale_size_manual(breaks=waiver(), values=c(2, 3)) +
#   scale_shape_manual(breaks=waiver(), values=c(16, 4)) +
#   labs(x="", y="Subyearling Coho count", colour="Imputation method", fill="Imputation method", size="Data type", shape="Data type") +
#   theme_bw() +
#   theme(axis.text = element_text(colour="black"),
#         axis.text.x = element_text(angle=45, hjust=1, size=10),
#         axis.title = element_text(face="bold", size=13),
#         panel.grid.major.x = element_line(colour="gray80", size=0.5),
#         panel.grid.major.y = element_line(colour="gray80", size=0.5),
#         panel.grid.minor.x = element_blank(),
#         legend.title = element_text(face="bold", size=11),
#         legend.text = element_text(size=10),
#         strip.text = element_text(size=12, face="bold")) +
#   facet_wrap(~year, nrow=2, scales="free_y") +
#   guides(color = guide_legend(override.aes = list(alpha = 1)),
#          size = guide_legend(override.aes = element_blank()))



# Save as PDF: 
pdf(file = here::here("outputs", "figures", "RST infill-CPUE-abundance", "Imputation diagnostic plot - Coho subyearling.pdf"),   # The directory you want to save the file in
    width = 16, # The width of the plot in inches
    height = 10) # The height of the plot in inches

print(  
  ggplot() +
    geom_point(data=COSUB_impVal %>% 
                 filter(!is.na(value), estimate_type=="observed" & data_series=="coho_subyearling_obs"),
               aes(x=as.Date(doy, origin="2024-12-31"), y=value, size=validation_type, shape=validation_type), 
               colour="black", fill="black", alpha=1, stroke=1.2, size=2.5) +  #
    geom_point(data=COSUB_impVal %>% 
                 filter(!is.na(value), estimate_type=="infill"),
               aes(x=as.Date(doy, origin="2024-12-31"), y=value, fill=data_series), colour="transparent", shape=21, size=4, alpha=0.3) +
    geom_jitter(data=COSUB_impVal %>% 
                  filter(!is.na(value), validation_type=="validation" & data_series!="coho_subyearling_obs"),
                aes(x=as.Date(doy, origin="2024-12-31"), y=value, colour=data_series), size=4, stroke=1.2, alpha=0.7, shape=4, width=0.1) +
    scale_x_date(date_breaks="3 day", date_labels="%b %d") +
    scale_size_manual(breaks=waiver(), values=c(3, 4)) +
    scale_shape_manual(breaks=waiver(), values=c(16, 4)) +
    labs(x="", y="Sub-yearling Coho catch", colour="Imputation method", fill="Imputation method", size="Data type", shape="Data type") +
    theme_bw() +
    theme(axis.text = element_text(colour="black", size=18),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title = element_text(face="bold", size=20),
          panel.grid.major.x = element_line(colour="gray80", size=0.5),
          panel.grid.major.y = element_line(colour="gray80", size=0.5),
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(face="bold", size=18),
          legend.text = element_text(size=16),
          strip.text = element_text(size=20, face="bold")) +
    facet_wrap(~year, nrow=2, scales="free_y") +
    guides(color = guide_legend(override.aes = list(alpha = 1))#,
           #size = guide_legend(override.aes = element_blank())
    )
)

dev.off()






# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== METRICS TO ASSESS IMPUTATION MODELS ===============

# ----- Calculate overall magnitude of differences between estimates ------------
# First used MAPE, but realized it is not stable at/near 0, so also included MAE and MASE. Tried SMAPE but returned Inf/NaN. 
# Also tried MAE with and without the observed zero count to see if it changed the "top model" - it did not. These are called MAE_w0 (with zero) and MAE_no0 (without zero)

infill_evaluation_table.COSUB <- COSUB_impVal %>% 
  pivot_wider(names_from = data_series, values_from = value) %>%
  filter(validation_type=="validation") %>% 
  select(-c(coho_subyearling_obs_validation)) %>%
  pivot_longer(cols=c(coho_subyearling_interp.linear:coho_subyearling_MA.exp3), names_to = "infill_method", values_to = "infill_value") %>%
  mutate(# Just doing this to show my work for future me: 
    Error = (coho_subyearling_obs - infill_value),   # Calculate error
    `Error/Obs` = Error/coho_subyearling_obs,        # Calculate error divided by observed value (part of MAPE)
    `Abs(Error/Obs)` = abs(`Error/Obs`),            # Calculate absolute value (for MAPE)
    `Abs(Error)` = abs(Error)) %>%                  # Calculate aboslute value (for MAE)
  arrange(doy) %>%                                  # Arrange by DOY because MASE is for time series and it is assumed the data are in temporal order
  group_by(year, infill_method) %>%                 # Calculate metrics for each of the 10 "models" being assessed
  mutate(#        n_w0 = n(),                       # This is all long-hand work to ensure I understood how the metric calculations work. For simplicity of coding, I installed the Metrics package and verified my work 
    #        n_no0 = length(na.omit(APE)),
    #        
    #        sumAPE = case_when(!is.na(APE) ~ sum(APE, na.rm=T),
    #                           is.na(APE) ~ NA),
    #        MAPE = sumAPE/n_APE,
    #        sumAE_w0 = sum(AE),
    #        sumAE_no0 = sum(AE[chinook_natural_obs>0]),
    #        MAE_w0 = sumAE_w0/n_AE,
    #        MAE_no0 = sumAE_no0/n_APE,
    MAE_w0 = Metrics::mae(coho_subyearling_obs, infill_value),                               # Calculate MAE including the observed zero to see if it has an affect
    MAE_no0 = Metrics::mae(coho_subyearling_obs[coho_subyearling_obs>0], infill_value),      # Calculate MAE excluding the observed zero to see if it has an affect
    MASE = Metrics::mase(coho_subyearling_obs, infill_value, step_size = 1)                  # Calculate MASE. Step of 1 indicates the previous day is informative for the naive model. For example, a step of 4 would be used for quarterly work where you imply the last quarter was more informative.
  ) %>% 
  print()


# Summarize the results of the metrics above, and join it to a table that calculates MAPE (had to exclude the zero count and it was just easier this way)
infill_summary.COSUB <- full_join(infill_evaluation_table.COSUB %>% 
                              filter(coho_subyearling_obs>0) %>%
                              group_by(year, infill_method) %>%
                              mutate(MAPE = Metrics::mape(coho_subyearling_obs, infill_value)) %>% 
                              group_by(year, infill_method) %>%
                              summarize(MAPE=unique(MAPE)),
                              infill_evaluation_table.COSUB %>%
                              group_by(year, infill_method) %>%
                              summarize(MAE_w0 = unique(MAE_w0),
                                        MAE_no0 = unique(MAE_no0),
                                        MASE=unique(MASE))
) %>%
  arrange(year, MAE_w0) %>%
  print()


# ----- DECISION: 



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



# =============== EXPORT ===============

write.csv(infill_summary.COSUB, file=here::here("outputs", "R_OUT - imputation method metrics subyearling Coho.csv"), row.names=F)




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# Clean up for source()
#remove(CNNO_impVal, infill_evaluation_table)