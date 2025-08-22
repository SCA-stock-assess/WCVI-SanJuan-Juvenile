
# I HAVE DECIDED TO MOVE INFILLING FOR EACH SPECIES-STAGE TO THEIR OWN SCRIPTS
# Want to do an in-depth evaluation for each species-stage thoroughly, and it is too bulky in the parent 01-cpue-infilling-abundance.R script
# Therefore, consider these child scripts of the parent script. 

# CHINOOK NATURAL 


#------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create a bunch of different imputed time series using various methods ------------
eventMeta_totals_testing.interp <- eventMeta_totals_validation %>%
  filter(year==2024) %>%
  mutate(chinook_fry_interp.linear = imputeTS::na_interpolation(ts(chinook_natural_obs_validation), option="linear"),
         chinook_fry_interp.stine = imputeTS::na_interpolation(ts(chinook_natural_obs_validation), option="stine"),
         chinook_fry_kal.structs = imputeTS::na_kalman(ts(chinook_natural_obs_validation), model="StructTS"),
         chinook_fry_kal.arima = imputeTS::na_kalman(ts(chinook_natural_obs_validation), model="auto.arima"),
         chinook_fry_MA.simp2 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="simple", k=2),
         chinook_fry_MA.simp3 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="simple", k=3),
         
         chinook_fry_MA.linear2 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="linear", k=2),
         chinook_fry_MA.linear3 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="linear", k=3),
         
         chinook_fry_MA.exp2 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="exponential", k=2),
         chinook_fry_MA.exp3 = imputeTS::na_ma(ts(chinook_natural_obs_validation), weighting="exponential", k=3),
         
         infill_type = case_when(is.na(chinook_natural_obs_validation) & !is.na(chinook_natural_obs) ~ "ground truth",
                                 TRUE ~ "known value"))  


imputeTS::statsNA(ts(eventMeta_totals_testing.interp[eventMeta_totals_testing.interp$year==2024,]$chinook_fry_obs))


# Visualize imputeTS options ------------

ggplot() +
  geom_point(data=eventMeta_totals_testing.interp %>% filter(year==2024), 
             aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_natural_obs, fill=infill_type, colour=infill_type, size=infill_type), shape=21) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_interp.linear), colour="dodger blue", fill="dodger blue", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_interp.stine), colour="turquoise", fill="turquoise", stroke=2, shape=4, size=3, alpha=0.7, 
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_kal.structs), colour="blue", fill="blue", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_kal.arima), colour="navy", fill="navy", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.simp2), colour="red", fill="red", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.simp3), colour="orange", fill="orange", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.linear2), colour="purple", fill="purple", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.linear3), colour="magenta", fill="magenta", stroke=2, shape=4, size=3, alpha=0.7, 
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.exp2), colour="dark green", fill="dark green", stroke=2, shape=4, size=3, alpha=0.7,
              width=0.3) +
  
  geom_jitter(data=eventMeta_totals_testing.interp %>% filter(year==2024 & infill_type=="ground truth"),
              aes(x=as.Date(doy,origin="2024-12-31"), y=chinook_fry_MA.exp3), colour="green", fill="green", stroke=2, shape=4, size=3, alpha=0.7, 
              width=0.3) +
  
  scale_x_date(date_breaks="1 day", date_labels="%b %d") +
  scale_size_manual(breaks=waiver(), values = c(5,3)) +
  
  scale_fill_manual(breaks=waiver(), values=c("black", "gray70")) +
  scale_colour_manual(breaks=waiver(), values=c("black", "gray70")) +
  
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major.x = element_line(colour="gray80"),
        panel.grid.minor.x = element_blank())





# Calculate overall magnitude of differences between estimates ------------
# First used MAPE, but realized it is not stable at/near 0, so also included MAE and MASE. Tried SMAPE but returned Inf/NaN. 
# Also tried MAE with and without the observed zero count to see if it changed the "top model" - it did not. These are called MAE_w0 (with zero) and MAE_no0 (without zero)

infill_evaluation_table <- eventMeta_totals_testing.interp %>% 
  filter(year==2024 & infill_type=="ground truth") %>% 
  select(-c(year, gear, usid, set_type, date_start, datetime_start, datetime_stop, chum_fry_obs:hrs_fished, chinook_fry_imptest, estimate_type)) %>%
  pivot_longer(cols=c(chinook_fry_interp.linear:chinook_fry_MA.exp3), names_to = "infill_method", values_to = "infill_value") %>%
  mutate(# Just doing this to show my work for future me: 
    Error = (chinook_fry_obs - infill_value),   # Calculate error
    `Error/Obs` = Error/chinook_fry_obs,        # Calculate error divided by observed value (part of MAPE)
    `Abs(Error/Obs)` = abs(`Error/Obs`),        # Calculate absolute value (for MAPE)
    `Abs(Error)` = abs(Error)) %>%              # Calculate aboslute value (for MAE)
  arrange(doy) %>%                                   # Arrange by DOY because MASE is for time series and it is assumed the data are in temporal order
  group_by(infill_method) %>%                        # Calculate metrics for each of the 10 "models" being assessed
  mutate(#        n_w0 = n(),                       # This is all long-hand work to ensure I understood how the metric calculations work. For simplicity of coding, I installed the Metrics package and verified my work 
    #        n_no0 = length(na.omit(APE)),
    #        
    #        sumAPE = case_when(!is.na(APE) ~ sum(APE, na.rm=T),
    #                           is.na(APE) ~ NA),
    #        MAPE = sumAPE/n_APE,
    #        sumAE_w0 = sum(AE),
    #        sumAE_no0 = sum(AE[chinook_fry_obs>0]),
    #        MAE_w0 = sumAE_w0/n_AE,
    #        MAE_no0 = sumAE_no0/n_APE,
    MAE_w0 = Metrics::mae(chinook_fry_obs, infill_value),                              # Calculate MAE including the observed zero to see if it has an affect
    MAE_no0 = Metrics::mae(chinook_fry_obs[chinook_fry_obs>0], infill_value),          # Calculate MAE excluding the observed zero to see if it has an affect
    MASE = Metrics::mase(chinook_fry_obs, infill_value, step_size = 1)                 # Calculate MASE. Step of 1 indicates the previous day is informative for the naive model. For example, a step of 4 would be used for quarterly work where you imply the last quarter was more informative.
  ) %>% 
  print()


# Summarize the results of the metrics above, and join it to a table that calculates MAPE (had to exclude the zero count and it was just easier this way)
infill_summary <- full_join(infill_evaluation_table %>% 
                              filter(chinook_fry_obs>0) %>%
                              group_by(infill_method) %>%
                              mutate(MAPE = Metrics::mape(chinook_fry_obs, infill_value)) %>% 
                              group_by(infill_method) %>%
                              summarize(MAPE=unique(MAPE)),
                            
                            
                            infill_evaluation_table %>%
                              group_by(infill_method) %>%
                              summarize(MAE_w0 = unique(MAE_w0),
                                        MAE_no0 = unique(MAE_no0),
                                        MASE=unique(MASE))
) %>%
  print()




# ---- DECISION: 
#   While MAPE indicates the moving-average 3-day window model performs best, the other metrics all support the Kalman (Structural TS) infilling model. A quick 
#   Google suggests this could be due to the error distribution of the models, where some models have small error and small values and large error at large  
#   values. It sounds like this is not necessarily surprising...
#   Given that a zero count is just as important to be able to predict, but that MAPE cannot evaluate prediction ability at a true zero, I will go with the
#   other metrics (MAE/MASE). They are in agreement, plus visual examination of the prediction values indicates that the Kalmon model is the most consistent 
#   with its predictions, regardless of whether it's near zero or a bigger value. 

# --> Kalmon StrucTS wins for Chinook fry! 