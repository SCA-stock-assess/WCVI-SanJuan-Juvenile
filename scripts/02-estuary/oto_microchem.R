# Estuary residence based on otolith microchemistry
# March 2026



# Set up -----------------
library(tidyverse)
library(leaflet)
"%notin%" <- Negate("%in%")
options(scipen = 9999999)


# Read in all juvi biodata -----------------
all.biodat <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                 pattern="^R_OUT - San Juan PSSI master database",
                                                 full.names = T),
                                 sheet="biosampling") %>%
  filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
  janitor::clean_names()  %>%
  mutate(month = lubridate::month(date, label=T, abbr=T)) %>%
  full_join(.,
            read.csv(here::here("data", "stat_weeks.csv"))) %>%
  left_join(.,
            readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                               pattern="^R_OUT - San Juan PSSI master database",
                                               full.names = T),
                               sheet="sample_event_meta") %>%
              janitor::clean_names() %>%
              filter(grepl("beach seine|purse seine|rst|ipt", gear, ignore.case=T)) %>%
              select(site_name_clean, lat_dd, long_dd, usid),
            by="usid")  


# Read in all microchemistry data -----------------
oto.microchem <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                    pattern="^R_OUT - San Juan PSSI master database",
                                                    full.names = T),
                                    sheet="otolith microchem raw") %>%
  janitor::clean_names()



# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ======================================================== Average estuary residence ==========================================================

## Average residency time by year/origin (Section 3.2.1) --------------
oto.microchem %>%
  filter(stage=="adult") %>%
  group_by(#year_collected,
           origin) %>%
  summarize(avg_res = mean(estuary_days, na.rm=T),
            sd_res = sd(estuary_days, na.rm=T),
            max = max(estuary_days, na.rm=T))


## Average residency time by origin/age --------------
oto.microchem %>%
  filter(stage=="adult") %>%
  group_by(r_resolved_total_age,
           origin) %>%
  summarize(avg_res = mean(estuary_days, na.rm=T),
            sd_res = sd(estuary_days, na.rm=T))


## Summary table (Table xx) --------------
write.csv(
  x = oto.microchem %>%
    filter(stage=="adult") %>%
    group_by(year_collected,
             r_resolved_total_age,
             #r_resolved_brood_year, 
             origin) %>%
    summarize(n=n(),
              avg_res = mean(estuary_days, na.rm=T),
              sd_res = sd(estuary_days, na.rm=T), 
              min = min(estuary_days),
              max = max(estuary_days)),
  
  file = here::here("outputs", "R_OUT - Otolith microchemistry summary table for report.csv"), 
  row.names=F)



## Estuary res stats (Section 3.2.1) ---------------  
adult.est <- oto.microchem %>%
  filter(stage=="adult")

### Check assumptions -----
# Check for normality, equal variance
qqnorm(as.numeric(adult.est$estuary_days))
qqline(as.numeric(adult.est$estuary_days), col="red")
# points do not follow line at tails, suggests non-normal

shapiro.test(as.numeric(adult.est$estuary_days))
# w=0.74343, p = 0.0000000002765
# significance implies non-normality

fligner.test(adult.est$estuary_days ~ as.factor(adult.est$origin))
# Fligner-Killeen:med chi-squared = 0.2954, df = 1, p-value = 0.5868
# p < 0.05 indicates unequal variances -- equal var 
# Used this test because it's robust to non-normality 

lm.adEst <- lm(adult.est$estuary_days ~ as.factor(adult.est$origin))
resid.adEst <- resid(lm.adEst)
plot(resid.adEst)
# Not an equal spread 

lm.adEstAge <- lm(adult.est$estuary_days ~ as.factor(adult.est$origin) + adult.est$r_resolved_total_age)
resid.adEstAge <- resid(lm.adEstAge)
plot(resid.adEstAge)
# Not an equal spread 


### Run the test -----
# Had to install coin package as base stats kruskal.test wouldn't work 
wilcox.test(as.numeric(estuary_days) ~ as.factor(origin), data=adult.est, paired=F)

coin::kruskal_test(as.numeric(estuary_days) ~ as.factor(r_resolved_total_age), data=adult.est)
dunntest <- FSA::dunnTest(as.numeric(estuary_days) ~ as.factor(r_resolved_total_age), data=adult.est)


