# San Juan RST 
# CPUE/infilling/abundance calculations
# Jan 2026



# ========================= SET UP =========================

# Load libraries -------------------
library(tidyverse)

# Load helpers ---------------
"%notin%" <- Negate("%in%")


# ========================= LOAD DATA =========================

## Sample events ----------------- 
eventMeta <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                pattern="^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                                sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear)) %>% 
  mutate(datetime_start = lubridate::ymd_hm(paste0(date_start, time_start)),
         datetime_stop = lubridate::ymd_hm(paste0(date_stop, time_stop))) %>% 
  print()



## RST Environmentals ----------------- 
enviros <- readxl::read_excel(path = list.files(path = "//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database/",
                                                pattern = "^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                              sheet="enviro") %>% 
  filter(grepl("RST|IPT", usid, ignore.case=T)) %>%
  janitor::clean_names() %>%
  mutate(year = lubridate::year(date_enviros),
         doy = lubridate::yday(date_enviros)) %>%
  rename(rst_rpms = rst_rp_ms) %>% 
  mutate_at("rst_rpms", as.numeric)


## Hydromet flow data ----------------- 
hydro <- full_join(
  read.csv(file=list.files(path = here::here("data", "enviro"),
                           pattern = "SANJUAN_historical*",
                           full.names = TRUE),
           skip=1) %>%
    mutate(PARAM = case_when(PARAM==1 ~ "discharge (cms)",
                             PARAM==2 ~ "level (m)"),
           Date = lubridate::ymd(Date),
           year = lubridate::year(Date),
           month = lubridate::month(Date, label=T, abbr=T),
           DOY = lubridate::yday(Date)),
  
  read.csv(file=list.files(path = here::here("data", "enviro"),
                           pattern = "SANJUAN_realtime*",
                           full.names = TRUE),
           skip=9) %>%
    mutate(Parameter = case_when(Parameter==46 ~ "level (m)",
                                 Parameter==6 ~ "discharge (cms)"),
           Date = lubridate::ymd(stringr::str_sub(string=Date..PST., start=1, end=10)),
           time = stringr::str_sub(string=Date..PST., start=12, end=19),
           year = lubridate::year(Date..PST.),
           month = lubridate::month(Date..PST., label=T, abbr=T),
           DOY = lubridate::yday(Date..PST.)) %>% 
    rename(PARAM=Parameter,
           Value=Value..m..s.)
) %>%
  janitor::clean_names()



## Hydromet temp data ----------------- 
wcvi_hydromet <- readxl::read_excel(path=here::here("data", "enviro", "SanJuan_WCVIhydromet_2022-2025.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::ymd_hms(station_time),
         date = as.Date(date),
         water_temp_celcius_corrected = case_when(is.na(water_temp_celcius_corrected) ~ water_temp_celcius,
                                                  TRUE ~ water_temp_celcius_corrected)) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(water_temp_celcius_corrected, na.rm=T)) %>%
  mutate(year = lubridate::year(date)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== RST OPS ===============

## RPMs ----------------- 
rpms <- enviros %>%
  group_by(year) %>% 
  summarize(mean = mean(rst_rpms, na.rm=T),
            sd = sd(rst_rpms, na.rm=T)) %>%
  print()


## RPMs and FLOW (APPENDIX FIGURE 33) ----------------- 

pdf(file = here::here("outputs", "figures", "RST RPMs and flow.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  # geom_ribbon(data=hydro %>% 
  #               filter(year%in%c(2023:2025), doy %in% c(46:182), grepl("discharge", param, ignore.case=T)) %>%
  #               group_by(year, doy) %>% 
  #               summarize(mean = mean(value, na.rm=T),
  #                         min = min(value, na.rm=T),
  #                         max = max(value, na.rm=T)),
  #             aes(x=as.Date(doy, origin="2022-12-31"), ymin=(mean-min)/25, ymax=(mean+max)/25), fill="dodger blue", colour="transparent", alpha=0.2) +
  geom_line(data=hydro %>%
              filter(year%in%c(2023:2025), doy %in% c(46:182), grepl("discharge", param, ignore.case=T)) %>%
              group_by(year, doy) %>% 
              summarize(mean = mean(value, na.rm=T)),
            aes(x=as.Date(doy, origin="2022-12-31"), y=mean), colour="dodger blue", size=1) +
  geom_errorbar(data=enviros %>%
                  group_by(year, doy) %>%
                  summarize(mean = mean(rst_rpms, na.rm=T),
                            sd = sd(rst_rpms, na.rm=T)),
                aes(x=as.Date(doy, origin="2022-12-31"), ymin = (mean-sd)*10, ymax=(mean+sd)*10), size=1, width=1.2, alpha=0.8) +
  geom_point(data=enviros %>%
               group_by(year, doy) %>%
               summarize(mean = mean(rst_rpms, na.rm=T),
                         sd = sd(rst_rpms, na.rm=T)),
             aes(x=as.Date(doy, origin="2022-12-31"), y = mean*10), shape=21, size=4, fill="gray30", alpha=0.8) +
  labs(y="Discharge (cms)") +
  scale_x_date(date_labels = "%b %d", date_breaks = "5 day") +
  scale_y_continuous(sec.axis = sec_axis(~./10, name="RST RPMs")) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=18),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(colour="dodger blue"),
        axis.title = element_text(face="bold", size=19),
        strip.text = element_text(size=16)) +
  facet_wrap(~year, scales="free_y", nrow=3) 

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== WATER QUALITY ===============

## Temp (APPENDIX FIGURE 34) ----------------- 
cn.atu <- wcvi_hydromet %>%
  mutate(outmigration_year = case_when(date %in% as.Date(c(as.Date("2022-10-01"):as.Date("2023-06-30"))) ~ 2023,
                                       date %in% as.Date(c(as.Date("2023-10-01"):as.Date("2024-06-30"))) ~ 2024,
                                       date %in% as.Date(c(as.Date("2024-10-01"):as.Date("2025-06-30"))) ~ 2025,
                                       TRUE ~ NA)) %>%
  filter(!is.na(outmigration_year)) %>%
  group_by(outmigration_year) %>%
  filter(month(date) %in% c(10,11,12,1,2,3,4,5,6)) %>%
  arrange(date) %>%
  group_by(outmigration_year) %>%
  mutate(ATU = cumsum(mean_temp),
         date2 = lubridate::make_datetime(9999, month(date), day(date)),
         day_num = row_number()) %>%
  print()

View(cn.atu %>%
       group_by(outmigration_year) %>%
       filter(ATU >= 825 & ATU <= 1025) %>%
       summarize(earliest_emerg = min(date),
                 latest_emerg = max(date)))


pdf(file = here::here("outputs", "figures", "ATUs.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot(data = cn.atu) +
  geom_hline(yintercept = 825, colour="red", linetype="dashed")+
  geom_hline(yintercept = 1025, colour="red", linetype="dashed")+
  geom_line(aes(x=day_num, y=ATU, group=as.factor(outmigration_year), colour=as.factor(outmigration_year)), 
            size=1.5, alpha=0.8) +
  scale_x_continuous(breaks=scales::pretty_breaks(20)) +
  labs(x="Days elapsed since egg deposition (Oct 01)", y="ATUs", colour="Outmigration \nyear") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.title = element_text(face="bold", size=19),
        legend.title = element_text(face="bold", size=18),
        legend.text = element_text(size=17))

dev.off()

## DO ----------------- 




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

