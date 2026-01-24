# San Juan RST 
# CPUE/infilling/abundance calculations
# Jan 2026



# ========================= SET UP =========================

# Load libraries -------------------
library(tidyverse)

# Load helpers ---------------
"%notin%" <- Negate("%in%")


# ========================= LOAD JUVENILE DATA =========================

# Sample events ----------------- 
eventMeta <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                pattern="^R_OUT - San Juan PSSI master database",
                                                full.names = T),
                                sheet="sample_event_meta") %>% 
  filter(grepl("RST|IPT", gear)) %>% 
  mutate(datetime_start = lubridate::ymd_hm(paste0(date_start, time_start)),
         datetime_stop = lubridate::ymd_hm(paste0(date_stop, time_stop))) %>% 
  print()



# Environmentals ----------------- 
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


# ========================= LOAD HYDROMET DATA =========================
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


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== RST OPS ===============

# RPMs ----------------- 
rpms <- enviros %>%
  group_by(year) %>% 
  summarize(mean = mean(rst_rpms, na.rm=T),
            sd = sd(rst_rpms, na.rm=T)) %>%
  print()


# RPMs and FLOW ----------------- 

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
        axis.title = element_text(face="bold", size=19)) +
  facet_wrap(~year, scales="free_y", nrow=3) 

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# =============== WATER QUALITY ===============

# Temp ----------------- 



# DO ----------------- 

