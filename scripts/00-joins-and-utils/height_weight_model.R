# Code to prove that height is a reasonable proxy for weight

# Load helpers ------------
library(tidyverse)


# Load data ------------
biodat.fish <- readxl::read_excel(path=list.files(path="//ENT.DFO-MPO.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/# Juvi Database",
                                                      pattern="^R_OUT - San Juan PSSI master database",
                                                      full.names = T),
                                      sheet="biosampling detailed w GSI") %>%
  janitor::clean_names()  %>%
  mutate(species_stage_simple = case_when(grepl("coho", species, ignore.case=T) ~ stringr::str_to_title(paste0(species, " ", life_stage)),
                                          grepl("chum", species, ignore.case=T) ~ "Chum",
                                          grepl("chinook", species, ignore.case=T) ~ stringr::str_to_title(paste0(ad_clip, " ", species)),
                                          TRUE ~ species)) %>%
  print()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============== ALL FISH ===============  

## Create dataset -------------
df_all <- biodat.fish %>% 
  mutate_at("field_weight_g", as.numeric) %>%
  filter(grepl("chinook", species, ignore.case=T), gear%in%c("6' RST", "Beach seine", "Dip net", "Mini purse seine"), 
         !is.na(height_mm) & !is.na(lab_weight_g), lethal_tag_no!="SJ25-449") %>%
  mutate(obs_weight = case_when(!is.na(field_weight_g) ~ field_weight_g,
                                is.na(field_weight_g) ~ lab_weight_g,
                                TRUE ~ NA))

### Plot raw data -----
ggplot() +
  geom_point(data=df_all,
             aes(x=(height_mm), y=as.numeric(obs_weight), fill=hatchery_origin, shape=gear), size=4, alpha=0.7, stroke=1) +
  scale_shape_manual(values=c(21, 22, 24, 25)) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +  
  scale_x_continuous(limits=c(0,60)) +
  labs(x="Height (mm)", y="Weight (g)", shape="Gear") +
  theme_bw() +
  guides(fill = guide_legend("Chinook origin", override.aes = list(shape = 21))) 


### Plot raw data -----
ggplot() +
  geom_point(data=df_all,
             aes(x=log(height_mm), y=log(as.numeric(obs_weight)), fill=hatchery_origin, shape=gear), size=4, alpha=0.7, stroke=1) +
  scale_shape_manual(values=c(21, 22, 24, 25)) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +  
  #scale_x_continuous(limits=c(0,60)) +
  labs(x="Height (mm)", y="Weight (g)", shape="Gear") +
  theme_bw() +
  guides(fill = guide_legend("Chinook origin", override.aes = list(shape = 21))) 

# >> It appears the relationship between weigh~height is different in the RST samples than in the beach/purse seine, so will explore fitting a model to the RST
# data separately from the other data. 

# Try fitting exp model to full dataset!



## Fit exponential model ---------------------

### Exponential model function ----
expo_fun <- function(x, A, b) {
  A * exp(b * x)
}

### Starting values ----
  ## Starting value for A: A is the scale factor—the value of y when x=0 (if the model extrapolates back to zero height). 
  ## I don't have values for x=0, but the smallest observed weight is a reasonable proxy for the lower end. So arbitrarily set to 10-6 so A>0 but still much smaller than smallest observation.
  ## Starting value for b: b is the growth (or decay) rate. So b>0 means curve increases exponentially with x
  ## To start with a guess, use small positive slope like b=0.1. You can also estimate b from the data (log(ymax)-log(ymin))/(xmax - xmin) but not sure about relying on the log-linear relationship in this case. 
start_list_all <- list(
  A = max(1e-6, min(df_all$obs_weight, na.rm = TRUE)),
  b = 0.01
)

### Fit exponential model (unweighted) ----
fit_expo_all <- nls(
  obs_weight ~ expo_fun(height_mm, A, b),
  data = df_all,
  start = start_list_all,
  control = nls.control(maxiter=500, warnOnly=TRUE)
)

print(summary(fit_expo_all))

### Predictions & residuals ----
df_all <- df_all %>%
  mutate(
    y_hat_expo = predict(fit_expo_all),
    resid_expo = obs_weight - y_hat_expo
  )

### Metrics ----
rmse <- function(resid) sqrt(mean(resid^2))
r2_nl <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

metrics_all <- tibble(
  Model = "Exponential (nls, unweighted)",
  RMSE  = rmse(df_all$resid_expo),
  R2    = r2_nl(df_all$obs_weight, df_all$y_hat_expo),
  AIC   = AIC(fit_expo_all)
)

print(metrics_all)

### Plot raw data with predicted model fits ----
grid_x_all <- seq(min(df_all$height_mm), max(df_all$height_mm), length.out = 400)

coE <- coef(fit_expo_all)
# A = 0.5904260  
# b = 0.1256155  
# Formula:  weight = A*e^(b*height)

curve_df_all <- tibble(
  height_mm = grid_x_all,
  y_hat     = expo_fun(grid_x_all, A = coE["A"], b = coE["b"])
)

# Plot:
pdf(file = here::here("outputs", "figures", "fish traits", "Weight ~ height - All fish exponential model.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_point(data=df_all, aes(x=height_mm, y=obs_weight, fill=hatchery_origin), alpha=0.8, size=4, shape=21, stroke=1) +
  geom_line(data=curve_df_all, aes(x=height_mm, y=y_hat), color="black", linewidth=1) +
  scale_y_continuous(breaks=seq(0, 130, by=25), limits=c(0, 130)) +
  scale_x_continuous(breaks=seq(2, 60, by=10), limits=c(2, 55)) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Weight (g)", fill="Chinook origin") +
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=26),
        axis.text = element_text(colour="black", size=25),
        legend.title = element_text(face="bold", size=21),
        legend.text = element_text(size=20))

dev.off()


# >> not convinced this is a good model - at small body size looks like it would over-estimate weight, and at large body size 
#    would under-estimate.


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# =============== RST ===============  

## Create dataset -------------
rst_biodata <- biodat.fish %>% 
  mutate_at("field_weight_g", as.numeric) %>%
  filter(grepl("chinook", species, ignore.case=T), gear=="6' RST", !is.na(height_mm) & !is.na(field_weight_g))

### Plot raw data -----
ggplot(data=rst_biodata) +
  geom_point(aes(x=height_mm, y=as.numeric(field_weight_g), fill=hatchery_origin), shape=21, size=4, stroke=1, alpha=0.8) +
  scale_y_continuous(limits=c(0, 3)) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Field wet weight (g)", fill="Hatchery origin?") +
  theme_bw() 

### Plot log-log data -----
ggplot(data=rst_biodata) +
  geom_point(aes(x=log(height_mm), y=log(as.numeric(field_weight_g)), fill=hatchery_origin), shape=21, size=4, stroke=1, alpha=0.8) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Field wet weight (g)", fill="Hatchery origin?") +
  theme_bw() 

# >> Does not log transform well



## Fit exponential model ---------------------

### Exponential model function ----
expo_fun <- function(x, A, b) {
  A * exp(b * x)
}

### Starting values ----
  ## Starting value for A: A is the scale factor—the value of y when x=0 (if the model extrapolates back to zero height). 
  ## I don't have values for x=0, but the smallest observed weight is a reasonable proxy for the lower end. So arbitrarily set to 10-6 so A>0 but still much smaller than smallest observation.
  ## Starting value for b: b is the growth (or decay) rate. So b>0 means curve increases exponentially with x
  ## To start with a guess, use small positive slope like b=0.1. You can also estimate b from the data (log(ymax)-log(ymin))/(xmax - xmin) but not sure about relying on the log-linear relationship in this case. 
start_list <- list(
  A = max(1e-6, min(rst_biodata$field_weight_g, na.rm = TRUE)),
  b = 0.1
)

### Fit exp model (unweighted) ----
fit_expo <- nls(
  field_weight_g ~ expo_fun(height_mm, A, b),
  data = rst_biodata,
  start = start_list,
  control = nls.control(maxiter=500, warnOnly=TRUE)
)

print(summary(fit_expo))

### Predictions & residuals ----
rst_biodata <- rst_biodata %>%
  mutate(
    y_hat_expo = predict(fit_expo),
    resid_expo = field_weight_g - y_hat_expo
  )

### Metrics ----
rmse <- function(resid) sqrt(mean(resid^2))
r2_nl <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

metrics <- tibble(
  Model = "Exponential (nls, unweighted)",
  RMSE  = rmse(rst_biodata$resid_expo),
  R2    = r2_nl(rst_biodata$field_weight_g, rst_biodata$y_hat_expo),
  AIC   = AIC(fit_expo)
)

print(metrics)

### Plot raw data with predicted model fits ----
grid_x <- seq(min(rst_biodata$height_mm), max(rst_biodata$height_mm), length.out = 400)

coE <- coef(fit_expo)
# A = 0.1418124 
# b = 0.2288399 
# Formula:  weight = A*e^(b*height)

curve_df <- tibble(
  height_mm = grid_x,
  y_hat     = expo_fun(grid_x, A = coE["A"], b = coE["b"])
)

# Plot: 
pdf(file = here::here("outputs", "figures", "fish traits", "Weight ~ height - RST exponential model.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_point(data=rst_biodata, 
             aes(x=height_mm, y=field_weight_g, fill=hatchery_origin), shape=21, alpha=0.8, size=4, stroke=1) +
  geom_line(data=curve_df, aes(x=height_mm, y=y_hat), color = "black", linewidth = 1) +
  scale_y_continuous(breaks=seq(0,3,by=0.5), limits=c(0,3)) +
  scale_x_continuous(breaks=seq(2,14,by=1), limits=c(2,14)) +
  scale_fill_manual(labels=c("N" = "Natural-origin",
                             "U" = "Unknown",
                             "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Field weight (g)", fill="Chinook origin") +
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=26),
        axis.text = element_text(colour="black", size=25),
        legend.title = element_text(face="bold", size=21),
        legend.text = element_text(size=20))

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# Combo plot for report
# PLOT ALL FISH RELATIONSHIP
pdf(file = here::here("outputs", "figures", "fish traits", "Weight ~ height - All+RST fish exponential model.pdf"),   
    width = 16, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggpubr::ggarrange(ggplot() +
                    geom_point(data=df_all, aes(x=height_mm, y=obs_weight, fill=hatchery_origin), alpha=0.8, size=4, shape=21, stroke=1) +
                    geom_line(data=curve_df_all, aes(x=height_mm, y=y_hat), color="red", linewidth=1) +
                    geom_text(aes(x=10, y=130, label="A) All Chinook"), size=7) +
                    scale_y_continuous(breaks=seq(0, 130, by=25), limits=c(0, 130)) +
                    scale_x_continuous(breaks=seq(2, 60, by=10), limits=c(2, 55)) +
                    scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
                    labs(x="Height (mm)", y="Weight (g)", fill="Chinook origin") +
                    theme_bw() +
                    theme(axis.title = element_text(face="bold", size=26),
                          axis.text = element_text(colour="black", size=25),
                          legend.title = element_text(face="bold", size=21),
                          legend.text = element_text(size=20)),
                  
                  ggplot() +
                    geom_point(data=rst_biodata, 
                               aes(x=height_mm, y=field_weight_g, fill=hatchery_origin), shape=21, alpha=0.8, size=4, stroke=1) +
                    geom_line(data=curve_df, aes(x=height_mm, y=y_hat), color = "red", linewidth = 1) +
                    geom_text(aes(x=4.2, y=3, label="B) RST Chinook"), size=7) +
                    scale_y_continuous(breaks=seq(0,3,by=0.5), limits=c(0,3)) +
                    scale_x_continuous(breaks=seq(2,14,by=1), limits=c(2,14)) +
                    scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
                    labs(x="Height (mm)", y="", fill="Chinook origin") +
                    theme_bw() +
                    theme(axis.title = element_text(face="bold", size=26),
                          axis.text = element_text(colour="black", size=25),
                          legend.title = element_text(face="bold", size=21),
                          legend.text = element_text(size=20)),
                  
                  common.legend = T, legend = "right"
)

dev.off()


# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# ============ BEACH + PURSE SEINE FISH ============

## Create dataset -------------
df_bspbs <- df_all %>%
  filter(grepl("seine", gear, ignore.case=T))

### Plot raw data -----
ggplot(data=df_bspbs) +
  geom_point(aes(x=height_mm, y=obs_weight, fill=hatchery_origin), shape=21, size=4, stroke=1, alpha=0.8) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Field wet weight (g)", fill="Hatchery origin?") +
  theme_bw() 

### Plot log-log data -----
ggplot(data=df_bspbs) +
  geom_point(aes(x=log(height_mm), y=log(obs_weight), fill=hatchery_origin), shape=21, size=4, stroke=1, alpha=0.8) +
  geom_smooth(aes(x=log(height_mm), y=log(obs_weight)), method = "lm", se = F, colour="red") +
  ggpubr::stat_regline_equation(aes(x=log(height_mm), y=log(obs_weight), label=paste(..eq.label.., sep = "~~~")),
                                label.x.npc=0.04, label.y.npc = 1) +
  ggpubr::stat_regline_equation(aes(x=log(height_mm), y=log(obs_weight), label=paste(..rr.label.., sep = "~~~")),
                                label.x.npc=0.07, label.y.npc=0.9) +
  scale_fill_manual(labels=c("N" = "Natural-origin", "U" = "Unknown", "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="log-height (mm)", y="log-weight (g)", fill="Hatchery origin?") +
  theme_bw()  


## Fit log-log linear model ---------------------
summary(lm(log(df_bspbs$obs_weight) ~ log(df_bspbs$height_mm)))



## Fit exponential model ---------------------

### Exponential model function ----
expo_fun <- function(x, A, b) {
  A * exp(b * x)
}

### Starting values ----
  ## Starting value for A: A is the scale factor—the value of y when x=0 (if the model extrapolates back to zero height). 
  ## I don't have values for x=0, but the smallest observed weight is a reasonable proxy for the lower end. So arbitrarily set to 10-6 so A>0 but still much smaller than smallest observation.
  ## Starting value for b: b is the growth (or decay) rate. So b>0 means curve increases exponentially with x
  ## To start with a guess, use small positive slope like b=0.1. You can also estimate b from the data (log(ymax)-log(ymin))/(xmax - xmin) but not sure about relying on the log-linear relationship in this case. 
start_list_bsps <- list(
  A = max(1e-6, min(df_bspbs$obs_weight, na.rm = TRUE)),
  b = 0.1
)

### Fit model (unweighted) ----
fit_expo_bsps <- nls(
  obs_weight ~ expo_fun(height_mm, A, b),
  data = df_bspbs,
  start = start_list_bsps,
  control = nls.control(maxiter=500, warnOnly=TRUE)
)

print(summary(fit_expo_bsps))

### Predictions & residuals ----
df_bspbs <- df_bspbs %>%
  mutate(
    y_hat_expo = predict(fit_expo_bsps),
    resid_expo = obs_weight - y_hat_expo
  )

### Metrics ----
rmse <- function(resid) sqrt(mean(resid^2))
r2_nl <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

metrics_bsps <- tibble(
  Model = "Exponential (nls, unweighted)",
  RMSE  = rmse(df_bspbs$resid_expo),
  R2    = r2_nl(df_bspbs$obs_weight, df_bspbs$y_hat_expo),
  AIC   = AIC(fit_expo_bsps)
)

print(metrics)

### Plot raw data with predicted model fit ----
grid_x_bsps <- seq(min(df_bspbs$height_mm), max(df_bspbs$height_mm), length.out = 400)

coE <- coef(fit_expo_bsps)
# A = 2.31907905  
# b = 0.07564079  
# Formula:  weight = A*e^(b*height)

curve_df_bsps <- tibble(
  height_mm = grid_x_bsps,
  y_hat     = expo_fun(grid_x_bsps, A = coE["A"], b = coE["b"])
)

# Plot: 
pdf(file = here::here("outputs", "figures", "fish traits", "Weight ~ height - RST exponential model.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

ggplot() +
  geom_point(data=df_bspbs, 
             aes(x=height_mm, y=obs_weight, fill=hatchery_origin), shape=21, alpha=0.8, size=4, stroke=1) +
  geom_line(data=curve_df_bsps, aes(x=height_mm, y=y_hat), color = "red", linewidth = 1) +
  #scale_y_continuous(breaks=seq(0,3,by=0.5), limits=c(0,3)) +
  #scale_x_continuous(breaks=seq(2,14,by=1), limits=c(2,14)) +
  scale_fill_manual(labels=c("N" = "Natural-origin",
                             "U" = "Unknown",
                             "Y" = "Hatchery-origin"), values=c("dodger blue", "gray70", "orange")) +
  labs(x="Height (mm)", y="Field weight (g)", fill="Chinook origin") +
  theme_bw() +
  theme(axis.title = element_text(face="bold", size=26),
        axis.text = element_text(colour="black", size=25),
        legend.title = element_text(face="bold", size=21),
        legend.text = element_text(size=20))

dev.off()


# >> the log-log transformed relationship was slightly better R2 than the exp relationship and easier to implement, but show both in report












































# =============== Gompertz model code from Copilot ===============
# Retaining this code for learning purposes, but it came from copilot and I am not able to verify it so will not be using it. 

# Gompertz function (same form as Python run)
gompertz_fun <- function(x, K, b, x0) {
  K * exp(-exp(-b * (x - x0)))
}

# Starting values  
x <- df$height_mm
y <- df$field_weight_g

K0 <- quantile(y, 0.95, na.rm = TRUE)
b0 <- 1 / (sd(x, na.rm = TRUE) + 1e-6)
y_safe <- pmax(y, 1e-6)
# time of max slope on log scale (heuristic)
dy_log <- diff(log(y_safe)) / diff(x)
x0_guess <- tryCatch({
  stats::approx(seq_along(dy_log), x[-1], xout = which.max(dy_log))$y
}, error = function(e) NA_real_)
if (is.na(x0_guess) || length(x0_guess) == 0) x0_guess <- median(x, na.rm = TRUE)

start_vals <- list(K = as.numeric(K0), b = b0, x0 = x0_guess)

# Bounds (match Python generosity)
lower <- c(K = 0, b = 0, x0 = min(x) - 10)
upper <- c(K = Inf, b = Inf, x0 = max(x) + 10)

# Fit (UNWEIGHTED)
fit_gomp <- minpack.lm::nlsLM(
  field_weight_g ~ gompertz_fun(height_mm, K, b, x0),
  data    = df,
  start   = start_vals,
  lower   = lower,
  upper   = upper,
  control = minpack.lm::nls.lm.control(maxiter = 1000)
)

summary(fit_gomp)

# Predictions
df <- df %>%
  mutate(y_hat_gomp = predict(fit_gomp),
         resid_gomp = field_weight_g - y_hat_gomp)

# Metrics (unweighted, to match Python)
rmse <- function(resid) sqrt(mean(resid^2))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2)/sum((y - mean(y))^2)

metrics <- tibble(
  Model = "Gompertz (unweighted)",
  RMSE  = rmse(df$resid_gomp),
  R2    = r2(df$field_weight_g, df$y_hat_gomp),
  AIC   = AIC(fit_gomp)
)
print(metrics)

# Plot (points by origin + gompertz curve)
grid_x <- seq(min(df$height_mm), max(df$height_mm), length.out = 400)
co <- coef(fit_gomp)
curve_df <- tibble(
  height_mm = grid_x,
  y_hat     = gompertz_fun(grid_x, K = co["K"], b = co["b"], x0 = co["x0"])
)

p <- ggplot(df, aes(x = height_mm, y = field_weight_g)) +
  geom_point(aes(color = hatchery_origin), alpha = 0.85, size = 2.2) +
  geom_line(data = curve_df, aes(y = y_hat), color = "#ff7f0e", linewidth = 1.2) +
  labs(title = "Gompertz S-curve fit (unweighted, matches earlier plot)",
       x = "Height (mm)", y = "Field weight (g)", color = "Hatchery origin") +
  theme_bw()

print(p)







### 































