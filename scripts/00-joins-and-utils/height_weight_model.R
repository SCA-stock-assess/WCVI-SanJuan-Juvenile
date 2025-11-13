# Code to prove that height is a reasonable proxy for weight




# READ DATA:




# Plot: Weight ~ Height ---------------  
# Plot to prove that height is a good proxy for modelling weight 

# Raw:
ggplot(data=prs.biodat.fish %>% 
         filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
  geom_point(aes(x=height, y=lab_weight_g, fill=hatchery_origin), shape=21, size=3) +
  theme_bw()



# Log-transformed
ggplot(data=prs.biodat.fish %>% 
         filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1)) +
  geom_point(aes(x=log(height), y=log(lab_weight_g), fill=hatchery_origin), shape=21, size=3) +
  geom_smooth(aes(x=log(height), y=log(lab_weight_g)), method = "lm", se = T, colour="black") +
  ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..eq.label.., sep = "~~~")),
                                label.x.npc=0.04, label.y.npc = 1) +
  ggpubr::stat_regline_equation(aes(x=log(height), y=log(lab_weight_g), label=paste(..rr.label.., sep = "~~~")),
                                label.x.npc=0.07, label.y.npc=0.9) +
  labs(x="Log ( Fish field height )", y="Log ( Fish lab weight )", fill="Hatchery origin") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(colour="black", size=15),
        axis.title = element_text(face="bold", size=17),
        legend.position=c(0.8,0.2),
        legend.title = element_text(face="bold", size=17),
        legend.text = element_text(size=15),
        legend.background = element_rect(colour="black", fill=alpha("white", 0.7)))  


# Create dataset to fit model
prs.HW.data <- prs.biodat.fish %>% 
  filter(!is.na(lab_weight_g), species=="chinook", lab_weight_g>1, !is.na(height))

linear_log_HW_model <- lm(log(prs.HW.data$lab_weight_g) ~ log(prs.HW.data$height))

log_a <- coef(linear_log_HW_model)[1] # Intercept corresponds to log(a)
b <- coef(linear_log_HW_model)[2]    # Slope corresponds to b
a <- exp(log_a)