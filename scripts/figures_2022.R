### figures_2022.R
### this R script is used to create figures 
### showing significant fixed effect of canopy and leaf traits 
### on flammability from analysis.R and also model
### anova and coefficient tables



library("randomcoloR")

source("../analysis_2022.R") # The script that performed model selection

##################################################################################
# Generating thirteen disinct color
#################################################################################

set.seed(2643598)
palette <- distinctColorPalette(13)   


##################################################################################
# The best model for canopy traits for temperature integration using scaled 
# variables from model_data from analysis.R script
##################################################################################



best_degsec_canopy_mod_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                              canopy_density_gm_cm3 + (1 | genus), 
                                              data = model_data, method = "KR")

anova(best_degsec_canopy_mod_mixed) # Checking by mixed

#####################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
#######################################################################################


best_degsec_canopy_plot_data <- final_data %>% 
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)


#########################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
#########################################################################################


best_degsec_canopy_plot_data$predicted_degsec_100 <- predict(best_canopy_pc1_model, 
                                                             newdata = best_degsec_canopy_plot_data)

##########################################################################################
# Keeping unscled total_dry_mass_gm, canopy density, degsec_100 and predicted degsec
# _100 to summarise them by group
###########################################################################################

predicted_degsec_canopy_plot_data <- best_degsec_canopy_plot_data %>%
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() 
# where we will put the mean value, the bigger dots in 
# the plot
############################################################################################

degsec_by_group <- final_data %>% group_by(genus) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


###########################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by
# summarized data
###########################################################################################

ggplot(best_degsec_canopy_plot_data, aes(total_dry_mass_g, degsec_100, 
                                         color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

############################################################################################################
# Same way for canopy density
#####################################################################################

ggplot(best_degsec_canopy_plot_data, aes(canopy_density_gm_cm3, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")")))  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange", "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))



#################################################################################################
# Now the leaf traits, create the best leaf traits model for temperature integration 
# once again from analysis.R
#################################################################################################

best_degsec_leaf_mod <- afex::lmer(degsec_100 ~ leaf_mass_per_area + (1|genus),
                                   data = model_data, REML = FALSE) 


#################################################################################################
# Getting the unscaled fixed terms from final_data 
#################################################################################################

best_degsec_leaf_plot_data <- final_data %>% 
  select(leaf_mass_per_area, degsec_100, genus)


#####################################################################################
# Predicting the unscaled variables and naming the new variable as 
# predicted_degsec_100
###################################################################################


best_degsec_leaf_plot_data$predicted_degsec_100 <- predict(best_degsec_leaf_mod, 
                                                           newdata = best_degsec_leaf_plot_data)

########################################################################################
# Keeping unscaled leaf_mass_per_area degsec_100 and predicted degsec_100
# to summarise them by genus
########################################################################################

predicted_degsec_leaf_plot_data <- best_degsec_leaf_plot_data %>%
  select(predicted_degsec_100, leaf_mass_per_area, degsec_100, genus)

########################################################################################
# Summarising the fixed effects by group and making sure that the name of the 
# mean value after summarising them are same as best_degsec_
# canopy_plot_data  so that we can use them another layer in the plot as 
# geom_point() where we will put the mean value, the bigger dots in 
# the plot
###########################################################################################

degsec_leaf_by_group <- final_data %>% 
  group_by(genus) %>%
  summarize(leaf_mass_per_area = mean(leaf_mass_per_area),
            degsec_100 = mean(degsec_100))

####################################################################################
# Plotting the lma vs temperature integration
####################################################################################

ggplot(best_degsec_leaf_plot_data, aes(leaf_mass_per_area, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_leaf_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2" , "yellow", "#6FA3CE", "#D6E2A6", "red", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange", "black")) +
  scale_fill_manual(values = c("maroon2" , "yellow", "#6FA3CE", "#D6E2A6", "red", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))


##################################################################################
# Now the model without Juniperus
##################################################################################

# Best canopy model without Juniperus spp
#########################################################################################

best_degsec_canopy_mod_mixed_withoutj <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                                       canopy_density_gm_cm3 + (1 | genus), 
                                                     data = without_juniperus, method = "KR")

anova(best_degsec_canopy_mod_mixed)

##############################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
###############################################################################################


best_degsec_canopy_plot_data_withoutj <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)


###############################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
###############################################################################################


best_degsec_canopy_plot_data_withoutj$predicted_degsec_100 <- predict(best_canopy_pc1_model_withoutj, 
                                                                      newdata = best_degsec_canopy_plot_data_withoutj)

########################################################################################################################
# Keeping unscled total_dry_mass_gm, canopy density, degsec_100 and predicted 
# degsec_100 to summarise them by group
########################################################################################################################

predicted_degsec_canopy_plot_data_withoutj <- best_degsec_canopy_plot_data_withoutj %>%
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)

#########################################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after 
# summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() where we will put the mean
# value, the bigger dots in 
# the plot
###########################################################################################################################

degsec_by_group_withoutj <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  group_by(genus) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


#########################################################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by summarized data
#########################################################################################################################

ggplot(best_degsec_canopy_plot_data_withoutj, aes(total_dry_mass_g, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "red", "blue", "orange", 
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "red", "blue", "orange",
                               "black"))



###################################################################################
# Canopy density vs temperature integration
###################################################################################

ggplot(best_degsec_canopy_plot_data_withoutj, aes(canopy_density_gm_cm3, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "red", "blue", "orange", 
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "red", "blue", "orange",
                               "black"))




#########################################################################################
# Now the leaf traits, create the best leaf traits model for temperature 
# integration once again from analysis.R
#########################################################################################

best_degsec_leaf_mod_withoutj <- afex::lmer(degsec_100 ~ leaf_length_per_leaflet + (1|genus),
                                            data = without_juniperus, REML = FALSE) 


################################################################################################
# Getting the unscaled fixed terms from final_data from flam_pca.R
#################################################################################################

best_degsec_leaf_plot_data_withoutj <- final_data %>% 
  filter(genus != "Juniperus spp") %>%
  select(leaf_length_per_leaflet, degsec_100, genus)


###################################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
###################################################################################################


best_degsec_leaf_plot_data_withoutj$predicted_degsec_100 <- predict(best_degsec_leaf_mod_withoutj, 
                                                                    newdata = best_degsec_leaf_plot_data_withoutj)

######################################################################################################
# Keeping unscaled leaf_length_per_leaflet degsec_100 and predicted degsec_100 to summarise them by genus
######################################################################################################

predicted_degsec_leaf_plot_data_withoutj <- best_degsec_leaf_plot_data_withoutj %>%
  select(predicted_degsec_100, leaf_length_per_leaflet, degsec_100, genus)

#########################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after 
# summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the 
# plot as geom_point() where we will put the mean value, the bigger dots in 
# the plot
##########################################################################################################

degsec_leaf_by_group <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  group_by(genus) %>%
  summarize(leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            degsec_100 = mean(degsec_100))


#####################################################################################
# Leaf_length_per_leaflet vs temperature integration
######################################################################################

ggplot(best_degsec_leaf_plot_data_withoutj, aes(leaf_length_per_leaflet, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_leaf_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_leaf_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Leaf length per leaflet (cm)") +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                                "#73E17B", "red",
                                "#D1A7D6", "#DA61C2", "lightblue", "blue", 
                                "orange", "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "red",
                               "#D1A7D6", "#DA61C2", "lightblue", "blue",
                               "orange", "black"))



##################################################################################

