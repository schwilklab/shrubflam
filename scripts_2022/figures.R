### figures_2022.R
### this R script is used to create figures 
### showing significant fixed effect of canopy and leaf traits 
### on flammability from analysis_2022.R and also model
### anova and coefficient tables

source("../analysis_2022.R") # The script that performed model selection

#####################################################################################################################################
# The best model for canopy traits for temperature integration using scaled variables from model_data from analysis_2022.R script
#####################################################################################################################################




best_degsec_canopy_mod_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 + 
                                              (1 | genus), 
                                            data = model_data, method = "KR")

anova(best_degsec_canopy_mod_mixed)

######################################################################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
######################################################################################################################################


best_degsec_canopy_plot_data <- final_data %>% 
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)


##########################################################################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
##########################################################################################################################################


best_degsec_canopy_plot_data$predicted_degsec_100 <- predict(best_canopy_pc1_model, 
                                         newdata = best_degsec_canopy_plot_data)

#########################################################################################################################################
# Keeping unscled total_dry_mass_gm, leaf_stem_mass_ratio, degsec_100 and predicted degsec_100 to summarise them by group
#########################################################################################################################################

predicted_degsec_canopy_plot_data <- best_degsec_canopy_plot_data %>%
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, 
         degsec_100, genus)

#########################################################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() where we will put the mean value, the bigger dots in 
# the plot
#########################################################################################################################################

degsec_by_group <- final_data %>% group_by(genus) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


#########################################################################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by summarized data
#########################################################################################################################################

ggplot(best_degsec_canopy_plot_data, aes(total_dry_mass_g, degsec_100, 
                                         color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic"))

################################################################################################
# Canopy density_gm_cm3 vs temperature integration
################################################################################################

ggplot(best_degsec_canopy_plot_data, aes(canopy_density_gm_cm3, degsec_100, 
                                         color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic"))


##############################################################################################################################################
# Now the leaf traits, create the best leaf traits model for temperature integration once again from analysis_2022.R
##############################################################################################################################################

best_degsec_leaf_mod <- afex::lmer(degsec_100 ~ leaf_mass_per_area + (1|genus),
                               data = model_data, REML = FALSE) 

best_degsec_leaf_mixed <- afex::mixed(degsec_100 ~ leaf_mass_per_area + (1|genus),
                                      data = model_data, method = "KR")
anova(best_degsec_leaf_mixed)

###############################################################################################################################################
# Getting the unscaled fixed terms from final_data from flam_pca_2022.R
################################################################################################################################################

best_degsec_leaf_plot_data <- final_data %>% 
  select(leaf_mass_per_area, degsec_100, genus)


##########################################################################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
##########################################################################################################################################


best_degsec_leaf_plot_data$predicted_degsec_100 <- predict(best_leaf_pc1_model, 
                                                             newdata = best_degsec_leaf_plot_data)

#########################################################################################################################################
# Keeping unscaled leaf_mass_per_area degsec_100 and predicted degsec_100 to summarise them by group
#########################################################################################################################################

predicted_degsec_leaf_plot_data <- best_degsec_leaf_plot_data %>%
  select(predicted_degsec_100, leaf_mass_per_area, degsec_100, genus)

#########################################################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after summarising them are same as best_degsec_
# canopy_plot_data  so that we can use them another layer in the plot as geom_point() where we will put the mean value, the bigger dots in 
# the plot
#########################################################################################################################################

degsec_leaf_by_group <- final_data %>% 
  group_by(genus) %>%
  summarize(leaf_mass_per_area = mean(leaf_mass_per_area),
            degsec_100 = mean(degsec_100))


ggplot(best_degsec_leaf_plot_data, aes(leaf_mass_per_area, degsec_100, 
                                       color = genus)) +
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
        legend.text = element_text(face = "italic"))

####################################################################################
