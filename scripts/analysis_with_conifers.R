# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This script depends on scripts listed in run-all.R

library(MuMIn)

# MuMIn package for automated model selection through subsetting the maximum
# model, with optimal constraints for model inclusion. Model parameter and
# prediction averaging based on model weights derived from information criteria
# (AIC). source: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf
# REML IS EQUAL TO FALSE BECAUSE Faraway (2006) Extending the linear model with
# R (p. 156): The reason is that REML estimates the random effects by
# considering linear combinations of the data that remove the fixed effects. If
# these fixed effects are changed, the likelihoods of the two models will not
# be directly comparable
# https://stats.stackexchange.com/questions/116770/reml-or-ml-to-
# compare-two-mixed-effects-models-with-differing-fixed-effects-but


###############################################################################
# scaling the response variables since they measured in different units
###############################################################################

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)

model_data_withconifers <- final_data %>%
  mutate_at(c("total_dry_mass_g", "canopy_density_gm_cm3", "leaf_stem_mass_ratio",
              "canopy_moisture_content","leaf_mass_per_area", 
              "leaf_area_per_leaflet", "leaf_length_per_leaflet",
              "leaf_moisture_content", "mean_pre_burning_temp",
              "windspeed_miles_per_hour"), list(zscore))

names(model_data_withconifers)
dim(model_data_withconifers)

###############################################################################
# This analysis is without "Juniperus" species
# Making sure that variables with missing value is out from the analysis
###############################################################################

model_data_withconifers <- model_data_withconifers %>%
  dplyr::select(degsec_100, field_taxon,specific_epithet,
                ignition_delay, mean_pre_burning_temp,
                windspeed_miles_per_hour,
                display_name, analysis_group, total_dry_mass_g , 
                canopy_density_gm_cm3 , leaf_stem_mass_ratio , 
                canopy_moisture_content, leaf_mass_per_area , 
                leaf_area_per_leaflet , leaf_length_per_leaflet , 
                leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data_withconifers)

any(is.na(model_data_withconifers)) # FALSE

model_data_withconifers$ignition_delay <- log(model_data_withconifers$ignition_delay + 1)

###############################################################################
# A global model of canopy traits with two way interactions for temperature
# integration.
###############################################################################

options(na.action = "na.fail")

canopy_pc1_model_withconifers <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_g:leaf_stem_mass_ratio +
                                 total_dry_mass_g:canopy_density_gm_cm3 +
                                 total_dry_mass_g:canopy_moisture_content + 
                                 leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio:canopy_moisture_content + 
                                 canopy_density_gm_cm3:canopy_moisture_content + 
                                 mean_pre_burning_temp +
                                 windspeed_miles_per_hour +   
                                 (1 | specific_epithet), data = model_data_withconifers, REML = FALSE)
canopy_pc1_models_withconifers <- dredge(canopy_pc1_model_withconifers) 
best_canopy_pc1_model_withconifers <- get.models(canopy_pc1_models_withconifers, subset = TRUE)[[1]] 
canopy_mod_table_withconifers <- model.sel(canopy_pc1_models_withconifers)
canopy_mod_table_withconifers[1:8,]
summary(best_canopy_pc1_model_withconifers)

###############################################################################
# A global model of leaf traits with two way interaction for heat release
# Kendall rank correlation coefficient between leaf_area_per_leaflet and
# leaf_length_per_leaflet is 0.65 and decided to drop leaf_area_per_leaflet
# from the model to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.
##############################################################################

leaf_pc1_model_withconifers <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  +
                               leaf_mass_per_area:leaf_length_per_leaflet +
                               leaf_mass_per_area:leaf_moisture_content + 
                               leaf_length_per_leaflet:leaf_moisture_content +
                               mean_pre_burning_temp +
                               windspeed_miles_per_hour +   
                               (1| specific_epithet), data = model_data_withconifers, REML = FALSE)
leaf_pc1_models_withconifers <- dredge(leaf_pc1_model_withconifers)
best_leaf_pc1_model_withconifers <- get.models(leaf_pc1_models_withconifers, subset = TRUE)[[1]] 
leaf_mod_table_withconifers <- model.sel(leaf_pc1_models_withconifers)
leaf_mod_table_withconifers[1:8,]
summary(best_leaf_pc1_model_withconifers) 

###############################################################################
# Comparison between best canopy and leaf model for temperature integration
###############################################################################

AICc(best_canopy_pc1_model_withconifers, best_leaf_pc1_model_withconifers) 

###############################################################################
# Ignition delay vs canopy traits
###############################################################################

canopy_ignition_model_withconifers <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                                   canopy_density_gm_cm3 + canopy_moisture_content +
                                                   mean_pre_burning_temp +
                                                   windspeed_miles_per_hour +  
                                                   (1 | specific_epithet), data = model_data_withconifers, 
                                                   REML = FALSE)
canopy_ignition_models_withconifers <- dredge(canopy_ignition_model_withconifers) 
best_canopy_ignition_model_withconifers <- get.models(canopy_ignition_models_withconifers, subset = TRUE)[[1]]
canopy_ignition_mod_table_withconifers <- model.sel(canopy_ignition_models_withconifers)
canopy_ignition_mod_table_withconifers[1:8,]
summary(best_canopy_ignition_model_withconifers)

###############################################################################
# Ignition delay vs leaf traits
###############################################################################

leaf_traits_ignition_model_withconifers <- afex::lmer(ignition_delay ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                                        leaf_moisture_content  +
                                                        mean_pre_burning_temp +
                                                        windspeed_miles_per_hour +  
                                                        (1| specific_epithet), data = model_data_withconifers, 
                                                        REML = FALSE)
leaf_ignition_models_withconifers <- dredge(leaf_traits_ignition_model_withconifers) 
best_leaf_ignition_model_withconifers <- get.models(leaf_ignition_models_withconifers, subset = TRUE)[[1]] 
leaf_ignition_mod_table_withconifers <- model.sel(leaf_ignition_models_withconifers)
leaf_ignition_mod_table_withconifers[1:8,]
summary(best_leaf_ignition_model_withconifers) 

AICc(best_canopy_ignition_model_withconifers, best_leaf_ignition_model_withconifers) 

################################################################################
# Combination of canopy and leaf traits!
###############################################################################

canopy_leaf_ignition_model_withconifers <- afex::lmer(ignition_delay ~ canopy_density_gm_cm3 +
                                             leaf_moisture_content +
                                             leaf_mass_per_area + 
                                             (1 | specific_epithet), data = model_data_withconifers,
                                             REML = FALSE)
AICc(best_leaf_ignition_model_withconifers, canopy_leaf_ignition_model_withconifers)  

#############################################################################################################
###########################################################################################
# This part is for model building for anova table, first for heat release
# At first, canopy traits
# Checking the significance by Kenward-Roger's method
###########################################################################################

canopy_traits_heat_release_model_mixed_withconifers <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                                                     (1|specific_epithet), data = model_data_withconifers,
                                                                     method = "KR", REML = TRUE)
canopy_traits_anova_table_model_withconifers <- lme4::lmer(degsec_100 ~ total_dry_mass_g +
                                                (1 | specific_epithet),
                                                data = model_data_withconifers)
canopy_traits_anova_withconifers <- car::Anova(canopy_traits_anova_table_model_withconifers, 
                                  type = 2, test.statistic = "F")
canopy_anova_withconifers <- xtable::xtable(canopy_traits_anova_withconifers, digits = 3)
canopy_anova_coefficients_withconifers <- summary(canopy_traits_anova_table_model_withconifers)$coefficients
canopy_coeff_withconifers <- xtable::xtable(canopy_anova_coefficients_withconifers, digits = 3)

############################################################################################
# Now leaf traits for heat release
#############################################################################################

leaf_traits_heat_release_model_mixed_withconifers <- afex::mixed(degsec_100 ~ leaf_length_per_leaflet +
                                                      (1| specific_epithet), 
                                                      data = model_data_withconifers,
                                                      method = "KR", REML = TRUE)
leaf_traits_anova_table_model_withconifers <- lme4::lmer(degsec_100 ~ leaf_length_per_leaflet +
                                                           (1| specific_epithet), 
                                                           data = model_data_withconifers)
leaf_traits_anova_withconifers <- car::Anova(leaf_traits_anova_table_model_withconifers, 
                                             type = 2, test.statistic = "F")
leaf_anova_withconifers <- xtable::xtable(leaf_traits_anova_withconifers, digits = 3)
leaf_anova_coefficients_withconifers <- summary(leaf_traits_anova_table_model_withconifers)$coefficients
leaf_coeff_conifers <- xtable::xtable(leaf_anova_coefficients_withconifers, digits = 3)

###################################################################################################################
# Now for ignition delay, at first the canopy traits
###################################################################################################################

canopy_traits_ignition_model_mixed_withconifers <- afex::mixed(ignition_delay ~ canopy_density_gm_cm3 + 
                                                    canopy_moisture_content +
                                                    (1|specific_epithet), data = model_data_withconifers,
                                                    method = "KR", REML = TRUE)
canopy_traits_ignition_anova_table_model_withconifers <- lme4::lmer(ignition_delay ~ canopy_density_gm_cm3 + 
                                                                      canopy_moisture_content + 
                                                                      (1 | specific_epithet), 
                                                                      data = model_data_withconifers)
canopy_traits_ignition_anova_withconifers <- car::Anova(canopy_traits_ignition_anova_table_model_withconifers,
                                                        type = 2, test.statistic = "F")
canopy_ignition_xtable_withconifers <-  xtable::xtable(canopy_traits_ignition_anova_withconifers, digits = 3)
canopy_ignition_anova_coefficients_withconifers <- summary(canopy_traits_ignition_anova_table_model_withconifers)$coefficients
canopy_ignition_coeff_withconifers <- xtable::xtable(canopy_ignition_anova_coefficients_withconifers, digits = 3)

############################################################################################
# Now leaf traits
################### ##########################################################################

leaf_traits_ignition_model_mixed_withconifers <- afex::mixed(ignition_delay ~ leaf_mass_per_area +
                                                               leaf_moisture_content +
                                                               (1|specific_epithet), 
                                                               data = model_data_withconifers,
                                                               method = "KR", REML = TRUE)
leaf_traits_ignition_anova_table_model_withconifers <- lme4::lmer(ignition_delay ~ leaf_mass_per_area +
                                                                    leaf_moisture_content + 
                                                                    (1 | specific_epithet), 
                                                                    data = model_data_withconifers)
leaf_traits_ignition_anova_withconifers <- car::Anova(leaf_traits_ignition_anova_table_model_withconifers, type = 3,
                                         test.statistic = "F")
leaf_ignition_xtable_withconifers <-  xtable::xtable(leaf_traits_ignition_anova_withconifers, digits = 3)
leaf_ignition_anova_coefficients_withconifers <- summary(leaf_traits_ignition_anova_table_model_withconifers)$coefficients
leaf_ignition_coeff_withconifers <- xtable::xtable(leaf_ignition_anova_coefficients_withconifers, digits = 3)

