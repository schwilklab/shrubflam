# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This script depends on scripts listed in run-all.R

###############################################################################
# scaling the response variables since they measured in different units
###############################################################################



zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)

model_data <- final_data %>%
  mutate_at(c("total_dry_mass_g", "canopy_density_gm_cm3", "leaf_stem_mass_ratio",
              "canopy_moisture_content","leaf_mass_per_area", 
              "leaf_area_per_leaflet", "leaf_length_per_leaflet",
              "leaf_moisture_content", "mean_pre_burning_temp",
              "windspeed_miles_per_hour"), list(zscore))

names(model_data)

dim(model_data)

###############################################################################
# This analysis is without "Juniperus" species
# Making sure that variables with missing value is out from the analysis
###############################################################################

model_data_within_10_seconds <- model_data %>%
  filter(analysis_group != "Juniperus") %>%
  filter(ignition_delay <= 10) %>%
  dplyr::select(degsec_100, field_taxon,
                ignition_delay, mean_pre_burning_temp,
                windspeed_miles_per_hour,
                display_name, analysis_group, total_dry_mass_g , 
                canopy_density_gm_cm3 , leaf_stem_mass_ratio , 
                canopy_moisture_content, leaf_mass_per_area , 
                leaf_area_per_leaflet , leaf_length_per_leaflet , 
                leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data_within_10_seconds)

any(is.na(model_data_within_10_seconds)) # FALSE


model_data_within_10_seconds$ignition_delay <- log(model_data_within_10_seconds$ignition_delay + 1)


###############################################################################
# A global model of canopy traits with two way interactions for temperature
# integration.
###############################################################################

options(na.action = "na.fail")
canopy_pc1_model_within10seconds <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_g:leaf_stem_mass_ratio +
                                 total_dry_mass_g:canopy_density_gm_cm3 +
                                 total_dry_mass_g:canopy_moisture_content + 
                                 leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio:canopy_moisture_content + 
                                 canopy_density_gm_cm3:canopy_moisture_content + 
                                 mean_pre_burning_temp +
                                 windspeed_miles_per_hour +   
                                 (1 | analysis_group), data = model_data_within_10_seconds, REML = FALSE)




canopy_pc1_models_within10seconds <- dredge(canopy_pc1_model_within10seconds) 

best_canopy_pc1_model_within10seconds <- get.models(canopy_pc1_models_within10seconds, subset = TRUE)[[1]] 


canopy_mod_table_within10seconds <- model.sel(canopy_pc1_models_within10seconds)
canopy_mod_table_within10seconds[1:8,]




summary(best_canopy_pc1_model_within10seconds)


#sjPlot::tab_model(best_canopy_pc1_model_within10seconds)

###############################################################################
# A global model of leaf traits with two way interaction for heat release
##############################################################################


leaf_pc1_model_within10seconds <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  +
                               leaf_mass_per_area:leaf_length_per_leaflet +
                               leaf_mass_per_area:leaf_moisture_content + 
                               leaf_length_per_leaflet:leaf_moisture_content +
                               mean_pre_burning_temp +
                               windspeed_miles_per_hour +   
                               (1| analysis_group), data = model_data_within_10_seconds, REML = FALSE)

leaf_pc1_models_within10seconds <- dredge(leaf_pc1_model_within10seconds)

best_leaf_pc1_model_within10seconds <- get.models(leaf_pc1_models_within10seconds, subset = TRUE)[[1]] 

leaf_mod_table_within10seconds <- model.sel(leaf_pc1_models_within10seconds)
leaf_mod_table_within10seconds[1:8,]


summary(best_leaf_pc1_model_within10seconds) 

#sjPlot::tab_model(best_leaf_pc1_model_within10seconds)

###############################################################################
# Comparison between best canopy and leaf model for temperature integration
###############################################################################


AICc(best_canopy_pc1_model_within10seconds, best_leaf_pc1_model_within10seconds) 


################################################################################
# Adding best leaf traits to the best canopy traits model
################################################################################

leaf_plus_best_canopy_traits_model_within10seconds <- afex::lmer(degsec_100 ~ total_dry_mass_g +
                                                   leaf_length_per_leaflet +
                                                   leaf_moisture_content +
                                                   leaf_length_per_leaflet:leaf_moisture_content +
                                                   (1 | analysis_group),
                                                 data = model_data_within_10_seconds, REML = FALSE)




AICc(best_canopy_pc1_model_within10seconds, leaf_plus_best_canopy_traits_model_within10seconds) # Didn't improve


###############################################################################
# A global model of canopy traits and leaf traits for ignition delay.
# Ignition delay vs canopy traits
###############################################################################


canopy_ignition_model_within10seconds <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                      canopy_density_gm_cm3 + canopy_moisture_content +
                                      total_dry_mass_g:leaf_stem_mass_ratio +
                                      total_dry_mass_g:canopy_density_gm_cm3 +
                                      total_dry_mass_g:canopy_moisture_content + 
                                      leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                      leaf_stem_mass_ratio:canopy_moisture_content + 
                                      canopy_density_gm_cm3:canopy_moisture_content + 
                                      mean_pre_burning_temp +
                                      windspeed_miles_per_hour +  
                                      (1 | analysis_group), data = model_data_within_10_seconds, REML = FALSE)


canopy_ignition_models_within10seconds <- dredge(canopy_ignition_model_within10seconds) 

best_canopy_ignition_model_within10seconds <- get.models(canopy_ignition_models_within10seconds, subset = TRUE)[[1]]

canopy_ignition_mod_table_within10seconds <- model.sel(canopy_ignition_models_within10seconds)

canopy_ignition_mod_table_within10seconds[1:8,]

summary(best_canopy_ignition_model_within10seconds)

#sjPlot::tab_model(best_canopy_ignition_model_within10seconds)

###############################################################################
# Ignition delay vs leaf traits
###############################################################################

leaf_traits_ignition_model_within10seconds <- afex::lmer(ignition_delay ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                           leaf_moisture_content  +
                                           leaf_mass_per_area:leaf_length_per_leaflet +
                                           leaf_mass_per_area:leaf_moisture_content + 
                                           leaf_length_per_leaflet:leaf_moisture_content +
                                           mean_pre_burning_temp +
                                           windspeed_miles_per_hour +   
                                           (1 | analysis_group), data = model_data_within_10_seconds, REML = FALSE)

leaf_ignition_models_within10seconds <- dredge(leaf_traits_ignition_model_within10seconds) 


best_leaf_ignition_model_within10seconds <- get.models(leaf_ignition_models_within10seconds, subset = TRUE)[[1]] 



leaf_ignition_mod_table_within10seconds <- model.sel(leaf_ignition_models_within10seconds)

leaf_ignition_mod_table_within10seconds[1:8,]

summary(best_leaf_ignition_model_within10seconds) 
#sjPlot::tab_model(best_leaf_ignition_model_within10seconds)

AICc(best_canopy_ignition_model_within10seconds, best_leaf_ignition_model_within10seconds) 


###############################################################################
# Combination of leaf and canopy traits for ignition delay
################################################################################

canopy_leaf_ignition_model_within10seconds <- afex::lmer(ignition_delay ~ total_dry_mass_g +
                                                           leaf_length_per_leaflet +
                                                           leaf_mass_per_area +
                                                           leaf_moisture_content +
                                                           leaf_moisture_content: total_dry_mass_g +
                                                           (1 | analysis_group), 
                                                           data = model_data_within_10_seconds, REML = FALSE)



AICc(best_canopy_ignition_model_within10seconds, canopy_leaf_ignition_model_within10seconds) # Better

##########################################################################################################

