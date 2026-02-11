# Analysis

library(MuMIn)

#############################################################################################
# Summarising Data
#############################################################################################

species_sum <- alldata_2022 %>%
  group_by(specific_epithet) %>%
  summarise(flame_duration_mean = round(mean(flame_duration), 2),
            flame_duration_sd = round(sd(flame_duration), 2),
            ignition_delay_mean = round(mean(ignition_delay), 2),
            ignition_delay_sd = round(sd(ignition_delay), 2)) %>%
  arrange(desc(flame_duration_mean))

species_sum

#############################################################################################
# First weather
#############################################################################################

temp_int_weather <- lm(flame_duration ~  mean_pre_burning_temp +
                                windspeed_miles_per_hour, data = alldata_2022)
summary(temp_int_weather)
anova(temp_int_weather)

temp_ign_weather <- lm(ignition_delay ~  mean_pre_burning_temp +
                         windspeed_miles_per_hour, data = alldata_2022)

summary(temp_ign_weather)
anova(temp_ign_weather)

##############################################################################################
# Temperature integration
#############################################################################################

options(na.action = "na.fail")

temp_int_global_model <- lm(flame_duration ~ total_dry_mass_g + leaf_stem_mass_ratio + canopy_density_gm_cm3 + 
                                      canopy_moisture_content + leaf_mass_per_area + leaf_length_per_leaflet,
                            data = final_data)
temp_int_models <- dredge(temp_int_global_model, m.lim = c(1,2)) 
best_temp_int_mod <- get.models(temp_int_models, subset = TRUE)[[1]] 
temp_int_all_mod <- model.sel(temp_int_models)
temp_int_all_mod[1:4,]

summary(best_temp_int_mod)
summary(get.models(temp_int_models, subset = TRUE)[[2]])

anova(best_temp_int_mod)


###################################################################################
# Ignition delay
###################################################################################

temp_ign_global_model <- lm(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + canopy_density_gm_cm3 + 
                                      canopy_moisture_content + leaf_mass_per_area + leaf_length_per_leaflet,
                            data = final_data)
temp_ign_models <- dredge(temp_ign_global_model, m.lim = c(1,2)) 
best_temp_ign_mod <- get.models(temp_ign_models, subset = TRUE)[[1]] 
temp_ign_all_mod <- model.sel(temp_ign_models)
temp_ign_all_mod[1:4,]
summary(best_temp_ign_mod)
anova(best_temp_ign_mod)


#################################################################################
# Cleaning environment
#################################################################################
rm(temp_int_weather, temp_int_global_model, temp_int_models, temp_int_all_mod,
   temp_ign_global_model, temp_ign_models, temp_ign_all_mod)
#################################################################################
