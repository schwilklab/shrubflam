
library(MuMIn)

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)

model_data_withconifers <- final_data %>%
  mutate_at(c("total_dry_mass_g", "canopy_density_gm_cm3", "leaf_stem_mass_ratio",
              "canopy_moisture_content","ldmc","leaf_mass_per_area", 
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
                canopy_density_gm_cm3 , ldmc, leaf_stem_mass_ratio , 
                canopy_moisture_content, leaf_mass_per_area , 
                leaf_area_per_leaflet , leaf_length_per_leaflet , 
                leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data_withconifers)

any(is.na(model_data_withconifers)) # FALSE

model_data_withconifers$ignition_delay <- log(model_data_withconifers$ignition_delay + 1)

model_data_withoutj <- model_data_withconifers %>%
  filter(analysis_group != "Juniperus") 
dim(model_data_withoutj)
any(is.na(model_data_withoutj)) 


#############################################################################################
# First waether
#############################################################################################

temp_int_weather <- afex::mixed(degsec_100 ~  mean_pre_burning_temp +
                                windspeed_miles_per_hour +
                                (1 | specific_epithet), data = model_data_withconifers, method = "KR", REML = TRUE)
temp_int_weather_lme4 <- lme4::lmer(degsec_100 ~ mean_pre_burning_temp +
                                      windspeed_miles_per_hour +
                                      (1 | specific_epithet), data = model_data_withconifers)
temp_int_weather_anova <- car::Anova(lme4::lmer(degsec_100 ~ mean_pre_burning_temp +
                                                  windspeed_miles_per_hour +
                                                  (1| specific_epithet), data = model_data_withconifers),
                                                  type = 2, test.statistic = "F")

temp_ign_weater <- afex::mixed(ignition_delay ~  mean_pre_burning_temp +
                                 windspeed_miles_per_hour + (1 | specific_epithet), 
                                 data = model_data_withconifers, method = "KR", REML = TRUE)

temp_ign_weater_lme4 <- lme4::lmer(ignition_delay ~ mean_pre_burning_temp +
                                     windspeed_miles_per_hour +
                                     (1 | specific_epithet), data = model_data_withconifers)

temp_ign_weather_anova <- car::Anova(lme4::lmer(ignition_delay ~ mean_pre_burning_temp +
                                                  windspeed_miles_per_hour +
                                                  (1| specific_epithet), data = model_data_withconifers),
                                     type = 2, test.statistic = "F")

##############################################################################################
# Temperature integration
#############################################################################################

options(na.action = "na.fail")

temp_int_global_model <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + canopy_density_gm_cm3 + 
                                      canopy_moisture_content + leaf_mass_per_area + leaf_length_per_leaflet +
                                      total_dry_mass_g:leaf_mass_per_area + 
                                      (1 | specific_epithet), data = model_data_withconifers, REML = FALSE)
temp_int_models <- dredge(temp_int_global_model, m.lim = c(1,2)) 
best_temp_int_mod <- get.models(temp_int_models, subset = TRUE)[[1]] 
temp_int_all_mod <- model.sel(temp_int_models)
temp_int_all_mod[1:4,]

#sjPlot::tab_model(best_temp_int_mod)

##########################################################################################
# Now fitting model with "KR" and REML
##########################################################################################

mixed_temp_int <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                leaf_mass_per_area +
                                (1 | specific_epithet), data = model_data_withconifers, method = "KR",
                                REML = TRUE)

final_model_temp_int <- lme4::lmer(degsec_100 ~ total_dry_mass_g +
                                     leaf_mass_per_area +
                                     (1 | specific_epithet),
                                   data = model_data_withconifers)

r.squaredGLMM(final_model_temp_int)

temp_int_model <-  car::Anova(lme4::lmer(degsec_100 ~ total_dry_mass_g + leaf_mass_per_area  +
                                           (1| specific_epithet), data = model_data_withconifers),
                              type = 2, test.statistic = "F")

temp_int_model_xtable <- xtable::xtable(temp_int_model, digits = 3)
mixed_temp_int_model_coeff <- summary(mixed_temp_int)$coefficients
mixed_temp_int_model_coeff_xtable <- xtable::xtable(mixed_temp_int_model_coeff, digits = 3)


####################################################################################
# Now without Juniperus
###################################################################################
temp_int_global_model_withoutj <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + canopy_density_gm_cm3 + 
                                      canopy_moisture_content + leaf_mass_per_area + leaf_length_per_leaflet +
                                      total_dry_mass_g:leaf_mass_per_area + 
                                      (1 | specific_epithet), data = model_data_withoutj, REML = FALSE)
temp_int_models_withoutj <- dredge(temp_int_global_model_withoutj, m.lim = c(1,2)) 
best_temp_int_mod_withoutj <- get.models(temp_int_models_withoutj, subset = TRUE)[[1]] 
temp_int_all_mod_withoutj <- model.sel(temp_int_models_withoutj)
temp_int_all_mod_withoutj[1:4,]
summary(best_temp_int_mod_withoutj)

#sjPlot::tab_model(best_temp_int_mod_withoutj)

##########################################################################################
# Now fitting model with "KR" and REML
##########################################################################################

mixed_temp_int_withoutj <- afex::mixed(degsec_100 ~ total_dry_mass_g +
                                (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                REML = TRUE)

final_model_temp_int_withoutj <- lme4::lmer(degsec_100 ~ total_dry_mass_g + 
                                     (1 | specific_epithet),  data = model_data_withoutj)

r.squaredGLMM(final_model_temp_int_withoutj)

temp_int_model_withoutj <-  car::Anova(lme4::lmer(degsec_100 ~ total_dry_mass_g + (1| specific_epithet),
                                                  data = model_data_withoutj),
                                       type = 2, test.statistic = "F")

temp_int_model_xtable_withoutj <- xtable::xtable(temp_int_model_withoutj, digits = 3)
mixed_temp_int_model_coeff_withoutj <- summary(mixed_temp_int_withoutj)$coefficients
mixed_temp_int_model_coeff_xtable_withoutj <- xtable::xtable(mixed_temp_int_model_coeff_withoutj, digits = 3)

second_best_withoutj <- lme4::lmer(degsec_100 ~ total_dry_mass_g + leaf_length_per_leaflet +
                                     (1 | specific_epithet), data = model_data_withoutj)
car::Anova(second_best_withoutj, type = 2, test.statistics = "F")
r.squaredGLMM(second_best_withoutj)

second_best_withoutj_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + leaf_length_per_leaflet +
                                            (1 | specific_epithet), data = model_data_withoutj, 
                                          method = "KR", REML = TRUE)

third_best <- lme4::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio +
                           (1 | specific_epithet), data = model_data_withoutj)

r.squaredGLMM(third_best)
car::Anova(third_best, type = 2, test.statistics = "F")

third_best_mixed <-  afex::mixed(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio +
                                   (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                 REML = TRUE)

fourth_best_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + leaf_mass_per_area +
                                   (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                 REML = TRUE)
fourth_best <- lme4::lmer(degsec_100 ~ total_dry_mass_g + leaf_mass_per_area +
                            (1 | specific_epithet), data = model_data_withoutj)

r.squaredGLMM(fourth_best)
car::Anova(fourth_best, type = 2, test.statistics = "F")

fifth_best_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 +
                                  (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                REML = TRUE)
fifth_best <- lme4::lmer(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 +
                           (1 | specific_epithet), data = model_data_withoutj)
r.squaredGLMM(fifth_best)

###################################################################################
# Ignition delay
###################################################################################

temp_ign_global_model <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + canopy_density_gm_cm3 + 
                                      canopy_moisture_content + leaf_mass_per_area + leaf_length_per_leaflet + 
                                      (1 | specific_epithet), data = model_data_withconifers, REML = FALSE)
temp_ign_models <- dredge(temp_ign_global_model, m.lim = c(1,2)) 
best_temp_ign_mod <- get.models(temp_ign_models, subset = TRUE)[[1]] 
temp_ign_all_mod <- model.sel(temp_ign_models)
temp_ign_all_mod[1:4,]
summary(best_temp_ign_mod)

##########################################################################################
# Now fitting model with "KR" and REML
##########################################################################################

mixed_temp_ing <- afex::mixed(ignition_delay ~ canopy_moisture_content + leaf_mass_per_area +
                                         (1 | specific_epithet), data = model_data_withconifers, method = "KR",
                                       REML = TRUE)

final_model_ing <- lme4::lmer(ignition_delay ~  canopy_moisture_content +
                                leaf_mass_per_area + (1 | specific_epithet),
                                            data = model_data_withconifers)
r.squaredGLMM(final_model_ing)

temp_ing_model <-  car::Anova(lme4::lmer(ignition_delay ~  canopy_moisture_content +
                                           leaf_mass_per_area +  (1| specific_epithet),
                                         data = model_data_withconifers),
                              type = 2, test.statistic = "F")

temp_ign_model_xtable <- xtable::xtable(temp_ing_model, digits = 3)
mixed_temp_ign_model_coeff <- summary(mixed_temp_ing)$coefficients
mixed_temp_ign_model_coeff_xtable <- xtable::xtable(mixed_temp_ign_model_coeff, digits = 3)

second_best_ign <- lme4::lmer(ignition_delay ~  canopy_moisture_content +
                                canopy_density_gm_cm3 + (1 | specific_epithet),
                              data = model_data_withconifers)
r.squaredGLMM(second_best_ign)

car::Anova(second_best_ign, type = 2, test.statistics = "F")



sjPlot::tab_model(get.models(temp_ign_models, subset = TRUE)[[1]]) 

##########################################################################################
# EWithout Juniperus
##########################################################################################

temp_ign_global_model_withoutj <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                               canopy_density_gm_cm3 + canopy_moisture_content +
                                               leaf_mass_per_area + leaf_length_per_leaflet +
                                               (1 | specific_epithet), data = model_data_withoutj, REML = FALSE)
temp_ign_models_withoutj <- dredge(temp_ign_global_model_withoutj, m.lim = c(1,2)) 
best_temp_ign_mod_withoutj <- get.models(temp_ign_models_withoutj, subset = TRUE)[[1]] 
temp_ign_all_mod_withoutj <- model.sel(temp_ign_models_withoutj)
temp_ign_all_mod_withoutj[1:4,]
summary(best_temp_ign_mod_withoutj)

##########################################################################################
# Now fitting model with "KR" and REML
##########################################################################################

mixed_temp_ing_withoutj <- afex::mixed(ignition_delay ~ total_dry_mass_g + canopy_moisture_content +
                                (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                REML = TRUE)

final_model_ing_withoutj <- lme4::lmer(ignition_delay ~ canopy_moisture_content + total_dry_mass_g + (1 | specific_epithet),
                              data = model_data_withoutj)

r.squaredGLMM(final_model_ing_withoutj)

temp_ing_model_withoutj <-  car::Anova(lme4::lmer(ignition_delay ~ canopy_moisture_content + total_dry_mass_g +
                                                    (1| specific_epithet), data = model_data_withconifers),
                                       type = 2, test.statistic = "F")

temp_ign_model_xtable_withoutj <- xtable::xtable(temp_ing_model_withoutj, digits = 3)
mixed_temp_ign_model_coeff_withoutj <- summary(mixed_temp_ing_withoutj)$coefficients
mixed_temp_ign_model_coeff_xtable_withoutj <- xtable::xtable(mixed_temp_ign_model_coeff_withoutj, digits = 3)

second_best_ign_withoutj <- lme4::lmer(ignition_delay ~ canopy_moisture_content + (1 | specific_epithet),
                                       data = model_data_withoutj)

r.squaredGLMM(second_best_ign_withoutj)
car::Anova(second_best_ign_withoutj, type = 2, test.statistics = "F")

second_best_ign_withoutj_mixed <- afex::mixed(ignition_delay ~  canopy_moisture_content +
                                                (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                              REML = TRUE)

third_best_ign_withoutj <- lme4::lmer(ignition_delay ~ canopy_moisture_content + leaf_mass_per_area +
                                        (1 | specific_epithet), data = model_data_withoutj)

r.squaredGLMM(third_best_ign_withoutj)
car::Anova(third_best_ign_withoutj, type = 2, test.statistics = "F")

third_best_ign_withoutj_mixed <-  afex::mixed(ignition_delay ~  canopy_moisture_content + leaf_mass_per_area +
                                                (1 | specific_epithet), data = model_data_withoutj, method = "KR",
                                              REML = TRUE)

#sjPlot::tab_model(get.models(temp_ign_models_withoutj, subset = TRUE)[[4]]) 
