# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This script depends on scripts listed in run-all.R
# Without Conifers




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
# Making sure that variables with missing value is out from the analysis
###############################################################################

model_data_withoutj <- model_data %>%
  filter(analysis_group != "Juniperus") %>%
  dplyr::select(degsec_100, field_taxon,
         ignition_delay, mean_pre_burning_temp,
         windspeed_miles_per_hour,
         display_name, analysis_group, total_dry_mass_g , 
         canopy_density_gm_cm3 , leaf_stem_mass_ratio , 
         canopy_moisture_content, leaf_mass_per_area , 
         leaf_area_per_leaflet , leaf_length_per_leaflet , 
         leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data_withoutj)

any(is.na(model_data_withoutj)) # FALSE


model_data_withoutj$ignition_delay <- log(model_data_withoutj$ignition_delay + 1)

###############################################################################
# A global model of canopy traits with two way interactions for temperature
# integration.
###############################################################################

options(na.action = "na.fail")

null_model <- afex::lmer(degsec_100 ~  (1 | analysis_group), data = model_data_withoutj, REML = FALSE)

canopy_pc1_model <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_g:leaf_stem_mass_ratio +
                                 total_dry_mass_g:canopy_density_gm_cm3 +
                                 total_dry_mass_g:canopy_moisture_content + 
                                 leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio:canopy_moisture_content + 
                                 canopy_density_gm_cm3:canopy_moisture_content + 
                                 mean_pre_burning_temp +
                                 windspeed_miles_per_hour +
                                 (1 | analysis_group), data = model_data_withoutj, REML = FALSE)


canopy_pc1_models <- dredge(canopy_pc1_model) # Performs an automated

# model selection with subsets of the supplied global model. source: ?dredge

best_canopy_pc1_model <- get.models(canopy_pc1_models, subset = TRUE)[[1]] # returns list and 
# indexing the first one, top model.

canopy_mod_table <- model.sel(canopy_pc1_models)
canopy_mod_table[1:8,]

# Generate or extract a list of fitted model objects from a "model.selection"
# table, object returned by dredge. The argument subset must be explicitly
# provided. This is to assure that a potentially long list of models is not
# fitted unintentionally. To evaluate all models, set subset to NA or TRUE.
# source: ?get.models

summary(best_canopy_pc1_model)

#sjPlot::tab_model(best_canopy_pc1_model)

###############################################################################
# A global model of leaf traits with two way interaction for heat release
# Kendall rank correlation coefficient between leaf_area_per_leaflet and
# leaf_length_per_leaflet is 0.67 and decided to drop leaf_area_per_leaflet
# from the model to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.
##############################################################################



leaf_pc1_model <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  + 
                               leaf_mass_per_area:leaf_length_per_leaflet +
                               leaf_mass_per_area:leaf_moisture_content + 
                               leaf_length_per_leaflet:leaf_moisture_content + 
                               mean_pre_burning_temp +
                               windspeed_miles_per_hour +
                               (1| analysis_group), data = model_data_withoutj, REML = FALSE)

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 

leaf_mod_table <- model.sel(leaf_pc1_models)

leaf_mod_table[1:8,]

summary(best_leaf_pc1_model) 

#sjPlot::tab_model(best_leaf_pc1_model)

###############################################################################
# Comparison between best canopy and leaf model for temperature integration
###############################################################################

AICc(best_canopy_pc1_model, best_leaf_pc1_model, null_model) 


################################################################################
# Combination of leaf and canopy traits
################################################################################

leaf_plus_best_canopy_traits_model <- afex::lmer(degsec_100 ~ total_dry_mass_g +
                                                   leaf_length_per_leaflet +
                                                   leaf_moisture_content +
                                                   total_dry_mass_g:leaf_length_per_leaflet +
                                                   total_dry_mass_g:leaf_moisture_content +
                                                   leaf_length_per_leaflet:leaf_moisture_content +
                                                   (1 | analysis_group),
                                                   data = model_data_withoutj, REML = FALSE)


AICc(leaf_plus_best_canopy_traits_model, best_canopy_pc1_model) # didn't improve the model

###############################################################################
# Ignition delay vs canopy traits
###############################################################################

null_model_for_ignition <- afex::lmer(ignition_delay ~ (1 | analysis_group), data = model_data_withoutj, REML = FALSE)

canopy_ignition_model <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                      canopy_density_gm_cm3 + canopy_moisture_content +
                                      total_dry_mass_g:leaf_stem_mass_ratio +
                                      total_dry_mass_g:canopy_density_gm_cm3 +
                                      total_dry_mass_g:canopy_moisture_content + 
                                      leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                      leaf_stem_mass_ratio:canopy_moisture_content + 
                                      canopy_density_gm_cm3:canopy_moisture_content + 
                                      mean_pre_burning_temp +
                                      windspeed_miles_per_hour +
                                      (1 | analysis_group), data = model_data_withoutj, REML = FALSE)


canopy_ignition_models <- dredge(canopy_ignition_model) 
best_canopy_ignition_model <- get.models(canopy_ignition_models, subset = TRUE)[[1]]
canopy_ignition_mod_table <- model.sel(canopy_ignition_models)
canopy_ignition_mod_table[1:8,]
summary(best_canopy_ignition_model)
#sjPlot::tab_model(best_canopy_ignition_model)

###############################################################################
# Ignition delay vs leaf traits
###############################################################################

leaf_traits_ignition_model <- afex::lmer(ignition_delay ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                           leaf_moisture_content  + 
                                           leaf_mass_per_area:leaf_length_per_leaflet +
                                           leaf_mass_per_area:leaf_moisture_content + 
                                           leaf_length_per_leaflet:leaf_moisture_content  +
                                           mean_pre_burning_temp +
                                           windspeed_miles_per_hour +
                                           (1 | analysis_group), data = model_data_withoutj, REML = FALSE)

leaf_ignition_models <- dredge(leaf_traits_ignition_model) 

best_leaf_ignition_model <- get.models(leaf_ignition_models, subset = TRUE)[[1]] 
leaf_ignition_mod_table <- model.sel(leaf_ignition_models)
leaf_ignition_mod_table[1:8,]
summary(best_leaf_ignition_model) 
#sjPlot::tab_model(best_leaf_ignition_model)

AICc(best_canopy_ignition_model, best_leaf_ignition_model, null_model_for_ignition) 

###############################################################################
# Combination of leaf and canopy traits on ignition delay
###############################################################################

best_canopy_leaf_traits_ignition_model <- afex::lmer(ignition_delay ~ total_dry_mass_g +
                                                              leaf_length_per_leaflet +
                                                              leaf_mass_per_area + 
                                                              leaf_moisture_content +
                                                              leaf_moisture_content:total_dry_mass_g +
                                                              total_dry_mass_g:leaf_length_per_leaflet +
                                                              total_dry_mass_g:leaf_mass_per_area +
                                                              leaf_length_per_leaflet:leaf_mass_per_area +
                                                              leaf_length_per_leaflet:leaf_moisture_content +
                                                              leaf_mass_per_area:leaf_moisture_content +
                                                              (1 | analysis_group),
                                                              data = model_data_withoutj, REML = FALSE)



AICc(best_canopy_ignition_model, best_canopy_leaf_traits_ignition_model) # Improved

###########################################################################################
# This part is for model building for anova table, first for heat release
# At first, canopy traits
# Checking the significance by Kenward-Roger's method
###########################################################################################


canopy_traits_heat_release_model_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                                        (1|analysis_group), data = model_data_withoutj,
                                                      method = "KR", REML = TRUE)



canopy_traits_anova_table_model <- lme4::lmer(degsec_100 ~ total_dry_mass_g + 
                                                (1 | analysis_group),
                                              data = model_data_withoutj)

canopy_traits_anova <- car::Anova(canopy_traits_anova_table_model,                                   type = 2, test.statistic = "F")
canopy_anova <- xtable::xtable(canopy_traits_anova, digits = 3)
canopy_anova_coefficients <- summary(canopy_traits_anova_table_model)$coefficients
canopy_coeff <- xtable::xtable(canopy_anova_coefficients, digits = 3)

############################################################################################
# Now leaf traits for heat release
#############################################################################################

leaf_traits_heat_release_model_mixed <- afex::mixed(degsec_100 ~ leaf_length_per_leaflet +
                                                      leaf_moisture_content +
                                                      leaf_length_per_leaflet:leaf_moisture_content +
                                                      (1|analysis_group), data = model_data_withoutj,
                                                       method = "KR", REML = TRUE)

leaf_traits_anova_table_model <- lme4::lmer(degsec_100 ~ leaf_length_per_leaflet +
                                              leaf_moisture_content +
                                              leaf_length_per_leaflet:leaf_moisture_content +
                                              (1 | analysis_group), data = model_data_withoutj)

leaf_traits_anova <- car::Anova(leaf_traits_anova_table_model, type = 2, 
                                test.statistic = "F")

leaf_anova <- xtable::xtable(leaf_traits_anova, digits = 3)
leaf_anova_coefficients <- summary(leaf_traits_anova_table_model)$coefficients
leaf_coeff <- xtable::xtable(leaf_anova_coefficients, digits = 3)

###################################################################################################################
# Now for ignition delay, at first the canopy traits
###################################################################################################################

canopy_traits_ignition_model_mixed <- afex::mixed(ignition_delay ~ canopy_moisture_content +
                                                    total_dry_mass_g + 
                                                    canopy_moisture_content: total_dry_mass_g +
                                                    (1|analysis_group), data = model_data_withoutj,
                                                    method = "KR", REML = TRUE)

canopy_traits_ignition_anova_table_model <- lme4::lmer(ignition_delay ~ canopy_moisture_content +
                                                         total_dry_mass_g + 
                                                         canopy_moisture_content: total_dry_mass_g +
                                                         (1 | analysis_group), 
                                                         data = model_data_withoutj)


canopy_traits_ignition_anova <- car::Anova(canopy_traits_ignition_anova_table_model, type = 2, 
                                           test.statistic = "F")

canopy_ignition_xtable <-  xtable::xtable(canopy_traits_ignition_anova, digits = 3)

canopy_ignition_anova_coefficients <- summary(canopy_traits_ignition_anova_table_model)$coefficients

canopy_ignition_coeff <- xtable::xtable(canopy_ignition_anova_coefficients, digits = 3)

############################################################################################
# Now leaf traits
#############################################################################################

leaf_traits_ignition_model_mixed <- afex::mixed(ignition_delay ~ leaf_length_per_leaflet +
                                                  leaf_mass_per_area + 
                                                  leaf_moisture_content +
                                                  (1|analysis_group), 
                                                  data = model_data_withoutj,
                                                  method = "KR", REML = TRUE)


leaf_traits_ignition_anova_table_model <- lme4::lmer(ignition_delay ~  leaf_length_per_leaflet +
                                                       leaf_mass_per_area + 
                                                       leaf_moisture_content +
                                                       (1 | analysis_group), data = model_data_withoutj)


leaf_traits_ignition_anova <- car::Anova(leaf_traits_ignition_anova_table_model, type = 2,
                                         test.statistic = "F")
leaf_ignition_xtable <-  xtable::xtable(leaf_traits_ignition_anova, digits = 3)
leaf_ignition_anova_coefficients <- summary(leaf_traits_ignition_anova_table_model)$coefficients
leaf_ignition_coeff <- xtable::xtable(leaf_ignition_anova_coefficients, digits = 3)

##############################################################################################
# The combination of leaf and canopy traits on ignition delay time
##############################################################################################

leaf_plus_canopy_traits_ignition_mixed <- afex::mixed(ignition_delay ~ total_dry_mass_g +
                                                        leaf_length_per_leaflet +
                                                        leaf_mass_per_area + 
                                                        leaf_moisture_content +
                                                        leaf_moisture_content:total_dry_mass_g +
                                                        (1 | analysis_group), data = model_data_withoutj,
                                                        method = "KR", REML = TRUE)


leaf_plus_canopy_traits_ignition_anova_table_model <- lme4::lmer(ignition_delay ~ total_dry_mass_g +
                                                                   leaf_length_per_leaflet +
                                                                   leaf_mass_per_area + 
                                                                   leaf_moisture_content +
                                                                   leaf_moisture_content:total_dry_mass_g +
                                                                   (1 | analysis_group), data = model_data_withoutj)


leaf_plus_canopy_ignition_anova <- car::Anova(leaf_plus_canopy_traits_ignition_anova_table_model, type = 2,
                                              test.statistic = "F")

leaf_plus_canopy_ignition_xtable <-  xtable::xtable(leaf_plus_canopy_ignition_anova, digits = 3)

leaf_plus_canopy_ignition_anova_coefficients <- summary(leaf_plus_canopy_traits_ignition_anova_table_model)$coefficients

leaf_plus_canopy_ignition_coeff <- xtable::xtable(leaf_plus_canopy_ignition_anova_coefficients, digits = 3)

