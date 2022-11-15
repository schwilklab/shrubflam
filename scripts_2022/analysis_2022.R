#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud

# This script is for selecting the best model for each kind of
# traits and compare them to see which traits are better.


library(MuMIn)

# MuMIn package for automated model selection through subsetting
# the maximum model, with optimal constraints for model inclusion.
# Model parameter and prediction averaging based on model weights 
# derived from information criteria (AIC).
# source: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf



# All scripts that are in source() required to run before running 
# analysis_2022.R script

source("./flam_pca_2022.R") # The Rscript where I performed the principle component analysis.

# REML IS EQUAL TO FALSE BECAUSE 
# Faraway (2006) Extending the linear model with R (p. 156):
# The reason is that REML estimates the random effects by considering linear combinations of the data that remove the fixed effects. 
# If these fixed effects are changed, the likelihoods of the two models will not be directly comparable # Source .......
# Source : https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but




####################################################################################################################################
# scaling the response variables since they measured in different
# units
####################################################################################################################################

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)


model_data <- final_data %>%
  mutate_at(c("total_dry_mass_gm", "canopy_density_gm_cm3", "leaf_stem_mass_ratio", "canopy_moisture_content",
              "leaf_mass_per_area", "leaf_area_per_leaflet", "leaf_length_per_leaflet","leaf_moisture_content", "windspeed_miles_per_hour"), list(zscore))


################################################################################################################
# Removing two samples whose wind speed data is missing.
###################################################################################################################

model_data <- model_data %>%
  select(PC1, PC2, degsec_100, flame_height, temp_d1_pre, species_id, temp_d2_pre, self_ignition,
         group, total_dry_mass_gm , canopy_density_gm_cm3 , leaf_stem_mass_ratio , canopy_moisture_content,
         leaf_mass_per_area , leaf_area_per_leaflet , leaf_length_per_leaflet , leaf_moisture_content) %>%
  na.omit()

dim(model_data)


##################################################################################################################
# A global model of canopy traits with two way interactions for cumulative temperature
# over 100 degree Celsius
###################################################################################################################


options(na.action = "na.fail")


canopy_pc1_model <- afex::lmer(degsec_100 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_gm*leaf_stem_mass_ratio +
                                 total_dry_mass_gm*canopy_density_gm_cm3 +
                                 total_dry_mass_gm*canopy_moisture_content + 
                                 leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio*canopy_moisture_content + 
                                 canopy_density_gm_cm3*canopy_moisture_content + 
                                 (1|group), data = model_data, REML = FALSE)


canopy_pc1_models <- dredge(canopy_pc1_model) # Performs an automated

# model selection with subsets of the supplied global model. source: ?dredge


best_canopy_pc1_model <- get.models(canopy_pc1_models, subset = TRUE)[[1]] # returns list and 
# indexing the first one, top model.
# Generate or extract a list of fitted model objects from a "model.selection" table,
# object returned by dredge. The argument subset must be explicit ly provided. 
# This is to assure that a potentially long list of models is not fitted unintentionally. 
# To evaluate all models, set subset to NA or TRUE. source: ?get.models

summary(best_canopy_pc1_model)

# total mass, leaf stem mass ratio  and interaction between them
# is the best model.


#############################################################################################################################
#############################################################################################################################
# A global model of leaf traits with two way interaction for heat release
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.67 and
# decided to drop leaf_area_per_leaflet from the model
# to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.



leaf_pc1_model <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  + 
                               leaf_mass_per_area*leaf_length_per_leaflet +
                               leaf_mass_per_area*leaf_moisture_content + 
                               leaf_length_per_leaflet*leaf_moisture_content + 
                               (1|group), data = model_data, REML = FALSE)

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 

summary(best_leaf_pc1_model) # Leaf mass per area is the best model 


######################################################################################################
# Comparison between best canopy and leaf model for PC1
######################################################################################################


AIC(best_canopy_pc1_model, best_leaf_pc1_model) # AIC for canopy 2649.693
# and leaf 2756.840


####################################################################################
# Combinations of best canopy model and best leaf model
####################################################################################

leaf_canopy_model <- afex::lmer(degsec_100 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
                                  leaf_mass_per_area + 
                                  total_dry_mass_gm*leaf_stem_mass_ratio +
                                  total_dry_mass_gm*leaf_mass_per_area +
                                  leaf_stem_mass_ratio*leaf_mass_per_area + 
                                  (1|group), data = model_data, REML = FALSE)


leaf_canopy_models <- dredge(leaf_canopy_model)

best_leaf_canopy_model <- get.models(leaf_canopy_models, subset = TRUE)[[1]]

summary(best_leaf_canopy_model)  # Leaf_mass_per_area,
# total_dry_mass_gm, the interaction between leaf_mass_per_area and total_dry_mass
 
AIC(best_canopy_pc1_model, best_leaf_canopy_model) # best leaf_canopy_model 2649.693
# and best_canopy_model 2647.234


#############################################################################################################################################
# Does canopy traits are more important than leaf traits
# if we drop the most flammable group, Juniperus from analysis?
#############################################################################################################################################


without_juniperus <- model_data %>% # creating a new data set without Juniperus group
  filter(group != "Juniperus")

########################################################################################################
# A global model of canopy traits with two way interaction for heat release
#########################################################################################################

canopy_pc1_model_withoutj <- afex::lmer(degsec_100 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
                                          canopy_density_gm_cm3 + canopy_moisture_content +
                                          total_dry_mass_gm*leaf_stem_mass_ratio + 
                                          total_dry_mass_gm*canopy_density_gm_cm3 +
                                          total_dry_mass_gm*canopy_moisture_content + 
                                          leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                          leaf_stem_mass_ratio*canopy_moisture_content +
                                          canopy_density_gm_cm3*canopy_moisture_content +
                                          (1|group), data = without_juniperus, REML = FALSE )



canopy_pc1_models_withoutj <- dredge(canopy_pc1_model_withoutj)


best_canopy_pc1_model_withoutj <- get.models(canopy_pc1_models_withoutj, subset = TRUE)[[1]]

summary(best_canopy_pc1_model_withoutj) # Total_mass and canopy density without interaction


######################################################################################################################
# A global model with leaf traits with two way interaction for heat release
######################################################################################################################


leaf_pc1_model_withoutj <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                            leaf_moisture_content   +
                                            leaf_mass_per_area*leaf_length_per_leaflet +
                                            leaf_mass_per_area*leaf_moisture_content + 
                                            leaf_length_per_leaflet*leaf_moisture_content + 
                                            (1|group), data = without_juniperus, REML = FALSE)


leaf_pc1_models_withoutj <- dredge(leaf_pc1_model_withoutj)

best_leaf_pc1_model_withoutj <- get.models(leaf_pc1_models_withoutj, subset = TRUE)[[1]]

summary(best_leaf_pc1_model_withoutj)  # Null model

#######################################################################################################
# Comparison of best canopy and leaf model for heat release
#######################################################################################################

AIC(best_canopy_pc1_model_withoutj, best_leaf_pc1_model_withoutj) # Canopy 1969.167
# and leaf 1999.703


# Yes, the hypothesis(canopy traits are more important than leaf traits) holds true when we
# remove the most flammable group, Juniperus from the analysis.

