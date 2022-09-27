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
# Removing two samples whose windspeed data is missing
###################################################################################################################

model_data <- model_data %>%
  select(PC1, PC2, group, total_dry_mass_gm , canopy_density_gm_cm3 , leaf_stem_mass_ratio , canopy_moisture_content,
         leaf_mass_per_area , leaf_area_per_leaflet , leaf_length_per_leaflet , leaf_moisture_content, windspeed_miles_per_hour) %>%
  na.omit()

dim(model_data)

####################################################################################################################################
# Correlation of all morphological traits
#############################################################################################################################


morphological_traits_cor_data <- cor_data %>%
  select(total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)


morphological_traits_cor <- cor(morphological_traits_cor_data, method = "kendall",
                                use = "pairwise")

corrplot::corrplot(morphological_traits_cor, method = "number")

# morphological_traits_cor
# Kendall rank correlation coefficient between canopy_moisture_content and 
# leaf_moisture_content is 0.648013566, not a problem since they will used
# in two different model.
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.68502852



###################################################################################################
# Correlation of canopy traits and flammability traits
######################################################################################################

canopy_flam_data <- cor_data %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content)

canopy_flam_cor <- cor(canopy_flam_data, method = "kendall", 
                       use = "pairwise")


corrplot::corrplot(canopy_flam_cor, method = "number", type = "upper")


##################################################################################################################
# A global model of canopy traits with two way interactions for PC1
###################################################################################################################

options(na.action = "na.fail")


canopy_pc1_model <- afex::lmer(PC1 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
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
# object returned by dredge. The argument subset must be explicitely provided. 
# This is to assure that a potentially long list of models is not fitted unintentionally. 
# To evaluate all models, set subset to NA or TRUE. source: ?get.models

summary(best_canopy_pc1_model)

best_canopy_pc1_model <- afex::lmer(PC1 ~  total_dry_mass_gm + canopy_density_gm_cm3 + 
                                      leaf_stem_mass_ratio + 
                                      canopy_density_gm_cm3:total_dry_mass_gm + 
                                      (1|group), data = model_data, REML = FALSE)
summary(best_canopy_pc1_model)

# Correlation of fixed effect between total mass and the interaction
# between total mass and canopy density is 0.898, is it a problem?

###########################################################################################################
# Correlation of leaf  traits and flammability traits
###########################################################################################################

leaf_flam_data <- cor_data %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)

leaf_flam_cor <- cor(leaf_flam_data, method = "kendall",
                     use = "pairwise")


corrplot::corrplot(leaf_flam_cor, method = "number", type = "upper")


#############################################################################################################################
# A global model of leaf traits with two way interaction for PC1
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.68502852 and
# decided to drop leaf_area_per_leaflet from the model
# to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.
#############################################################################################################################

leaf_pc1_model <- afex::lmer(PC1 ~ leaf_mass_per_area +  leaf_length_per_leaflet +
                               leaf_moisture_content  + leaf_mass_per_area*leaf_length_per_leaflet +
                               leaf_mass_per_area*leaf_moisture_content + 
                               leaf_length_per_leaflet*leaf_moisture_content + 
                               (1|group), data = model_data, REML = FALSE)

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] # Null model

summary(best_leaf_pc1_model)

best_leaf_pc1_model <- afex::lmer(PC1 ~ 1 + (1|group),
                                  data = model_data, REML = FALSE) 

summary(best_leaf_pc1_model)


######################################################################################################
# Comparison between best canopy and leaf model for PC1
######################################################################################################


AIC(best_canopy_pc1_model, best_leaf_pc1_model) # AIC for canopy 344.0820 
# and leaf 463.2763

######################################################################################################
# Plotting of best canopy model
######################################################################################################


sjPlot::plot_model(best_canopy_pc1_model, # Will take this plot in results.R script later
                   show.values =TRUE,
                   show.p = TRUE, se = TRUE,
                   show.data = TRUE,
                   vline.color = "red",
                   intercept = TRUE,
                   sort.est = TRUE,
                   ci.lvl = 0.95,
                   auto.label = TRUE,
                   title = "Canopy traits effect on flammability (PC1 score)") 


#####################################################################################################################################
# A global model with canopy traits with two way interaction for PC2
####################################################################################################################################


canopy_pc2_model <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_gm*leaf_stem_mass_ratio + 
                                 total_dry_mass_gm*canopy_density_gm_cm3 +
                                 total_dry_mass_gm*canopy_moisture_content + 
                                 leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio*canopy_moisture_content + 
                                 canopy_density_gm_cm3*canopy_moisture_content + 
                                 (1|group), data = model_data, REML = FALSE)


canopy_pc2_models <- dredge(canopy_pc2_model)


best_canopy_pc2_model <- get.models(canopy_pc2_models, subset = TRUE)[[1]]

summary(best_canopy_pc2_model)

best_canopy_pc2_model <- afex::lmer(PC2 ~ leaf_stem_mass_ratio + total_dry_mass_gm + 
                                      leaf_stem_mass_ratio:total_dry_mass_gm +
                                      (1 | group), data = model_data, REML = FALSE)                                  

summary(best_canopy_pc2_model)


#####################################################################################################
# A global model of leaf traits with two way interaction for PC2
########################################################################################################


leaf_pc2_model <- afex::lmer(PC2 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content +  
                               leaf_mass_per_area*leaf_length_per_leaflet +
                               leaf_mass_per_area*leaf_moisture_content + 
                               leaf_length_per_leaflet*leaf_moisture_content + 
                               (1|group), data = model_data, REML = FALSE)



leaf_pc2_models <- dredge(leaf_pc2_model)

best_leaf_pc2_model <- get.models(leaf_pc2_models, subset = TRUE)[[1]]

summary(best_leaf_pc2_model)

best_leaf_pc2_model <- afex::lmer(PC2 ~ leaf_moisture_content + (1 | group),
                                  data = model_data, REML = FALSE)

###################################################################################################
# Comparison between best canopy and leaf model for PC2
###################################################################################################

AIC(best_canopy_pc2_model, best_leaf_pc2_model) # Canopy 358.2332, leaf 365.7563

###############################################################################################
# Does adding windspeed with the best model for pc2 improves the model?
###############################################################################################

windspeed_model <- afex::lmer( PC2 ~ leaf_stem_mass_ratio + total_dry_mass_gm + 
                                 leaf_stem_mass_ratio:total_dry_mass_gm +
                                 windspeed_miles_per_hour +
                                 (1|group), data = model_data, REML = FALSE)


AIC(best_canopy_pc2_model, windspeed_model) # Canopy 358.2332 and

# windspeed 355.0866, Yes, 
# Adding windspeed improves the model.


#####################################################################################################
# Plotting of best canopy model for PC2
#####################################################################################################

sjPlot::plot_model(best_canopy_pc2_model, # Will take this plot in results.R script later
                   show.values =TRUE,
                   show.p = TRUE, se = TRUE,
                   show.data = TRUE,
                   vline.color = "red",
                   intercept = TRUE,
                   sort.est = TRUE,
                   ci.lvl = 0.95,
                   auto.label = TRUE,
                   title ="Canopy traits effect on flammability (PC2 score)") 


#############################################################################################################################################
# Does canopy traits are more important than leaf traits
# if we drop the most flammable group, Juniperus from analysis?
#############################################################################################################################################


without_juniperus <- model_data %>% # creating a new data set without Juniperus group
  filter(group != "Juniperus")

##########################################################################################################
# A global model of canopy traits with two way interaction for PC1
#########################################################################################################

canopy_pc1_model_withoutj <- afex::lmer(PC1 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
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

summary(best_canopy_pc1_model_withoutj)

best_canopy_pc1_model_withoutj <- afex::lmer(PC1 ~ total_dry_mass_gm + 
                                               canopy_density_gm_cm3 +
                                               (1|group), data = without_juniperus, REML = FALSE)
summary(best_canopy_pc1_model_withoutj) 

######################################################################################################################
# A global model with leaf traits with two way interaction for PC1
######################################################################################################################

leaf_pc1_model_withoutj <- afex::lmer(PC1 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                            leaf_moisture_content  + 
                                            leaf_mass_per_area*leaf_length_per_leaflet +
                                            leaf_mass_per_area*leaf_moisture_content + 
                                            leaf_length_per_leaflet*leaf_moisture_content + 
                                            (1|group), data = without_juniperus, REML = FALSE)


leaf_pc1_models_withoutj <- dredge(leaf_pc1_model_withoutj)

best_leaf_pc1_model_withoutj <- get.models(leaf_pc1_models_withoutj, subset = TRUE)[[1]]

summary(best_leaf_pc1_model_withoutj)

best_leaf_pc1_model_withoutj <- afex::lmer(PC1 ~ leaf_length_per_leaflet +
                                             leaf_moisture_content +
                                             (1|group), data = without_juniperus, REML = FALSE)

#######################################################################################################
# Comparison of best canopy and leaf model for PC1
#######################################################################################################

AIC(best_canopy_pc1_model_withoutj, best_leaf_pc1_model_withoutj) # AIC for canopy 183.5310 and 
# leaf 248.3674


###########################################################################################################
# A global model for canopy traits with two way interaction for PC2
############################################################################################################

canopy_pc2_model_withoutj <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + 
                                          canopy_density_gm_cm3 + canopy_moisture_content +
                                          total_dry_mass_gm*leaf_stem_mass_ratio + 
                                          total_dry_mass_gm*canopy_density_gm_cm3 +
                                          total_dry_mass_gm*canopy_moisture_content + 
                                          leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                          leaf_stem_mass_ratio*canopy_moisture_content + 
                                          canopy_density_gm_cm3*canopy_moisture_content + 
                                          (1|group), data = without_juniperus, REML = FALSE)



canopy_pc2_models_withoutj <- dredge(canopy_pc2_model_withoutj)


best_canopy_pc2_model_withoutj <- get.models(canopy_pc2_models_withoutj, subset = TRUE)[[1]]

summary(best_canopy_pc2_model_withoutj) # Lots of stuff!!!!!


####################################################################################################
# A global model of leaf traits with two way interaction for PC2
####################################################################################################

leaf_pc2_model_withoutj <- afex::lmer(PC2 ~ leaf_mass_per_area +  
                                        leaf_length_per_leaflet +
                                        leaf_moisture_content + 
                                        leaf_mass_per_area*leaf_length_per_leaflet +
                                        leaf_mass_per_area*leaf_moisture_content + 
                                        leaf_length_per_leaflet*leaf_moisture_content + 
                                        (1|group), data = without_juniperus, REML = FALSE)


leaf_pc2_models_withoutj <- dredge(leaf_pc2_model_withoutj)

best_leaf_pc2_model_withoutj <- get.models(leaf_pc2_models_withoutj, subset = TRUE)[[1]] # Null model

summary(best_leaf_pc2_model_withoutj)

###########################################################################################################
# Comparison of best canopy and leaf model for PC2
###########################################################################################################


AIC(best_canopy_pc2_model_withoutj, best_leaf_pc2_model_withoutj) # Canopy 274.7805 and
#leaf 298.4111

# Yes, the hypothesis(canopy traits are more important than leaf traits) holds true even after
# removing the most flammable group, Juniperus from the analysis.

#########################################################################################################