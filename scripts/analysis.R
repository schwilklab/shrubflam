# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# Febraury 2023

# This script depends on scripts listed in run-all.R

## DWS: This script is NUTS! it is hundreds of line sof code. We agreed on a
## pretty straightforward analysis method and you are fishing.

library(MuMIn)

# MuMIn package for automated model selection through subsetting
# the maximum model, with optimal constraints for model inclusion.
# Model parameter and prediction averaging based on model weights 
# derived from information criteria (AIC).
# source: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf


# Principle component analysis

# REML IS EQUAL TO FALSE BECAUSE Faraway (2006) Extending the linear model with
# R (p. 156): The reason is that REML estimates the random effects by
# considering linear combinations of the data that remove the fixed effects. If
# these fixed effects are changed, the likelihoods of the two models will not
# be directly comparable
# https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but



##################################################################################
# scaling the response variables since they measured in different
# units
##################################################################################

## DWS: But this makes interpretation difficult. Do you need to do this?

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)

model_data <- final_data %>%
  mutate_at(c("total_dry_mass_g", "canopy_density_gm_cm3", "leaf_stem_mass_ratio",
              "canopy_moisture_content","leaf_mass_per_area", 
              "leaf_area_per_leaflet", "leaf_length_per_leaflet",
              "leaf_moisture_content"), list(zscore))

names(model_data)
####################################################################################
# Making sure that variables with missing value is out from the analysis
###################################################################################

model_data <- model_data %>%
  select(degsec_100, species_id, ignition_delay, genus, specific_epithet, display_name, analysis_group, total_dry_mass_g , 
         canopy_density_gm_cm3 , leaf_stem_mass_ratio , 
         canopy_moisture_content, leaf_mass_per_area , 
         leaf_area_per_leaflet , leaf_length_per_leaflet , 
         leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data)

model_data$degsec_100 <- log(model_data$degsec_100) # log transformation of response variable

## DWS: So you are making z score and then taking log. How will you graph this?

#################################################################################
# A global model of canopy traits with two way interactions for 
# temperature integration.
##################################################################################


options(na.action = "na.fail")
canopy_pc1_model <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_g:leaf_stem_mass_ratio +
                                 total_dry_mass_g:canopy_density_gm_cm3 +
                                 total_dry_mass_g:canopy_moisture_content + 
                                 leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio:canopy_moisture_content + 
                                 canopy_density_gm_cm3:canopy_moisture_content + 
                                 (1 | analysis_group), data = model_data, REML = FALSE)


## DWS: What is "group"? Looks like it is genus. Why is this not called
## "genus"? Why are you ruling out random slopes?

canopy_pc1_models <- dredge(canopy_pc1_model) # Performs an automated

# model selection with subsets of the supplied global model. source: ?dredge


best_canopy_pc1_model <- get.models(canopy_pc1_models, subset = TRUE)[[1]] # returns list and 
# indexing the first one, top model.

## DWS: But we need the model table to compare. Is the best model good? are
## models close in AICc? I looked at model table and your slection cannot
## select among top models, they are equivalent.

canopy_mod_table <- model.sel(canopy_pc1_models)
canopy_mod_table[1:8,]

## DWS: So top four models pretty close.

# Generate or extract a list of fitted model objects from a "model.selection"
# table, object returned by dredge. The argument subset must be explicitely
# provided. This is to assure that a potentially long list of models is not
# fitted unintentionally. To evaluate all models, set subset to NA or TRUE.
# source: ?get.models

summary(best_canopy_pc1_model)

# total mass and canopy density without interaction
# is the best model.

#sjPlot::tab_model(best_canopy_pc1_model)






##################################################################################
##################################################################################
# A global model of leaf traits with two way interaction for heat release
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.67 and
# decided to drop leaf_area_per_leaflet from the model
# to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.



leaf_pc1_model <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  + 
                               leaf_mass_per_area:leaf_length_per_leaflet +
                               leaf_mass_per_area:leaf_moisture_content + 
                               leaf_length_per_leaflet:leaf_moisture_content + 
                               (1| analysis_group), data = model_data, REML = FALSE)

## DWS: Your use of "*" above does not make sense to me. It looks like you are
## already specifying the main effects?

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 

leaf_mod_table <- model.sel(leaf_pc1_models)
leaf_mod_table[1:8,]


best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 


summary(best_leaf_pc1_model) # Leaf mass per area is the best model 

## DWS: BUt not by much!

#sjPlot::tab_model(best_leaf_pc1_model)

#################################################################################
# Comparison between best canopy and leaf model for temperature integration
#################################################################################


AIC(best_canopy_pc1_model, best_leaf_pc1_model) # AIC for canopy 1.817
# and leaf 74.915


#################################################################################
# Combinations of best canopy model and best leaf model
#################################################################################

leaf_canopy_model <- afex::lmer(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 + 
                                  leaf_mass_per_area + 
                                  total_dry_mass_g:canopy_density_gm_cm3 +
                                  total_dry_mass_g:leaf_mass_per_area +
                                  canopy_density_gm_cm3:leaf_mass_per_area + 
                                  (1|analysis_group), data = model_data, REML = FALSE)


leaf_canopy_models <- dredge(leaf_canopy_model)
best_leaf_canopy_model <- get.models(leaf_canopy_models, subset = TRUE)[[1]]
summary(best_leaf_canopy_model)  # total_dry_mass and canopy density 
# without interaction

AIC(best_canopy_pc1_model, best_leaf_pc1_model, best_leaf_canopy_model) # canopy 
# 1.817, leaf 74.915, combination 1.817


## DWS: I don't really understand this.

##################################################################################
# Does canopy traits are more important than leaf traits
# if we drop the most flammable genus, Juniperus from analysis?
##################################################################################


without_juniperus <- model_data %>% # creating a new data set without Juniperus group
  filter(genus != "Juniperus spp")

##################################################################################
# A global model of canopy traits with two way interaction for 
# temperature integration
##################################################################################

canopy_pc1_model_withoutj <- afex::lmer(degsec_100 ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                          canopy_density_gm_cm3 + canopy_moisture_content +
                                          total_dry_mass_g:leaf_stem_mass_ratio + 
                                          total_dry_mass_g:canopy_density_gm_cm3 +
                                          total_dry_mass_g:canopy_moisture_content + 
                                          leaf_stem_mass_ratio:canopy_density_gm_cm3 +
                                          leaf_stem_mass_ratio:canopy_moisture_content +
                                          canopy_density_gm_cm3:canopy_moisture_content +
                                          (1|analysis_group), data = without_juniperus, REML = FALSE )

canopy_pc1_models_withoutj <- dredge(canopy_pc1_model_withoutj)
best_canopy_pc1_model_withoutj <- get.models(canopy_pc1_models_withoutj, subset = TRUE)[[1]]
summary(best_canopy_pc1_model_withoutj) # Total_mass and canopy density without interaction

#sjPlot::tab_model(best_canopy_pc1_model_withoutj)

##################################################################################
# A global model with leaf traits with two way interaction for heat release
##################################################################################


leaf_pc1_model_withoutj <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                                            leaf_moisture_content   +
                                            leaf_mass_per_area:leaf_length_per_leaflet +
                                            leaf_mass_per_area:leaf_moisture_content + 
                                            leaf_length_per_leaflet:leaf_moisture_content + 
                                            (1|analysis_group), data = without_juniperus, REML = FALSE)


leaf_pc1_models_withoutj <- dredge(leaf_pc1_model_withoutj)
best_leaf_pc1_model_withoutj <- get.models(leaf_pc1_models_withoutj, subset = TRUE)[[1]]
summary(best_leaf_pc1_model_withoutj)  # leaf_length_per_leaflet

#sjPlot::tab_model(best_leaf_pc1_model_withoutj)

#######################################################################################################
# Comparison of best canopy and leaf model for heat release
#######################################################################################################

AIC(best_canopy_pc1_model_withoutj, best_leaf_pc1_model_withoutj) # Canopy -25.10
# and leaf -4.57

leaf_canopy_model_withoutj <- afex::lmer(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 +
                                           leaf_length_per_leaflet + 
                                           total_dry_mass_g*canopy_density_gm_cm3 +
                                           canopy_density_gm_cm3*leaf_length_per_leaflet +
                                           total_dry_mass_g*leaf_length_per_leaflet +
                                           (1 | analysis_group), data = without_juniperus, REML = FALSE)


leaf_canopy_model_withoutj_models <- dredge(leaf_canopy_model_withoutj)
best_leaf_canopy_model_withoutj <- get.models(leaf_canopy_model_withoutj_models, 
                                              subset = TRUE)[[1]]
summary(best_leaf_canopy_model_withoutj) # same as best canopy



##################################################################################

#################################################################
# Ignition delay vs canopy traits
#################################################################


canopy_ignition_model <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 (1 | analysis_group), data = model_data, REML = FALSE)


canopy_ignition_models <- dredge(canopy_ignition_model) # Performs an automated

# model selection with subsets of the supplied global model. source: ?dredge


best_canopy_ignition_model <- get.models(canopy_ignition_models, subset = TRUE)[[1]] # returns list and 
# indexing the first one, top model.

## DWS: But we need the model table to compare. Is the best model good? are
## models close in AICc? I looked at model table and your slection cannot
## select among top models, they are equivalent.

canopy_ignition_mod_table <- model.sel(canopy_ignition_models)
canopy_ignition_mod_table[1:8,]
summary(best_canopy_ignition_model)
#sjPlot::tab_model(best_canopy_ignition_model)

#########################################################################################
# Ignition delay vs leaf traits
###########################################################################################

leaf_traits_ignition_model <- afex::lmer(ignition_delay ~ leaf_mass_per_area +
                                           leaf_length_per_leaflet + leaf_moisture_content +
                                           (1 | analysis_group), data = model_data, REML = FALSE)

leaf_ignition_models <- dredge(leaf_traits_ignition_model) 


best_leaf_ignition_model <- get.models(leaf_ignition_models, subset = TRUE)[[1]] 



leaf_ignition_mod_table <- model.sel(leaf_ignition_models)
leaf_ignition_mod_table[1:8,]

summary(best_leaf_ignition_model)

#sjPlot::tab_model(best_leaf_ignition_model)

AIC(best_canopy_ignition_model, best_leaf_ignition_model) # canopy 579.15
# leaf 584.87

#########################################################################################
# Combinations
########################################################################################

leaf_canopy_ignition_model <- afex::lmer(ignition_delay ~ canopy_density_gm_cm3 +
                                           leaf_moisture_content +
                                           leaf_mass_per_area + (1 | analysis_group),
                                         data = model_data, REML = FALSE)

leaf_canopy_ignition_models <- dredge(leaf_canopy_ignition_model)
best_leaf_canopy_ignition_model <- get.models(leaf_canopy_ignition_models, subset = TRUE)[[1]]
#sjPlot::tab_model(best_leaf_canopy_ignition_model)
summary(best_leaf_canopy_ignition_model)

AIC(best_canopy_ignition_model, best_leaf_ignition_model, best_leaf_canopy_ignition_model)

##########################################################################################
# Without Juniperus
#########################################################################################

#################################################################
# Ignition delay vs canopy traits
#################################################################


canopy_ignition_model_withoutj <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                               canopy_density_gm_cm3 + canopy_moisture_content +
                                               (1 | analysis_group), data = without_juniperus, REML = FALSE)



canopy_ignition_models_withoutj <- dredge(canopy_ignition_model_withoutj) 
best_canopy_ignition_model_withoutj <- get.models(canopy_ignition_models_withoutj, subset = TRUE)[[1]] 
summary(best_canopy_ignition_model_withoutj) # Canopy moisture and total dry mass
#sjPlot::tab_model(best_canopy_ignition_model_withoutj)


#######################################################################################################
# Leaf traits
######################################################################################################

leaf_traits_ignition_model_withoutj <- afex::lmer(ignition_delay ~ leaf_mass_per_area +
                                                    leaf_length_per_leaflet + leaf_moisture_content +
                                                    (1 | analysis_group), data = without_juniperus, REML = FALSE)
leaf_ignition_models_withoutj <- dredge(leaf_traits_ignition_model_withoutj) 
best_leaf_ignition_model_withoutj <- get.models(leaf_ignition_models_withoutj, subset = TRUE)[[1]] 

summary(best_leaf_ignition_model_withoutj) # leaf length per leaflet
# and leaf moisture content
#sjPlot::tab_model(best_leaf_ignition_model_withoutj)
AIC(best_canopy_ignition_model_withoutj, best_leaf_ignition_model_withoutj) # canopy 364.07
# leaf 370.25


## DWS: those are not numbers I get.,

######################################################################################################
#  Combinations
#####################################################################################################

leaf_canopy_ignition_model_withoutj <- afex::lmer(ignition_delay ~ total_dry_mass_g +
                                                    leaf_length_per_leaflet + leaf_moisture_content +
                                                    (1 | analysis_group), data = without_juniperus, REML = FALSE)
leaf_canopy_ignition_models_withoutj <- dredge(leaf_canopy_ignition_model_withoutj)
best_leaf_canopy_ignition_model_withoutj <- get.models(leaf_canopy_ignition_models_withoutj, subset = TRUE)[[1]]
summary(best_leaf_canopy_ignition_model_withoutj)
#sjPlot::tab_model(best_leaf_canopy_ignition_model_withoutj)
AIC(best_leaf_canopy_ignition_model_withoutj, best_canopy_ignition_model_withoutj, best_leaf_ignition_model_withoutj)

