#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud


library(MuMIn)



# REML IS EQUAL TO FALSE BECAUSE 
# Faraway (2006) Extending the linear model with R (p. 156):
# The reason is that REML estimates the random effects by considering linear combinations of the data that remove the fixed effects. 
# If these fixed effects are changed, the likelihoods of the two models will not be directly comparable # Source .......
# Source : https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but
# I have borrowed that book(Faraway (2006))\ from library and I need to check it out completely.

####################################################################################################################################
# scaling the response variables since they measured in different
# units
####################################################################################################################################

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)


model_data <- final_data %>%
  mutate_at(c("total_dry_mass_gm", "canopy_density_gm_cm3", "leaf_stem_mass_ratio", "canopy_moisture_content",
              "leaf_mass_per_area", "leaf_area_per_leaflet", "leaf_length_per_leaflet","leaf_moisture_content", "windspeed_miles_per_hour"), list(zscore))


model_data <- model_data %>%
  select(PC1, PC2, group, total_dry_mass_gm , canopy_density_gm_cm3 , leaf_stem_mass_ratio , canopy_moisture_content,
         leaf_mass_per_area , leaf_area_per_leaflet , leaf_length_per_leaflet , leaf_moisture_content, windspeed_miles_per_hour) %>%
  na.omit()

dim(model_data)

####################################################################################################################################

##################################################################################################################
# A model of canopy traits with two way interactions for PC1
###################################################################################################################

options(na.action = "na.fail")


canopy_pc1_model <- afex::lmer(PC1 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_gm*leaf_stem_mass_ratio + total_dry_mass_gm*canopy_density_gm_cm3 +
                                 total_dry_mass_gm*canopy_moisture_content + leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio*canopy_moisture_content + canopy_density_gm_cm3*canopy_moisture_content + (1|group), 
                               data = model_data, REML = FALSE)

summary(canopy_pc1_model) 

canopy_pc1_models <- dredge(canopy_pc1_model)


best_canopy_pc1_model <- get.models(canopy_pc1_models, subset = TRUE)[[1]]

summary(best_canopy_pc1_model)



#############################################################################################################################
# A model of leaf traits with two way interaction for PC1
#############################################################################################################################

leaf_pc1_model <- afex::lmer(PC1 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                               leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet + leaf_mass_per_area*leaf_length_per_leaflet +
                               leaf_mass_per_area*leaf_moisture_content + leaf_area_per_leaflet*leaf_length_per_leaflet + leaf_area_per_leaflet*leaf_moisture_content +
                               leaf_length_per_leaflet*leaf_moisture_content + (1|group), data = model_data,
                             REML = FALSE)
summary(leaf_pc1_model)

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]]

best_leaf_pc1_model

AIC(best_canopy_pc1_model, best_leaf_pc1_model) # AIC for canopy 344.0820 and leaf 463.2763

sjPlot::plot_model(best_canopy_pc1_model,
                   show.values =TRUE,
                   show.p = TRUE, se = TRUE,
                   show.data = TRUE,
                   vline.color = "red",
                   intercept = TRUE,
                   sort.est = TRUE,
                   ci.lvl = 0.95,
                   auto.label = TRUE,
                   title ="Canopy traits effect on flammability (PC1 score)") 

#####################################################################################################################################
# A model with canopy traits with interaction for PC2
####################################################################################################################################


canopy_pc2_model <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                                 total_dry_mass_gm*leaf_stem_mass_ratio + total_dry_mass_gm*canopy_density_gm_cm3 +
                                 total_dry_mass_gm*canopy_moisture_content + leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                 leaf_stem_mass_ratio*canopy_moisture_content + canopy_density_gm_cm3*canopy_moisture_content + (1|group),
                               data = model_data, REML = FALSE)

summary(canopy_pc2_model) 

canopy_pc2_models <- dredge(canopy_pc2_model)


best_canopy_pc2_model <- get.models(canopy_pc2_models, subset = TRUE)[[1]]

summary(best_canopy_pc2_model)


#####################################################################################################
# A model of leaf traits with interaction for PC2
########################################################################################################


leaf_pc2_model <- afex::lmer(PC2 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                               leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet + leaf_mass_per_area*leaf_length_per_leaflet +
                               leaf_mass_per_area*leaf_moisture_content + leaf_area_per_leaflet*leaf_length_per_leaflet + leaf_area_per_leaflet*leaf_moisture_content +
                               leaf_length_per_leaflet*leaf_moisture_content + (1|group), 
                             data = model_data, REML = FALSE)


# boundary (singular) fit: see help('isSingular')??????????

# How to avoid isSingular, source: https://rdrr.io/cran/lme4/man/isSingular.html

# There is not yet consensus about how to deal with singularity, or more generally to choose which \
#random-effects specification (from a range of choices of varying complexity) to use. Some proposals include:

# avoid fitting overly complex models in the first place, i.e. design experiments/restrict models a priori such that the 
# variance-covariance matrices can be estimated precisely enough to avoid singularity (Matuschek et al 2017)

# use some form of model selection to choose a model that balances predictive accuracy and overfitting/type I error 
# (Bates et al 2015, Matuschek et al 2017)

# “keep it maximal”, i.e. fit the most complex model consistent with the experimental design, removing only terms required to 
# allow a non-singular fit (Barr et al. 2013), or removing further terms based on p-values or AIC

# use a partially Bayesian method that produces maximum a posteriori (MAP) estimates using regularizing priors to force the 
# estimated random-effects variance-covariance matrices away from singularity (Chung et al 2013, blme package)

# use a fully Bayesian method that both regularizes the model via informative priors and gives estimates and credible intervals 
# for all parameters that average over the uncertainty in the random effects parameters (Gelman and Hill 2006, McElreath 2015; MCMCglmm, rstanarm and brms packages)


# Should I go for a simple model ?



summary(leaf_pc2_model)

leaf_pc2_models <- dredge(leaf_pc2_model)

best_leaf_pc2_model <- get.models(leaf_pc2_models, subset = TRUE)[[1]]

best_leaf_pc2_model


AIC(best_canopy_pc2_model, best_leaf_pc2_model) # Canopy 358.2332, leaf 365.7563

###############################################################################################
# Does adding windspeed with the best model for pc2 improves the model?
###############################################################################################

windspeed_model <- afex::lmer( PC2 ~ total_dry_mass_gm + windspeed_miles_per_hour +
                                 (1|group), data = model_data, REML = FALSE)


AIC(best_canopy_pc2_model, windspeed_model) # Canopy 358.2332  , windspeed 355.0866, Yes, 
# Adding windspeed improves the model


sjPlot::plot_model(best_canopy_pc2_model,
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
# Without most flammable group
#############################################################################################################################################


without_juniperus <- model_data %>%
  filter(group != "Juniperus")

canopy_pc1_model_withoutj <- afex::lmer(PC1 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                                          total_dry_mass_gm*leaf_stem_mass_ratio + total_dry_mass_gm*canopy_density_gm_cm3 +
                                          total_dry_mass_gm*canopy_moisture_content + leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                          leaf_stem_mass_ratio*canopy_moisture_content + canopy_density_gm_cm3*canopy_moisture_content + (1|group), 
                                        data = without_juniperus, REML = FALSE )

summary(canopy_pc1_model_withoutj) 

canopy_pc1_models_withoutj <- dredge(canopy_pc1_model_withoutj)


best_canopy_pc1_model_withoutj <- get.models(canopy_pc1_models_withoutj, subset = TRUE)[[1]]

summary(best_canopy_pc1_model_withoutj)


leaf_pc1_model_withoutj <- afex::lmer(PC1 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                                        leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet + leaf_mass_per_area*leaf_length_per_leaflet +
                                        leaf_mass_per_area*leaf_moisture_content + leaf_area_per_leaflet*leaf_length_per_leaflet + leaf_area_per_leaflet*leaf_moisture_content +
                                        leaf_length_per_leaflet*leaf_moisture_content + (1|group),
                                      data = without_juniperus, REML = FALSE)
summary(leaf_pc1_model_withoutj)

leaf_pc1_models_withoutj <- dredge(leaf_pc1_model_withoutj)

best_leaf_pc1_model_withoutj <- get.models(leaf_pc1_models_withoutj, subset = TRUE)[[1]]

best_leaf_pc1_model_withoutj

summary(best_leaf_pc1_model_withoutj)

AIC(best_canopy_pc1_model_withoutj, best_leaf_pc1_model_withoutj) # AIC for canopy 183.5310 and leaf 237.3045



canopy_pc2_model_withoutj <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                                          total_dry_mass_gm*leaf_stem_mass_ratio + total_dry_mass_gm*canopy_density_gm_cm3 +
                                          total_dry_mass_gm*canopy_moisture_content + leaf_stem_mass_ratio*canopy_density_gm_cm3 +
                                          leaf_stem_mass_ratio*canopy_moisture_content + canopy_density_gm_cm3*canopy_moisture_content + (1|group), 
                                        data = without_juniperus, REML = FALSE)

summary(canopy_pc2_model_withoutj) 

canopy_pc2_models_withoutj <- dredge(canopy_pc2_model_withoutj)


best_canopy_pc2_model_withoutj <- get.models(canopy_pc2_models_withoutj, subset = TRUE)[[1]]

summary(best_canopy_pc2_model_withoutj)



leaf_pc2_model_withoutj <- afex::lmer(PC2 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                                        leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet + leaf_mass_per_area*leaf_length_per_leaflet +
                                        leaf_mass_per_area*leaf_moisture_content + leaf_area_per_leaflet*leaf_length_per_leaflet + leaf_area_per_leaflet*leaf_moisture_content +
                                        leaf_length_per_leaflet*leaf_moisture_content + (1|group), data = without_juniperus, REML = FALSE)

summary(leaf_pc2_model_withoutj)

leaf_pc2_models_withoutj <- dredge(leaf_pc2_model_withoutj)

best_leaf_pc2_model_withoutj <- get.models(leaf_pc2_models_withoutj, subset = TRUE)[[1]]

summary(best_leaf_pc2_model_withoutj)


AIC(best_canopy_pc2_model_withoutj, best_leaf_pc2_model_withoutj) # Canopy 274.7805, leaf 298.4111