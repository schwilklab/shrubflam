#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud


library(MuMIn)
library(partR2) # partR2, an R package that quantifies part R2 for fixed effect predictors based on (generalized) linear mixed-effect model 
# fits.source: partR2: partitioning R2 in generalized linear mixed models

# All scripts that are in source required to run before running 
# flam_pca_2022.R script

source("./flam_pca_2022") # The script where I performed the principle component analysis.

# REML IS EQUAL TO FALSE BECAUSE 
# Faraway (2006) Extending the linear model with R (p. 156):
# The reason is that REML estimates the random effects by considering linear combinations of the data that remove the fixed effects. 
# If these fixed effects are changed, the likelihoods of the two models will not be directly comparable # Source .......
# Source : https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but.

####################################################################################################################################
# scaling the response variables since they measured in different
# units
####################################################################################################################################

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)


model_data <- final_data %>%
  mutate_at(c("total_dry_mass_gm", "canopy_density_gm_cm3", "leaf_stem_mass_ratio", "canopy_moisture_content",
              "leaf_mass_per_area", "leaf_area_per_leaflet", "leaf_length_per_leaflet","leaf_moisture_content", "windspeed_miles_per_hour"), list(zscore))



##################################################################################################################################################

#################################################################################################################################################
# Removing na of two samples whose windspeed data are missing
#################################################################################################################################################

options(na.action = "na.fail")

model_data <- model_data %>%
  select(PC1, PC2, group, total_dry_mass_gm , canopy_density_gm_cm3 , leaf_stem_mass_ratio , canopy_moisture_content,
         leaf_mass_per_area , leaf_area_per_leaflet , leaf_length_per_leaflet , leaf_moisture_content, windspeed_miles_per_hour) %>%
  na.omit()

dim(model_data)

####################################################################################################################################
# A single model with both leaf and canopy traits without interaction
###################################################################################################################################


model_1_pc1 <- afex::lmer(PC1 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                        leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet + 
                        leaf_moisture_content + (1|group), data = model_data, REML = FALSE)

summary(model_1_pc1) 

# total_dry_mass_gm p value < 2e-16 ***
# leaf_stem_mass_ratio p value 0.00618 * 
# canopy_density_gm_cm3 p value 0.00153 ** 

plot(model_1_pc1) # Doesn't look like normal

pc1_models <- dredge(model_1_pc1)

best_pc1_model <- get.models(pc1_models, subset = TRUE)[[1]] # total_dry_mass_gm, canopy_density_gm_cm3 and leaf_stem_mass_ratio

summary(best_pc1_model)

plot(best_pc1_model) # Doesn't look like normal

#########################################################################################################################################
# Getting the inclusive R square value for those variables which are significant.
#########################################################################################################################################

inclusive_r2_pc1 <- partR2(model_1_pc1, partvars = c("total_dry_mass_gm", "leaf_stem_mass_ratio", "canopy_density_gm_cm3"), nboot=100)

summary(inclusive_r2_pc1)

#########################################################################################################################################
# A single model with both leaf and canopy traits without interaction for PC2
#########################################################################################################################################

model_2_pc2 <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + canopy_density_gm_cm3 + canopy_moisture_content +
                        leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                        leaf_moisture_content + (1|group), data = model_data, REML = FALSE)

summary(model_2_pc2) 


# total_dry_mass_gm p value 0.0016 **
# leaf_stem_mass_ratio p value 0.0341 . and slope -0.28625

plot(model_2_pc2)

pc2_models <- dredge(model_2_pc2)

best_pc2_model <- get.models(pc2_models, subset = TRUE)[[1]] # total_dry_mass_gm and leaf_stem_mass_ratio


summary(best_pc2_model)

best_pc2_model <- afex::lmer(PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio +
                               (1|group), data = model_data, REML = FALSE)

plot(best_pc2_model)

#########################################################################################################################################
# Getting the inclusive R square value for those variables which are significant.
#########################################################################################################################################

inclusive_r2_pc2 <- partR2(model_2_pc2, partvars = c("total_dry_mass_gm", "leaf_stem_mass_ratio"), nboot = 100)

summary(inclusive_r2_pc2)


#############################################################################################################################
# Does adding windspeed in the best model for PC2 improve the model? Since PC2 is influenced by wind speed.
############################################################################################################################


windspeed_model <- afex::lmer( PC2 ~ total_dry_mass_gm + leaf_stem_mass_ratio + windspeed_miles_per_hour +
                                 (1|group), data = model_data, REML = FALSE)


AIC(best_pc2_model, windspeed_model) # best_pc2_model 358.5528  , windspeed_model  352.8284, yes , adding windspeed improves the model.


############################################################################################################################################
# Plotting the models
###########################################################################################################################################

sjPlot::plot_model(model_1_pc1,
                   show.values =TRUE,
                   show.p = TRUE, se = TRUE,
                   show.data = TRUE,
                   vline.color = "red",
                   intercept = TRUE,
                   sort.est = TRUE,
                   ci.lvl = 0.95,
                   auto.label = TRUE,
                   title ="Shrub traits effect on flammability (PC1 score)")

sjPlot::plot_model(model_2_pc2,
                   show.values =TRUE,
                   show.p = TRUE, se = TRUE,
                   show.data = TRUE,
                   vline.color = "red",
                   intercept = TRUE,
                   sort.est = TRUE,
                   ci.lvl = 0.95,
                   auto.label = TRUE,
                   title ="Shrub traits effect on flammability (PC2 score)") 

##############################################################################################################################################

##############################################################################################################################################
# Model of separate canopy traits and leaf traits with interactions for PC1
#############################################################################################################################################

canopy_traits_pc1 <- afex::lmer(PC1 ~ total_dry_mass_gm + canopy_density_gm_cm3 +
                                  leaf_stem_mass_ratio + canopy_moisture_content +
                                  total_dry_mass_gm*canopy_density_gm_cm3*leaf_stem_mass_ratio*canopy_moisture_content +
                                  (1|group), data = model_data, REML = FALSE)



canopy_traits_pc1_models <- dredge(canopy_traits_pc1)

best_canopy_traits_pc1 <- get.models(canopy_traits_pc1_models, subset = TRUE)[[1]]

summary(best_canopy_traits_pc1)


best_canopy_traits_pc1 <- afex::lmer(PC1 ~ total_dry_mass_gm + canopy_density_gm_cm3 + leaf_stem_mass_ratio + total_dry_mass_gm*canopy_density_gm_cm3 +
                                       (1|group), data = model_data, REML = FALSE)

summary(best_canopy_traits_pc1)


leaf_traits_pc1 <- afex::lmer(PC1 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                                leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet*leaf_length_per_leaflet*leaf_moisture_content + 
                                  (1|group), data = model_data, REML = FALSE)



leaf_traits_pc1_models <- dredge(leaf_traits_pc1)

best_leaf_traits_pc1 <- get.models(leaf_traits_pc1_models, subset = TRUE)[[1]]

summary(best_leaf_traits_pc1) # Null model


AIC(best_canopy_traits_pc1, best_leaf_traits_pc1) # canopy 344.0820, leaf 463.2763

##############################################################################################################################################
# Model of separate canopy traits and leaf traits with interactions for PC2
#############################################################################################################################################

canopy_traits_pc2 <- afex::lmer(PC2 ~ total_dry_mass_gm + canopy_density_gm_cm3 +
                                  leaf_stem_mass_ratio + canopy_moisture_content +
                                  total_dry_mass_gm*canopy_density_gm_cm3*leaf_stem_mass_ratio*canopy_moisture_content +
                                  (1|group), data = model_data, REML = FALSE)



canopy_traits_pc2_models <- dredge(canopy_traits_pc2)

best_canopy_traits_pc2 <- get.models(canopy_traits_pc2_models, subset = TRUE)[[1]] 

summary(best_canopy_traits_pc2) # only total_dry_mass_gm 0.000473 ***



leaf_traits_pc2 <- afex::lmer(PC2 ~ leaf_mass_per_area + leaf_area_per_leaflet + leaf_length_per_leaflet +
                                leaf_moisture_content + leaf_mass_per_area*leaf_area_per_leaflet*leaf_length_per_leaflet*leaf_moisture_content + 
                                (1|group), data = model_data, REML = FALSE)

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

leaf_traits_pc2_models <- dredge(leaf_traits_pc2)

best_leaf_traits_pc2 <- get.models(leaf_traits_pc2_models, subset = TRUE)[[1]]

summary(best_leaf_traits_pc2) # Leaf moisture content


AIC(best_canopy_traits_pc2, best_leaf_traits_pc2) # canopy 358.2332, leaf 365.7563


###############################################################################################################################################
