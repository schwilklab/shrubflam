# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This script depends on scripts listed in run-all.R

## DWS: This script is NUTS! it is hundreds of line of code. We agreed on a
## pretty straightforward analysis method and you are fishing. AM: The reason
## behind why it looked liked tons of model is that all the models were fitted
## with and without "Juniperus" species, therefore, it made the number of
## models double. Moreover, there was another type of global model were created
## with the combination of best canopy traits and best leaf traits to see
## whether the combination is better than individual types of traits or not!!
## However, Now I am just removing the models of the combination of best canopy
## traits and leaf traits and just testing whether the best canopy traits and
## leaf traits have statistically significant effect on heat release and
## ignition delay if we remove the most flammable group "Juniperus" from the
## analysis.
## 

library(MuMIn)

# MuMIn package for automated model selection through subsetting the maximum
# model, with optimal constraints for model inclusion. Model parameter and
# prediction averaging based on model weights derived from information criteria
# (AIC). source: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf


# Principle component analysis

# REML IS EQUAL TO FALSE BECAUSE Faraway (2006) Extending the linear model with
# R (p. 156): The reason is that REML estimates the random effects by
# considering linear combinations of the data that remove the fixed effects. If
# these fixed effects are changed, the likelihoods of the two models will not
# be directly comparable
# https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but



###############################################################################
# scaling the response variables since they measured in different units
###############################################################################

## DWS: But this makes interpretation difficult. Do you need to do this? AM:
## all the traits were measured in different units, completely different than
## each other and so far my knowledge scaling as z-score makes comparison
## easier.

## NO, You do not understand what I am saying. This has nothing to do with
## p-values. Talk to me.

## However, in this case, the main concern is AICc value which tells which
## traits fit the data best. Therefore, estimates and p-value might not be an
## important issue in this case, unless reviewer or thesis committee asked for.
## Therefore, may be not doing scaling would be ok. But for now I am leaving it
## as it is and can be changed later after Dr. Schwilk's opinion on this
## matter.

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

model_data <- model_data %>%
  dplyr::select(degsec_100, field_taxon,
         ignition_delay, mean_pre_burning_temp,
         windspeed_miles_per_hour,
         display_name, analysis_group, total_dry_mass_g , 
         canopy_density_gm_cm3 , leaf_stem_mass_ratio , 
         canopy_moisture_content, leaf_mass_per_area , 
         leaf_area_per_leaflet , leaf_length_per_leaflet , 
         leaf_moisture_content, sample_id) %>%
  na.omit()

dim(model_data)

any(is.na(model_data)) # FALSE

#model_data$degsec_100 <- log10(model_data$degsec_100 + 1) # log transformation of
                                                    # response variable

## DWS: So you are making z score and then taking log. How will you graph this?
## AM: Log transformation made the residuals better though not completely
## normal. If we don't need to worry about testing model assumptions, then not
## doing log transformation is ok.

## DWS: Make sure you understand trade-off here.


###############################################################################
# A global model of canopy traits with two way interactions for temperature
# integration.
###############################################################################


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


## AM: The reason behind ruling out the random slopes is that it made the model
## overfitted (isSingular?).

## DWS: well what about a smaller model?

canopy_pc1_models <- dredge(canopy_pc1_model) # Performs an automated

# model selection with subsets of the supplied global model. source: ?dredge

best_canopy_pc1_model <- get.models(canopy_pc1_models, subset = TRUE)[[1]] # returns list and 
# indexing the first one, top model.

## DWS: But we need the model table to compare. Is the best model good? are
## models close in AICc? I looked at model table and your slection cannot
## select among top models, they are equivalent.

canopy_mod_table <- model.sel(canopy_pc1_models)
canopy_mod_table[1:8,]

## DWS: So top four models pretty close. AM: comparing by AICc, it is true that
## they looks really close. However, the weight of the model tells that the
## best model is really much better than the second best (much higher weight).


## I'm not sure I agree that 32% is super convincing.

## So, now the question is how much weight will increase by changing one unit
## of AICc value? So far, in this case it looks like it changes a lot. And I
## don't know whether mathmetically it makes sense or not.

## DWS: ??

# Generate or extract a list of fitted model objects from a "model.selection"
# table, object returned by dredge. The argument subset must be explicitly
# provided. This is to assure that a potentially long list of models is not
# fitted unintentionally. To evaluate all models, set subset to NA or TRUE.
# source: ?get.models

summary(best_canopy_pc1_model)

# total mass and canopy density without interaction
# is the best model.

#sjPlot::tab_model(best_canopy_pc1_model)

###############################################################################
# A global model of leaf traits with two way interaction for heat release
# Kendall rank correlation coefficient between leaf_area_per_leaflet and
# leaf_length_per_leaflet is 0.67 and decided to drop leaf_area_per_leaflet
# from the model to avoid the high collinearity between two fixed effects.
# theoretical explanation will be given in thesis/paper.



leaf_pc1_model <- afex::lmer(degsec_100 ~ leaf_mass_per_area + leaf_length_per_leaflet +
                               leaf_moisture_content  + 
                               leaf_mass_per_area:leaf_length_per_leaflet +
                               leaf_mass_per_area:leaf_moisture_content + 
                               leaf_length_per_leaflet:leaf_moisture_content + 
                               (1| analysis_group), data = model_data, REML = FALSE)

leaf_pc1_models <- dredge(leaf_pc1_model)

best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 

leaf_mod_table <- model.sel(leaf_pc1_models)
leaf_mod_table[1:8,]


best_leaf_pc1_model <- get.models(leaf_pc1_models, subset = TRUE)[[1]] 


summary(best_leaf_pc1_model) # Leaf mass per area is the best model 

## DWS: BUt not by much!

## AM: This issue is addressed in the last model.

## DWS: Talk to me.

#sjPlot::tab_model(best_leaf_pc1_model)

###############################################################################
# Comparison between best canopy and leaf model for temperature integration
###############################################################################


AIC(best_canopy_pc1_model, best_leaf_pc1_model) # AIC for canopy 3.18
# and leaf 76.26

###############################################################################
# Does pre_burning temperature
# and windspeed improves the best model?
###############################################################################

pre_burning_temp_canopy_traits_model <- afex::lmer(degsec_100 ~ total_dry_mass_g +
                                                     canopy_density_gm_cm3 +
                                                     mean_pre_burning_temp +
                                                     windspeed_miles_per_hour +
                                                     (1 | analysis_group),
                                                   data = model_data, REML = FALSE)

summary(pre_burning_temp_canopy_traits_model) # pre_burning_temp: p-value = 0.231

AIC(best_canopy_pc1_model, pre_burning_temp_canopy_traits_model) # AICc = 5.73,
# didn't improve the model.


###############################################################################
# Does the best canopy traits and leaf traits have significant effect on heat
# release if we remove the most flammable group "Juniperus" from the analysis?
###############################################################################


without_juniperus <- model_data %>% # creating a new data set without Juniperus group
  filter(analysis_group != "Juniperus")

dim(without_juniperus) #93
any(is.na(without_juniperus)) # FALSE


heat_release_without_juniperus <- afex::lmer(degsec_100 ~ total_dry_mass_g +
                                               canopy_density_gm_cm3 +
                                               leaf_mass_per_area + (1 | analysis_group),
                                             data = without_juniperus, REML = FALSE)

summary(heat_release_without_juniperus)
anova(heat_release_without_juniperus)
## Total dry mass is stiatistically significant, p < 0.001
## canopy density is marginally significant p = 0.073
## and LMA is not significant at all p = .959

###############################################################################
# A global model of canopy traits and leaf traits without interaction (to avoid
# overfitting problem) for ignition delay.
###############################################################################


###############################################################################
# Ignition delay vs canopy traits
###############################################################################


canopy_ignition_model <- afex::lmer(ignition_delay ~ total_dry_mass_g + leaf_stem_mass_ratio + 
                                 canopy_density_gm_cm3 + canopy_moisture_content +
                                 (1 | analysis_group), data = model_data, REML = FALSE)


canopy_ignition_models <- dredge(canopy_ignition_model) 

best_canopy_ignition_model <- get.models(canopy_ignition_models, subset = TRUE)[[1]]

canopy_ignition_mod_table <- model.sel(canopy_ignition_models)
canopy_ignition_mod_table[1:8,]
summary(best_canopy_ignition_model)

# Canopy_density and canopy moisture content best fit the dataset

#sjPlot::tab_model(best_canopy_ignition_model)

###############################################################################
# Ignition delay vs leaf traits
###############################################################################

leaf_traits_ignition_model <- afex::lmer(ignition_delay ~ leaf_mass_per_area +
                                           leaf_length_per_leaflet + leaf_moisture_content +
                                           (1 | analysis_group), data = model_data, REML = FALSE)

leaf_ignition_models <- dredge(leaf_traits_ignition_model) 


best_leaf_ignition_model <- get.models(leaf_ignition_models, subset = TRUE)[[1]] 



leaf_ignition_mod_table <- model.sel(leaf_ignition_models)
leaf_ignition_mod_table[1:8,]

summary(best_leaf_ignition_model) # LMA and leaf_moisture_content

#sjPlot::tab_model(best_leaf_ignition_model)

AIC(best_canopy_ignition_model, best_leaf_ignition_model) # canopy 579.15
# leaf 584.87

###############################################################################
# Does pre_burning temperature
# and windspeed improves the best model?
###############################################################################

pre_burning_ignition_model <- afex::lmer(ignition_delay ~ canopy_density_gm_cm3 +
                                           canopy_moisture_content +
                                           mean_pre_burning_temp +
                                           windspeed_miles_per_hour +
                                           (1 | analysis_group),
                                         data = model_data, REML = FALSE)


AIC(best_canopy_ignition_model, pre_burning_ignition_model) # Didn't improve the model

# AIC for pre burning ignition = 582.2, pretty close

###############################################################################
# Without Juniperus, Do the best traits have significant effect on ignition
# delay if we remove "Juniperus" group? One important note: The leaf and canopy
# moisture content are highly correlated and I am using only leaf moisture
# content since leaf is one of the first thing that's get ingited.
###############################################################################

ignition_delay_without_juniperus <- afex::lmer(ignition_delay ~ canopy_density_gm_cm3 +
                                                 leaf_mass_per_area + leaf_moisture_content +
                                                 (1 | analysis_group), data = without_juniperus,
                                               REML = FALSE)

summary(ignition_delay_without_juniperus) # Only moisture content , p = 0.027
anova(ignition_delay_without_juniperus)

###############################################################################
# Does remaining less important traits have any significant effect on heat
# release and ignition delay? The reason for creating separate model for canopy
# traits and leaf traits is that some of the traits are correlated with each
# other.
###############################################################################

leaf_stem_canopy_mc_heat_release_model <- afex::lmer(degsec_100 ~ leaf_stem_mass_ratio +
                                                       canopy_moisture_content +
                                                       (1 | analysis_group),
                                                     data = model_data, REML = FALSE)

summary(leaf_stem_canopy_mc_heat_release_model) # none of them have any
# significant effect on heat release
# leaf:stem: p- value = 0.113
# canopy_mc: p-value = 0.592

leaf_length_leaf_mc_heat_release_model <- afex::lmer(degsec_100 ~ leaf_length_per_leaflet +
                                                       leaf_moisture_content +
                                                       (1 | analysis_group),
                                                     data = model_data, REML = FALSE)

summary(leaf_length_leaf_mc_heat_release_model) # none of them had any 
# significant effect!!
# leaf_mc: p-value = 0.374
# leaf_length: p-value = 0.786

###############################################################################
# Same for ignition delay
###############################################################################

total_dry_mass_leaf_stem_ignition <- afex::lmer(ignition_delay ~ total_dry_mass_g +
                                                  leaf_stem_mass_ratio +
                                                  (1|analysis_group),
                                                data = model_data, REML = FALSE)

summary(total_dry_mass_leaf_stem_ignition) # total dry mass has
# significant effect
# total_dry_mass: p-value = 0.023
# leaf:stem: p-value = 0.3872
 

leaf_length_ignition_model <- afex::lmer(ignition_delay ~ leaf_length_per_leaflet +
                                           (1 | analysis_group),
                                         data = model_data, REML = FALSE)

summary(leaf_length_ignition_model) # no effect: p-value = 0.552

###########################################################################################
# This part is for model building for anova table, first for heat release
# At first, canopy traits
# Checking the significance by Kenward-Roger's method
###########################################################################################


canopy_traits_heat_release_model_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + canopy_density_gm_cm3 +
                                                        (1|analysis_group), data = model_data,
                                                      method = "KR", REML = TRUE)



canopy_traits_anova_table_model <- lme4::lmer(degsec_100 ~ total_dry_mass_g + 
                                                canopy_density_gm_cm3 +
                                                (1 | analysis_group),
                                              data = model_data)


canopy_traits_anova <- car::Anova(canopy_traits_anova_table_model, 
                                  type = 2, test.statistic = "F")
canopy_anova <- xtable::xtable(canopy_traits_anova, digits = 3)

canopy_anova_coefficients <- summary(canopy_traits_anova_table_model)$coefficients
canopy_coeff <- xtable::xtable(canopy_anova_coefficients, digits = 3)




###########################################################################################
# without Juniperus for heat release
###########################################################################################


canopy_traits_heat_release_model_mixed_withoutj <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                                                 canopy_density_gm_cm3 +
                                                                 (1|analysis_group), 
                                                               data = without_juniperus,
                                                               method = "KR", REML = TRUE)




canopy_traits_anova_table_model_withoutj <- lme4::lmer(degsec_100 ~ total_dry_mass_g + 
                                                         canopy_density_gm_cm3 +
                                                         (1 | analysis_group), 
                                                       data = without_juniperus)


canopy_traits_anova_withoutj <- car::Anova(canopy_traits_anova_table_model_withoutj, type = 2, 
                                           test.statistic = "F")
canopy_anova_withoutj <- xtable::xtable(canopy_traits_anova_withoutj, digits = 3)
canopy_anova_coefficients <- summary(canopy_traits_anova_table_model)$coefficients
canopy_coeff <- xtable::xtable(canopy_anova_coefficients, digits = 3)



############################################################################################
# Now leaf traits for heat release
#############################################################################################

leaf_traits_heat_release_model_mixed <- afex::mixed(degsec_100 ~ leaf_mass_per_area +
                                                      (1|analysis_group), data = model_data,
                                                    method = "KR", REML = TRUE)



leaf_traits_anova_table_model <- lme4::lmer(degsec_100 ~ leaf_mass_per_area +
                                              (1 | analysis_group), data = model_data)


leaf_traits_anova <- car::Anova(leaf_traits_anova_table_model, type = 2, 
                                test.statistic = "F")

leaf_anova <- xtable::xtable(leaf_traits_anova, digits = 3)
leaf_anova_coefficients <- summary(leaf_traits_anova_table_model)$coefficients
leaf_coeff <- xtable::xtable(leaf_anova_coefficients, digits = 3)

############################################################################################
# leaf traits for heat release without Juniperus
#############################################################################################

leaf_traits_heat_release_model_mixed_withoutj <- afex::mixed(degsec_100 ~ leaf_mass_per_area +
                                                               (1|analysis_group), data = without_juniperus,
                                                             method = "KR", REML = TRUE)



leaf_traits_anova_table_model_withoutj <- lme4::lmer(degsec_100 ~ leaf_mass_per_area +
                                                       (1 | analysis_group), data = without_juniperus)


leaf_traits_anova_withoutj <- car::Anova(leaf_traits_anova_table_model_withoutj, type = 2, test.statistic = "F")
leaf_anova_withoutj <- xtable::xtable(leaf_traits_anova_withoutj, digits = 3)
leaf_anova_coefficients_withoutj <- summary(leaf_traits_anova_table_model_withoutj)$coefficients
leaf_coeff_withoutj <- xtable::xtable(leaf_anova_coefficients_withoutj, digits = 3)


###################################################################################################################
# Now for ignition delay, at first the canopy traits
###################################################################################################################

canopy_traits_ignition_model_mixed <- afex::mixed(ignition_delay ~ canopy_density_gm_cm3 + 
                                                    canopy_moisture_content +
                                                    (1|analysis_group), data = model_data,
                                                  method = "KR", REML = TRUE)

canopy_traits_ignition_anova_table_model <- lme4::lmer(ignition_delay ~ canopy_density_gm_cm3 +
                                                          canopy_moisture_content +
                                                         (1 | analysis_group), 
                                                       data = model_data)


canopy_traits_ignition_anova <- car::Anova(canopy_traits_ignition_anova_table_model, type = 2, 
                                           test.statistic = "F")
canopy_ignition_xtable <-  xtable::xtable(canopy_traits_ignition_anova, digits = 3)
canopy_ignition_anova_coefficients <- summary(canopy_traits_ignition_anova_table_model)$coefficients
canopy_ignition_coeff <- xtable::xtable(canopy_ignition_anova_coefficients, digits = 3)



############################################################################################
# Now leaf traits
#############################################################################################

leaf_traits_ignition_model_mixed <- afex::mixed(ignition_delay ~ leaf_mass_per_area + 
                                                  leaf_moisture_content +
                                                  (1|analysis_group), 
                                                  data = model_data,
                                                  method = "KR", REML = TRUE)


leaf_traits_ignition_anova_table_model <- lme4::lmer(ignition_delay ~  leaf_mass_per_area + 
                                                       leaf_moisture_content +
                                                       (1 | analysis_group), data = model_data)


leaf_traits_ignition_anova <- car::Anova(leaf_traits_ignition_anova_table_model, type = 2,
                                         test.statistic = "F")
leaf_ignition_xtable <-  xtable::xtable(leaf_traits_ignition_anova, digits = 3)
leaf_ignition_anova_coefficients <- summary(leaf_traits_ignition_anova_table_model)$coefficients
leaf_ignition_coeff <- xtable::xtable(leaf_ignition_anova_coefficients, digits = 3)
########################################################################################
# Without Juniperus
########################################################################################

ignition_model_mixed_withoutj <- afex::mixed(ignition_delay ~ canopy_density_gm_cm3 + 
                                               leaf_mass_per_area + 
                                               leaf_moisture_content +
                                               (1|analysis_group), 
                                               data = without_juniperus,
                                               method = "KR", REML = TRUE)

ignition_anova_table_model_withoutj <- lme4::lmer(ignition_delay ~  canopy_density_gm_cm3 +
                                                                  leaf_mass_per_area + 
                                                                  leaf_moisture_content +
                                                                  (1 | analysis_group), 
                                                  data = without_juniperus)

ignition_anova_withoutj <- car::Anova(ignition_anova_table_model_withoutj, type = 2,
                                      test.statistic = "F")
ignition_xtable_withoutj <-  xtable::xtable(ignition_anova_withoutj, digits = 3)
ignition_anova_coefficients_withoutj <- summary(ignition_anova_table_model_withoutj)$coefficients
ignition_coeff_withoutj <- xtable::xtable(ignition_anova_coefficients_withoutj, digits = 3)


