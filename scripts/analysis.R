#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021
# Need to set the directory as setwd("../scripts") to read the scripts
# Need to make sure that the number of observations are same for each 
# model in model_data.

## DWS: I recommended changing to use root of repo as the working directory.
## Ten set that in your rstudio project file (not in version control) and
## document in the readme. That has become the standard way to do this.

#################################################################################
## Random intercept model


library(afex)
library(MuMIn)
library(ggplot2)
library(dplyr)
library(pscl)

#source("./read_data.R")
## You are calling read_data twice! because it is also called in flam_pca.
source("./flam_pca.R")

########################################################################
# scaling the response variables since they measured in different
# units
########################################################################

zscore <- function(x) (x -mean(x, na.rm=TRUE))/ sd(x, na.rm = TRUE)


model_data <- model_data %>%
  mutate_at(c("canopy_density","total_mass_g","leaf_mass_area",
              "leaf_area_per_leaflet","moisture_content","windspeed",
              "air.temp.F","rh"), list(zscore))

#model_data[ ,c("canopy_density","total_mass_g","leaf_mass_area",
             #"leaf_area_per_leaflet","moisture_content","windspeed",
             #"air.temp.F","rh")] <- scale(model_data[ ,c("canopy_density","total_mass_g",
                                                         #"leaf_mass_area","leaf_area_per_leaflet",
                                                         #"moisture_content","windspeed",
                                                       #"air.temp.F","rh")],scale=TRUE)

## DWS: I would do this as a mutate() call.

###############################################################################
# At first do the random intercept model 
###################################################################################


#################################################################################
# Getting the best model for leaf traits
#################################################################################

options(na.action = "na.fail")

leaf.traits.model <- afex::lmer( PC1 ~ leaf_mass_area + leaf_area_per_leaflet +
                                    leaf_mass_area*leaf_area_per_leaflet +
                                    ( 1|group), data = model_data)

leaf.traits.models <- dredge(leaf.traits.model)

get.models(leaf.traits.models, subset = TRUE)[[1]] # The best model,null model

get.models(leaf.traits.models, subset = TRUE)[[2]] # The second best model, only 
# leaf_mass_area (LMA)

leaf.models <- model.avg(leaf.traits.models, rank = "AICc",
          rank.args = list(REML = TRUE))

#plot(leaf.models, full = TRUE, intercept = TRUE)
 
summary(model.avg(leaf.traits.models)) # AIC value for null model
# and leaf_mass_area(LMA) is 428.87 and 431.46 respectively

lma.model <- afex::lmer(PC1 ~ leaf_mass_area +
                              (1|group), data = model_data)

#################################################################################
# Getting the best model for canopy_traits
#################################################################################

canopy.traits.model <- afex::lmer( PC1 ~ total_mass_g + canopy_density + moisture_content +
                                     total_mass_g*canopy_density +
                                     total_mass_g*moisture_content +
                                     canopy_density*moisture_content + 
                                     ( 1| group), data = model_data)

canopy.traits.models <- dredge(canopy.traits.model)

get.models(canopy.traits.models, subset = TRUE)[[1]] # Only total mass is the
# best model

get.models(canopy.traits.models,subset = TRUE)[[2]] # Moisture_content +
# total_mass_g is the second best

canopy.models <- model.avg(canopy.traits.models, rank = "AICc",
          rank.args = list(REML = TRUE))

#canopy.models.fig <- plot(canopy.models, full = TRUE, intercept = TRUE) +
  #prestheme.nogridlines


summary(model.avg(canopy.traits.models))

null.model <- afex::lmer(PC1 ~ 1 + (1|group),
                         data = model_data)

total.biomass.model <- afex::lmer( PC1 ~ total_mass_g +
                                     (1|group), data = model_data)

AIC(null.model,lma.model, total.biomass.model) # Total biomass is better (lower AIC, 412.1905)

biomass.lma.model <- afex::lmer(PC1 ~ total_mass_g + leaf_mass_area +
                                      (1|group), data = model_data)

AIC(null.model,total.biomass.model,lma.model,biomass.lma.model) # total.biomass.model 
# is the best (lower AIC 412.1905)

#################################################################################
# What about only those samples are got ignited
#################################################################################
# Canopy traits

ignited_samples <- filter(model_data,
                          ignition == 1)

canopy.traits.model.ignited <- afex::lmer(PC1 ~ total_mass_g + canopy_density + moisture_content +
                                            total_mass_g*canopy_density +
                                            total_mass_g*moisture_content +
                                            canopy_density*moisture_content + (1|group), 
                                          data = ignited_samples)

canopy.traits.model.ignited2 <- dredge(canopy.traits.model.ignited)

get.models(canopy.traits.model.ignited2, subset = TRUE)[[1]] # Only total mass is the
# best model

get.models(canopy.traits.model.ignited2,subset = TRUE)[[2]] # Moisture_content +

# total_mass_g is the second best
model.avg(canopy.traits.model.ignited2, rank = "AICc",
          rank.args = list(REML = TRUE))

summary(model.avg(canopy.traits.model.ignited2))


# Same thing

#############################################################################
# Leaf traits
#############################################################################

leaf.traits.model.ignited <- afex::lmer( PC1 ~ leaf_mass_area + leaf_area_per_leaflet +
                                           leaf_mass_area*leaf_area_per_leaflet +
                                           ( 1|group), data = ignited_samples)

leaf.traits.model.ignited2 <- dredge(leaf.traits.model.ignited)

get.models(leaf.traits.model.ignited2, subset = TRUE)[[1]] # The best model,null model

get.models(leaf.traits.model.ignited2, subset = TRUE)[[2]] # The second best model, only 
# leaf_mass_area (LMA)

model.avg(leaf.traits.model.ignited2, rank = "AICc",
          rank.args = list(REML = TRUE))

summary(model.avg(leaf.traits.model.ignited2)) # The full model is the best (AIC 255.01)

################################################################################################
# Model comparison
################################################################################################

null.ignited <- afex::lmer(PC1 ~ 1 + (1|group),
                           data = ignited_samples)

biomass.ignited <- afex::lmer(PC1 ~ total_mass_g +
                                (1|group), data = ignited_samples)

lma.model.ignited <- afex::lmer(PC1 ~ leaf_mass_area + leaf_area_per_leaflet +
                                  leaf_mass_area*leaf_area_per_leaflet +
                                  (1|group), data = ignited_samples)

AIC(null.ignited,biomass.ignited,lma.model.ignited)  # Still biomass, lowes AIC (340.1540)
# is better

leaf.traits.biomass.ignited <- afex::lmer(PC1 ~ total_mass_g +
                                            leaf_mass_area + leaf_area_per_leaflet +
                                            leaf_mass_area*leaf_area_per_leaflet +
                                            (1|group), data = ignited_samples)

AIC(null.ignited,biomass.ignited,lma.model.ignited,leaf.traits.biomass.ignited) # Still biomass, AIC (340.1540)


########################################################################
## Does moisture content affects ignition?
########################################################################


## Null model for comparison.

log.null.moisture <- glm(ignition ~ 1,
                         family = binomial(link = "cloglog"), 
                         data = alldata) 

log.mod.moisture <- glm(ignition ~ moisture_content,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.moisture) # p value 0.0573 , not statistically significant!!


pscl::pR2(log.mod.moisture)["McFadden"] # McFadden 0.04627074

########################################################################
## Does windspeed affects ignition?
########################################################################

log.mod.windspeed <- glm(ignition~windspeed,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.windspeed)


#########################################################
#  variance inflation factors for random mixed effect model
# source(https://stackoverflow.com/questions/
# 26633483/collinearity-after-accounting-for-random-mixed-effects)

vif.lme <- function (fit) {
  vif.lme <- vcov(fit)
  nam <- names(fixef(fit))
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }
## DWS: function above does not make sense, it refers to "v" before it is defined:
## Error in vif.lme(traits.model) : object 'v' not found

#vif.lme(traits.model)
