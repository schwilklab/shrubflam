#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

#################################################################################
## Random intercept model

library(lme4)
library(afex)
source("./flam_pca.R")

#################################################################################
# Does leaf traits and canopy traits influence flammability?

#null.model <- lmer(PC1~1+ (1| ),
                   #data=model_data, method="REML")
#traits.model <- lmer(PC1~total_mass_g+canopy_density+leaf_mass_area+
                       #leaf_area_per_leaflet+(1| ),
                     #data=model_data, method="REML")
# summary(traits.model)
# anova(null.model,traits.model)
# plot(resid(traits.model))

#################################################################################
#  variance inflation factors for random mixed effect model
# source(https://stackoverflow.com/questions/
# 26633483/collinearity-after-accounting-for-random-mixed-effects)

vif.lme <- function (fit) {
## adapted from rms::vif
vif.lme <- vcov(fit)
nam <- names(fixef(fit))
## exclude intercepts
ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)] }
d <- diag(v)^0.5
v <- diag(solve(v/(d %o% d)))
names(v) <- nam
v }
vif.lme(traits.model)
#################################################################################

#################################################################################
# Alternative method using afex:mixed() and Kenward-Roger approximation for
# degrees of freedom:
#traits.mixed.model <- mixed(PC1~total_mass_g+canopy_density+leaf_mass_area+
                              #leaf_area_per_leaflet+(1| ),
                              #data=model_data, method="KR")
#summary(traits.mixed.model)
#################################################################################