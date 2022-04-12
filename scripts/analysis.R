#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021
# Need to set the directory as setwd("../scripts") to read the scripts
# Need to make sure that the number of observations are same for each 
# model in model_data.
#################################################################################
## Random intercept model

library(lme4)
library(afex)
library(ggplot2)
library(dplyr)
library(pscl)
source("./read_data.R")
source("./flam_pca.R")


########################################################################
# scaling the response variables since they measured in different
# units
########################################################################


model_data[ ,c("canopy_density","total_mass_g","leaf_mass_area",
             "leaf_area_per_leaflet","moisture_content","windspeed",
             "air.temp.F","rh")] <- scale(model_data[ ,c("canopy_density","total_mass_g",
                                                         "leaf_mass_area","leaf_area_per_leaflet",
                                                         "moisture_content","windspeed",
                                                       "air.temp.F","rh")],scale=TRUE)

###############################################################################
# At first do the random intercept model 
###################################################################################

#################################################################################
# Does leaf traits and canopy traits influence flammability?
##################################################################################

null.model <- afex::lmer(PC1 ~ 1 + (1|group),
                   data=model_data)

traits.model <- afex::lmer(PC1 ~ total_mass_g + canopy_density + leaf_mass_area +
                       leaf_area_per_leaflet + moisture_content +
                       (1|group), data = model_data)


summary(traits.model)
anova(null.model,traits.model)
plot(resid(traits.model))


interaction.model <- afex::lmer(PC1 ~ total_mass_g*canopy_density*moisture_content +
                                      total_mass_g*leaf_mass_area*moisture_content +
                                      leaf_area_per_leaflet + (1 | group), data = model_data)

plot(interaction.model)

summary(interaction.model) 
AIC(null.model,traits.model,interaction.model)


#################################################################################
#Samples those only get ignited
#################################################################################

null.ignited <- afex::lmer(PC1 ~ 1 + (1|group),
                           data = filter(model_data, ignition == 1))

traits.model.ignited <- afex::lmer(PC1 ~ total_mass_g + canopy_density + leaf_mass_area+
                                     leaf_area_per_leaflet + moisture_content +
                                     (1|group), data = filter(model_data, ignition == 1))

summary(traits.model.ignited)

interaction.model.ignited <- afex::lmer(PC1 ~ total_mass_g*canopy_density*moisture_content +
                                          total_mass_g*leaf_mass_area*moisture_content +
                                          leaf_area_per_leaflet + (1 | group), data = filter(model_data,
                                                                                             ignition == 1))


summary(interaction.model.ignited)

AIC(null.ignited, traits.model.ignited, interaction.model.ignited)

##############################################################################
# Models without Juniperus
##############################################################################

null.without.juniperus <- afex::lmer( PC1 ~ 1 + (1|group),
                                      data = filter(model_data,
                                                    group != "Juniperus"))

traits.without.juniperus <- afex::lmer(PC1 ~ total_mass_g + canopy_density + leaf_mass_area +
                                         leaf_area_per_leaflet + moisture_content +
                                         (1|group), data = filter(model_data,
                                                                  group != "Juniperus"))
summary(traits.without.juniperus)

interaction.without.juniperus <- afex::lmer(PC1 ~ total_mass_g*canopy_density*moisture_content +
                                              total_mass_g*leaf_mass_area*moisture_content +
                                              leaf_area_per_leaflet + (1 | group),
                                              data = filter(model_data, group != "Juniperus"))
summary(interaction.without.juniperus)

AIC(null.without.juniperus, traits.without.juniperus, interaction.without.juniperus)

########################################################################
## Does moisture content affects ignition?
########################################################################

ggplot(alldata,aes(moisture_content,ignition))+
  geom_point()+
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red")+
  scale_x_continuous(limits = c(2.406877,131.754543),
                     breaks = c(0,25,50,75,100,125))+
  xlab("Moisture content (%)")+
  ylab("Probability of getting ignited")+
  theme_bw()+
  theme(axis.title = element_text(size=12,face = "bold"))

## Null model for comparison.

log.null.moisture <- glm(ignition~1,
                         family = binomial(link = "cloglog"), 
                         data = alldata) 

log.mod.moisture <- glm(ignition~moisture_content,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.moisture) # p value 0.0573 , not statistically significant!!


pscl::pR2(log.mod.moisture)["McFadden"] #McFadden 0.04627074

########################################################################
## Does windspeed affects ignition?
########################################################################

ggplot(alldata,aes(windspeed,ignition))+
  geom_point()+
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red")+
  scale_x_continuous(limits = c(0,8.7),
                     breaks = c(0,2,4,6,8))+
  xlab("Windspeed")+
  ylab("Probability of getting ignited")+
  theme_bw()

log.mod.windspeed <- glm(ignition~windspeed,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.windspeed)





#################################################################################

#################################################################################
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
vif.lme(traits.model)
ggplot(model_data, aes(total_mass_g,PC1))+
  geom_point()
