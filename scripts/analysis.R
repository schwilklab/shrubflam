#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

#################################################################################
## Random intercept model

library(lme4)
library(afex)
library(ggplot2)
library(dplyr)
library(pscl)
source("./read_data.R")
source("./flam_pca.R")
source("./ggplot_theme.R")

## Does moisture content affects ignition?

ggplot(alldata,aes(moisture_content,ignition))+
  geom_point()+
  geom_smooth(method = "glm",method.args=list(family=binomial(link = "cloglog")), 
              fullrange=TRUE, se=FALSE,color="red")+
  scale_x_continuous(limits = c(2.406877,131.754543),
                     breaks = c(0,25,50,75,100,125))+
  xlab("Moisture content (%)")+
  ylab("Probability of getting ignited")+
  theme_bw()

## Null model for comparison.

log.null.moisture <- glm(ignition~1,
                         family = binomial(link = "cloglog"), 
                         data = alldata) 

log.mod.moisture <- glm(ignition~moisture_content,
                        family = binomial(link = "cloglog"),
                        data = alldata)

summary(log.mod.moisture) # p value 0.0573 , not statistically significant!!


pscl::pR2(log.mod.moisture)["McFadden"] #McFadden 0.04627074



#### figure
model_data_sum <- model_data %>% group_by(group) %>%
  summarize(across(c(canopy_density, total_mass_g, PC1), list(mean = mean, sum  = sum)), na.rm=TRUE)

fig1 <- ggplot(model_data, aes(total_mass_g, PC1)) +
  geom_point(alpha=0.4, size=3) +
  geom_point(data=model_data_sum, aes(total_mass_g_mean, PC1_mean), size=5) +
  #scale_colour_manual(schwilkcolors) +
  bestfit +
  #geom_smooth(method="lm",se=FALSE, color="black") +
  xlab("Mass per 70 cm (g)") +
  ylab("Flammability (PC1 score)") +
  prestheme.nogridlines +
  theme(legend.position="none")

fig1
ggsave("../results/shrubflam_fig1.pdf", fig1, width=0.8*col1, height=0.8*col1)


#################################################################################
# Does leaf traits and canopy traits influence flammability?

null.model <- lmer(PC1~1+ (1|group),
                   data=model_data)

traits.model <- lmer(PC1 ~ total_mass_g + canopy_density + leaf_mass_area+
                       leaf_area_per_leaflet + moisture_content + windspeed + 
                       air.temp.F + rh + (1|group), data = model_data)
summary(traits.model)
anova(null.model,traits.model)
plot(resid(traits.model))


#################################################################################
# Alternative method using afex:mixed() and Kenward-Roger approximation for
# degrees of freedom:
#################################################################################

traits.mixed.model <- mixed(PC1 ~ total_mass_g + canopy_density + 
                              leaf_mass_area + leaf_area_per_leaflet +
                              moisture_content + windspeed + 
                              air.temp.F + rh + (1|group),
                              data=model_data, method="KR")
summary(traits.mixed.model)

traits.mixed.model <- mixed(PC1 ~ total_mass_g + canopy_density + 
                              leaf_area_per_leaflet + moisture_content + 
                              air.temp.F + (1|group),
                              data=model_data, method="KR")
summary(traits.mixed.model)




#################################################################################
#Samples those only get ignited
#################################################################################

traits.model.ignited <- lmer(PC1 ~ total_mass_g+canopy_density + leaf_mass_area +
                               leaf_area_per_leaflet + windspeed + 
                               air.temp.F + rh + (1|group),
                             data=filter(model_data,ignition==1))
summary(traits.model.ignited)

#################################################################################
# Without juniperus species
#################################################################################

traits.model.wjuniperus <- lmer(PC1 ~ total_mass_g + canopy_density + leaf_mass_area +
                                  leaf_area_per_leaflet + moisture_content + 
                                  windspeed + air.temp.F + rh + (1|group),
                                data=filter(model_data, genus !="Juniperus"))
summary(traits.model.wjuniperus)
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
