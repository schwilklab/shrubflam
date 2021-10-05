#!/usr/bin/Rscript --vanilla

# Dylan Schwilk
# Shrub Flammability
# 2021-10-05

library(ggplot2)
library(lme4)
library(afex)

source("./read_data.R")

species_x_theme <- theme(axis.text.x = element_text(angle = 30,
                                                    hjust = 1,
                                                    face = "italic"))


pca <- prcomp(select(alldata, massconsumed, heat_release_J, vol.burned,
                     flame.ht, flame.dur), scale=TRUE)

biplot(pca) # So PC1 is NEG flam
# summary(pca)
alldata$flam_PC1 <- 0-pca$x[,1]



# Does fuel moisture influence heat release?
ggplot(alldata, aes(moisture_content, heat_release_J, color=display_name)) +
  geom_point(size = 3) +
  geom_smooth(aes(group=display_name), method="lm", se=FALSE) +
  xlab("Moisture content") +
  ylab("Heat Release (J)")

## No evidence for overall fuel mostiure effect. Hmm?? Is data correct? What
## about density?
ggplot(alldata, aes(canopy_density, heat_release_J, color=display_name)) +
  geom_point(size = 3) +
  geom_smooth(aes(group=display_name), method="lm", se=FALSE) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  ylab("Heat Release (J)")

# Juniper cating weird? higher density = lower flam? Let's look at this without
# junipers:
ggplot(filter(alldata, genus!="Juniperus"), aes(canopy_density, heat_release_J, color=display_name)) +
  geom_point(size = 3) +
  geom_smooth(aes(group=display_name), method="lm", se=FALSE) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  ylab("Heat Release (J)")


# We can try comparing model with canopy density as a fixed effect to one with
# just the random effect of species:
mod.den <- lmer(heat_release_J ~ canopy_density + (1 | display_name), alldata)
mod.den.null <- lmer(heat_release_J ~ 1 + (1 | display_name), alldata)
summary(mod.den)
anova(mod.den.null,mod.den)
## DWS: so nope, no evidence for significant fixed effect

# Altenrative method using afex:mixed() and Kenward-Roger approximation for
# degrees of freedom:
mod.den.mixed <- mixed(heat_release_J ~ canopy_density + (1 | display_name),
                       data=alldata,  method="KR")
summary(mod.den.mixed)
## DWS: so consistent with our model comparison approach above, p value around
## 0.22 so no canopy density effect.


ggplot(alldata, aes(total_mass_g, flam_PC1, color=display_name)) +
  geom_point(size = 3) +
  geom_smooth(aes(group=display_name), method="lm", se=FALSE) +
  xlab("Sample mass (g)") +
  ylab("Flammability score (PC1)")

mod.mass.mixed <- mixed(flam_PC1 ~ total_mass_g + (1 | display_name),
                       data=alldata,  method="KR")
summary(mod.mass.mixed)

## DWS: So it is all about biomass maybe? Why is density not showing up then?
## Weird.
