# Azaj Mahmud
# Shrub Flammability
# Summer 2021

library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Getting canopy measurement data
canopy_measurement <- read_csv("../data/year_2021/canopy_measurements.csv")


# Getting burn_trials data
burn_trials <- read_csv("../data/year_2021/burn_trials.csv")

# Getting site data
site_data <- read_csv("../data/year_2021/sites.csv")

# Getting leaf measurement data

leaf_measurement <- read_csv("../data//year_2021/leaf_measurements.csv")

# Separate the species into genus and species of site data
site_data <- tidyr::separate(site_data,species,into = c("genus","species"),sep = " ")

## DWS: careful, if this runs on your machine youmust be handling the
## environemtn outside of the code somehow and that can be dangerous.

# Changing the canopy width at bottom,middle and top of inches to cm and
# determine average width.

canopy_measurement <- canopy_measurement %>%
  mutate(bottom_width_in=bottom_width_in * 2.54,
         middle_width_in=middle_width_in * 2.54,
         top_width_in=top_width_in * 2.54,
         average_width_cm=(bottom_width_in + middle_width_in + top_width_in) / 3)

# Measuring moisture content on dry basis and canopy volume. Assumed the canopy
# is a cylinder whose radius and height are the average width and the length of
# the branch respectively.

canopy_measurement <- canopy_measurement %>%
  mutate(canopy_volume_cm3 = 3.1416 * (average_width_cm)^2 * length_cm,
    moisture_content=((fresh_mass_g - dry_mass_g) / dry_mass_g) * 100)
  

# Get the pre mass(mass before burning) out of burn trials data to determine
# the canopy density (Total mass/volume) since a tiny part of the sample
# separated from the sample to determine the moisture content.So the total mass
# is the addition of pre.mass and the mass of tiny part that used for measuring
# moisture content.

pre_mass <- burn_trials %>% select(sample_id,mass.pre)

# Merge pre_mass with canopy measurement by sample_id

canopy_density_data <- left_join(canopy_measurement, pre_mass)

# Determining canopy density as gram per centimeter cube.

canopy_density_data <- canopy_density_data %>%
  mutate(total_mass_g=fresh_mass_g + mass.pre,
         canopy_density=total_mass_g / canopy_volume_cm3)

# Merging the canopy density data with the site data to create plot
# group by genus and species.

canopy_density_data <- left_join(site_data, canopy_density_data
                                 by=c("sample_id","species_id"))

# select the samples which are 70 cm

## canopy_density_data <- canopy_density_data %>% filter(length_cm=="70")

## DWS Danger! Don't compare a number to a string! Your legnths are numeric, I
## hope, lets check:
## class(canopy_density_data$length_cm)
## [1] "numeric"
## Good. So let's fix this
canopy_density_data <- canopy_density_data %>% filter(length_cm==70)

## Creating plot
# canopy density by genus

ggplot(canopy_density_data, aes(x=reorder(genus,canopy_density),
                                y=canopy_density)) +
  geom_boxplot(aes(color=genus), outlier.colour = "red", outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "italic")) +
  xlab("Genus") +
  ylab(expression(paste("Canopy density (", g / cm^3, ")")))

# canopy density by species

ggplot(canopy_density_data,aes(x=reorder(species,canopy_density),y=canopy_density))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face = "italic"))+
  xlab("Species")+
  ylab(expression(paste("Canopy density (", g / cm^3, ")")))


## DWS: SI unit for grams is "g". there is no "gm"

# Plot of moisture content in dry basis by genus

ggplot(canopy_density_data,aes(x=reorder(genus,moisture_content),
                               y=moisture_content)) +
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic")) +
  xlab("Genus") +
  ylab("Moisture content (%)")

# Plot of moisture content in dry basis by species

ggplot(canopy_density_data,aes(x=reorder(species,moisture_content),
                               y=moisture_content)) +
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic")) +
  xlab("Species") +
  ylab("Moisture Content In Percentage")

## DWS: What is point of all these plots? Also, you can re-use your ggplot
## settings to save lines of code.

# Measuring Leaf Mass per Area.

## DWS: Why aren't all the sample level traits in a single data frame? This
## seems overly complicated. Read and clean the data first, then start figures.

lma_data <- leaf_measurement %>%
  mutate(leaf_mass_area=lma_dry_mass_g / leaf_area_cm2)

# Get the length column from the canopy measurement

length_data <- select(canopy_measurement,sample_id,species_id,length_cm)

# Merging lma_data with length data

lma_data <- merge(lma_data,length_data,by=c("sample_id","species_id"))

## DWS: I'd be consistent and just use dplyr "_join" functions rather than
## mixing base merge and dplyr.

# Merge the leaf measurement data with the site by sample and species id
# for creating plot create plot of LMA by genus and species.

lma_data <- left_join(site_data,lma_data,by=c("sample_id","species_id"))

# Filter samples which are 70 cm

lma_data <- lma_data %>%
  filter(length_cm==70) %>%
  na.omit()  ## DWS: why are there NAs? Why do this filteringa t all and why do
             ## it 2x?

## DWS: CAREFUL: do you know what you had wrong here? Big danger! It probably
## did what you wanted despite being wrong. Do you know why?


# Plot of LMA by genus

ggplot(lma_data,aes(x=reorder(genus,leaf_mass_area),y=leaf_mass_area))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x  = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab(expression(paste("LMA (", g / cm^3, ")")))
# Plot of LMA by species

## DWS: what is the point of this? This is impossible to read!

ggplot(lma_data,aes(x=reorder(species,leaf_mass_area),y=leaf_mass_area))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Species") +
  ylab(expression(paste("LMA (", g / cm^3, ")")))
  

## DWS: how did you calculate LMA for Juniperus and Pinus? Did you take caliper
## measurements? that leaf area meter won't work for those, right?

# Plot of dry matter percent by genus

ggplot(lma_data, aes(x=reorder(genus, dry_material_perc), y=dry_material_perc))+
     geom_boxplot(aes(color=genus), outlier.color = "red", outlier.size = 2)+
     theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
     xlab("Genus")+
     ylab("Dry mass (%)")

# Plot of dry matter percent by species

ggplot(lma_data,aes(x=reorder(species,dry_material_perc),y=dry_material_perc))+
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Dry mass (%)")

## DWS: WHY?

# Measurements of burning trials. Determining temperature difference of discs,
# and mass consumed during burning.

burning_trials <- burn_trials %>%
  mutate(burn.date=mdy(burn.date),
         tempdiff=(temp.d1.post-temp.d1.pre+temp.d2.post-temp.d2.pre)/2,
         massconsumed=(mass.pre-mass.post)/mass.pre)

# Get the genus and species by doing left join the site data with burning
# trials data

burning_trials <- left_join(site_data,burning_trials,
                            by=c("sample_id","species_id"))

# Merge the length data with burning trials

burning_trials <- merge(burning_trials,length_data,
                        by=c("sample_id","species_id"))

## DWS: why are you using left_join() for some emrges and merge() for others?

# Filter samples which are 70 cm

## burning_trials <- burning_trials%>%
##   filter(length_cm=="70")


## DWS: WHY? WHY? Why? NO.


# Plot of flame duration by genus

## DWS: what are you doing with all these figures? I have no clue about the
## purpose? What questions do you have about genus as a category? What are you
## trying to do?

ggplot(burning_trials, aes(genus,flame.dur)) + geom_jitter() +
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Flame duration (s)")

# Plot of flame duration by species

ggplot(burning_trials,aes(species,flame.dur))+geom_jitter()+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Flame duration (s)")

## DWS: Use SI unit standard abbreviations.

# Plot of mass consumed by genus

ggplot(burning_trials,aes(genus,massconsumed))+
  geom_jitter(width = 0.2, size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
          xlab("Genus")+
  ylab("Mass consumed (g)")

## DWS: Why the change to a dotplot?

# Plot of mass consumed by species

ggplot(burning_trials,aes(species,massconsumed))+
  geom_jitter(width = 0.2,size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Mass Consumed")

# Plot of tempdiff by genus

ggplot(burning_trials,aes(x=reorder(genus,tempdiff),y=tempdiff))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Temperature Difference In Celsius")

## DWS; what is temperature difference? 

# Plot of tempdiff by species

ggplot(burning_trials,aes(x=reorder(species,tempdiff),y=tempdiff))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Temperature Difference In Celsius")

# Plot of flame height by genus

ggplot(burning_trials,aes(x=reorder(genus,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Flame Duration In Centimeter")

# Plot of flame height by species

ggplot(burning_trials,aes(x=reorder(species,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("species")+
  ylab("Flame Height In Centimeter")


# Plot of volume burned by genus

ggplot(burning_trials,aes(x=reorder(genus,vol.burned),y=vol.burned))+
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Volume Burned In Percentage")

# Plot of volume burned by species

ggplot(burning_trials,aes(x=reorder(species,vol.burned),y=vol.burned))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Volume Burned In Percentage")

# Plot of maximum temperature by genus

## DWS: maximum temperature of what?

ggplot(burning_trials,aes(x=reorder(genus,max.temp),y=max.temp))+
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Maximum Temperature In Celsius")

# Plot of maximum temperature by species

ggplot(burning_trials,aes(x=reorder(species,max.temp),y=max.temp))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Maximum Temperature In Celsius")

# Determining heat release
# Average mass of two disc is 53.17065
# The specific heat of aluminium is 0.89 Joule/gram times degree celsius
# Heat release = Mass*difference in temperature*specific heat

burning_trials <- burning_trials%>%
  mutate(heat_release_j=tempdiff*53.1765*0.89)

# Plot of heat release by genus

ggplot(burning_trials,aes(x=reorder(genus,heat_release_j),y=heat_release_j))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Heat Release In Joule")

# Plot of heat release by species

ggplot(burning_trials,aes(x=reorder(species,heat_release_j),y=heat_release_j))+
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Heat Release In Joule")

# PCA analysis

pca <- prcomp(select(burning_trials,massconsumed, tempdiff, vol.burned, flame.ht, flame.dur),scale=TRUE)

summary(pca)

plot(pca,type="l")

biplot(pca,scale = 0)

burning_trials$PC1 <- pca$x[,1]

# ggplot of flammability score (PC1) by genus

ggplot(burning_trials,aes(x=reorder(genus,PC1),PC1))+
  geom_boxplot(aes(color=genus,fill=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Flammability Score (PC1)")
ggsave("Flammability Score (PC1) By Genus.pdf")

# ggplot of flammability score (PC1) by species

ggplot(burning_trials,aes(x=reorder(species,PC1),PC1))+
  geom_boxplot(aes(color=species,fill=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Flammability Score (PC1)")
ggsave("Flammability Score (PC1) By Species.pdf")




