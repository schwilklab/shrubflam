#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2021

# Read all data files, clean and merge.

library(readr)
library(dplyr)
library(lubridate)

# TODO: Xiulin Gao used 0.921 J/g as the SH of the aluminum alloy of which our
# disks are made. We need a citation for this and DWS emailed her on
# 2021-09-24. We need to measure the disk masses since I don't know which is
# which. From the grass experiments we had 52.91g and 53.21g. I'll put in 53g
# for both as a temporary measure.

SPECIFIC_HEAT_AL = 0.921 # in J/g
MASS_DISK_1      = 52.91  # g
MASS_DISK_2      = 53.21  # g

###############################################################################
## Read all the data files
###############################################################################

## We need to read them all in first before cleaning because some calculations
## require columns from multiple files

samples <- read_csv("./data/year_2021/samples.csv")
canopy_measurements <- read_csv("./data/year_2021/canopy_measurements.csv")
leaf_measurements <- read_csv("./data/year_2021/leaf_measurements.csv")
burn_trials <- read_csv("./data/year_2021/burn_trials.csv")
species_table <- read_csv("./data/year_2021/species_table.csv")
juniperus_leaf_area <- read.csv("./data/year_2021/leaf_area_juniperus.csv")

###############################################################################
## Calculations and cleaning
###############################################################################

###############################################################################
## merging samples data and species data
###############################################################################

samples <- species_table %>%
  filter(species %in% samples$species) %>%
  dplyr::select(species, species_id, group, herbivore_defense, herbivore_preference)%>%
  right_join(samples, by = c("species","species_id"))

###############################################################################

## Samples (one row per individual plant with collection data)

###############################################################################
samples <- tidyr::separate(samples, species, into = c("genus","specific_epithet"),
                           sep = " ")

###############################################################################
## Canopy level traits

# Get the pre mass (mass before burning) out of burn trials data. This is
# logically a canopy level trait but was measured at time of burning. We need
# this to determine the canopy density (Total mass/volume) since a tiny part of
# the sample separated from the sample to determine the moisture content.So the
# total mass is the addition of pre.mass and the mass of tiny part that used
# for measuring moisture content.

canopy_measurements <- left_join(canopy_measurements,
                                 dplyr::select(burn_trials, sample_id, mass.pre))

# Calculate derived canopy traits. For volume, we assume the canopy is a
# cylinder whose diameter and height are the average width and the length of the
# branch respectively. Clean up the data frame by removing the non metric
# measurements. Remove mass.pre since we get it with the burn trial data in the
# big merge later.

canopy_measurements <- canopy_measurements %>%
  mutate(average_width_cm = 2.54*(bottom_width_in +
                               middle_width_in +
                               top_width_in) / 3,
         canopy_volume_cm3 = pi * (average_width_cm/2)^2 * length_cm,
         moisture_content = ((fresh_mass_g - dry_mass_g) / dry_mass_g) * 100,
         # total mass should be dry mass equivalent (estimated)
         total_mass_g = (fresh_mass_g + mass.pre) * dry_mass_g/fresh_mass_g,
         canopy_density = total_mass_g / canopy_volume_cm3) %>%
  dplyr::select(-bottom_width_in, -middle_width_in, -top_width_in, -mass.pre)


###############################################################################
## Leaf level traits
# Juniperus leaves are needle shape and measured their one side projected
# leaf area, that's why multiplying them by pi since we assuemed the needles
# are cylindrical shape.
###############################################################################

leaf_measurements$leaf_area_cm2[leaf_measurements$sample_id == "ED07"] <- 99.61 
# The value of ED07 was .61 instead of actual value 99.61 which
# I found when I created plot the relationship between leaf_area_per_leaflet
# and PC1 among groups (codes in exploratory.figures.R)


leaf_measurements <- leaf_measurements%>% # Multiplying the juniperus
  # leaf area by pi
  mutate(leaf_area_cm2=ifelse(species_id %in% c("2011","1022"), leaf_area_cm2*pi,leaf_area_cm2))

leaf_measurements <- leaf_measurements %>%
  mutate(leaf_mass_area = lma_dry_mass_g / leaf_area_cm2)%>%
  mutate(leaf_area_per_leaflet = leaf_area_cm2/number_of_leaflet)


####################################################################################
# Calculating leaf area per (Surface area of cylinder) needle for Juniperus species
# where each needle assumed to be a cylinder whose length 
# is the height and diameter is diameter respectively.
# The diameter measured in mm and converted to cm.
#####################################################################################

juniperus_leaf_area <- juniperus_leaf_area%>%
  mutate(diameter_mm = diameter_mm*0.1)%>% # converting mm to cm
  mutate(radius = diameter_mm/2)%>%
  mutate(leaf_area_per_leaflet = 2*pi*radius*(radius+length_cm))%>%
  group_by(sample_id)%>%
  summarise(leaf_area_per_leaflet = mean(leaf_area_per_leaflet))


# Assigning leaf area of Juniperus species in leaf_measurements

leaf_measurement <- leaf_measurements%>%
  filter(sample_id  %in% juniperus_leaf_area$sample_id )%>% #grabbing only juniperus
  dplyr::select(-leaf_area_per_leaflet)%>% # Removing leaf_area_per_leaflet since they are empty
  left_join(juniperus_leaf_area, by = "sample_id")%>% # Merging with juniperus_leaf_area
  rbind(filter(leaf_measurements,
               !sample_id  %in% juniperus_leaf_area$sample_id)) # Grabbing samples except Juniperus
# and attaching with only Juniperus samples by rbind
  


###############################################################################
## Burning trials
# Measurements of burning trials. Determining temperature difference of discs,
# and mass consumed during burning.
###############################################################################

burn_trials <- burn_trials %>%
  mutate(burn.date = mdy(burn.date),
         heat1 = (temp.d1.post - temp.d1.pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (temp.d2.post - temp.d2.pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_J = (heat1+heat2)/2.0, # For when we want one value
         massconsumed = (mass.pre-mass.post) / mass.pre) %>%
  dplyr::select(-temp.d1.post, -temp.d1.pre, -temp.d2.post, -temp.d1.pre)


###############################################################################
## Merge all the data
# The first nine  trails removed from the analysis since the drying period was
# tweenty four hours for those samples and drying period for rest of all the samples
# were thirty six hours.
# Since I am gooing to combine both years data set, I am eliminitating the
# samples which didn't get ignited in 10 seconds since in 2022 the blowtorch
# was on until a sample got ignited.
###############################################################################

alldata <- left_join(samples, canopy_measurements) %>%
  left_join(leaf_measurement) %>%
  left_join(burn_trials) %>%
  mutate(species = paste(genus, specific_epithet),
         display_name = paste(substr(genus, 1, 1), ". ", specific_epithet, sep=""))%>%
  mutate(ignition=ifelse(flame.dur==0,0,1)) %>%
  filter(ignition == 1) %>%
  filter(! trial %in% 1:9) %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  rename(site = property) %>%
  filter(! display_name %in% c("C. canadensis",
                               "C. obovatus", "Y. rupicola",
                               "P. remota")) # only one sample per
# species was collected for those species and didn't collect
# neither of thyem in 2021

dim(alldata) # 94, after removing first nine trials
# and those samples didn't get ignited

## DWS: if we need to filter out short branches, that can be done here.


#####################################################################
## clean up work space, export two data frame, alldata and 
# samples_more_than_three
#####################################################################

rm(canopy_measurements, leaf_measurements,leaf_measurement, burn_trials, 
   samples,juniperus_leaf_area)

