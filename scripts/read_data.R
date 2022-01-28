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
MASS_DISK_1      = 0.53  # g
MASS_DISK_2      = 0.53  # g

###############################################################################
## Read all the data files
###############################################################################

## We need to read them all in first before cleaning because some calculations
## require columns from multiple files
samples <- read_csv("../data/year_2021/samples.csv")
canopy_measurements <- read_csv("../data/year_2021/canopy_measurements.csv")
leaf_measurements <- read_csv("../data/year_2021/leaf_measurements.csv")
burn_trials <- read_csv("../data/year_2021/burn_trials.csv")

###############################################################################
## Calculations and cleaning
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
                                 select(burn_trials, sample_id, mass.pre))

# Calculate derived canopy traits. For volume, we assume the canopy is a
# cylinder whose radius and height are the average width and the length of the
# branch respectively. Clean up the data frame by removing the non metric
# measurements. Remove mass.pre since we get it with the burn trial data in the
# big merge later.
canopy_measurements <- canopy_measurements %>%
  mutate(average_width_cm = 2.54*(bottom_width_in +
                               middle_width_in +
                               top_width_in) / 3,
         canopy_volume_cm3 = pi * (average_width_cm)^2 * length_cm,
         moisture_content = ((fresh_mass_g - dry_mass_g) / dry_mass_g) * 100,
         # total mass should be dry mass equivalent (estimated)
         total_mass_g = (fresh_mass_g + mass.pre) * dry_mass_g/fresh_mass_g,
         canopy_density = total_mass_g / canopy_volume_cm3) %>%
  select(-bottom_width_in, -middle_width_in, -top_width_in, -mass.pre)


###############################################################################
## Leaf level traits

leaf_measurements <- leaf_measurements %>%
  mutate(leaf_mass_area = lma_dry_mass_g / leaf_area_cm2)%>%
  mutate(leaf_area=leaf_area_cm2/number_of_leaf)

###############################################################################
## Burning trials

# Measurements of burning trials. Determining temperature difference of discs,
# and mass consumed during burning.

burn_trials <- burn_trials %>%
  mutate(burn.date = mdy(burn.date),
         heat1 = (temp.d1.post - temp.d1.pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (temp.d2.post - temp.d2.pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_J = (heat1+heat2)/2.0, # For when we want one value
         massconsumed = (mass.pre-mass.post) / mass.pre) %>%
  select(-temp.d1.post, -temp.d1.pre, -temp.d2.post, -temp.d1.pre)


###############################################################################
## Merge all the data
###############################################################################

alldata <- left_join(samples, canopy_measurements) %>%
  left_join(leaf_measurements) %>%
  left_join(burn_trials) %>%
  mutate(species = paste(genus, specific_epithet),
         display_name = paste(substr(genus, 1, 1), ". ", specific_epithet, sep=""))

## DWS: if we need to filter out short branches, that can be done here.

## clean up workspace, export only one dataframe, alldata
rm(canopy_measurements, leaf_measurements, burn_trials, samples)
