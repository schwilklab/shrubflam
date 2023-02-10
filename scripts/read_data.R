# read-data.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud

# Read all data files, clean and merge

library(readr)
library(dplyr)
library(lubridate) 

###############################################################################
# Constants
###############################################################################

# Xiulin Gao used 0.921 J/g as the Specific heat of the aluminum alloy of which
# our discs are made. From the grass experiments we had 52.91g and 53.21g which
# are the weight of Disc one and Disc two respectively(Gao and Schwilk, 2021).
# The gas flow from the Blue Rhino gas cylinder was 20.35 gram per minute.

SPECIFIC_HEAT_AL = 0.921 # in J/g
MASS_DISK_1 = 52.91  # 
MASS_DISK_2 = 53.21 # g

###############################################################################
## Read all the data files
###############################################################################

## We need to read them all in first before cleaning because some calculations
## require columns from multiple files

species <- read_csv("./data/species.csv") %>%
  mutate(display_name = paste(substr(genus, 1,1), ". ", specific_epithet, sep=""))
## Display name could be function because unknown species sould really be
## something like "Senegalia spp". But fine for now because I'm not sure this
## will even be used.





samples_2022 <- read_csv("./data/year_2022/samples_2022.csv")
canopy_measurements_2022 <- read_csv("./data/year_2022/canopy_measurements_2022.csv")
leaf_measurements_2022 <- read_csv("./data/year_2022/leaf_measurements_2022.csv")
moisture_measurements_2022 <- read_csv("./data/year_2022/moisture_content_2022.csv")
burn_trials_2022 <- read_csv("./data/year_2022/flam_trials_2022.csv")
juniperus_leaf_area_2022 <- read_csv("./data/year_2022/juniperus_leaf_area_2022.csv")


###############################################################################
## Calculations and cleaning
###############################################################################

###############################################################################
## merging samples data and species data
###############################################################################

# The number of samples is 139, to make things simpler I am filtering only
# those species which were collected this year from the species data set
# (second line before right join). Since there are some species there which
# were not collected this year. May be removing from the csv file is a better
# option but it's just adding a single line of code.

#############################################################################

samples_2022 <- species %>%
  filter(species %in% samples_2022$field_taxon) %>% 
  select(species_id, genus, specific_epithet, display_name, analysis_group,
         herbivore_defense, herbivore_preference) %>% 
  right_join(samples_2022, by = "species_id" ) %>%
  select(-notes)

## DWS: Note: herbivore_preference column not found anywhere. Original script
## broke here anyway.
## AM: got the herbivore_preference, fixed. 

names(samples_2022)

dim(samples_2022) # 139 samples.
class(samples_2022$species_id)
class(samples_2022$sample_id)
any(is.na(samples_2022))
# TRUE.


###############################################################################
# Removing species_id from samples_2022, canopy_measurements_2022,
# burn_trials_2022 and moisture_measurements_2022 because I will merge them later 
# only by by sample_id. However,
# I didn't remove species_id from the leaf measurements because
# I used it later to filter particular species to calculate
# the leaf traits and removing species_id will make things
# complicated later.

## DWS: You don't understand what I am talking about. You are storing the same
## data in multiple places, that is the issue. If you need species id, it is a
## single step to merge so I do not agree with your rebuttal.


################################################################################

samples_2022 <- select(samples_2022, -species_id)
canopy_measurements_2022 <- select(canopy_measurements_2022, -species_id)
burn_trials_2022 <- select(burn_trials_2022, -species_id)
moisture_measurements_2022 <- select(moisture_measurements_2022, -species_id)


###############################################################################
# Moisture measurements
###############################################################################

# Field moisture content which is measured from the separate samples from same
# plants during collection. Both twig and leaf were collected in the
# samples.The leaf and canopy moisture content are sub samples of few leaves
# and twig with few leaves separated from the burning samples right before
# burning. All the moisture content measurements calculated based on dry mass
# as percentage.

# Field moisture content from the first field trip was biased since the paper
# towel were heavily soaked.

moisture_measurements_2022 <- moisture_measurements_2022 %>%
  mutate(field_moisture_content = ((field_fresh_mass_gm - field_dry_mass_gm)/field_dry_mass_gm)*100,
         canopy_moisture_content = ((canopy_fresh_mass_gm - canopy_dry_mass_gm)/canopy_dry_mass_gm)*100,
         leaf_moisture_content = ((leaf_fresh_mass_gm - leaf_dry_mass_gm)/leaf_dry_mass_gm)*100) %>%
  select(-field_fresh_mass_gm, -field_dry_mass_gm,
         -leaf_fresh_mass_gm)

# Keeping the canopy_fresh_mass and dry_mass and leaf_dry_mass because I will
# need them to calculate the total weight of the burning sample in dry basis
# and the whole weight of burning samples.

dim(moisture_measurements_2022)


###############################################################################
# Merge fuel moisture data
###############################################################################

# Merging moisture_measurements_2022 with samples_2022 Getting mass_pre(total
# mass of burning samples from burn_trials) in order to calculate the dry mass
# of burning samples based on the ratio of the of the dry mass and fresh mass
# of canopy (twig with leaves) which used to measure the canopy moisture
# content and measured before burning. To do that we multiplied the total mass
# by the ratio of canopy_dry_mass_gm and canopy_fresh_mass_gm which was the dry
# and fresh weight of the twig with few leaves separated from each burning
# samples right before burning.


samples_2022 <- left_join(samples_2022, moisture_measurements_2022, 
                          by = c("sample_id")) %>%  #"species_id"  should only join by sample id, right? AM: Yes.fixed it
  left_join(select(burn_trials_2022, sample_id, mass_pre)) %>%
  mutate(field_moisture_content = ifelse(site == "Edwards 2020-22", NA, field_moisture_content)) # Replacing field moisture content
# value of the first field trip with NA since samples for measuring the Field moisture content from the first field trip was biased since the paper towel were heavily soaked.

  
class(samples_2022$sample_id)
class(samples_2022$mass_pre)
dim(samples_2022) # 139


any(is.na(moisture_measurements_2022$canopy_moisture_content))
any(is.na(moisture_measurements_2022$leaf_moisture_content))
any(is.na(moisture_measurements_2022$field_moisture_content))
# Last is TRUE because a few of the samples don't have their field moisture
# content because didn't measure the field moisture content for those samples
# which were leaked or found that the paper towel are heavily soaked (except
# the first first field trip).

range(samples_2022$canopy_moisture_content)
range(samples_2022$leaf_moisture_content)  
range(samples_2022$field_moisture_content, na.rm = TRUE)

###############################################################################
# Calculating the total dry mass of burning samples
###############################################################################

samples_2022 <- samples_2022 %>%
  mutate(total_dry_mass_gm =mass_pre*(canopy_dry_mass_gm/canopy_fresh_mass_gm)) %>%
  select(-mass_pre, -canopy_fresh_mass_gm)

dim(samples_2022) # 139


#############################################################################
# Canopy measurements
#############################################################################

# Calculating the leaf stem mass ratio of the unburned samples in terms of dry weight.
# Calculating the canopy volume based on their apparent shape(truncated cone,
# two truncated cone and cylinder).

# Calculating the canopy volume of the samples which has cylindrical shaped
# canopy first.

# pi <- 3.1416 ## DWS: Why would you overwrite the built in pi?

cylindrical_shaped <- canopy_measurements_2022 %>%
  filter( type == "cylinder") %>%
  mutate(average_diameter = (bottom_diameter_cm + top_diameter_cm + maximum_diameter)/3,
         average_radius = average_diameter/2,
         canopy_volume_cm3 = 3.1416*average_radius^2*70) %>%  #pi*r^2*h
  select(- average_diameter, -average_radius)
  
names(cylindrical_shaped)
dim(cylindrical_shaped) # only two


###############################################################################
# Only the sample id DC39, a ziziphus sample which is showing really high
# density, decided to calculate it's canopy density alone
###############################################################################

## DWS: This seems fishy. What is going on? 
## AM: The reason for doing that was: high value of canopy density of 
## one of the samples from Ziziphus made me to think that probably the entire
## calculation was wrong in the code, therefore I calculated the canopy density
## of that sample separately and checked it manually to confirm
## the calculation. It just added one more step and may be that's not a 
## good practice.

ziziphus_CD39 <- canopy_measurements_2022 %>%
  filter( sample_id == "DC39") %>%
  mutate(bottom_radius = bottom_diameter_cm/2, # Calculating the radius out of diameter
         top_radius = top_diameter_cm/2,
         max_radius = maximum_diameter/2) %>%
  mutate(canopy_volume_cm3 =  (1/3)*3.1416*distance_from_bottom_cm*(bottom_radius^2 + bottom_radius*max_radius + max_radius^2) + # volume of two truncated cone
           (1/3)*3.1416*(70-distance_from_bottom_cm)*(top_radius^2 + top_radius*max_radius + max_radius^2)) %>%
  mutate(total_mass_gm_paired_branch = dry_leaf_weight_gm + dry_stem_weight_gm, # total dry mass of unburned samples
         leaf_stem_mass_ratio = dry_leaf_weight_gm/dry_stem_weight_gm) %>%
  select(sample_id, total_mass_gm_paired_branch, leaf_stem_mass_ratio, canopy_volume_cm3)


###############################################################################
# Sample those have either truncated cone or combination of two truncated cone
###############################################################################

canopy_measurements_2022 <- canopy_measurements_2022 %>%
  filter(type != "cylinder") %>%
  filter(sample_id != "DC39") %>%
  mutate(distance_from_bottom_cm = ifelse(sample_id == "UV44", 42, distance_from_bottom_cm)) %>% # Forget to put the value of distance_from_the_bottom_cm
  #for sample_id UV44 in excel file.
  mutate(type = ifelse(type == "two_right_cone", "two_truncated_cone", type))  %>% # changing the name from two_right_cone to two_truncated_cone.
  mutate(bottom_radius = bottom_diameter_cm/2, # Calculating the radius out of diameter
         top_radius = top_diameter_cm/2,
         max_radius = maximum_diameter/2) %>%
  mutate(canopy_volume_cm3 = ifelse(type == "truncated_cone", (1/3)*3.1416*70*(bottom_radius^2 + bottom_radius*top_radius + top_radius^2), # volume of truncated cone 1/3*pi*h(r1^2 + r1*r2 + r2^2)
                                        (1/3)*3.1416*distance_from_bottom_cm*(bottom_radius^2 + bottom_radius*max_radius + max_radius^2) + # volume of two truncated cone
                                          (1/3)*3.1416*(70-distance_from_bottom_cm)*(top_radius^2 + top_radius*max_radius + max_radius^2))) %>%
  select(- bottom_radius, -top_radius, - max_radius) %>%
  rbind(cylindrical_shaped) %>% # binded with cylindrical shaped, Is that ok?
  mutate(total_mass_gm_paired_branch = dry_leaf_weight_gm + dry_stem_weight_gm, # total dry mass of unburned samples
         leaf_stem_mass_ratio = dry_leaf_weight_gm/dry_stem_weight_gm) %>%
  select(sample_id, total_mass_gm_paired_branch, leaf_stem_mass_ratio, canopy_volume_cm3) %>%
  rbind(ziziphus_CD39) # binded by ziziphus DC39
  
dim(canopy_measurements_2022) # 139
any(is.na(canopy_measurements_2022$leaf_stem_mass_ratio)) # FALSE
any(is.na(canopy_measurements_2022$canopy_volume_cm3)) # FALSE

###############################################################################
# Merging samples_2022 with canopy_measurements_2022 and calculating canopy
# density
###############################################################################

samples_2022 <- left_join(samples_2022, canopy_measurements_2022, by = c("sample_id"))

## DWS: You are storing species id all over the place! Seems like recipe for
## errors
## AM: fixed it

dim(samples_2022) # 139

# Calculating canopy density

samples_2022 <- samples_2022 %>%
  mutate(canopy_density_gm_cm3 = (total_dry_mass_gm + canopy_dry_mass_gm + leaf_dry_mass_gm) /canopy_volume_cm3) %>% # Added
  # the dry weight of the few leaves and a twig with few leaves with
  # total_dry_mass_gm since those were separated from the burning samples right
  # before burning to calculate the leaf and canopy moisture content.
  select(- canopy_volume_cm3, - canopy_dry_mass_gm, leaf_dry_mass_gm) # Removing canopy_volume_cm3

dim(samples_2022) #139
any(is.na(samples_2022$leaf_stem_mass_ratio)) #FALSE
any(is.na(samples_2022$canopy_density_gm_cm3)) #FALSE

###############################################################################
# Leaf measurements, calculating LMA and leaf area and leaf length per leaflet
###############################################################################

# Calculating leaf area, average leaf length of five leaves (few samples have
# three leaves) of each samples which was separated from the subsamples
# (twig+leaves) which used to measure the field moisture content. Calculating
# mean leaf length of five leaves (few samples have three leaves) of each
# samples by apply() function

###############################################################################
# The length of the leaflet measured by a meter scale where there is 0.4 mm gap
# between the starting point of the scale and zero and I measured leaflet
# length from the starting point of the scale not from zero for all the species
# except senegalia wrightii. That's why I am adding 0.04 cm to all the leaflet
# length to all the species except Senegalia wrightii.
###############################################################################

names(leaf_measurements_2022)

senegalia_leaf_measurements <- leaf_measurements_2022 %>%
  filter(species_id == 2005) # Species_id of Senegalia wrightii.

dim(senegalia_leaf_measurements)

without_senegalia <- leaf_measurements_2022 %>%
  filter( species_id != 2005) %>%
  mutate( leaf1_length_cm = leaf1_length_cm + 0.04 ) %>%
  mutate( leaf2_length_cm = leaf2_length_cm + 0.04 ) %>%
  mutate( leaf3_length_cm = leaf3_length_cm + 0.04 ) %>%
  mutate( leaf4_length_cm = leaf4_length_cm + 0.04 ) %>%
  mutate( leaf5_length_cm = leaf5_length_cm + 0.04)

dim(without_senegalia)

leaf_measurements_2022 <- without_senegalia %>%
  rbind(senegalia_leaf_measurements)

dim(leaf_measurements_2022) #139

###############################################################################
# Average length of Leaflet.
###############################################################################

leaf_measurements_2022$average_leaf_length_cm <- apply(leaf_measurements_2022[,7:11], 1, mean, na.rm = TRUE) # Average leaf length of five individual leaflet

# some samples have leaf length of three leaves that's why na.rm was used.

leaf_measurements_2022 <- leaf_measurements_2022 %>%
  select(-leaf1_length_cm, -leaf2_length_cm, -leaf3_length_cm, -leaf4_length_cm,
         -leaf5_length_cm) # Removing leaf length of individual leaf length since we already got the average


dim(leaf_measurements_2022) #139

any(is.na(leaf_measurements_2022$average_leaf_length_cm))

names(leaf_measurements_2022)

###############################################################################
# Calculating the LMA and leaf area of Juniperus species
###############################################################################

# I calculated the leaf area of juniperus species manually. I assumed the
# individual brnachlets of Juniperus is cylinder in shape. The diameter and
# leaf length is measured by the slide calipers and meter scales respectively
# and calculated the surface area according to the formula of cylinder where
# the breadth and length of individual branchlets are diameter and height of a
# cylinder respectively. row 311 is an empty row.


juniperus_leaf_area_2022 <- juniperus_leaf_area_2022[-311, ] %>% 
  mutate(leaf_radius_cm = leaf_diameter_cm/2,  
         leaf_length_cm = leaf_length_cm + 0.04, # Adding the 0.04 same way did with length of individual leaflet.
  # turning diameter into radius
  leaf_area_per_leaflet = 2*pi*leaf_radius_cm*(leaf_radius_cm + leaf_length_cm)) %>% # 2*pi*r(r+h)
  group_by(sample_id) %>%
  summarise(leaf_area_cm2 = sum(leaf_area_per_leaflet), # Total leaf area which will be 
            # used to calculate LMA  
            leaf_area_per_leaflet = mean(leaf_area_per_leaflet))

dim(juniperus_leaf_area_2022)
any(is.na(juniperus_leaf_area_2022$leaf_area_cm2))
any(is.na(juniperus_leaf_area_2022$leaf_area_per_leaflet))

###############################################################################
# Getting only Juniperus from leaf_measurements_2022
###############################################################################


only_juniperus <- leaf_measurements_2022 %>%
  filter(species_id %in% c("1022", "2011", "9000")) %>%
  select(-leaf_area_cm2) %>% # dropping leaf_area_cm2 since I already calculated it in
# juniperus_leaf_area_2022
  full_join(juniperus_leaf_area_2022[-311, ], by = c("sample_id")) %>%
  mutate(leaf_mass_per_area = lma_dry_mass_gm/leaf_area_cm2)

dim(only_juniperus)

###############################################################################
# Getting all the species except Juniperus from leaf_measurements_2022
###############################################################################

without_juniperus <- leaf_measurements_2022 %>%
  filter(!species_id %in% c("1022","2011", "9000") ) %>% # ashei, pinchotii, virginiana
  mutate(leaf_mass_per_area = lma_dry_mass_gm/leaf_area_cm2, # lma, totla dry mass by total leaf area.
         leaf_area_per_leaflet = leaf_area_cm2/number_of_leaflet) #  The number of leaflet is not
# fixed though most of the samples have five leaflet.


dim(without_juniperus)



###############################################################################
# Making only_juniperus and without_juniperus as a single data frame
###############################################################################

leaf_measurements_2022 <- without_juniperus %>%
  rbind(only_juniperus) %>%
  rename(leaf_length_per_leaflet = average_leaf_length_cm) %>% # renaming the average leaf_length_cm to 
  # leaf_length_per_leaflet
  select(-lma_fresh_mass_gm, -lma_dry_mass_gm, -leaf_area_cm2, number_of_leaflet)

dim(leaf_measurements_2022) #139
any(is.na(leaf_measurements_2022$leaf_length_per_leaflet)) #FALSE
any(is.na(leaf_measurements_2022$leaf_area_per_leaflet)) #FALSE
any(is.na(leaf_measurements_2022$leaf_mass_per_area)) #FALSE

#senegalia_outlier <- leaf_measurements_2022 %>%
#filter(species_id == 2005)

# View(senegalia_outlier), The sample KD18 is showing exceptionally high LMA
# value, the reason is the number of leaflet (70) and they are tiny, something
# went wrong with them during leaf area measurements, decided to drop the
# sample.



###############################################################################
# Measurements of burning trials Calculating the temperature difference of
# disks and massconsumed during burning
###############################################################################

burn_trials_2022 <- burn_trials_2022 %>%
  mutate(burn_date = mdy(date),
         heat1 = (temp_d1_post - temp_d1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (temp_d2_post - temp_d2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         massconsumed = (mass_pre - mass_post)) %>%
  select(-temp_d1_post, -temp_d2_post, -date)

dim(burn_trials_2022) # 139


###############################################################################
# Creating a separate dataset which I will use to merge with dataset from 2021
# First, let's get rid of species_id and keeping only sample_id for every merge
# from now on
###############################################################################
leaf_measurements_2022 <- select(leaf_measurements_2022, -species_id)

#burn_trials_2022 <- select(burn_trials_2022, -species_id)

## DWS: That really should be fixed either in data or on first read of the
## data. Why is species id occurring all over?

herbivore_2022 <- samples_2022 %>%
  left_join(leaf_measurements_2022, by = "sample_id") %>%
  left_join(burn_trials_2022, by = "sample_id") %>%
  filter(! sample_id %in% c("KD07", "UV01")) %>%  # KD07 has missing values of
                                                  # flammability measurements
                                                  # and mistakenly, UV01 has
                                                  # ignited with blow torch
                                                  # though it has self ignition
                                                  # and has existing
                                                  # flame(descriptions on notes
                                                  # in flam_trials_2022,csv.)
  mutate(air_temp_f = ifelse(sample_id == "UV16", NA, air_temp_f)) %>%
  mutate(label = paste(sample_id, trials, sep = "_"))


## DWS: Why are you filtering out KD07 and UV01 everywhere. DO THINGS ONE TIME!
## This is so error-prone. AM: The reason for doing this I have only two
## samples with missing values for flammability measurements. And this dataset
## will be used to do the herbivore_analysis. Since outliers of leaf_traits and
## canopy traits doesn't matter for herbivore_analysis I am just simply
## removing two sample whose flammability traits is not usable (please, see
## justification on the codes and notes in the burn_trials.csv)


####################################################################################
# Merging all data
####################################################################################

alldata_2022 <- samples_2022 %>%
  left_join(leaf_measurements_2022, by = c("sample_id")) %>%
  left_join(burn_trials_2022, by = c("sample_id")) %>%
  filter( ! field_taxon %in% c("Rhus microphylla","Arbutus xalapensis" ,"Mimosa borealis",
                               "Unknown spp", "Ulmus crassifolia", "Frangula caroliniana"), # Dropping those species which has less than three samples!!
          ! sample_id %in% c("KD07", "UV01")) %>%  # KD07 has missing values of flammability measurements and mistakenly, UV01 has ignited with blow torch though 
     # it has self ignition and has existing flame(descriptions on notes in flam_trials_2022,csv.)
   mutate(air_temp_f = ifelse(sample_id == "UV16", NA, air_temp_f)) %>%  # The air temperature for  UV16 is 40.3 which is a mistake during data entry.
  filter(! sample_id %in% c("KD18", "DK34", "KD15", "UV04",
                            "DK30")) %>% # outliers in a sense something went wrong during measurement
  # for those samples since I measured some traits for some species manually.
  filter(canopy_density_gm_cm3 < 0.05,
         leaf_length_per_leaflet < 10)

dim(alldata_2022) # 116 , that is is exact number of samples I used all the analysis so far.
any(is.na(alldata_2022$canopy_density_gm_cm3)) # FALSE
any(is.na(alldata_2022$leaf_area_per_leaflet)) #FALSE
any(is.na(alldata_2022$leaf_length_per_leaflet)) # FALSE
any(is.na(alldata_2022$leaf_mass_per_area)) #FALSE
any(is.na(alldata_2022$leaf_stem_mass_ratio)) #FALSE
any(is.na(alldata_2022$canopy_moisture_content)) #FALSE
any(is.na(alldata_2022$leaf_moisture_content)) #FALSE
 



#####################################################################################
# Changing the variable names and creating new label
#####################################################################################

alldata_2022 <- alldata_2022 %>%
  rename(total_dry_mass_g = total_dry_mass_gm) %>%
  mutate(label = paste(sample_id, trials, sep = "_")) %>%
  mutate(mean_pre_burning_temp = (temp_d1_pre + temp_d2_pre)/2)

 
######################################################################################
# Cleaning up work space, only keeping the alldata_2022
######################################################################################

rm(samples_2022, canopy_measurements_2022,leaf_measurements_2022, moisture_measurements_2022,
   burn_trials_2022, juniperus_leaf_area_2022)


