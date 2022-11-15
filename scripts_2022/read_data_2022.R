#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud

# Read all data files, clean and merge

library(readr)
library(dplyr)
library(lubridate) 


# Xiulin Gao used 0.921 J/g as the Specific heat of the aluminum alloy of which our
# discs are made. From the grass experiments we had 52.91g and 53.21g which 
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

samples_2022 <- read_csv("../data/year_2022/samples_2022.csv")
canopy_measurements_2022 <- read_csv("../data/year_2022/canopy_measurements_2022.csv")
leaf_measurements_2022 <- read_csv("../data/year_2022/leaf_measurements_2022.csv")
moisture_measurements_2022 <- read_csv("../data/year_2022/moisture_content_2022.csv")
burn_trials_2022 <- read_csv("../data/year_2022/flam_trials_2022.csv")
juniperus_leaf_area_2022 <- read_csv("../data/year_2022/juniperus_leaf_area_2022.csv")
species_table_2022 <- read_csv("../data/year_2022/species_table_2022.csv")


###############################################################################
## Calculations and cleaning
###############################################################################

###############################################################################
## merging samples data and species data
###############################################################################

samples_2022 <- species_table_2022%>%
  select(species, species_id, group, herbivore_defence)%>%
  right_join(samples_2022, by=c("species","species_id"))%>%
  select(-notes)

dim(samples_2022)

any(is.na(samples_2022))

###############################################################################

## Samples (one row per individual plant with collection data)

###############################################################################

samples_2022 <- tidyr::separate(samples_2022, species, into = c("genus","specific_epithet"),
                           sep = " ")

class(samples_2022$species_id)

class(samples_2022$sample_id)

dim(samples_2022)

###############################################################################
# Moisture measurements
###############################################################################

# Field moisture content which is measured from the separate samples from same 
# plants during collection. Both twig and leaf were collected in the
# samples.The leaf and canopy moisture content are sub samples of few leaves and twig with few 
# leaves separated from the burning samples right before burning. All the moisture content
# measurements calculated based on dry mass as percentage.

# Field moisture content from the first field trip was biased since the paper towel were
# heavily soaked.

moisture_measurements_2022 <- moisture_measurements_2022%>%
  mutate(field_moisture_content = ((field_fresh_mass_gm - field_dry_mass_gm)/field_dry_mass_gm)*100,
         canopy_moisture_content = ((canopy_fresh_mass_gm - canopy_dry_mass_gm)/canopy_dry_mass_gm)*100,
         leaf_moisture_content = ((leaf_fresh_mass_gm - leaf_dry_mass_gm)/leaf_dry_mass_gm)*100) %>%
  select(-field_fresh_mass_gm, -field_dry_mass_gm,
         -leaf_fresh_mass_gm) # Keeping the canopy_fresh_mass and dry_mass and leaf_dry_mass
# because I will need them to calculate the total weight of the burning sample in dry basis and the whole weight
# of burning samples.

dim(moisture_measurements_2022)


#####################################################################################
# Merging moisture_measurements_2022 with samples_2022
# Getting mass_pre(total mass of burning samples from burn_trials)
# in order to calculate the dry mass of burning samples based on the ratio of the 
# of the dry mass and fresh mass of canopy (twig with leaves) which used to measure
# the canopy moisture content and measured before burning. To do that we multiplied
# the total mass by the ratio of canopy_dry_mass_gm and canopy_fresh_mass_gm
# which was the dry and fresh weight of the twig with few leaves separated
# from each burning samples right before burning.
#####################################################################################


samples_2022 <- left_join(samples_2022, moisture_measurements_2022,
                          by = c("sample_id", "species_id")) %>%
  left_join(select(burn_trials_2022, sample_id, mass_pre)) %>%
  mutate(field_moisture_content = ifelse(site == "Edwards 2020-22", NA, field_moisture_content)) # Replacing field moisture content
# value of the first field trip with NA since samples for measuring the Field moisture content from the first field trip was biased since the paper towel were heavily soaked.

  
class(samples_2022$sample_id)

class(samples_2022$mass_pre)

dim(samples_2022)


any(is.na(moisture_measurements_2022$canopy_moisture_content))

any(is.na(moisture_measurements_2022$leaf_moisture_content))

any(is.na(moisture_measurements_2022$field_moisture_content)) # TRUE, since few of the
# samples don't have their field moisture content because didn't measure the field moisture content
# for those samples which were leaked or found that the paper towel
# are heavily soaked (except the first first field trip).


range(samples_2022$canopy_moisture_content)

range(samples_2022$leaf_moisture_content)  

range(samples_2022$field_moisture_content, na.rm = TRUE)

####################################################################################################################
# Calculating the total dry mass of burning samples
###################################################################################################################

samples_2022 <- samples_2022 %>%
  mutate(total_dry_mass_gm =mass_pre*(canopy_dry_mass_gm/canopy_fresh_mass_gm)) %>%
  select(-mass_pre, -canopy_fresh_mass_gm)


dim(samples_2022)

########################################################################################################

#############################################################################
# Canopy measurements
#############################################################################

# Calculating the leaf stem mass ratio of the unburned samples in terms of dry weight.
# Calculating the canopy volume based on their apparent shape(truncated cone,
# two truncated cone and cylinder).

# Calculating the canopy volume of the samples which has cylindrical shaped
# canopy first.

pi <- 3.1416

cylindrical_shaped <- canopy_measurements_2022 %>%
  filter( type == "cylinder") %>%
  mutate(average_diameter = (bottom_diameter_cm + top_diameter_cm + maximum_diameter)/3,
         average_radius = average_diameter/2,
         canopy_volume_cm3 = pi*average_radius^2*70) %>%  #pi*r^2*h
  select(- average_diameter, -average_radius)
  
names(cylindrical_shaped)

dim(cylindrical_shaped) # only two


##################################################################################################################
# Only the sample id DC39, a ziziphus sample which is showing really high density, decided to calculate it's
# canopy density alone
###################################################################################################################

ziziphus_CD39 <- canopy_measurements_2022 %>%
  filter( sample_id == "DC39") %>%
  mutate(bottom_radius = bottom_diameter_cm/2, # Calculating the radius out of diameter
         top_radius = top_diameter_cm/2,
         max_radius = maximum_diameter/2) %>%
  mutate(canopy_volume_cm3 =  (1/3)*pi*distance_from_bottom_cm*(bottom_radius^2 + bottom_radius*max_radius + max_radius^2) + # volume of two truncated cone
           (1/3)*pi*(70-distance_from_bottom_cm)*(top_radius^2 + top_radius*max_radius + max_radius^2)) %>%
  mutate(total_mass_gm_paired_branch = dry_leaf_weight_gm + dry_stem_weight_gm, # total dry mass of unburned samples
         leaf_stem_mass_ratio = dry_leaf_weight_gm/dry_stem_weight_gm) %>%
  select(sample_id,species_id, total_mass_gm_paired_branch, leaf_stem_mass_ratio, canopy_volume_cm3)


####################################################################################################################
# Sample those have either truncated cone or combination of two truncated cone
#####################################################################################################################

canopy_measurements_2022 <- canopy_measurements_2022 %>%
  filter(type != "cylinder") %>%
  filter(sample_id != "DC39") %>%
  mutate(distance_from_bottom_cm = ifelse(sample_id == "UV44", 42, distance_from_bottom_cm)) %>% # Forget to put the value of distance_from_the_bottom_cm
  #for sample_id UV44 in excel file.
  mutate(type = ifelse(type == "two_right_cone", "two_truncated_cone", type))  %>% # changing the name from two_right_cone to two_truncated_cone.
  mutate(bottom_radius = bottom_diameter_cm/2, # Calculating the radius out of diameter
         top_radius = top_diameter_cm/2,
         max_radius = maximum_diameter/2) %>%
  mutate(canopy_volume_cm3 = ifelse(type == "truncated_cone", (1/3)*pi*70*(bottom_radius^2 + bottom_radius*top_radius + top_radius^2), # volume of truncated cone 1/3*pi*h(r1^2 + r1*r2 + r2^2)
                                        (1/3)*pi*distance_from_bottom_cm*(bottom_radius^2 + bottom_radius*max_radius + max_radius^2) + # volume of two truncated cone
                                          (1/3)*pi*(70-distance_from_bottom_cm)*(top_radius^2 + top_radius*max_radius + max_radius^2))) %>%
  select(- bottom_radius, -top_radius, - max_radius) %>%
  rbind(cylindrical_shaped) %>% # binded with cylindrical shaped, Is that ok?
  mutate(total_mass_gm_paired_branch = dry_leaf_weight_gm + dry_stem_weight_gm, # total dry mass of unburned samples
         leaf_stem_mass_ratio = dry_leaf_weight_gm/dry_stem_weight_gm) %>%
  select(sample_id,species_id, total_mass_gm_paired_branch, leaf_stem_mass_ratio, canopy_volume_cm3) %>%
  rbind(ziziphus_CD39) # binded by ziziphus DC39
  
dim(canopy_measurements_2022)

any(is.na(canopy_measurements_2022$leaf_stem_mass_ratio))

any(is.na(canopy_measurements_2022$canopy_volume_cm3))

##############################################################################################################################################
# Merging samples_2022 with canopy_measurements_2022 and calculating canopy density
##############################################################################################################################################

samples_2022 <- left_join(samples_2022, canopy_measurements_2022, by = c("species_id", "sample_id"))

dim(samples_2022)

# Calculating canopy density

samples_2022 <- samples_2022 %>%
  mutate(canopy_density_gm_cm3 = (total_dry_mass_gm + canopy_dry_mass_gm + leaf_dry_mass_gm) /canopy_volume_cm3) %>% # Added the dry weight of the few 
  # leaves and a twig with few leaves with total_dry_mass_gm since those were separated from the burning samples right before burning to calculate the 
  # leaf and canopy moisture content.
  select(- canopy_volume_cm3, - canopy_dry_mass_gm, leaf_dry_mass_gm) # Removing canopy_volume_cm3

dim(samples_2022)

any(is.na(samples_2022$leaf_stem_mass_ratio))

any(is.na(samples_2022$canopy_density_gm_cm3))

###########################################################################################################################
# Leaf measurements, calculating LMA and leaf area and leaf length per leaflet
###########################################################################################################################

# Calculating leaf area, average leaf length of five leaves (few samples have three leaves)
# of each samples which was separated from the subsamples (twig+leaves) which used to measure the field moisture content.
# Calculating mean leaf length of five leaves (few samples have three leaves) of each samples by apply() function

##########################################################################################################################################
# The length of the leaflet measured by a meter scale where there is 0.4 mm gap between the starting point of the scale and zero and 
# I measured leaflet length from the starting point of the scale not from zero for all the species except senegalia wrightii. That's why
# I am adding 0.04 cm to all the leaflet length to all the species except Senegalia wrightii.
########################################################################################################################################

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

dim(leaf_measurements_2022)

########################################################################################################################################
# Average length of Leaflet.
########################################################################################################################################

leaf_measurements_2022$average_leaf_length_cm <- apply(leaf_measurements_2022[,7:11], 1, mean, na.rm = TRUE) # Average leaf length of five individual leaflet
# some samples has leaf length of three leaves that's why na.rm was used!!!!

leaf_measurements_2022 <- leaf_measurements_2022 %>%
  select(-leaf1_length_cm, -leaf2_length_cm, -leaf3_length_cm, -leaf4_length_cm,
         -leaf5_length_cm) # Removing leaf length of individual leaf length since we already got the average


dim(leaf_measurements_2022)

any(is.na(leaf_measurements_2022$average_leaf_length_cm))

names(leaf_measurements_2022)

#########################################################################################################################
# Calculating the LMA and leaf area of Juniperus species
#########################################################################################################################
# I calculated the leaf area of juniperus species manually. I assumed the individual brnachlets
# of Juniperus is  cylinder in shape. The diameter and leaf length is measured by the slide
# calipers and meter scales respectively and calculated the surface area according to the 
# formula of cylinder where the breadth and length of individual branchlets are diameter
# and height of a cylinder respectively.


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

#########################################################################################################################################
# Getting only Juniperus from leaf_measurements_2022
#########################################################################################################################################

only_juniperus <- leaf_measurements_2022 %>%
  filter(species_id %in% c("1022", "2011", "9000")) %>%
  select(-leaf_area_cm2) %>% # dropping leaf_area_cm2 since I already calculated it in
# juniperus_leaf_area_2022
  full_join(juniperus_leaf_area_2022[-311, ], by = c("sample_id")) %>%
  mutate(leaf_mass_per_area = lma_dry_mass_gm/leaf_area_cm2)

dim(only_juniperus)

#########################################################################################################################################
# Getting all the species except Juniperus from leaf_measurements_2022
##########################################################################################################################################

without_juniperus <- leaf_measurements_2022 %>%
  filter(!species_id %in% c("1022","2011", "9000") ) %>% # ashei, pinchotii, virginiana
  mutate(leaf_mass_per_area = lma_dry_mass_gm/leaf_area_cm2, # lma, totla dry mass by total leaf area.
         leaf_area_per_leaflet = leaf_area_cm2/number_of_leaflet) #  The number of leaflet is not
# fixed though most of the samples have five leaflet.


dim(without_juniperus)



#######################################################################################################################################
# Making only_juniperus and without_juniperus as a single data frame
#######################################################################################################################################

leaf_measurements_2022 <- without_juniperus %>%
  rbind(only_juniperus) %>%
  rename(leaf_length_per_leaflet = average_leaf_length_cm) %>% # renaming the average leaf_length_cm to 
  # leaf_length_per_leaflet
  select(-lma_fresh_mass_gm, -lma_dry_mass_gm, -leaf_area_cm2, number_of_leaflet)

dim(leaf_measurements_2022)

any(is.na(leaf_measurements_2022$leaf_length_per_leaflet))

any(is.na(leaf_measurements_2022$leaf_area_per_leaflet))

any(is.na(leaf_measurements_2022$leaf_mass_per_area))

#senegalia_outlier <- leaf_measurements_2022 %>%
#filter(species_id == 2005)

#View(senegalia_outlier), The sample KD18 is showing exceptionally
# high LMA value, the reason is the number of leaflet (70) and they are tiny,
# something went wrong with them during leaf area measurements, decided to drop
# the sample.



############################################################################################################
# Measurements of burning trials
# Calculating the temperature difference of disks and massconsumed during burning
#############################################################################################################

burn_trials_2022 <- burn_trials_2022 %>%
  mutate(burn_date = mdy(date),
         heat1 = (temp_d1_post - temp_d1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (temp_d2_post - temp_d2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         massconsumed = (mass_pre - mass_post)) %>%
  select(-temp_d1_post, -temp_d2_post, -date)


dim(burn_trials_2022)

####################################################################################
# Merging all data
####################################################################################

alldata_2022 <- samples_2022 %>%
  left_join(leaf_measurements_2022, by = c("sample_id","species_id")) %>%
  left_join(burn_trials_2022, by = c("sample_id", "species_id")) %>%
  filter( ! species_id %in% c(2006,1085,1009,3000,2022, 2038), # Dropping those species which has less than three samples!!
          ! sample_id %in% c("KD07", "UV01")) %>%  # KD07 has missing values of flammability measurements and mistakenly, UV01 has ignited with blow torch though 
     # it has self ignition and has existing flame(descriptions on notes in flam_trials_2022,csv.)
   mutate(air_temp_f = ifelse(sample_id == "UV16", NA, air_temp_f)) # The air temperature for 
# UV16 is 40.3 which is a mistake during data entry.

dim(alldata_2022)

any(is.na(alldata_2022$canopy_density_gm_cm3))

any(is.na(alldata_2022$leaf_area_per_leaflet))

any(is.na(alldata_2022$leaf_length_per_leaflet))

any(is.na(alldata_2022$leaf_mass_per_area))

any(is.na(alldata_2022$leaf_stem_mass_ratio))

any(is.na(alldata_2022$canopy_moisture_content))

any(is.na(alldata_2022$leaf_moisture_content))

##################################################################################
# The flame height is in mili meter but I changed the unit to cm
# manually during data entry but for some samples the unit remain
# unchanged, I was in rush to push the data into github, A big mistake
# Fixing them one by one from the data sheet
#######################################################################


alldata_2022 <- alldata_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DC29", 115, flame_height)) # It is 115 actually, not 150

alldata_2022 <- alldata_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DK49", 35, flame_height))


alldata_2022 <- alldata_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DV05", 150, flame_height))

alldata_2022 <- alldata_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DV06", 130, flame_height))


##########################################################################
# All the flame height is in mili meter from 08/14/2022 date
##########################################################################

alldata_2022$burn_date <- as.character(alldata_2022$burn_date) # Changing the date to character


alldata_2022 <- alldata_2022 %>%
  mutate( flame_height = ifelse(burn_date == "2022-08-14", flame_height/10, flame_height))


alldata_2022$burn_date <- as.Date(alldata_2022$burn_date) # Now as Date



######################################################################################
# Cleaning up work space, only keeping the alldata_2022
######################################################################################

rm(samples_2022, canopy_measurements_2022,leaf_measurements_2022, moisture_measurements_2022,
   burn_trials_2022, juniperus_leaf_area_2022, species_table_2022)

