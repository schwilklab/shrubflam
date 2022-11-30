#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022

library(ggplot2)

# All scripts in source are required to run before running
# this script.

source("../read_data_2022.R") # script that read, clean and merged all the traits data.
source("../read_hobos_2022.R") # script that reads the thermocouples data logger data during burning.
source("../flam_pca_2022.R") # script that did the pca analysis.

###############################################################################################
# Exploratory figures, the measurements vs species 
###############################################################################################

dim(alldata_2022)

ggplot(alldata_2022, aes(species, total_dry_mass_g)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))


ggplot(alldata_2022, aes(species, leaf_stem_mass_ratio)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))

ggplot(alldata_2022, aes(species, canopy_density_gm_cm3)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold")) # Removing
# the outliers of canopy_density_gm_cm3 of Pinchottii, since I measured
# the canopy_volume manually.

pinchottii_density <- alldata_2022 %>%
  filter(species_id == 2011) %>%
  select(sample_id, canopy_density_gm_cm3)

#View(pinchottii_density) # DK34

ashei_density <- alldata_2022 %>%
  filter(species_id == 1022) %>%
  select(sample_id, canopy_density_gm_cm3)

#View(ashei_density) #KD15

secundiflora_density <- alldata_2022 %>%
  filter(species_id == 1021) %>%
  select(sample_id, canopy_density_gm_cm3)

#View(secundiflora_density) # UV04

ggplot(alldata_2022, aes(species, canopy_moisture_content)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))



ggplot(alldata_2022, aes(species, leaf_mass_per_area)) + # LMA is
  #really high for one of the wrightii, need to check, this is impossible,
  # definitely I messed it up, the number of leaflet is 70!!! and the 
  # leaflets are tiny. Dropping the sample.
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))

# One senegalia is showing exceptionally high LMA, removing it
# One pinchottii is showing really high compared to other sample,
# I did measure their leaf area manually, that's why droping
# the sample

pinchotti_lma <- alldata_2022 %>%
  filter( species_id == 2011) %>%
  select(sample_id, leaf_mass_per_area)

#View(pinchotti_lma) 
#DK30

ggplot(alldata_2022, aes(species, leaf_area_per_leaflet)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))


ggplot(alldata_2022, aes(species, leaf_length_per_leaflet)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))


ggplot(alldata_2022, aes(species, leaf_moisture_content)) +
  geom_point() +
  theme(axis.text.x = element_text( angle = 45, hjust = 1, face = "bold"))


###############################################################################################
# Does leaf_stem_mass_ratio influence flammability?
################################################################################################

ggplot(final_data, aes(leaf_stem_mass_ratio, degsec_100,
                       color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab( "Leaf stem mass ratio (dry basis)" ) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) 

dim(final_data)

#ggplot(final_data, aes(leaf_stem_mass_ratio, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = 'lm', se = FALSE) + 
  #xlab("Leaf stem mass ratio (dry basis)") +
  #ylab(" Flammability score (PC2)")

################################################################################################
# Does canopy density influence flammability?
################################################################################################


ggplot(final_data, aes(canopy_density_gm_cm3, degsec_100, 
                       color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE ) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(canopy_density_gm_cm3, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE ) +
  #xlim(0,0.015) +
  #xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  #ylab("Flammability score (PC2)")


##########################################################################################
# Does total mass influence flammability?
###########################################################################################

ggplot(final_data, aes(total_dry_mass_g, degsec_100, 
                  color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Total dry mass (dry basis)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(total_dry_mass_gm, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlab("Total dry mass (dry basis)") +
  #ylab("Flammability score (PC2)")

############################################################################################
# Does field moisture influence flammability?
#############################################################################################

ggplot(final_data, aes(field_moisture_content, degsec_100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Field moisture content (%)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(field_moisture_content, PC2)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlab("Field moisture content (%)") +
  #ylab("Flammability score (PC2)")

#############################################################################################
# Does leaf moisture influence flammability?
#############################################################################################

ggplot(final_data, aes(leaf_moisture_content, degsec_100,
                       color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Leaf moisture content (%)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(leaf_moisture_content, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlab("Leaf moisture content (%)") +
  #ylab("Flammability score (PC2)")

################################################################################################
# Does canopy moisture influence flammability?
################################################################################################

ggplot(final_data, aes(canopy_moisture_content, degsec_100,
                       color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Canopy moisture content (%)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))
  
#ggplot(final_data, aes(canopy_moisture_content, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlab("Canopy moisture content (%)") +
  #ylab("Flammability score (PC2)")


######################################################################################
# Does LMA influence flammability?
######################################################################################


ggplot(final_data, aes(leaf_mass_per_area, degsec_100, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2,")"))) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))


ggplot(model_data, aes(leaf_mass_per_area, degsec_100, 
                       color = genus)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

########################################################################################
# Does leaf length per leaflet influence flammability?
########################################################################################
names(final_data)

ggplot(final_data, aes(leaf_length_per_leaflet, degsec_100,
                       color = genus)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(expression(paste("Leaf length per leaflet (",cm^2,")"))) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(leaf_length_per_leaflet, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlab("Leaf length per leaflet (cm)") +
  #ylab("Flammability score (PC2)")

##########################################################################################
# Does leaf area influence flammability?
##########################################################################################

ggplot(final_data, aes(leaf_area_per_leaflet, degsec_100, 
                       color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(expression(paste("Leaf area per leaflet (",cm^2,") "))) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) ))

#ggplot(final_data, aes(leaf_area_per_leaflet, PC2, color = specific_epithet)) +
  #geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #xlim(0, 10) +
  #xlab(expression(paste("Leaf area per leaflet (",cm^2,") "))) +
  #ylab("Flammability score (PC2)")

############################################################################################
# Does leaf moisture content influence self ignition?
############################################################################################

ggplot(final_data, aes(leaf_moisture_content, self_ignition)) +
  geom_point() +
  geom_smooth(method = "glm", method.args =list(family = binomial(link = "cloglog")), 
              fullrange = TRUE, se = FALSE, color = "red") +
  xlab("Leaf Moisture content (%)") +
  ylab(" Probability of self ignition (%)")
  

##############################################################################################
# Does canopy moisture content influence self ignition?
##############################################################################################

ggplot(final_data, aes(canopy_moisture_content, self_ignition)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "cloglog")), 
              fullrange = TRUE, se = FALSE, color = "red") +
  xlab("Canopy moisture content (%)") +
  ylab("Probability of self ignition (%)")

#################################################################################################
# Does disc temperature influence self ignition? 
#################################################################################################

ggplot(final_data, aes(temp_d1_pre, self_ignition)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "cloglog")), 
              fullrange = TRUE, se = FALSE, color = "red") +
  xlab("Temperature of disc one (\u00B0C)") +
  ylab("Probability of self ignition (%)")
  


ggplot(final_data, aes(temp_d2_pre, self_ignition)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "cloglog")), 
              fullrange = TRUE, se = FALSE, color = "red") +
  xlab("Temperature of disc two (\u00B0C)") +
  ylab("Probability of self ignition (%)")

###################################################################################################
# Does  moisture contents influence ignition delay?
###################################################################################################

ggplot(final_data, aes(leaf_moisture_content, ignition_delay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Leaf moisture content (%)") +
  ylab("Ignition delay (s)")


ggplot(final_data, aes(canopy_moisture_content, ignition_delay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Canopy moisture content (%)") +
  ylab("Ignition delay (s)")

###############################################################################################
# Does disc temp influence ignition delay?
###############################################################################################

ggplot(final_data, aes(temp_d1_pre, ignition_delay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Temperature of disc one (\u00B0C)") +
  ylab("Ignition delay (s)")

ggplot(final_data, aes(temp_d2_pre, ignition_delay)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Temperature of disc two (\u00B0C)") +
  ylab("Ignition delay (s)")



################################################################################################
# Does weather during burning influence flammability? Wind speed.
#################################################################################################

ggplot(final_data, aes(windspeed_miles_per_hour, self_ignition)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "cloglog")), 
              fullrange = TRUE, se = FALSE, color = "red") +
  xlab(expression(paste("Windspeed (", miles/hour,")"))) +
  ylab("Probability of self ignition (%)")

ggplot(final_data, aes(windspeed_miles_per_hour, degsec_100)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab(expression(paste("Windspeed (", miles/hour,") "))) +
  ylab("Flammability score(PC1)")


##############################################################################################
# Will test the influence of temperature and humidity on flammability from hobos data later.
#############################################################################################

