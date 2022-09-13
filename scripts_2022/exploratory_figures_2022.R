#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022

# All scripts in source are required to run before running
# this script.

source("../read_data_2022.R") # script that read, clean and merged all the traits data.
source("../read_hobos_2022.R") # script that reads the thermocouples data logger data during burning.
source("../flam_pca_2022.R") # script that did the pca analysis.

###############################################################################################
# Exploratory figures
###############################################################################################

###############################################################################################
# Does leaf_stem_mass_ratio influence flammability?
################################################################################################

ggplot(final_data, aes(leaf_stem_mass_ratio, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlab( "Leaf stem mass ratio (dry basis)" ) +
  ylab( "Flammability score (PC1)") 

ggplot(final_data, aes(leaf_stem_mass_ratio, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) + 
  xlab("Leaf stem mass ratio (dry basis)") +
  ylab(" Flammability score (PC2)")

################################################################################################
# Does canopy density influence flammability?
################################################################################################


ggplot(final_data, aes(canopy_density_gm_cm3, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE ) +
  xlim(0,0.015) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(canopy_density_gm_cm3, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE ) +
  xlim(0,0.015) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  ylab("Flammability score (PC2)")


##########################################################################################
# Does total mass influence flammability?
###########################################################################################
ggplot(final_data, aes(total_dry_mass_gm, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Total dry mass (dry basis)") +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(total_dry_mass_gm, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Total dry mass (dry basis)") +
  ylab("Flammability score (PC2)")

############################################################################################
# Does field moisture influence flammability?
#############################################################################################

ggplot(final_data, aes(field_moisture_content, PC1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Field moisture content (%)") +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(field_moisture_content, PC2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Field moisture content (%)") +
  ylab("Flammability score (PC2)")

#############################################################################################
# Does leaf moisture influence flammability?
#############################################################################################

ggplot(final_data, aes(leaf_moisture_content, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Leaf moisture content (%)") +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(leaf_moisture_content, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Leaf moisture content (%)") +
  ylab("Flammability score (PC2)")

################################################################################################
# Does canopy moisture influence flammability?
################################################################################################

ggplot(final_data, aes(canopy_moisture_content, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Canopy moisture content (%)") +
  ylab("Flammability score (PC1)")
  
ggplot(final_data, aes(canopy_moisture_content, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Canopy moisture content (%)") +
  ylab("Flammability score (PC2)")


######################################################################################
# Does LMA influence flammability?
######################################################################################


ggplot(final_data, aes(leaf_mass_per_area, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 0.05) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  ylab("Flammability score (PC1)")


ggplot(final_data, aes(leaf_mass_per_area, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 0.05) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  ylab("Flammability score (PC2)")

########################################################################################
# Does leaf length per leaflet influence flammability?
########################################################################################

ggplot(final_data, aes(leaf_length_per_leaflet, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab(expression(paste("Leaf length per leaflet (",cm^2,")"))) +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(leaf_length_per_leaflet, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Leaf length per leaflet (cm)") +
  ylab("Flammability score (PC2)")

##########################################################################################
# Does leaf area influence flammability?
##########################################################################################

ggplot(final_data, aes(leaf_area_per_leaflet, PC1, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 10) +
  xlab(expression(paste("Leaf area per leaflet (",cm^2,") "))) +
  ylab("Flammability score (PC1)")

ggplot(final_data, aes(leaf_area_per_leaflet, PC2, color = specific_epithet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 10) +
  xlab(expression(paste("Leaf area per leaflet (",cm^2,") "))) +
  ylab("Flammability score (PC2)")

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

ggplot(final_data, aes(windspeed_miles_per_hour, PC1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab(expression(paste("Windspeed (", miles/hour,") "))) +
  ylab("Flammability score(PC1)")

ggplot(final_data, aes(windspeed_miles_per_hour, PC2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlab(expression(paste("Windspeed (", miles/hour,") "))) +
  ylab("Flammability score(PC2)")
  

##############################################################################################
# Will test the influence of temperature and humidity on flammability from hobos data later.
#############################################################################################

