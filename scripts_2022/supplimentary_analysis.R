#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022

# All scripts in source are required to run before running
# this script.

library(car)
library(corrplot)

source("../flam_pca_2022.R") # script that did the pca analysis.
source("../analysis_2022.R") # script that did the model selection



# Rscript for analysing to test whether the experimental design has
# effect on flammability or not!! The correlation matrix of flammability
# traits and morphological traits

#########################################################################
# Checking whether each species has at least three samples or not
##########################################################################

three_samples_check <- final_data %>%
  select(species_id, group, specific_epithet, windspeed_miles_per_hour,
         total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content, PC1, PC2, degsec_100,
         flame_height, temp_d1_pre, temp_d2_pre, self_ignition)

dim(three_samples_check)

xtabs(~species_id, three_samples_check) # Yes, each species at 
# least three samples.

########################################################################
# Creating a separate data set with flammability traits and morphological
# traits to create the correlation matrix
########################################################################


cor_data <- alldata_2022 %>%
  filter(! sample_id %in% c("KD18", "DK34", "KD15", "UV04",
                            "DK30")) %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label") %>%
  select(heat_release_j, massconsumed, windspeed_miles_per_hour,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)

dim(cor_data)

####################################################################################################################################
# Correlation of all morphological traits
#############################################################################################################################


morphological_traits_cor_data <- cor_data %>%
  select(total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)


morphological_traits_cor <- cor(morphological_traits_cor_data, method = "kendall",
                                use = "pairwise")

corrplot::corrplot(morphological_traits_cor, method = "number")

# morphological_traits_cor
# Kendall rank correlation coefficient between canopy_moisture_content and 
# leaf_moisture_content is 0.64, not a problem since they will be used
# in two different model.
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.67



###################################################################################################
# Correlation of canopy traits and flammability traits
######################################################################################################

canopy_flam_data <- cor_data %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay,
         leaf_stem_mass_ratio)

canopy_flam_cor <- cor(canopy_flam_data, method = "kendall", 
                       use = "pairwise")


corrplot::corrplot(canopy_flam_cor, method = "number", type = "upper")

###########################################################################################################
# Correlation of leaf  traits and flammability traits
###########################################################################################################

leaf_flam_data <- cor_data %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)

leaf_flam_cor <- cor(leaf_flam_data, method = "kendall",
                     use = "pairwise")


corrplot::corrplot(leaf_flam_cor, method = "number", type = "upper")

#################################################################################
# Does disks temperature influenced self_ignition?
#################################################################################

three_samples_check$average_disk_temp <- (three_samples_check$temp_d1_pre + three_samples_check$temp_d2_pre)/2



self_ig_disc_av <- glm(self_ignition ~ average_disk_temp,
                       data = three_samples_check, family = binomial(link = "cloglog"))

summary(self_ig_disc_av) # p = 0.491


################################################################################
# Does disk temperature influence flammability?
################################################################################


pc1_disc_avg <- afex::lmer(degsec_100 ~ average_disk_temp + (1|group),
                           data = three_samples_check, REML = FALSE)

summary(pc1_disc_avg) # p value 0.762

pc1_av_lm <- lm(degsec_100 ~ average_disk_temp, 
                data = three_samples_check) 

summary(pc1_av_lm) # p value = 0.505



pc2_disc_avg <- afex::lmer(flame_height ~ average_disk_temp + (1|group),
                           data = three_samples_check, REML = FALSE)

summary(pc2_disc_avg) # p value 0.748

pc2_av_lm <- lm(flame_height ~ average_disk_temp, 
                data = three_samples_check)

summary(pc2_av_lm) # p value = 0.530


################################################################################################
# Does weather during burning influence flammability? Wind speed, particularly flame
# height
#################################################################################################

three_samples_check <- three_samples_check %>%
  na.omit()

ws <- lm(flame_height ~ windspeed_miles_per_hour, 
         data = three_samples_check)

summary(ws) # p 0.895

ws_degsec <- lm(degsec_100 ~ windspeed_miles_per_hour , 
                data = three_samples_check)

summary(ws_degsec) # p is 0.362

##############################################################################
# Does different method influence the flammability ?
###############################################################################

# Filtered the burning date where the air_fuel mixture was different than other 
# burning days

third_trial <- final_data %>%
  filter(burn_date == "2022-06-21") %>%
  filter(species_id != 1009 ) %>% # Removing mimosa spp since I have collected only one sample and couldn't compare.
  filter(! sample_id %in% c("DC38", "DC37", "DC39",
  "DC32", "DC25", "DC29")) # Last six sample burnt the same way like other burning day.

trial_species <- unique(third_trial$species_id) # getting the unique species that
# got burnt during that trial.

# Separate data frame for common species

subgroup <- final_data %>%
  filter(species_id %in% trial_species) %>%
  mutate(method = ifelse(burn_date == "2022-06-21", "method1","method2"))


# Does temperature integration influenced by methods?

method1_mod <- lme4::lmer(degsec_100 ~ method + (1|species_id), data = subgroup , REML = TRUE)

Anova(method1_mod, test.statistic = "F") # p value 0.457



################################################################################
# Does flammability varies across sites among samples of  species?
#################################################################################

summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1022))) # p 0.59 # Juniperus ashei
summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1036))) # p 0.226 # Rhus virens
summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1000))) # 554 # Diospyros texana
summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1002))) # p 0.522 # Mahonia trifoliolata
summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1021))) # p 0.0.037  # Sophora secundiflora
summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1113))) # p 0.709 # Prospis glandulosa



################################################################################################

