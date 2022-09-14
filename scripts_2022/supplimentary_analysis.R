#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022

# All scripts in source are required to run before running
# this script.

library(car)
source("../read_data_2022.R") # script that read, clean and merged all the traits data.
source("../read_hobos_2022.R") # script that reads the thermocouples data logger data during burning.
source("../flam_pca_2022.R") # script that did the pca analysis.



# Rscript for analysing to test whether the experimental design has
# effect on flammability or not!!

#################################################################################
# Does disks temperature influenced self_ignition?
#################################################################################

final_data$average_disk_temp <- (final_data$temp_d1_pre + final_data$temp_d2_pre)/2


# Disk one

self_ig_disc_one <- glm(self_ignition ~ temp_d1_pre, data = final_data,
                        family = binomial(link = "cloglog"))

summary(self_ig_disc_one) # Didn't influence. p = 0.755


self_ig_disc_two <- glm(self_ignition ~ temp_d2_pre, data = final_data,
                        family = binomial(link = "cloglog"))

summary(self_ig_disc_two) # Didn't influence. p = 0.196


self_ig_disc_av <- glm(self_ignition ~ average_disk_temp,
                       data = final_data, family = binomial(link = "cloglog"))

summary(self_ig_disc_av) # p = 0.538


################################################################################
# Does disk temperature influence flammability?
################################################################################

pc1_disc1 <- afex::lmer(PC1 ~ temp_d1_pre +(1|group), data = final_data) # Within species

summary(pc1_disc1) # Didn't influence. p =  0.133

pc1_d1_lm <- lm(PC1 ~ temp_d1_pre, data = final_data)

summary(pc1_d1_lm)  # p value = 0.11


pc1_disc2 <- afex::lmer(PC1 ~ temp_d2_pre + (1|group), data = final_data) # within species

summary(pc1_disc2) #  p = 0.970

pc1_d2_lm <- lm(PC1 ~ temp_d2_pre, data = final_data)

summary(pc1_d2_lm) # p value = 0.877

pc1_disc_avg <- afex::lmer(PC1 ~ average_disk_temp + (1|group), data = final_data)

summary(pc1_disc_avg) # p value 0.344

pc1_av_lm <- lm(PC1 ~ average_disk_temp, data = final_data) 

summary(pc1_av_lm) # p value = 0.281



pc2_disc1 <- afex::lmer(PC2 ~ temp_d1_pre +(1|group), data = final_data) # within species

summary(pc2_disc1) # p value = 0.355

pc2_d1_lm <- lm(PC2 ~ temp_d1_pre, data = final_data)

summary(pc2_d1_lm) # p value = 0.729


pc2_disc2 <- afex::lmer(PC2 ~ temp_d2_pre + (1|group), data = final_data) # within species

summary(pc2_disc2) # p value = 0.585

pc2_d2_lm <- lm(PC2 ~ temp_d2_pre, data = final_data) 

summary(pc2_d2_lm) # p = 0.732

pc2_disc_avg <- afex::lmer(PC2 ~ average_disk_temp + (1|group), data = final_data)

summary(pc2_disc_avg) # p value 0.355

pc2_av_lm <- lm(PC2 ~ average_disk_temp, data = final_data)

summary(pc2_av_lm) # p value = 0.668


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


# Does PC1 influenced by methods?

method1_mod <- lme4::lmer(PC1 ~ method + (1|species_id), data = subgroup , REML = TRUE)

Anova(method1_mod, test.statistic = "F") # p value 0.7598

# Does PC2 influenced by methods?

method2_mod <- lme4::lmer(PC2 ~ method + (1|species_id), data = subgroup, REML = TRUE)

Anova(method2_mod) # p value  0.2908

################################################################################################
# Does weather during burning influence flammability? Wind speed.
#################################################################################################

pc1_ws <- lm(PC1 ~ windspeed_miles_per_hour, data = final_data)

summary(pc1_ws) # p = 0.263


pc2_ws <- lm(PC2 ~ windspeed_miles_per_hour, data = final_data)

summary(pc2_ws) # Significant p = 0.0105 but slope is negative (0.09538), 


pc1_air_temp <- lm(PC1 ~ air_temp_f, data = final_data)

summary(pc1_air_temp) # p - value 0.312

pc2_air_temp <- lm(PC2 ~ air_temp_f, data = final_data)

summary(pc2_air_temp) # p value  0.329

###################################################################################
# Will test the effect temperature and humidity once again after reading the hobos data
# during burning
########################################################################################


################################################################################
# Does flammability varies across sites among samples of  species?
#################################################################################

#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1022))) # p 0.587 # Juniperus ashei
#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1036))) # p 0.108 # Rhus virens
#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1000))) # p 0.552 # Diospyros texana
#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1002))) # p 0.222 # Mahonia trifoliolata
#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1021))) # p 0.0224 # Sophora secundiflora
#summary(aov(PC1 ~ site, data = filter(final_data, species_id == 1113))) # p 0.199 # Prospis glandulosa



