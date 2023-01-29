#!/usr/bin/Rscript --vanilla

# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2022

library(ggpubr)
library(nlme)
library(car)
library(multcomp)


source("./read_data.R") # The script that used to clean the dataset of 2021
source("./read_hobos.R") # The script that used to calculate the flammability
# metrics from k-type thermocouple sensors from 2021
source("./read_data_2022.R") # The script that used to clean the dataset of 2022
source("./read_hobos_2022.R") # The script that used to calculate the flammability
# metrics from k-type thermocouple sensors from 2022





###################################################################################
# First getting the necessary variables from collection of 2021
# The first nine  trails removed from the analysis since the drying period was
# tweenty four hours for those samples and drying period for rest of all the samples
# were thirty six hours.
# Since I am gooing to combine both years data set, I am eliminitating the
# samples which didn't get ignited in 10 seconds since in 2022 the blowtorch
# was on until a sample got ignited.
# Merging all data with hobo data for 2021
###################################################################################

herbivore_2021 <- alldata %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider, by = "label") %>%
  filter(ignition == 1) %>%
  filter(! trial %in% 1:9) %>%
  rename(degsec_100 = degsec.100) %>%
  dplyr::select(species, species_id, degsec_100, herbivore_preference,
         herbivore_defense, property) %>%
  rename(site = property)

unique(herbivore_2021$species)


###################################################################################
# The collection from 2022
# Merging measurement data with hobo data



herbivore_2022 <- herbivore_2022 %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label")


unique(herbivore_2022$species)
###################################################################################
# Merging 2021 with 2022
###################################################################################


herbivore_data <- herbivore_2022 %>%
  dplyr::select(species, species_id, degsec_100, herbivore_defense,
         herbivore_preference, site) %>%
  rbind(herbivore_2021) %>%
  mutate(herbivore_defense = ifelse(herbivore_defense == "non_armed",
                                    "unarmed", herbivore_defense)) %>%
  mutate(species = ifelse(species == "Forestiera reticulata",
                          "Forestiera pubescens", species)) %>%
  mutate(species = ifelse(species == "Juniperus pinchottii",
                          "Juniperus pinchotii", species)) %>%
  mutate(species = ifelse(species == "Quercus fusiformis",
                          "Quercus virginiana", species)) %>%
  mutate(species = ifelse(species == "Acacia berlandieri" ,
                          "Senegalia berlandieri" , species)) %>%
  filter(! species %in% c("Cotinus obovatus", "Arbutus xalapensis",
                          "Cercis canadensis", "Pinus remota",
                          "Yucca rupicola", "Quercus laceyi",
                          "Ulmus crassifolia", "Prunus serotina",
                          "Frangula caroliniana", "Unknown spp")) # removing those species which has less than three samples





herbivore_data <- herbivore_data %>%
  mutate(site = ifelse(site == "Dickens Park", "Dickens spring",
                       site)) # changing the name of the site, both are same
# site but as different name

unique(herbivore_data$species)

unique(herbivore_data$herbivore_defense)

herbivore_data$herbivore_defense <- as.factor(herbivore_data$herbivore_defense)


################################################################
# Analysis
################################################################

herbivore_data <- herbivore_data[-161,] # One of the sample from Sophora
# is showing 0 as temperature integration!!!

xtabs(~species, data = herbivore_data)

unique(herbivore_data$species)

dim(herbivore_data)

xtabs(~ herbivore_defense, data = herbivore_data)



#########################################################################################################


nested_herbivore_defence_model <- lme(degsec_100 ~ herbivore_defense, data = herbivore_data, 
                                      random = ~ 1|species,  method = "REML") 

nested_herbivore_defence_hetero <- lme(degsec_100 ~ herbivore_defense, weights = varIdent(form = ~ 1| herbivore_defense),
                                       random = ~ 1|species, data = herbivore_data, method = "REML") # Counting the heterogenity
# between groups

AIC(nested_herbivore_defence_model, nested_herbivore_defence_hetero) # Lower AICc after counting the heterogenity
# between groups




anova(nested_herbivore_defence_hetero) # p = 0.111, no significant difference

#car::Anova(nested_herbivore_defence_hetero, type = 3, test.statistic = "F")

adjusted_nested_defence <- glht(nested_herbivore_defence_hetero, 
                                linfct = mcp(herbivore_defense = "Tukey"))

summary(adjusted_nested_defence) # p = 0.0947

####################################################################################################
# Herbivore preference
####################################################################################################

herbivore_preference_data <- herbivore_data %>%
  dplyr::select(degsec_100, species, species_id, herbivore_preference, site) %>%
  na.omit()

dim(herbivore_preference_data)

xtabs(~herbivore_preference, data = herbivore_preference_data)

unique(herbivore_preference_data$species)

herbivore_preference_data$herbivore_preference <- as.factor(herbivore_preference_data$herbivore_preference)

####################################################################################################################
# Models
####################################################################################################################

nested_herbivore_preference <- lme(degsec_100 ~ herbivore_preference,
                                   random = ~ 1 | species, 
                                   data = herbivore_preference_data, method = "REML")



nested_herbivore_preference_withweight <- lme(degsec_100 ~ herbivore_preference, weights = varIdent(form = ~ 1|herbivore_preference),
                                              random = ~ 1| species, 
                                              data = herbivore_preference_data, 
                                              method = "REML")


AIC(nested_herbivore_preference, nested_herbivore_preference_withweight) # Better after counting heterogenity
# between groups, lower AICc


summary(nested_herbivore_preference_withweight) # p =  0.01

anova(nested_herbivore_preference_withweight)

car::Anova(nested_herbivore_preference_withweight, type = 3, 
           test.statistic = "F")

adjusted_nested_herbivore_preference <- glht(nested_herbivore_preference_withweight,
                                             linfct = mcp( herbivore_preference = "Tukey"))

summary(adjusted_nested_herbivore_preference) # adjusted p = 0.002

confint(adjusted_nested_herbivore_preference)

intervals(nested_herbivore_preference_withweight, which = c("fixed"))


plot(resid(nested_herbivore_preference_withweight))
abline(h = 0)




###############################################################
# Site per species
###############################################################



herbivore_property <- herbivore_data %>%
  group_by(species) %>%
  summarise(n = length(unique(site)))


herbivore_species <- herbivore_data %>%
  group_by(species, site) %>%
  summarise(n = n())


View(herbivore_property)

View(herbivore_species)
# has less than three sites per species

herbivore_site <- herbivore_data %>%
  group_by(species) %>%
  summarise(n = length(unique(site))) %>%
  filter(n >= 2)




#########################################################
# Does site has effect on flammability
#########################################################

herbivore_site_effect <- herbivore_data %>%
  filter(species %in% herbivore_site$species)

dim(herbivore_site_effect)
unique(herbivore_site_effect$species)
xtabs(~ herbivore_defense, data = herbivore_site_effect)

###############################################################

site_effect <- sapply(split(herbivore_site_effect, 
                            herbivore_site_effect$species), function(i){
                              summary(aov(degsec_100 ~ as.factor(site), data = i))
                            })

site_effect
#  Prosopis glandulosa, Sophora secundiflora, Juniperus virginiana
# Forestiera pubescens varies among sites


########################################################################################################
# Model without Prosopis glandulosa, Sophora secundiflora, Juniperus virginiana
# Forestiera pubescens

####################################################################################################

herbivore_preference_data_without_site_effect <- herbivore_preference_data %>%
  filter(! species %in% c("Prosopis glandulosa", "Juniperus virginiana",
                          "Forestiera pubescens", "Sophora secundiflora"))

unique(herbivore_preference_data_without_site_effect$species)

xtabs(~ herbivore_preference, data = herbivore_preference_data_without_site_effect)


nested_without_site_efffect <- lme(degsec_100 ~ herbivore_preference, weights = varIdent(form = ~ 1|herbivore_preference),
                                   random = ~1|species, data = herbivore_preference_data_without_site_effect, method = "REML")  

summary(nested_without_site_efffect) # p = 0.0054

anova(nested_without_site_efffect)

car::Anova(nested_without_site_efffect, type = 3, test.statistic = "F") # 0.0002



adjusted_without_site_effect <- glht(nested_without_site_efffect, linfct = mcp(herbivore_preference = "Tukey"))

summary(adjusted_without_site_effect)

##################################################################################################
# Plots 
################################################################################################

ggboxplot(herbivore_preference_data,x = "herbivore_preference",
          y = "degsec_100",
          color = "herbivore_preference", 
          add = "jitter",
          shape = "herbivore_preference") +
  ylab("Temperature integration (\u00B0C.s )" ) +
  xlab("White-tailed deer preference") +
  labs(color = "",
       shape = "")


plot(ranef(nested_herbivore_preference_withweight, condVar = TRUE)) # Plots of random effects

# source: https://cran.microsoft.com/snapshot/2017-12-15/web/packages/sjPlot/vignettes/sjplmer.html
