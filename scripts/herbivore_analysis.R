#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2022

library(ggpubr)
library(nlme)







###################################################################################
# First getting the necessary variables from collection of 2021
# Merging all data with hobo data for 2021
###################################################################################


###################################################################################
# The collection from 2022
# Merging measurement data with hobo data



herbivore_data_2022 <- herbivore_2022 %>%
  left_join(hobos_wider_2022, by = "label")


unique(herbivore_2022$display_name)

###################################################################################
# Merging 2021 with 2022
###################################################################################


herbivore_data <- herbivore_data_2022 %>%
  dplyr::select(display_name, degsec_100, 
                herbivore_preference,
                herbivore_defense, site) %>%
  rbind(herbivore_2021) %>%
  mutate(herbivore_defense = ifelse(herbivore_defense == "non armed",
                                    "unarmed", herbivore_defense)) %>%
  mutate( display_name = ifelse(display_name == "R. micrphylla",
                                "R. microphylla", display_name)) %>%
  filter(! display_name %in% c( "A. xalapensis","Q. laceyi",
                          "U. crassifolia", "P. serotina",
                          "F. caroliniana", "U. spp", "C. spp")) # removing those species which has less than three samples



herbivore_data <- herbivore_data %>%
  mutate(site = ifelse(site == "Dickens Park", "Dickens spring",
                       site)) # changing the name of the site, both are same
# site but as different name

unique(herbivore_data$display_name)

xtabs(~ display_name, herbivore_data)
unique(herbivore_data$herbivore_defense)

herbivore_data$herbivore_defense <- as.factor(herbivore_data$herbivore_defense)


################################################################
# Analysis
################################################################

#herbivore_data <- herbivore_data[-161,] # One of the sample from Sophora
# is showing 0 as temperature integration!!!

xtabs(~display_name, data = herbivore_data)

unique(herbivore_data$display_name)

dim(herbivore_data)

xtabs(~ herbivore_defense, data = herbivore_data)



#########################################################################################################


nested_herbivore_defence_model <- lme(degsec_100 ~ herbivore_defense, data = herbivore_data, 
                                      random = ~ 1|display_name,  method = "REML") 

nested_herbivore_defence_hetero <- lme(degsec_100 ~ herbivore_defense, weights = varIdent(form = ~ 1| herbivore_defense),
                                       random = ~ 1|display_name, data = herbivore_data, method = "REML") # Counting the heterogenity
# between groups

AIC(nested_herbivore_defence_model, nested_herbivore_defence_hetero) # Lower AICc after counting the heterogenity
# between groups




anova(nested_herbivore_defence_hetero) # p = 0.111, no significant difference

#car::Anova(nested_herbivore_defence_hetero, type = 3, test.statistic = "F")

#adjusted_nested_defence <- glht(nested_herbivore_defence_hetero, 
                                #linfct = mcp(herbivore_defense = "Tukey"))

#summary(adjusted_nested_defence) # p = 0.0947

####################################################################################################
# Herbivore preference
####################################################################################################

herbivore_preference_data <- herbivore_data %>%
  dplyr::select(degsec_100, display_name, herbivore_preference, site) %>%
  na.omit()

dim(herbivore_preference_data)

xtabs(~herbivore_preference, data = herbivore_preference_data)

unique(herbivore_preference_data$display_name)

herbivore_preference_data$herbivore_preference <- as.factor(herbivore_preference_data$herbivore_preference)

####################################################################################################################
# Models
####################################################################################################################

nested_herbivore_preference <- lme(degsec_100 ~ herbivore_preference,
                                   random = ~ 1 | display_name, 
                                   data = herbivore_preference_data, method = "REML")



nested_herbivore_preference_withweight <- lme(degsec_100 ~ herbivore_preference, weights = varIdent(form = ~ 1|herbivore_preference),
                                              random = ~ 1| display_name, 
                                              data = herbivore_preference_data, 
                                              method = "REML")


AIC(nested_herbivore_preference, nested_herbivore_preference_withweight) # Better after counting heterogenity
# between groups, lower AICc


summary(nested_herbivore_preference_withweight) # p =  0.006

anova(nested_herbivore_preference_withweight)

#car::Anova(nested_herbivore_preference_withweight, type = 3, 
           #test.statistic = "F")

#adjusted_nested_herbivore_preference <- glht(nested_herbivore_preference_withweight,
                                             #linfct = mcp( herbivore_preference = "Tukey"))

#summary(adjusted_nested_herbivore_preference) # adjusted p = 0.002

#confint(adjusted_nested_herbivore_preference)

#ntervals(nested_herbivore_preference_withweight, which = c("fixed"))


#plot(resid(nested_herbivore_preference_withweight))
#abline(h = 0)




###############################################################
# Site per species
###############################################################



herbivore_property <- herbivore_data %>%
  group_by(display_name) %>%
  summarise(n = length(unique(site)))


herbivore_species <- herbivore_data %>%
  group_by(display_name, site) %>%
  summarise(n = n())


#View(herbivore_property)

#View(herbivore_species)
# has less than three sites per species

herbivore_site <- herbivore_data %>%
  group_by(display_name) %>%
  summarise(n = length(unique(site))) %>%
  filter(n >= 2)




#########################################################
# Does site has effect on flammability
#########################################################

herbivore_site_effect <- herbivore_data %>%
  filter(display_name %in% herbivore_site$display_name)

dim(herbivore_site_effect)
unique(herbivore_site_effect$display_name)
xtabs(~ herbivore_defense, data = herbivore_site_effect)

###############################################################

site_effect <- sapply(split(herbivore_site_effect, 
                            herbivore_site_effect$display_name), function(i){
                              summary(aov(degsec_100 ~ as.factor(site), data = i))
                            })

site_effect
# Prospis glandulosa, Sophora secundiflora, Juniperus virginiana
# Forestiera pubescens varies among sites


########################################################################################################
# Model without Ilex Vomitoria, Sophora secundiflora, Juniperus virginiana
# Forestiera pubescens

####################################################################################################

#herbivore_preference_data_without_site_effect <- herbivore_preference_data %>%
  #filter(! display_name %in% c("P. glandulosa", "J. virginiana",
                          #"F. pubescens", "S. secundiflora"))

#unique(herbivore_preference_data_without_site_effect$display_name)

#xtabs(~ herbivore_preference, data = herbivore_preference_data_without_site_effect)


#nested_without_site_efffect <- lme(degsec_100 ~ herbivore_preference, weights = varIdent(form = ~ 1|herbivore_preference),
                                   #random = ~1|display_name, data = herbivore_preference_data_without_site_effect, method = "REML")  

#summary(nested_without_site_efffect) # p = 0.003

#anova(nested_without_site_efffect)

#car::Anova(nested_without_site_efffect, type = 3, test.statistic = "F") # 0.0002



#adjusted_without_site_effect <- glht(nested_without_site_efffect, linfct = mcp(herbivore_preference = "Tukey"))

#summary(adjusted_without_site_effect)




