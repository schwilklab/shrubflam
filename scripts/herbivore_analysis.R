herbivore_2022 <- samples_2022 %>%
  left_join(leaf_measurements_2022, by = c("sample_id","species_id")) %>%
  left_join(burn_trials_2022, by = c("sample_id", "species_id")) %>%
  filter(! sample_id %in% c("KD07", "UV01")) %>%  # KD07 has missing values of flammability measurements and mistakenly, UV01 has ignited with blow torch though 
  # it has self ignition and has existing flame(descriptions on notes in flam_trials_2022,csv.)
  mutate(air_temp_f = ifelse(sample_id == "UV16", NA, air_temp_f))


##################################################################################
# The flame height is in mili meter but I changed the unit to cm
# manually during data entry but for some samples the unit remain
# unchanged, I was in rush to push the data into github, A big mistake
# Fixing them one by one from the data sheet
#######################################################################


herbivore_2022 <- herbivore_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DC29", 115, flame_height)) # It is 115 actually, not 150

herbivore_2022 <- herbivore_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DK49", 35, flame_height))


herbivore_2022 <- herbivore_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DV05", 150, flame_height))

herbivore_2022 <- herbivore_2022 %>%
  mutate(flame_height = ifelse( sample_id == "DV06", 130, flame_height))


##########################################################################
# All the flame height is in mili meter from 08/14/2022 date
##########################################################################

herbivore_2022$burn_date <- as.character(herbivore_2022$burn_date) # Changing the date to character


herbivore_2022 <- herbivore_2022 %>%
  mutate( flame_height = ifelse(burn_date == "2022-08-14", flame_height/10, flame_height))


herbivore_2022$burn_date <- as.Date(herbivore_2022$burn_date) # Now as Date

####################################################################################
# Joining with hobo data
####################################################################################

herbivore_2022 <- herbivore_2022 %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label")


###################################################################################
# 2021
###################################################################################

herbivore_2021 <- alldata %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider, by = "label") %>%
  filter(ignition == 1) %>%
  filter(! trial %in% 1:11) %>%
  rename(degsec_100 = degsec.100) %>%
  select(species, species_id, degsec_100)
 

herbivore_data <- herbivore_2022 %>%
  select(species, species_id, degsec_100) %>%
  rbind(herbivore_2021) %>%
  mutate(species = ifelse(species == "Forestiera reticulata", "Forestiera pubescens", species)) %>%
  filter(! species %in% c("Cotinus obovatus", "Arbutus xalapensis",
                          "Cercis canadensis", "Pinus remota",
                          "Yucca rupicola", "Quercus laceyi",
                          "Ulmus crassifolia", "Prunus serotina",
                          "Frangula caroliniana", "Unknown spp"))

xtabs(~species, data = herbivore_data)

unique(herbivore_data$species)
