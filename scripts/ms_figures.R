## ms_figures.R

# this R script is used to create figures showing significant fixed effect of
# canopy and leaf traits on flammability from analysis.R and also model anova
# and coefficient tables

#library(randomcoloR)  ## DWS: Why? You don't want random colors in code!
library(ggmap)

## This script depends on analysis.R. See run-all.R

## DWS: Where does the 2021 data coe from, the code you refer to never reads it.

## DWS: Why is this script so long? How many figures do you need to tell this
## story?

################################################################
# Creating a map,I learned to create this kind of map from Dr. Van-gestel's 
# R class.
################################################################

site_map_2021 <- alldata %>%
  select(property, lat_location,
         long_location) %>%
  rename(site = property,
         long = long_location,
         lat = lat_location)

## DWS: This errors out. alldata does not exist.

## DWS: I gave up here.

site_map <- alldata_2022 %>%
  select(site, lat, long) %>%
  rbind(site_map_2021) %>%
  mutate(site = ifelse(site == "Dickens Park",
                       "Dickens spring", site),
         site = ifelse(site == "Menard 2020-01" , "Menard", site),
         site = ifelse(site == "Kendall 2021-8", "Kendall 2021-08", site),
         site = ifelse(site == "Bastrop-Fayette 2022-01","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-02","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-03","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-04","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-05","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-06","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-07","Bastrop-Fayette", site))
unique(site_map$site)

## DWS: Why are you putting more data


#View(site_map)


texas <- map_data("state") %>%
  filter(region == "texas")

texas_counties <- map_data("county") %>%
  filter(region == "texas")

texas_county_names <- texas_counties %>%
  group_by(subregion) %>%
  summarise(mean_lat = mean(lat),
            mean_long = mean(long))



ggplot(texas, aes(long, lat)) +
  geom_polygon(color = "black", fill = "grey") +
  theme_bw() +
  geom_polygon(aes(group = group), data = texas_counties,
               fill = "NA", color = "white") +
  geom_polygon(color = "black", fill = "NA") +
  geom_point(aes(fill = site), color = "black",
             shape = 21, size = 2, data = site_map) +
  geom_text(data = texas_county_names,
            aes(mean_long, mean_lat, label = subregion),
            color = "white", size = 2, alpha = 0.5) +
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")"))


site_map_2022 <- alldata_2022 %>%
  select(site, lat, long) %>%
  mutate(site = ifelse(site == "Dickens Park",
                       "Dickens spring", site),
         site = ifelse(site == "Kendall 2021-8", "Kendall 2021-08", site),
         site = ifelse(site == "Bastrop-Fayette 2022-01","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-02","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-03","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-04","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-05","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-06","Bastrop-Fayette", site),
         site = ifelse(site == "Bastrop-Fayette 2022-07","Bastrop-Fayette", site),
         site = ifelse(site == "Kendall 2021-7", "Kendall 2021-07", site))

unique(site_map_2022$site)

ggplot(texas, aes(long, lat)) +
  geom_polygon(color = "black", fill = "grey") +
  theme_bw() +
  geom_polygon(aes(group = group), data = texas_counties,
               fill = "NA", color = "white") +
  geom_polygon(color = "black", fill = "NA") +
  geom_point(aes(fill = site), color = "black",
             shape = 21, size = 2, data = site_map_2022) +
  geom_text(data = texas_county_names,
            aes(mean_long, mean_lat, label = subregion),
            color = "white", size = 2, alpha = 0.5) +
  labs(fill = "",
       x = expression("Longitude ("*~degree*")"),
       y = expression("Latitude ("*~degree*")"))

##################################################################################
# Generating thirteen disinct color
#################################################################################

set.seed(2643598)
palette <- distinctColorPalette(13)

## DWS: good god why? If you need 13 colors you are thinking about this wrong.


##################################################################################
# The best model for canopy traits for temperature integration using scaled 
# variables from model_data from analysis.R script
##################################################################################



best_degsec_canopy_mod_mixed <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                              canopy_density_gm_cm3 + (1 | genus), 
                                              data = model_data, method = "KR")

anova(best_degsec_canopy_mod_mixed) # Checking by mixed

## DWS: Why is their model fitting in the figure code?

#####################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
#######################################################################################


best_degsec_canopy_plot_data <- final_data %>% 
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)


#########################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
#########################################################################################


best_degsec_canopy_plot_data$predicted_degsec_100 <- predict(best_canopy_pc1_model, 
                                                             newdata = best_degsec_canopy_plot_data)

##########################################################################################
# Keeping unscled total_dry_mass_gm, canopy density, degsec_100 and predicted degsec
# _100 to summarise them by group
###########################################################################################

predicted_degsec_canopy_plot_data <- best_degsec_canopy_plot_data %>%
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() 
# where we will put the mean value, the bigger dots in 
# the plot
############################################################################################

degsec_by_group <- final_data %>% group_by(genus) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


###########################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by
# summarized data
###########################################################################################

ggplot(best_degsec_canopy_plot_data, aes(total_dry_mass_g, degsec_100, 
                                         color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

############################################################################################################
# Same way for canopy density
#####################################################################################

ggplot(best_degsec_canopy_plot_data, aes(canopy_density_gm_cm3, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")")))  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange", "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))



#################################################################################################
# Now the leaf traits, create the best leaf traits model for temperature integration 
# once again from analysis.R
#################################################################################################

best_degsec_leaf_mod <- afex::lmer(degsec_100 ~ leaf_mass_per_area + (1|genus),
                                   data = model_data, REML = FALSE) 


#################################################################################################
# Getting the unscaled fixed terms from final_data 
#################################################################################################

best_degsec_leaf_plot_data <- final_data %>% 
  select(leaf_mass_per_area, degsec_100, genus)


#####################################################################################
# Predicting the unscaled variables and naming the new variable as 
# predicted_degsec_100
###################################################################################


best_degsec_leaf_plot_data$predicted_degsec_100 <- predict(best_degsec_leaf_mod, 
                                                           newdata = best_degsec_leaf_plot_data)

########################################################################################
# Keeping unscaled leaf_mass_per_area degsec_100 and predicted degsec_100
# to summarise them by genus
########################################################################################

predicted_degsec_leaf_plot_data <- best_degsec_leaf_plot_data %>%
  select(predicted_degsec_100, leaf_mass_per_area, degsec_100, genus)

########################################################################################
# Summarising the fixed effects by group and making sure that the name of the 
# mean value after summarising them are same as best_degsec_
# canopy_plot_data  so that we can use them another layer in the plot as 
# geom_point() where we will put the mean value, the bigger dots in 
# the plot
###########################################################################################

degsec_leaf_by_group <- final_data %>% 
  group_by(genus) %>%
  summarize(leaf_mass_per_area = mean(leaf_mass_per_area),
            degsec_100 = mean(degsec_100))

####################################################################################
# Plotting the lma vs temperature integration
####################################################################################

ggplot(best_degsec_leaf_plot_data, aes(leaf_mass_per_area, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_leaf_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2" , "yellow", "#6FA3CE", "#D6E2A6", "red", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange", "black")) +
  scale_fill_manual(values = c("maroon2" , "yellow", "#6FA3CE", "#D6E2A6", "red", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))


##################################################################################
# Now the model without Juniperus
##################################################################################

# Best canopy model without Juniperus spp
#########################################################################################

best_degsec_canopy_mod_mixed_withoutj <- afex::mixed(degsec_100 ~ total_dry_mass_g + 
                                                       canopy_density_gm_cm3 + (1 | genus), 
                                                     data = without_juniperus, method = "KR")

anova(best_degsec_canopy_mod_mixed)

##############################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
###############################################################################################


best_degsec_canopy_plot_data_withoutj <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)


###############################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
###############################################################################################


best_degsec_canopy_plot_data_withoutj$predicted_degsec_100 <- predict(best_canopy_pc1_model_withoutj, 
                                                                      newdata = best_degsec_canopy_plot_data_withoutj)

########################################################################################################################
# Keeping unscled total_dry_mass_gm, canopy density, degsec_100 and predicted 
# degsec_100 to summarise them by group
########################################################################################################################

predicted_degsec_canopy_plot_data_withoutj <- best_degsec_canopy_plot_data_withoutj %>%
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, degsec_100, genus)

#########################################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after 
# summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() where we will put the mean
# value, the bigger dots in 
# the plot
###########################################################################################################################

degsec_by_group_withoutj <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  group_by(genus) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


#########################################################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by summarized data
#########################################################################################################################

ggplot(best_degsec_canopy_plot_data_withoutj, aes(total_dry_mass_g, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "red", "blue", "orange", 
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "red", "blue", "orange",
                               "black"))



###################################################################################
# Canopy density vs temperature integration
###################################################################################

ggplot(best_degsec_canopy_plot_data_withoutj, aes(canopy_density_gm_cm3, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6",
                                "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "red", "blue", "orange", 
                                "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "red", "blue", "orange",
                               "black"))




#########################################################################################
# Now the leaf traits, create the best leaf traits model for temperature 
# integration once again from analysis.R
#########################################################################################

best_degsec_leaf_mod_withoutj <- afex::lmer(degsec_100 ~ leaf_length_per_leaflet + (1|genus),
                                            data = without_juniperus, REML = FALSE) 


################################################################################################
# Getting the unscaled fixed terms from final_data from flam_pca.R
#################################################################################################

best_degsec_leaf_plot_data_withoutj <- final_data %>% 
  filter(genus != "Juniperus spp") %>%
  select(leaf_length_per_leaflet, degsec_100, genus)


###################################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_degsec_100
###################################################################################################


best_degsec_leaf_plot_data_withoutj$predicted_degsec_100 <- predict(best_degsec_leaf_mod_withoutj, 
                                                                    newdata = best_degsec_leaf_plot_data_withoutj)

######################################################################################################
# Keeping unscaled leaf_length_per_leaflet degsec_100 and predicted degsec_100 to summarise them by genus
######################################################################################################

predicted_degsec_leaf_plot_data_withoutj <- best_degsec_leaf_plot_data_withoutj %>%
  select(predicted_degsec_100, leaf_length_per_leaflet, degsec_100, genus)

#########################################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value after 
# summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the 
# plot as geom_point() where we will put the mean value, the bigger dots in 
# the plot
##########################################################################################################

degsec_leaf_by_group <- final_data %>%
  filter(genus != "Juniperus spp") %>%
  group_by(genus) %>%
  summarize(leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            degsec_100 = mean(degsec_100))


#####################################################################################
# Leaf_length_per_leaflet vs temperature integration
######################################################################################

ggplot(best_degsec_leaf_plot_data_withoutj, aes(leaf_length_per_leaflet, degsec_100, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_leaf_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_leaf_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Leaf length per leaflet (cm)") +
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                                "#73E17B", "red",
                                "#D1A7D6", "#DA61C2", "lightblue", "blue", 
                                "orange", "black")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", 
                               "#73E17B", "red",
                               "#D1A7D6", "#DA61C2", "lightblue", "blue",
                               "orange", "black"))


##################################################################################
# The best model for canopy traits and leaf traits for ignition delay using scaled 
# variables from model_data from analysis.R script
##################################################################################



best_canopy_ignition_delay_mixed <- afex::mixed(ignition_delay ~ canopy_density_gm_cm3+ 
                                                  canopy_moisture_content +
                                                  (1 | genus), data = model_data, method = "KR")

anova(best_canopy_ignition_delay_mixed) # Checking by mixed

#####################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
#######################################################################################


best_canopy_ignition_plot_data <- final_data %>% 
  select(canopy_moisture_content, canopy_density_gm_cm3, ignition_delay, genus)


#########################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_ignition_delay
#########################################################################################


best_canopy_ignition_plot_data$predicted_ignition_delay <- predict(best_canopy_ignition_model, 
                                                                   newdata = best_canopy_ignition_plot_data)

##########################################################################################
# Keeping unscled canopy moisture content, canopy density, ignition delay and predicted ignition
# delay to summarise them by group
###########################################################################################

predicted_ignition_canopy_plot_data <- best_canopy_ignition_plot_data %>%
  select(canopy_moisture_content, canopy_density_gm_cm3,
         ignition_delay, genus, predicted_ignition_delay)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them
############################################################################################

ignition_delay_by_group <- final_data %>% group_by(genus) %>%
  summarize(canopy_moisture_content = mean(canopy_moisture_content), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            ignition_delay = mean(ignition_delay))


ggplot(best_canopy_ignition_plot_data, aes(canopy_density_gm_cm3,ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab(expression(paste("Canopy density (", g / cm^3, ")")))  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))



ggplot(best_canopy_ignition_plot_data, aes(canopy_moisture_content,ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab("Canopy moisture content (%)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))


######################################################################################
# Leaf traits vs ignition delay
######################################################################################

best_leaf_ignition_plot_data <- final_data %>% 
  select(leaf_moisture_content, leaf_mass_per_area, ignition_delay, genus)




#########################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_ignition_delay
#########################################################################################


best_leaf_ignition_plot_data$predicted_ignition_delay <- predict(best_leaf_ignition_model, 
                                                                 newdata = best_leaf_ignition_plot_data)

##########################################################################################
# Keeping unscled canopy moisture content, canopy density, ignition delay and predicted ignition
# delay to summarise them by group
###########################################################################################

predicted_ignition_leaf_plot_data <- best_leaf_ignition_plot_data %>%
  select(leaf_moisture_content, leaf_mass_per_area,
         ignition_delay, genus, predicted_ignition_delay)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them
############################################################################################

leaf_ignition_delay_by_group <- final_data %>% group_by(genus) %>%
  summarize(leaf_moisture_content = mean(leaf_moisture_content), 
            leaf_mass_per_area = mean(leaf_mass_per_area),
            ignition_delay = mean(ignition_delay))


ggplot(best_leaf_ignition_plot_data, aes(leaf_mass_per_area,ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = leaf_ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")")))  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))



ggplot(best_leaf_ignition_plot_data, aes(leaf_moisture_content,ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = leaf_ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab("Leaf moisture content (%)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))

#####################################################################################
# Canopy traits vs ignition delay without Juniperus spp.
#######################################################################################


best_canopy_ignition_plot_data_withoutj <- final_data %>% 
  filter(genus != "Juniperus spp") %>%
  select(canopy_moisture_content, total_dry_mass_g, ignition_delay, genus)


unique(best_canopy_ignition_plot_data_withoutj$genus)
#########################################################################################
# Predicting the unscaled variables and naming the new variable as predicted_ignition_delay
#########################################################################################


best_canopy_ignition_plot_data_withoutj$predicted_ignition_delay <- predict(best_canopy_ignition_model_withoutj, 
                                                                            newdata = best_canopy_ignition_plot_data_withoutj)

##########################################################################################
# Keeping unscled canopy moisture content, canopy density, ignition delay and predicted ignition
# delay to summarise them by group
###########################################################################################

predicted_ignition_canopy_plot_data_withoutj <- best_canopy_ignition_plot_data_withoutj %>%
  select(canopy_moisture_content, total_dry_mass_g,
         ignition_delay, genus, predicted_ignition_delay)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them
############################################################################################

ignition_delay_by_group_withoutj <- final_data %>% 
  filter(genus != "Juniperus spp") %>%
  group_by(genus) %>%
  summarize(canopy_moisture_content = mean(canopy_moisture_content), 
            total_dry_mass_g = mean(total_dry_mass_g),
            ignition_delay = mean(ignition_delay))


ggplot(best_canopy_ignition_plot_data_withoutj, aes(total_dry_mass_g, ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab("Total  dry mass (g)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))



ggplot(best_canopy_ignition_plot_data_withoutj, aes(canopy_moisture_content,ignition_delay, color = genus)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data_withoutj) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group_withoutj, size = 4.5 , alpha = 1, shape = 16,
             aes(color = genus)) +
  ylab("Ignition delay (s)") +
  xlab("Canopy moisture content (%)")  + 
  theme(legend.key.width = unit(0.5, "lines"),
        legend.title = element_blank(), 
        panel.background = element_rect(size = 1.6, fill = NA),
        panel.border = element_rect(size = 1.6, fill=NA),
        axis.title = element_text(size = 14),
        legend.text = element_text(face = "italic")) +
  scale_color_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                                "#73E17B", "black" ,
                                "#D1A7D6", "pink", "#D0A5D4", "orange",
                                "lightblue",  "blue","#D4CECE")) +
  scale_fill_manual(values = c("maroon2", "yellow", "#6FA3CE", "#D6E2A6", "red",
                               "#73E17B", "black",
                               "#D1A7D6", "pink", "#D0A5D4", "orange",
                               "lightblue", "blue" ,"#D4CECE"))


lmc_ignition_withoutj <- afex::mixed(ignition_delay ~ leaf_length_per_leaflet + leaf_moisture_content +
                                       (1 | genus), data = model_data, method = "KR")
summary(lmc_ignition_withoutj) 


##################################################################################
##########################################################################################
# Renamed pca plot
##########################################################################################

pca_data_renamed_2022 <- pca_data_2022 %>%
  rename(ID = ignition_delay,
         FH = flame_height,
         FD = flame_duration,
         TI = degsec_100,
         DH = dur_100,
         PT = peak_temp,
         MC = massconsumed,
         VB = vol_burned,
         HR = heat_release_j)

renamed_pca <- prcomp(pca_data_renamed_2022[,-1], scale = TRUE)

renamed_pca_plot <- fviz_pca_var(renamed_pca,col.var = "cos2",
                                 gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                                 repel = TRUE, col.circle = "white") +
  xlab("Principle component 1") +
  ylab("Principle component 2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(size = 1.6),
        plot.title = element_blank())


