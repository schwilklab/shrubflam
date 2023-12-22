#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022
# This script will produce results of analysis related to
# supplimentary information
# All scripts in source are required to run before running
# this script.

library(car)
library(corrplot)
library(maps)
library(glmmTMB) # The package is needed to get the output
# as pdf or in latex format from sjPlot object




#########################################################################
# Checking whether each species has at least three samples or not
##########################################################################

three_samples_check <- final_data %>%
  select(genus, windspeed_miles_per_hour,
         total_dry_mass_g, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content, PC1, PC2, degsec_100,
         flame_height, temp_d1_pre, temp_d2_pre, self_ignition)

dim(three_samples_check)

xtabs(~ genus, three_samples_check) # Yes, each species at 
# least three samples.


########################################################################
# Creating a separate data set with flammability traits and morphological
# traits to create the correlation matrix
########################################################################


cor_data <- alldata_2022 %>%
  left_join(hobos_wider_2022, by = "label") %>%
  select(heat_release_j, massconsumed, windspeed_miles_per_hour,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         total_dry_mass_g, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)

dim(cor_data)

####################################################################################################################################
# Correlation of all morphological traits
# ggplot2 code for corrplot, source: https://dk81.github.io/
# dkmathstats_site/rvisual-corrplots.html
#############################################################################################################################


morphological_traits_cor_data <- cor_data %>%
  select(total_dry_mass_g, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content) %>%
  rename(leaf_length_per_leaf = leaf_length_per_leaflet,
         leaf_area_per_leaf = leaf_area_per_leaflet) %>%
  rename(total_dry_mass = total_dry_mass_g,
         canopy_bulk_density = canopy_density_gm_cm3)


morphological_traits_cor <- cor(morphological_traits_cor_data, method = "kendall",
                                use = "pairwise")


morphological_traits_cor_plot_melted <- reshape2::melt(morphological_traits_cor)

morphological_traits_cor_plot_ggplot2 <- ggplot(morphological_traits_cor_plot_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid ="grey70", limits = c(-1, +1)) +
  labs(x = "", y = "", fill = "Correlation \n Measure") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"),
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,
                                   margin = margin(t = 0, r = 5, b = 0, l = 0)),
        legend.title = element_text(face="bold", colour="brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black",
            fontface = "bold", size = 5)

ggsave("./results/morphological_traits_correlation_plot.pdf",
       plot = morphological_traits_cor_plot_ggplot2,
       width = 18, height = 15, units = "cm")



# morphological_traits_cor
# Kendall rank correlation coefficient between canopy_moisture_content and 
# leaf_moisture_content is 0.64, not a problem since they will be used
# in two different model.
# Kendall rank correlation coefficient between leaf_area_per_leaflet and 
# leaf_length_per_leaflet is 0.65



###################################################################################################
# Correlation of canopy traits and flammability traits
######################################################################################################

canopy_flam_data <- cor_data %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay,
         leaf_stem_mass_ratio, canopy_density_gm_cm3, total_dry_mass_g,
         canopy_moisture_content)

canopy_flam_cor <- cor(canopy_flam_data, method = "kendall", 
                       use = "pairwise")


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




#################################################################################
# Saving the anova table of the best models without Juniperus
# using Kenward_Roger approximation
#################################################################################

#print(canopy_leaf_anova_withoutj, type = "html",
      #file = "./results/canopy_anova_table_withoutj.html")

#print(canopy_leaf_coeff_withoutj, type = "html",
      #file = "./results/canopy_anova_coeff_table_withoutj.html")


#print(ignition_xtable_withoutj, type = "html",
      #file = "./results/ignition_anova_table_without_juniperus.html")


#print(ignition_coeff_withoutj, type = "html",
      #file = "./results/ignition_coeff_table_without_juniperus.html")


#########################################################################
# Saving the Marginal R2 and conditional R2 of the best models
#########################################################################

# Source: https://stackoverflow.com/questions/62197268/knitting-
# output-from-sjplottab-model-or-other-html-tables-into-pdf-document

#########################################################################
# best canopy traits model first fitted by by Kenward-Roger's method
# by afex package
# sjPlot creates object as list
#########################################################################

best_canopy_traits_model_as_list <- sjPlot::tab_model(canopy_traits_heat_release_model_mixed)

# Creating vector containing HTML code

null_vector <- c() 
for (i in 1:3) {
  null_vector[i] <- best_canopy_traits_model_as_list[[i]]
}

# Reading into data frame.

best_canopy_traits_model_as_data_frame <- XML::readHTMLTable(null_vector, as.data.frame = TRUE)

# Data frame is retrieved as repeated items in list. 
# Keeping only one.

best_canopy_traits_model_as_data_frame <- best_canopy_traits_model_as_data_frame[[1]]

# Setting column names correctly.

colnames(best_canopy_traits_model_as_data_frame) <- best_canopy_traits_model_as_data_frame[1,]

# The first row apperaed twice

best_canopy_traits_model_as_data_frame <- best_canopy_traits_model_as_data_frame[-1,]

best_canopy_traits_model_as_xtable <- xtable::xtable(best_canopy_traits_model_as_data_frame)

#print(best_canopy_traits_model_as_xtable,
      #type = "html", file = "./results/best_canopy_traits_model_r2.html")


#########################################################################################
# Now the best leaf traits model for temperature integration
#########################################################################################

best_leaf_traits_model_as_list <- sjPlot::tab_model(leaf_traits_heat_release_model_mixed)

# Creating vector containing HTML code

null_vector2 <- c() 
for (i in 1:3) {
  null_vector2[i] <- best_leaf_traits_model_as_list[[i]]
}

# Reading into data frame.

best_leaf_traits_model_as_data_frame <- XML::readHTMLTable(null_vector2, as.data.frame = TRUE)

# Data frame is retrieved as repeated items in list. 
# Keeping only one.

best_leaf_traits_model_as_data_frame <- best_leaf_traits_model_as_data_frame[[1]]

# Setting column names correctly.

colnames(best_leaf_traits_model_as_data_frame) <- best_leaf_traits_model_as_data_frame[1,]

# The first row apperaed twice

best_leaf_traits_model_as_data_frame <- best_leaf_traits_model_as_data_frame[-1,]

best_leaf_traits_model_as_xtable <- xtable::xtable(best_leaf_traits_model_as_data_frame)

#print(best_leaf_traits_model_as_xtable,
      #type = "html", file = "./results/best_leaf_traits_model_r2.html")


####################################################################################
# Now the best models for ignition delay
####################################################################################

best_canopy_traits_ignition_model_as_list <- sjPlot::tab_model(canopy_traits_ignition_model_mixed)

# Creating vector containing HTML code

null_vector3 <- c() 
for (i in 1:3) {
  null_vector3[i] <- best_canopy_traits_ignition_model_as_list[[i]]
}

# Reading into data frame.

best_canopy_traits_ignition_model_as_data_frame <- XML::readHTMLTable(null_vector3, as.data.frame = TRUE)

# Data frame is retrieved as repeated items in list. 
# Keeping only one.

best_canopy_traits_ignition_model_as_data_frame <- best_canopy_traits_ignition_model_as_data_frame[[1]]

# Setting column names correctly.

colnames(best_canopy_traits_ignition_model_as_data_frame) <- best_canopy_traits_ignition_model_as_data_frame[1,]

# The first row apperaed twice

best_canopy_traits_ignition_model_as_data_frame <- best_canopy_traits_ignition_model_as_data_frame[-1,]

best_canopy_traits_ignition_model_as_xtable <- xtable::xtable(best_canopy_traits_ignition_model_as_data_frame)

#print(best_canopy_traits_ignition_model_as_xtable,
      #type = "html", file = "./results/best_canopy_traits_ignition_model_r2.html")

#######################################################################################
# Now best leaf traits model for ignition delay
########################################################################################


best_leaf_traits_ignition_model_as_list <- sjPlot::tab_model(leaf_traits_ignition_model_mixed)

# Creating vector containing HTML code

null_vector4 <- c() 
for (i in 1:3) {
  null_vector4[i] <- best_leaf_traits_ignition_model_as_list[[i]]
}

# Reading into data frame.

best_leaf_traits_ignition_model_as_data_frame <- XML::readHTMLTable(null_vector4, as.data.frame = TRUE)

# Data frame is retrieved as repeated items in list. 
# Keeping only one.

best_leaf_traits_ignition_model_as_data_frame <- best_leaf_traits_ignition_model_as_data_frame[[1]]

# Setting column names correctly.

colnames(best_leaf_traits_ignition_model_as_data_frame) <- best_leaf_traits_ignition_model_as_data_frame[1,]

# The first row apperaed twice

best_leaf_traits_ignition_model_as_data_frame <- best_leaf_traits_ignition_model_as_data_frame[-1,]

best_leaf_traits_ignition_model_as_xtable <- xtable::xtable(best_leaf_traits_ignition_model_as_data_frame)

#print(best_leaf_traits_ignition_model_as_xtable,
      #type = "html", file = "./results/best_leaf_traits_ignition_model_r2.html")


#############################################################################
# The next part will produce the maximum ignition delay, 
# maximum leaf moisture content of the final data
# the result will be reported in the discussion
#############################################################################

which.max(final_data$ignition_delay) #33
which.max(final_data$leaf_moisture_content) # 80
which.max(final_data$canopy_moisture_content) # 50

moisture_content_ignition_data <- final_data[c(33,80, 50), ] %>%
  select(sample_id, specific_epithet, display_name,
         leaf_moisture_content, canopy_moisture_content, ignition_delay)


maximum_moisture_ignition_delay_xtable <- xtable::xtable(moisture_content_ignition_data)

print(maximum_moisture_ignition_delay_xtable,
            type = "html", 
      file = "./results/maximum_moisture_ignition_delay_xtable.html")

####################################################################################      


