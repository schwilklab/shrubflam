#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022
# This script will produce results of analysis related to
# supplimentary information
# All scripts in source are required to run before running
# this script.

library(car)
library(corrplot)
library(ggcorrplot)
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

morphological_traits_cor <- as.data.frame(morphological_traits_cor)



morphological_traits_cor_plot <- ggcorrplot(morphological_traits_cor, hc.order = TRUE, type = "lower",
                                                    lab = TRUE) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 12))

ggsave("./results/morphological_traits_correlation_plot.pdf",
       plot = morphological_traits_cor_plot,
       width = 18, height = 15, units = "cm")




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



####################################################################################
# Flammability ranking and spearman correlation between shoot flammability ranking and
# flammability ranking by firewise
####################################################################################

flam_rank_data <- final_data %>%
  group_by(specific_epithet) %>%
  summarise(degsec_100 = mean(degsec_100))

cluster_analysis <- kmeans(flam_rank_data$degsec_100, 4)

flam_rank_data$clusters <- cluster_analysis$cluster

flam_rank_data <- flam_rank_data %>%
  filter(! display_name %in% c("C. erecta", "R. virens", "S. berlandieri", "S. obtusifolia", "R. trilobata",
                               "F. pubescens"))

unique(flam_rank_data$display_name)

flam_rank_data$shoot_flam <- c(1,1,1,3,2,2,3,1)
flam_rank_data$firewise <- c(2,2,3,3,1,2,3,3)

#View(flam_rank_data)

ranking_cor <- cor.test(flam_rank_data$shoot_flam, flam_rank_data$firewise, method=c("spearman"))

flam_rank_comparison_plot <-  ggplot(flam_rank_data, aes(x = shoot_flam, y = firewise)) +
  labs(x = "Shoot Flammability Ranking", y = "Firewise Ranking") +
  scale_x_continuous(limits = c(1, 3),        
                     breaks = seq(1, 3, by = 1), 
                     labels = c("Low", "Medium", "High")) +
  scale_y_continuous(limits = c(1, 3),        
                     breaks = seq(1, 3, by = 1), 
                     labels = c("Low", "Medium", "High")) +
  geom_segment(x = 1, xend = 3, y = 1, yend = 3, linetype = "dashed") +
  theme(panel.grid.major  = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_text(x = 2.8, y = 3, label = "Juniperus spp", color = "black", fontface = "italic", family = "sans", 
            alpha = 0.5, size = 4) +
  geom_text(x = 1.15, y = 1.95, label = "Condalia hookeri", color = "black", fontface = "italic", family = "sans", 
            alpha = 0.5, size = 4) +
  geom_text(x = 2.65, y = 2.95, label = "Sophora secundiflora", color = "black", fontface = "italic", family = "sans",
            alpha = 0.5, size = 4) +
  geom_text(x = 1.1, y = 3, label = "Ilex vomitoria", color = "black", fontface = "italic", family = "sans",
            alpha = 0.5, size = 4) +
  geom_text(x = 1.15, y = 2.95, label = "Senegalia wrightii", color = "black", fontface = "italic", family = "sans", 
            alpha = 0.5, size = 4) +
  geom_text(x = 2, y = 2, label = "Prosopis glandulosa", color = "black", fontface = "italic", family = "sans",
            alpha = 0.5, size = 4) +
  geom_text(x = 2, y = 1, label = "Mahonia trifoliolata", color = "black", fontface = "italic", family = "sans", 
            alpha = 0.5, size = 4) +
  geom_text(x = 1.15, y = 2, label = "Diospyros texana", color = "black", fontface = "italic", family = "sans", 
            alpha = 0.5, size = 4) +
  pubtheme




ggsave("./results/flam_rank_comparison_plot.pdf",
       plot = flam_rank_comparison_plot, height = 180,
       width = 170, units = "mm", dpi = 300)



degsec_by_group_final <- final_data %>%
  group_by(display_name) %>%
  summarise(degsec_100 = mean(degsec_100))

final_data$standard_error <- sd(final_data$degsec_100) / sqrt(length(final_data$degsec_100))

flam_rank_plot <- ggplot(final_data, aes(x = reorder(display_name, degsec_100), y = degsec_100)) +
  geom_point(alpha = 0.5) +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    position = position_dodge(width = 1),
    width = 0.5 ) +
  geom_point(data = degsec_by_group_final, size = 3 , alpha = 1) +
  labs(x = "Species",
       y = expression(Temperature ~ integration ~ (degree~C %.% s))) +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 45, hjust = 1, face = "italic")) +
  pubtheme


ggsave("./results/flam_rank_plot.pdf",
       plot = flam_rank_plot, height = 180,
       width = 170, units = "mm", dpi = 300)

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

colnames(best_canopy_traits_model_as_data_frame) <- best_canopy_traits_model_as_data_frame[1, ]

# The first row apperaed twice

best_canopy_traits_model_as_data_frame <- best_canopy_traits_model_as_data_frame[-1, -4]

best_canopy_traits_model_as_xtable <- xtable::xtable(best_canopy_traits_model_as_data_frame)

print(best_canopy_traits_model_as_xtable,
      type = "html", file = "./results/best_canopy_traits_model_r2.html")


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

best_leaf_traits_model_as_data_frame <- best_leaf_traits_model_as_data_frame[-1, -4]

best_leaf_traits_model_as_xtable <- xtable::xtable(best_leaf_traits_model_as_data_frame)

print(best_leaf_traits_model_as_xtable,
      type = "html", file = "./results/best_leaf_traits_model_r2.html")


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

best_canopy_traits_ignition_model_as_data_frame <- best_canopy_traits_ignition_model_as_data_frame[-1, -4]

best_canopy_traits_ignition_model_as_xtable <- xtable::xtable(best_canopy_traits_ignition_model_as_data_frame)

print(best_canopy_traits_ignition_model_as_xtable,
      type = "html", file = "./results/best_canopy_traits_ignition_model_r2.html")

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

best_leaf_traits_ignition_model_as_data_frame <- best_leaf_traits_ignition_model_as_data_frame[-1, -4]

best_leaf_traits_ignition_model_as_xtable <- xtable::xtable(best_leaf_traits_ignition_model_as_data_frame)

print(best_leaf_traits_ignition_model_as_xtable,
      type = "html", file = "./results/best_leaf_traits_ignition_model_r2.html")


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


