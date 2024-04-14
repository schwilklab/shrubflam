## ms_figures.R

# This R script is used to create figures showing significant fixed effect of
# canopy and leaf traits on flammability from analysis.R and also model anova
# and coefficient tables

library(xtable)

# Plot theme for publication standard
source("./scripts/ggplot_theme.R") 

## This script depends on analysis.R. See run-all.R

###############################################################################
# The best model for canopy traits for temperature integration using scaled
# variables from without_juniperus from analysis.R script
###############################################################################

###############################################################################
# Selecting scaled variables from model data 
###############################################################################


best_degsec_plot_data <- model_data %>% 
  dplyr::select(total_dry_mass_g, leaf_length_per_leaflet,
                leaf_moisture_content, degsec_100, display_name)

dim(best_degsec_plot_data)

###############################################################################
# Summarising the fixed effects by group 
###############################################################################


degsec_by_group <- model_data %>% 
  group_by(display_name) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            degsec_100 = mean(degsec_100))

degsec_by_group_by_raw_data <- final_data %>% 
  group_by(display_name) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            degsec_100 = mean(degsec_100))

##############################################################################
# Back transformation and adjusting the slope and intercept
#############################################################################
tb_x1 <- 1

tb_x1_transfromed <- tb_x1*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_x2 <- 3.5

tb_x2_transformed <- tb_x2*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_y1 = coef(summary(canopy_traits_heat_release_model_mixed))[1] + 
  coef(summary(canopy_traits_heat_release_model_mixed))[2]*tb_x1

tb_y2 = coef(summary(canopy_traits_heat_release_model_mixed))[1] + 
  coef(summary(canopy_traits_heat_release_model_mixed))[2]*tb_x2

tb_slope <- (tb_y2 - tb_y1) / (tb_x2_transformed - tb_x1_transfromed)
tb_intercept <- tb_y1 - tb_slope * tb_x1_transfromed


###############################################################################
# Plotting the scaled total mass and leaf length per leaf and adding layer by summarized
# data
###############################################################################

total_dry_mass <- ggplot(best_degsec_plot_data, aes(total_dry_mass_g, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  labs(color = "Species") +
  pubtheme +
  scale_color_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                                "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "grey", "#4900FF",
                                "#B600FF",  "#FF6D00", "#FF00DB")) +
  scale_fill_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                               "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "#4900FF",
                               "#B600FF",  "#FF6D00", "#FF00DB")) +
  theme(legend.text = element_text(face = "italic")) +
  geom_abline(intercept = coef(summary(canopy_traits_heat_release_model_mixed))[1], 
              slope = coef(summary(canopy_traits_heat_release_model_mixed))[2], size = 1.5, color = "black")


ggsave("./results/total_dry_mass.pdf",
       plot = total_dry_mass, height = 180,
       width = 170, units = "mm", dpi = 300)

backtransformed_total_dry_mass <- ggplot(final_data, aes(total_dry_mass_g, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  labs(color = "Species") +
  pubtheme +
  scale_color_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                                "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "grey", "#4900FF",
                                "#B600FF",  "#FF6D00", "#FF00DB")) +
  scale_fill_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                               "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "#4900FF",
                               "#B600FF",  "#FF6D00", "#FF00DB")) +
  theme(legend.text = element_text(face = "italic")) +
  geom_abline(intercept = tb_intercept, 
              slope = tb_slope, size = 1.5, color = "black")

ggsave("./results/total_dry_mass_back_transformed.pdf",
       plot = backtransformed_total_dry_mass, height = 180,
       width = 170, units = "mm", dpi = 300)

##################################################################################################
# leaf moisture content
##################################################################################################

leaf_moisture_content <- ggplot(model_data, aes(leaf_moisture_content, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Leaf moisture content (%)")  + 
  labs(color = "Species") +
  pubtheme +
  scale_color_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                                "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "grey", "#4900FF",
                                "#B600FF",  "#FF6D00", "#FF00DB")) +
  scale_fill_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                               "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "#4900FF",
                               "#B600FF",  "#FF6D00", "#FF00DB")) +
  theme(legend.text = element_text(face = "italic")) +
  geom_abline(intercept = coef(summary(leaf_traits_heat_release_model_mixed))[1], 
              slope = coef(summary(leaf_traits_heat_release_model_mixed))[3], size = 1.5, color = "black") 



ggsave("./results/leaf_moisture_content.pdf",
       plot = leaf_moisture_content, height = 180,
       width = 170, units = "mm", dpi = 300)

##############################################################################
# Back transformation and adjusting the slope and intercept for leaf moisture
#############################################################################
fmc_x1 <- 0

fmc_x1_transfromed <- fmc_x1*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_x2 <- 2

fmc_x2_transformed <- fmc_x2*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_y1 = coef(summary(leaf_traits_heat_release_model_mixed))[1] + 
  coef(summary(leaf_traits_heat_release_model_mixed))[3]*fmc_x1

fmc_y2 = coef(summary(leaf_traits_heat_release_model_mixed))[1] + 
  coef(summary(leaf_traits_heat_release_model_mixed))[3]*fmc_x2

fmc_slope <- (fmc_y2 - fmc_y1) / (fmc_x2_transformed - fmc_x1_transfromed)
fmc_intercept <- fmc_y1 - fmc_slope * fmc_x1_transfromed

leaf_moisture_content_back_transformed <- ggplot(final_data, aes(leaf_moisture_content, degsec_100, 
                                                                 color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Leaf moisture content (%)")  + 
  labs(color = "Species") +
  pubtheme +
  scale_color_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                                "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "grey", "#4900FF",
                                "#B600FF",  "#FF6D00", "#FF00DB")) +
  scale_fill_manual(values = c("brown", "black", "#FFDB00", "#B6FF00", "#49FF00",
                               "#FF0000", "#00FF92", "#00FFFF", "#0092FF", "#4900FF",
                               "#B600FF",  "#FF6D00", "#FF00DB")) +
  theme(legend.text = element_text(face = "italic")) +
  geom_abline(intercept = fmc_intercept, 
              slope = fmc_slope, size = 1.5, color = "black")

ggsave("./results/leaf_moisture_content_back_transformed.pdf",
       plot = leaf_moisture_content_back_transformed, height = 180,
       width = 170, units = "mm", dpi = 300)

############################################################################
# Saving model table as html from anova table, at first 
# temperature ingtegration for predictors from best
# canopy traits and best leaf traits model
############################################################################

print(canopy_anova, type = "html", file = "./results/canopy_anova_table.html")

print(canopy_coeff, type = "html", file = "./results/canopy_anova_coefficients.html")

print(leaf_anova, type = "html", file = "./results/leaf_anova_table.html")

print(leaf_coeff, type = "html", file = "./results/leaf_anova_coefficients.html")

################################################################################
# Now for Ignition for both canopy and leaf traits
################################################################################


print(canopy_ignition_xtable, type = "html", 
      file = "./results/canopy_ignition_anova_table.html")


print(canopy_ignition_coeff, type = "html", 
      file = "./results/canopy_ignition_coeff.html")

print(leaf_ignition_xtable, type = "html",
      file = "./results/leaf_ignition_anova_table.html")

print(leaf_ignition_coeff, type = "html",
      file = "./results/leaf_ignition_coeff.html")

################################################################################
# PCA plot
###############################################################################

pca_axis <- as.data.frame(flam_pca_2022$x)

variable_loadings <- as.data.frame(flam_pca_2022$rotation)

pca_plot <- ggplot(pca_axis, aes(x = PC1, y = PC2)) +
  geom_segment(data = variable_loadings, aes(
    x = 0, y = 0,
    xend = PC1, yend = PC2),
    arrow = arrow(length = unit(4, "mm")),
    color = "black") +
  labs(x = "Principle component 1 (75.9%)",
       y = "Principle component 2 (10.1%)") +
  pubtheme +
  xlim(-0.1, 0.62) +
  geom_text(x = variable_loadings[1,1] + 0.08, y = variable_loadings[1,2] - 0.01,
            label = "Mass consumed", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[2,1] + 0.07, y = variable_loadings[2,2] + 0.02,
            label = "Volume burned", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[3,1] + 0.065, y = variable_loadings[3,2],
            label = "Flame height", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[4,1] + 0.08, y = variable_loadings[4,2] - 0.01,
            label = "Flame duration", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[5,1] + 0.07, y = variable_loadings[5,2] - 0.03,
            label = "Duration over 100 \u00B0C", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[6,1] + 0.082, y = variable_loadings[6,2],
            label = "Peak temperature", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[7,1] + 0.1, y = variable_loadings[7,2] + 0.028,
            label = "Temperature integration", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = variable_loadings[8,1] + 0.01, y = variable_loadings[8,2]-0.015,
            label = "Ignition delay time", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) 
 
  


ggsave("./results/pca_plot.pdf",
       plot = pca_plot, height = 180,
       width = 170, units = "mm", dpi = 300)
