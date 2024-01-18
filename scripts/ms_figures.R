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

dim(model_data)

best_degsec_plot_data <- model_data %>% 
  dplyr::select(total_dry_mass_g, leaf_length_per_leaflet,
                leaf_moisture_content, degsec_100, analysis_group)

dim(best_degsec_plot_data)

###############################################################################
# Summarising the fixed effects by group 
###############################################################################


degsec_by_group <- model_data %>% 
  group_by(analysis_group) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            degsec_100 = mean(degsec_100))


###############################################################################
# Plotting the scaled total mass and leaf length per leaf and adding layer by summarized
# data
###############################################################################

total_dry_mass <- ggplot(model_data, aes(total_dry_mass_g, degsec_100)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  pubtheme +
  theme(legend.position = "none") +
  ## DWS: hard coding the values below is not ideal. This is fragile code.
  geom_abline(intercept = coef(summary(canopy_traits_heat_release_model_mixed))[1], 
              slope = coef(summary(canopy_traits_heat_release_model_mixed))[2], size = 1.5, color = "black")

ggsave("./results/total_dry_mass.pdf",
       plot = total_dry_mass, height = 180,
       width = 170, units = "mm", dpi = 300)


leaf_moisture_content <- ggplot(model_data, aes(leaf_moisture_content, degsec_100)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Leaf moisture content (%)")  + 
  pubtheme +
  theme(legend.position = "none") +
  geom_abline(intercept = coef(summary(leaf_traits_heat_release_model_mixed))[1], 
              slope = coef(summary(leaf_traits_heat_release_model_mixed))[3], size = 1.5, color = "black")



ggsave("./results/leaf_moisture_content.pdf",
       plot = leaf_moisture_content, height = 180,
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
  labs(x = "Principle component 1",
       y = "Principle component 2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")) +
  xlim(-0.1, 0.6) +
  geom_text(x = variable_loadings[1,1] + 0.08, y = variable_loadings[1,2] - 0.025,
            label = "Mass consumed (g)", size = 4, color = "black") +
  geom_text(x = variable_loadings[2,1] + 0.08, y = variable_loadings[2,2] + 0.02,
            label = "Volume burned (%)", size = 4, color = "black") +
  geom_text(x = variable_loadings[3,1] + 0.08, y = variable_loadings[3,2],
            label = "Flame height (cm)", size = 4, color = "black") +
  geom_text(x = variable_loadings[4,1] + 0.085, y = variable_loadings[4,2],
            label = "Flame duration (s)", size = 4, color = "black") +
  geom_text(x = variable_loadings[5,1] + 0.09, y = variable_loadings[5,2] - 0.03,
            label = "Duration over 100 \u00B0C (s)  ", size = 4, color = "black") +
  geom_text(x = variable_loadings[6,1] + 0.098, y = variable_loadings[6,2],
            label = "Peak temperature (\u00B0C)", size = 4, color = "black") +
  geom_text(x = variable_loadings[7,1] + 0.1, y = variable_loadings[7,2] + 0.04,
            label = "Temperature integration (\u00B0C.s)", size = 4, color = "black") +
  geom_text(x = variable_loadings[8,1] + 0.01, y = variable_loadings[8,2]-0.015,
            label = "Ignition delay time (s)", size = 4, color = "black") 
 
  

ggsave("./results/pca_plot.pdf",
       plot = pca_plot, height = 180,
       width = 170, units = "mm", dpi = 300)
