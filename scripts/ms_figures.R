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

best_degsec_canopy_plot_data <- model_data %>% 
  dplyr::select(total_dry_mass_g, degsec_100, analysis_group)

dim(best_degsec_canopy_plot_data)

###############################################################################
# Summarising the fixed effects by group 
###############################################################################


degsec_by_group <- model_data %>% 
  group_by(analysis_group) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            degsec_100 = mean(degsec_100))


###############################################################################
# Plotting the scaled total mass and adding layer by summarized
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
  geom_abline(intercept = canopy_anova_coefficients[1], 
              slope = canopy_anova_coefficients[2], size = 1.5, color = "black")

ggsave("./results/total_dry_mass.pdf",
       plot = total_dry_mass, height = 180,
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

