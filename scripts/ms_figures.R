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

best_degsec_canopy_plot_data_withoutj <- without_juniperus %>% 
  dplyr::select(total_dry_mass_g, canopy_density_gm_cm3, 
                degsec_100, analysis_group)

###############################################################################
# Summarising the fixed effects by group 
###############################################################################


degsec_by_group_withoutj <- without_juniperus %>% 
  group_by(analysis_group) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


###############################################################################
# Plotting the scaled total mass and adding layer by summarized
# data
###############################################################################

total_dry_mass <- ggplot(without_juniperus, aes(total_dry_mass_g, degsec_100)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_withoutj, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  pubtheme +
  theme(legend.position = "none") +
  ## DWS: hard coding the values below is not ideal. This is fragile code.
  geom_abline(intercept = canopy_leaf_coeff_withoutj$Estimate[1], slope = canopy_leaf_coeff_withoutj$Estimate[2], size = 1.5, color = "black")

ggsave("./results/total_dry_mass.pdf",
       plot = total_dry_mass, height = 180,
       width = 170, units = "mm", dpi = 300)

###############################################################################
# Leaf moisture content vs ignition delay without juniperus
###############################################################################

leaf_moisture_ignition_plot_data_withoutj <- without_juniperus %>% 
  dplyr::select(leaf_moisture_content,
         ignition_delay, analysis_group)

###############################################################################
# Summarising the fixed effects and response variable by group 
###############################################################################

leaf_ignition_delay_by_group <- without_juniperus %>% 
  group_by(analysis_group) %>%
  summarize(leaf_moisture_content = mean(leaf_moisture_content), 
            ignition_delay = mean(ignition_delay))


leaf_moisture_ignition <- ggplot(without_juniperus, aes(leaf_moisture_content,ignition_delay)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_jitter() +
  geom_point(data = leaf_ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16) +
  ylab("Ignition delay (s)") +
  xlab("Leaf moisture content (%)")  + 
  pubtheme +
  theme(legend.position = "none") +
  geom_abline(intercept = ignition_coeff_withoutj$Estimate[1] , slope = ignition_coeff_withoutj$Estimate[4], size = 1.5, color = "black")


leaf_moisture_ignition
ggsave("./results/leaf_moisture_ignition.pdf",
       plot = leaf_moisture_ignition, height = 180,
       width = 170, units = "mm", dpi = 300)


###############################################################################
# Renamed pca plot The problem is there are nine variables. First problem is
# overlapping into the plot, even repel function can't fix it. Decided to use
# the abbreviation here. Second problem is I had to rename the variables either
# in this script or flam_pca.R script. Thought, doing it at the end of
# everything might not be a bad idea because can fix it easily later.
###############################################################################

pca_data_renamed_2022 <- pca_data_2022 %>%
  select(-PC1, -PC2) %>%
  rename(ID = ignition_delay,
         FH = flame_height,
         FD = flame_duration,
         TI = degsec_100,
         DH = dur_100,
         PT = peak_temp,
         MC = massconsumed,
         VB = vol_burned)

renamed_pca <- prcomp(pca_data_renamed_2022[,-1], scale = TRUE)

renamed_pca_plot <- fviz_pca_var(renamed_pca,
                                 repel = TRUE, col.circle = "white") +
  xlab("Principle component 1") +
  ylab("Principle component 2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(),
        plot.title = element_blank()) 

ggsave("./results/pca_plot.pdf",
       plot = renamed_pca_plot, height = 180,
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

