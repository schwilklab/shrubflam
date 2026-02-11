## ms_figures.R

# This R script is used to create figures showing significant fixed effect of
# canopy and leaf traits on flammability from analysis.R and also model anova
# and coefficient tables

library(xtable)
library(patchwork)
library(corrplot)
library(ggcorrplot)

# Plot theme for publication standard
source("./scripts/ggplot_theme.R") 

## This script depends on analysis.R. See run-all.R

fig_data <- as.data.frame(unique(alldata_2022[, c("specific_epithet", "display_name")])) %>%
  left_join(final_data) %>%
  mutate(display_name = ifelse(display_name != "J. spp", "Other species", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "ashei", "J. ashei", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "pinchotii", "J. pinchotii", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "virginiana", "J. virginiana", display_name))


##########################################################################################
# Flame duraion vs total dry mass and leaf stem mass ratio
##########################################################################################

total_dry_mass <- ggplot(fig_data, aes(total_dry_mass_g, flame_duration, color = display_name)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1),
             size = 4.5, alpha = 0.8, shape = 16) + 
  ylim(0, 350) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black") +
  ylab("Flame duration (s)") +
  xlab("Total dry mass per 70 cm (g)")  + 
  labs(tag = "(a)") +
  labs(color = "") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = c(0.30, 0.85),
        legend.text = element_text(face = "italic"),
        plot.tag.position = c(0.04, 1),
        plot.tag = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

leaf_stem <- ggplot(fig_data, aes(leaf_stem_mass_ratio, flame_duration, color = display_name)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1),
             size = 4.5, alpha = 0.8, shape = 16) + 
  ylim(0, 350) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black") +
  ylab("") +
  xlab("Leaf stem mass ratio")  + 
  labs(tag = "(b)") +
  labs(color = "") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = "none",
        plot.tag.position = c(0.04, 1),
        plot.tag = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))
 
combined_crwon <- total_dry_mass | leaf_stem

ggsave("./results/figure3.pdf",
       plot = combined_crwon, height = 120,
       width = 200, units = "mm", dpi = 300)

##############################################################################
# Ignition delay time vs total dry mass
##############################################################################

total_dry_mass_ig <- ggplot(fig_data, aes(total_dry_mass_g, ignition_delay, color = display_name)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1),
             size = 4.5, alpha = 0.8, shape = 16) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5, color = "black") +
  ylab("Time to ignition (s)") +
  xlab("Total dry mass per 70 cm (g)")  + 
  #labs(tag = "(a)") +
  labs(color = "") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = c(0.25, 0.90),
        legend.text = element_text(face = "italic"),
        #plot.tag.position = c(0.04, 1),
        #plot.tag = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))


ggsave("./results/figure4.pdf",
       plot = total_dry_mass_ig, height = 120,
       width = 100, units = "mm", dpi = 300)

############################################################################
# Saving model table as html from anova table, at first 
# temperature ingtegration for predictors from best
# traits model
############################################################################

print(xtable::xtable(anova(best_temp_int_mod)), type = "html", 
      file = "./results/canopy_temp_int_anova_table.html")

################################################################################
# Now for Ignition
################################################################################

print(xtable::xtable(anova(best_temp_ign_mod)), type = "html", 
      file = "./results/canopy_ignition_anova_table.html")

##################################################################################
cor_data <- final_data %>%
  dplyr::select(total_dry_mass_g, canopy_density_gm_cm3, leaf_stem_mass_ratio,
                canopy_moisture_content, leaf_mass_per_area, 
                leaf_area_per_leaflet, leaf_length_per_leaflet,
                ignition_delay, flame_duration) %>%
  rename(LFMC = canopy_moisture_content,
         shoot_bulk_density = canopy_density_gm_cm3,
         total_dry_mass = total_dry_mass_g,
         leaf_area = leaf_area_per_leaflet,
         leaf_length = leaf_length_per_leaflet,
         time_to_ignition = ignition_delay,
         LMA = leaf_mass_per_area)
traits_cor <- cor(cor_data, method = "kendall")
traits_cor <- as.data.frame(traits_cor)
corr_plot <- ggcorrplot::ggcorrplot(traits_cor, type = "lower", 
                        lab = TRUE, show.legend = FALSE,      
                        colors = c(NA, NA),      
                        hc.order = TRUE) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 12),
        panel.grid = element_blank())

ggsave("./results/Supporting_info_Figure 1.pdf", corr_plot, height = 120,
       width = 200, units = "mm", dpi = 300)

################################################################################
# PCA plot
###############################################################################

pca_axis <- as.data.frame(flam_pca_2022$x)

pca_axis$PC1 <- -1 * pca_axis$PC1
pca_axis$PC2 <- -1 * pca_axis$PC2

variable_loadings <- as.data.frame(flam_pca_2022$rotation)

variable_loadings$PC1 <- -1*(variable_loadings$PC1)
variable_loadings$PC2 <- -1*(variable_loadings$PC2)

pca_plot <- ggplot(variable_loadings, aes(x = PC1, y = PC2)) +
  geom_point(data = pca_axis,
          aes(x = PC1, y = PC2),
             color = "black",
             alpha = 0.4,
             size = 3) +
  geom_segment(data = variable_loadings, aes(
    x = 0, y = 0,
    xend = PC1, yend = PC2),
    arrow = arrow(length = unit(4, "mm")),
    color = "black") +
  labs(x = "Principle component 1 (84.1%)",
       y = "Principle component 2 (8.9%)") +
  pubtheme +
  geom_text(x = variable_loadings[1,1] + 0.60, y = variable_loadings[1,2] + 0.05,
            label = "Volume burned", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[2,1] + 0.065, y = variable_loadings[2,2] + 0.05,
            label = "Flame height", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[3,1] + 0.75, y = variable_loadings[3,2] + 0.015,
            label = "Flame duration", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[4,1] + 0.75, y = variable_loadings[4,2] - 0.1,
            label = "Duration over 100 \u00B0C", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[5,1] + 0.95, y = variable_loadings[5,2],
            label = "Peak temperature", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[6,1] + 0.95, y = variable_loadings[6,2] + 0.15,
            label = "Temperature integration", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  geom_text(x = variable_loadings[7,1] + 0.07, y = variable_loadings[7,2]-0.030,
            label = "Time to ignition", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.5) +
  theme(axis.title = element_text(size = textsize, face = "bold"))

ggsave("./results/Figure2.pdf",
       plot = pca_plot, height = 180,
       width = 170, units = "mm", dpi = 300)

