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

###############################################################################
# The best model for canopy traits for temperature integration using scaled
# variables from without_juniperus from analysis.R script
###############################################################################

###############################################################################
# Selecting scaled variables from model data 
###############################################################################

final_data <- final_data %>%
  mutate(display_name = ifelse(display_name != "J. spp", "Other species", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "ashei", "J. ashei", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "pinchotii", "J. pinchotii", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "virginiana", "J. virginiana", display_name))
  
unique(final_data$display_name)

model_data_withconifers <- model_data_withconifers %>%
  mutate(display_name = ifelse(display_name != "J. spp", "Other species", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "ashei", "J. ashei", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "pinchotii", "J. pinchotii", display_name)) %>%
  mutate(display_name = ifelse(specific_epithet == "virginiana", "J. virginiana", display_name))

unique(model_data_withconifers$display_name)

###############################################################################
# Summarising the fixed effects by group 
###############################################################################


degsec_by_group_by_raw_data <- final_data %>% 
  group_by(specific_epithet) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            canopy_moisture_content = mean(canopy_moisture_content),
            degsec_100 = mean(degsec_100),
            ignition_delay = mean( ignition_delay)) %>%
  right_join(select(final_data, specific_epithet, display_name))

degsec_by_group_by_raw_data_for_ig <- model_data_withconifers %>% 
  group_by(specific_epithet) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            canopy_moisture_content = mean(canopy_moisture_content),
            degsec_100 = mean(degsec_100),
            ignition_delay = mean( ignition_delay)) %>%
  right_join(select(final_data, specific_epithet, display_name))

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


tb_x11 <- 1

tb_x11_transfromed <- tb_x11*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_x22 <- 3.5

tb_x22_transformed <- tb_x22*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_y11 = coef(summary(canopy_traits_heat_release_model_mixed_withconifers))[1] + 
  coef(summary(canopy_traits_heat_release_model_mixed_withconifers))[2]*tb_x11

tb_y22 = coef(summary(canopy_traits_heat_release_model_mixed_withconifers))[1] + 
  coef(summary(canopy_traits_heat_release_model_mixed_withconifers))[2]*tb_x22

tb_slope1 <- (tb_y22 - tb_y11) / (tb_x22_transformed - tb_x11_transfromed)
tb_intercept1 <- tb_y11 - tb_slope1 * tb_x11_transfromed


backtransformed_total_dry_mass <- ggplot(final_data, aes(total_dry_mass_g, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(bold(Temperature ~ integration ~ (degree~C %.% s )) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  labs(color = "Species") +
  labs(tag = "(a)") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = "none",
        plot.tag.position = c(0.08, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = tb_intercept, 
              slope = tb_slope, size = 1.5, color = "black") +
  geom_abline(intercept = tb_intercept1, 
              slope = tb_slope1, size = 1.5, color = "black", linetype = "dashed")



ll_x1 <- 0.5

ll_x1_transfromed <- ll_x1*sd(final_data$leaf_length_per_leaflet) + mean(final_data$leaf_length_per_leaflet)

ll_x2 <- 3.0

ll_x2_transformed <- ll_x2*sd(final_data$leaf_length_per_leaflet) + mean(final_data$leaf_length_per_leaflet)

ll_y1 = coef(summary(leaf_traits_heat_release_model_mixed))[1] + 
  coef(summary(leaf_traits_heat_release_model_mixed))[2]*ll_x1

ll_y2 = coef(summary(leaf_traits_heat_release_model_mixed))[1] + 
  coef(summary(leaf_traits_heat_release_model_mixed))[2]*ll_x2

ll_slope <- (ll_y2 - ll_y1) / (ll_x2_transformed - ll_x1_transfromed)
ll_intercept <- ll_y1 - ll_slope * ll_x1_transfromed




backtransformed_leaf_length <- ggplot(final_data, aes(leaf_length_per_leaflet, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("") +
  xlab("Leaf length (cm)")  + 
  labs(color = "") +
  pubtheme +
  labs(tag = "(b)") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = c(0.80, 0.85),
        legend.text = element_text(face = "italic"),
        plot.tag.position = c(0.01, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"),
        axis.text.y = element_blank()) +
  geom_abline(intercept = ll_intercept, 
              slope = ll_slope, size = 1.5, color = "black")

combined_degsec <- backtransformed_total_dry_mass | backtransformed_leaf_length

ggsave("./results/combined_degsec.pdf",
       plot = combined_degsec, height = 120,
       width = 200, units = "mm", dpi = 300)




##############################################################################
# Back transformation and adjusting the slope and intercept for leaf moisture
#############################################################################

fmc_x1 <- 0

fmc_x1_transfromed <- fmc_x1*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_x2 <- 2

fmc_x2_transformed <- fmc_x2*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_y1 = coef(summary(leaf_traits_ignition_model_mixed))[1] + 
  coef(summary(leaf_traits_ignition_model_mixed))[4]*fmc_x1

fmc_y2 = coef(summary(leaf_traits_ignition_model_mixed))[1] + 
  coef(summary(leaf_traits_ignition_model_mixed))[4]*fmc_x2

#fmc_y1_back <- exp(fmc_y1) - 1
#fmc_y2_back <- exp(fmc_y2) - 1

fmc_slope <- (fmc_y2 - fmc_y1) / (fmc_x2_transformed - fmc_x1_transfromed)
fmc_intercept <- fmc_y1 - fmc_slope * fmc_x1_transfromed

fmc_x11 <- 0

fmc_x11_transfromed <- fmc_x11*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_x22 <- 2

fmc_x22_transformed <- fmc_x22*sd(final_data$leaf_moisture_content) + mean(final_data$leaf_moisture_content)

fmc_y11 = coef(summary(leaf_traits_ignition_model_mixed_withconifers))[1] + 
  coef(summary(leaf_traits_ignition_model_mixed_withconifers))[3]*fmc_x11

fmc_y22 = coef(summary(leaf_traits_ignition_model_mixed_withconifers))[1] + 
  coef(summary(leaf_traits_ignition_model_mixed_withconifers ))[4]*fmc_x22

#fmc_y11_back <- exp(fmc_y11) - 1
#fmc_y22_back <- exp(fmc_y22) - 1

fmc_slope1 <- (fmc_y22 - fmc_y11) / (fmc_x22_transformed - fmc_x11_transfromed)
fmc_intercept1 <- fmc_y11 - fmc_slope1 * fmc_x11_transfromed

##################################################################################################

leaf_moisture_content_back_transformed <- ggplot(model_data_withconifers, aes(leaf_moisture_content, ignition_delay, 
                                                                 color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data_for_ig, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("") +
  xlab("Leaf moisture content")  + 
  labs(color = "") +
  pubtheme +
  labs(tag = "(b)") +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = "none",
        plot.tag.position = c(0.01, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = coef(summary(leaf_traits_ignition_model_mixed_withconifers))[1],
              slope = coef(summary(leaf_traits_ignition_model_mixed_withconifers))[3], size = 1.5, color = "black",
              linetype = "dashed") +
  geom_abline(intercept = coef(summary(leaf_traits_ignition_model_mixed))[1],
              slope = coef(summary(leaf_traits_ignition_model_mixed))[4], size = 1.5, color = "black")



shoot_moisture_content_back_transformed <- ggplot(model_data_withconifers, aes(canopy_moisture_content, ignition_delay, 
                                                                              color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data_for_ig, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("log(ignition delay time + 1) (s)") +
  xlab("Shoot moisture content")  + 
  labs(color = "") +
  pubtheme +
  labs(tag = "(a)") +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.20, 0.85),
        plot.tag.position = c(0.01, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = coef(summary(canopy_traits_ignition_model_mixed_withconifers))[1],
              slope = coef(summary(canopy_traits_ignition_model_mixed_withconifers))[3], size = 1.5, color = "black",
              linetype = "dashed") +
  geom_abline(intercept = coef(summary(canopy_traits_ignition_model_mixed))[1],
              slope = coef(summary(canopy_traits_ignition_model_mixed))[2], size = 1.5, color = "black") 

combined_id <- shoot_moisture_content_back_transformed | leaf_moisture_content_back_transformed

ggsave("./results/combined_id.pdf",
       plot = combined_id, height = 120,
       width = 200, units = "mm", dpi = 300)

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

print(canopy_ignition_xtable_withconifers, type = "html", 
      file = "./results/canopy_ignition_anova_table_withj.html")

print(canopy_ignition_coeff_withconifers, type = "html", 
      file = "./results/canopy_ignition_coeff_withj.html")

print(canopy_ignition_xtable, type = "html", 
      file = "./results/canopy_ignition_anova_table.html")

print(canopy_ignition_coeff, type = "html", 
      file = "./results/canopy_ignition_coeff.html")

print(leaf_ignition_xtable_withconifers, type = "html", 
      file = "./results/leaf_ignition_anova_table_withj.html")

print(leaf_ignition_coeff_withconifers, type = "html", 
      file = "./results/leaf_ignition_coeff_withj.html")

print(leaf_ignition_xtable, type = "html",
      file = "./results/leaf_ignition_anova_table.html")

print(leaf_ignition_coeff, type = "html",
      file = "./results/leaf_ignition_coeff.html")


cor_data <- pca_data_2022 %>%
  dplyr::select(- c("sample_id", "PC1", "PC2"))
traits_cor <- cor(cor_data, method = "spearman")
traits_cor <- as.data.frame(traits_cor)
corr_plot <- ggcorrplot::ggcorrplot(traits_cor, type = "lower", 
                        lab = TRUE, show.legend = FALSE,      
                        colors = c(NA, NA),      
                        hc.order = TRUE) + 
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 12),
        panel.grid = element_blank())

ggsave("./results/pca_cor_plot.pdf", corr_plot, height = 120,
       width = 200, units = "mm", dpi = 300)

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

