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
            leaf_mass_per_area = mean(leaf_mass_per_area),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            canopy_moisture_content = mean(canopy_moisture_content),
            degsec_100 = mean(degsec_100),
            ignition_delay = mean( ignition_delay)) %>%
  right_join(select(final_data, specific_epithet, display_name))

degsec_by_group_by_raw_data_for_ig <- model_data_withconifers %>% 
  group_by(specific_epithet) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g),
            leaf_mass_per_area = mean(leaf_mass_per_area),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            leaf_moisture_content = mean(leaf_moisture_content),
            canopy_moisture_content = mean(canopy_moisture_content),
            degsec_100 = mean(degsec_100),
            ignition_delay = mean( ignition_delay)) %>%
  right_join(select(final_data, specific_epithet, display_name))

##############################################################################
# Back transformation and adjusting the slope and intercept
#############################################################################

tb_x1 <- -1.07

tb_x1_transfromed <- tb_x1*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_x2 <- 3.78

tb_x2_transformed <- tb_x2*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_y1 = coef(summary(mixed_temp_int))[1] + 
  coef(summary(mixed_temp_int))[2]*tb_x1

tb_y2 = coef(summary(mixed_temp_int))[1] + 
  coef(summary(mixed_temp_int))[2]*tb_x2

tb_slope <- (tb_y2 - tb_y1) / (tb_x2_transformed - tb_x1_transfromed)
tb_intercept <- (tb_y1 - (tb_slope * tb_x1_transfromed))*-1

tb_x11 <- -1.07

tb_x11_transfromed <- tb_x11*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_x22 <- 3.5

tb_x22_transformed <- tb_x22*sd(final_data$total_dry_mass_g) + mean(final_data$total_dry_mass_g)

tb_y11 = summary(final_model_temp_int_withoutj)$coefficients[1] + 
  summary(final_model_temp_int_withoutj)$coefficients[2]*tb_x11

tb_y22 = summary(final_model_temp_int_withoutj)$coefficients[1] + 
  summary(final_model_temp_int_withoutj)$coefficients[2]*tb_x22

tb_slope1 <- (tb_y22 - tb_y11) / (tb_x22_transformed - tb_x11_transfromed)
tb_intercept1 <- tb_y11 - tb_slope1 * tb_x11_transfromed

backtransformed_total_dry_mass <- ggplot(final_data, aes(total_dry_mass_g, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab(expression(bold(Temperature ~ integration ~ (degree~C %.% s )) )) +
  xlab("Total dry mass per 70 cm (g)")  + 
  labs(color = "") +
  labs(tag = "(a)") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = c(0.20, 0.93),
        legend.text = element_text(face = "italic"),
        plot.tag.position = c(0.08, 1),
        plot.tag = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = tb_intercept, 
              slope = tb_slope, size = 1.5, color = "black") +
  geom_abline(intercept = tb_intercept1, 
              slope = tb_slope1, size = 1.5, color = "black", linetype = "dashed")

# LMA

lma_x1 <- 0

lma_x1_transfromed <- lma_x1*sd(final_data$leaf_mass_per_area) + mean(final_data$leaf_mass_per_area)

lma_x2 <- 2.5

lma_x2_transformed <- lma_x2*sd(final_data$leaf_mass_per_area) + mean(final_data$leaf_mass_per_area)

lma_y1 = summary(final_model_temp_int)$coefficients[1] + 
  summary(final_model_temp_int)$coefficients[3]*lma_x1

lma_y2 = summary(final_model_temp_int)$coefficients[1] + 
  summary(final_model_temp_int)$coefficients[3]*lma_x2

lma_slope <- (lma_y2 - lma_y1) / (lma_x2_transformed - lma_x1_transfromed)
lma_intercept <- lma_y1 - lma_slope * lma_x1_transfromed

backtransformed_LMA <- ggplot(final_data, aes(leaf_mass_per_area, degsec_100, color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("") +
  xlab(expression(bold(paste("LMA (", g / cm^2, ")"))))  + 
  labs(color = "Species") +
  labs(tag = "(b)") +
  pubtheme +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = "none",
        plot.tag.position = c(0.08, 1),
        plot.tag = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = lma_intercept, 
              slope = lma_slope, size = 1.5, color = "black") 
  

combined_degsec <- backtransformed_total_dry_mass | backtransformed_LMA

ggsave("./results/figure2.pdf",
       plot = combined_degsec, height = 120,
       width = 200, units = "mm", dpi = 300)



##################################################################################################

shoot_moisture_content_ig <- ggplot(model_data_withconifers, aes(canopy_moisture_content, ignition_delay, 
                                                                 color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data_for_ig, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("log(Time to ignition + 1) (s)") +
  xlab("LFMC (%)")  + 
  labs(color = "") +
  pubtheme +
  labs(tag = "(a)") +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = c(c(0.20, 0.90)),
        legend.text = element_text(face = "italic"),
        plot.tag.position = c(0.01, 1),
        plot.tag = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = coef(summary(final_model_ing))[1],
              slope = coef(summary(final_model_ing ))[3], size = 1.5, color = "black") +
  geom_abline(intercept = coef(summary(final_model_ing_withoutj))[1],
              slope = coef(summary(final_model_ing_withoutj))[2], size = 1.5, color = "black",
              linetype = "dashed") +
  scale_x_continuous(breaks = c(-2, 2.59), labels = c("8", "81"))
  


LMA_ig <- ggplot(model_data_withconifers, aes(leaf_mass_per_area, ignition_delay, 
                                                                              color = display_name)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_point(data = degsec_by_group_by_raw_data_for_ig, size = 4.5 , alpha = 1,
             shape = 16) +
  ylab("") +
  xlab(expression(bold(paste("LMA (", g / cm^2, ")")))) + 
  labs(color = "") +
  pubtheme +
  labs(tag = "(b)") +
  scale_color_manual(values = c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  scale_fill_manual(values =  c("#8B0000", "#C41E3A", "#FF6F61", "black")) +
  theme(legend.position = "none",
        plot.tag.position = c(0.01, 1),
        plot.tag = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  geom_abline(intercept = coef(summary(final_model_ing))[1],
        slope = coef(summary(final_model_ing ))[4], size = 1.5, color = "black") +
  scale_x_continuous(breaks = c(-1.10, 2.88), labels = c("0.005", "0.073"))

combined_id <- shoot_moisture_content_ig | LMA_ig

ggsave("./results/figure3.pdf",
       plot = combined_id, height = 120,
       width = 200, units = "mm", dpi = 300)

############################################################################
# Saving model table as html from anova table, at first 
# temperature ingtegration for predictors from best
# traits model
############################################################################

print(temp_int_model_xtable, type = "html", file = "./results/canopy_temp_int_anova_table.html")

print(mixed_temp_int_model_coeff_xtable, type = "html", file = "./results/temp_int_model_coeff.html")

print(temp_int_model_xtable_withoutj, type = "html", file = "./results/canopy_temp_int_anova_table_withoutj.html")

print(mixed_temp_int_model_coeff_xtable_withoutj , type = "html", file = "./results/temp_int_model_coeff_withoutj.html")


################################################################################
# Now for Ignition
################################################################################

print(temp_ign_model_xtable, type = "html", 
      file = "./results/canopy_ignition_anova_table.html")

print(mixed_temp_ign_model_coeff_xtable, type = "html", 
      file = "./results/canopy_ignition_coeff.html")

print(temp_ign_model_xtable_withoutj, type = "html", 
      file = "./results/canopy_ignition_anova_table__withoutj.html")

print(mixed_temp_ign_model_coeff_xtable_withoutj, type = "html", 
      file = "./results/canopy_ignition_coeff_withoutj.html")

##################################################################################
#cor_data <- pca_data_2022 %>%
  #dplyr::select(- c("sample_id", "PC1", "PC2"))
#traits_cor <- cor(cor_data, method = "spearman")
#traits_cor <- as.data.frame(traits_cor)
#corr_plot <- ggcorrplot::ggcorrplot(traits_cor, type = "lower", 
                        #lab = TRUE, show.legend = FALSE,      
                        #colors = c(NA, NA),      
                        #hc.order = TRUE) + 
  #theme(axis.text.y = element_text(face = "bold", size = 12),
        #axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 12),
        #panel.grid = element_blank())

#ggsave("./results/Supporting_info_Figure 1.pdf", corr_plot, height = 120,
       #width = 200, units = "mm", dpi = 300)

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
            label = "Time to ignition", size = 4,  color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  theme(axis.title = element_text(size = textsize, face = "bold"))
 

ggsave("./results/Figure1.pdf",
       plot = pca_plot, height = 180,
       width = 170, units = "mm", dpi = 300)

