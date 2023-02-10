## ms_figures.R

# this R script is used to create figures showing significant fixed effect of
# canopy and leaf traits on flammability from analysis.R and also model anova
# and coefficient tables

library(randomcoloR)  ## DWS: Why? You don't want random colors in code!
## AM: Don't know how to fix the color issue

## DWS: What is wrong with the suggestions I made in our meeting? "I don't
## know" is not very useful and doesn't tell me how to help you.

library(ggmap)

#library(factoextra) # unnecessary, already loaded in a previous script.

# Plot theme for publication standard
source("./scripts/ggplot_theme.R") 

## This script depends on analysis.R. See run-all.R


##################################################################################
# Generating thirteen disinct color
#################################################################################

set.seed(2643598)
palette <- distinctColorPalette(14)

## DWS: good god why? If you need 13 colors you are thinking about this wrong.


##################################################################################
# The best model for canopy traits for temperature integration using scaled 
# variables from model_data from analysis.R script
##################################################################################

## DWS: Why is their model fitting in the figure code?
## AM: removed the model.

#####################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
#######################################################################################

best_degsec_canopy_plot_data <- final_data %>% 
  select(total_dry_mass_g, canopy_density_gm_cm3, degsec_100, analysis_group)


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
  select(predicted_degsec_100, total_dry_mass_g, canopy_density_gm_cm3, 
         degsec_100, analysis_group)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them are same as best_degsec_
# canopy_plt_data  so that we can use them another layer in the plot as geom_point() 
# where we will put the mean value, the bigger dots in 
# the plot
############################################################################################

degsec_by_group <- final_data %>% group_by(analysis_group) %>%
  summarize(total_dry_mass_g = mean(total_dry_mass_g), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            degsec_100 = mean(degsec_100))


###########################################################################################
# Plotting the predicted vs unscaled total mass and adding layer by
# summarized data
###########################################################################################

total_dry_mass <- ggplot(best_degsec_canopy_plot_data, aes(total_dry_mass_g, degsec_100, 
                                         color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab("Total dry mass (g)")  + 
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/total_dry_mass.pdf",
       plot = total_dry_mass, height = beamer_height,
       width = 10, units = "cm")

############################################################################################################
# Same way for canopy density
#####################################################################################

canopy_density <- ggplot(best_degsec_canopy_plot_data, aes(canopy_density_gm_cm3, degsec_100, color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Canopy density (", g / cm^3, ")")))  + 
  pubtheme +
  theme(legend.position = "none")  +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/canopy_density.pdf",
       plot = canopy_density, height = beamer_height,
       width = 10, units = "cm")

#################################################################################################
# Now the leaf traits, using the best leaf traits model for temperature integration 
# once again from analysis.R
#################################################################################################



#################################################################################################
# Getting the unscaled fixed terms from final_data 
#################################################################################################

best_degsec_leaf_plot_data <- final_data %>% 
  select(leaf_mass_per_area, degsec_100, analysis_group)


#####################################################################################
# Predicting the unscaled variables and naming the new variable as 
# predicted_degsec_100
###################################################################################


best_degsec_leaf_plot_data$predicted_degsec_100 <- predict(best_leaf_pc1_model, 
                                                           newdata = best_degsec_leaf_plot_data)

########################################################################################
# Keeping unscaled leaf_mass_per_area degsec_100 and predicted degsec_100
# to summarise them by analysis_group
########################################################################################

predicted_degsec_leaf_plot_data <- best_degsec_leaf_plot_data %>%
  select(predicted_degsec_100, leaf_mass_per_area, degsec_100,
         analysis_group)

########################################################################################
# Summarising the fixed effects by group and making sure that the name of the 
# mean value after summarising them are same as best_degsec_
# canopy_plot_data  so that we can use them another layer in the plot as 
# geom_point() where we will put the mean value, the bigger dots in 
# the plot
###########################################################################################

degsec_leaf_by_group <- final_data %>% 
  group_by(analysis_group) %>%
  summarize(leaf_mass_per_area = mean(leaf_mass_per_area),
            degsec_100 = mean(degsec_100))

####################################################################################
# Plotting the lma vs temperature integration
####################################################################################

LMA <- ggplot(best_degsec_leaf_plot_data, aes(leaf_mass_per_area, degsec_100,
                                       color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_degsec_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = degsec_leaf_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")"))) +
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/LMA.pdf",
       plot = LMA, height = beamer_height,
       width = 10, units = "cm")
##################################################################################
# The best model for canopy traits and leaf traits for ignition delay using scaled 
# variables from model_data from analysis.R script
##################################################################################


#####################################################################################
# Selecting unscaled variables from final data to predict from scaled data in model_data
#######################################################################################


best_canopy_ignition_plot_data <- final_data %>% 
  select(canopy_moisture_content, canopy_density_gm_cm3, 
         ignition_delay, analysis_group)


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
         ignition_delay, analysis_group, predicted_ignition_delay)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them
############################################################################################

ignition_delay_by_group <- final_data %>% 
  group_by(analysis_group) %>%
  summarize(canopy_moisture_content = mean(canopy_moisture_content), 
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            ignition_delay = mean(ignition_delay))


canopy_density_ignition <- ggplot(best_canopy_ignition_plot_data, aes(canopy_density_gm_cm3,ignition_delay,
                                           color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab("Ignition delay (s)") +
  xlab(expression(paste("Canopy density (", g / cm^3, ")")))  + 
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/canopy_density_ignition.pdf",
       plot = canopy_density_ignition, height = beamer_height,
       width = 10, units = "cm")

canopy_moisture_ignition <- ggplot(best_canopy_ignition_plot_data, aes(canopy_moisture_content,ignition_delay, 
                                           color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_canopy_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab("Ignition delay (s)") +
  xlab("Canopy moisture content (%)")  + 
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/canopy_moisture_ignition.pdf",
       plot = canopy_moisture_ignition, height = beamer_height,
       width = 10, units = "cm")
######################################################################################
# Leaf traits vs ignition delay
######################################################################################

best_leaf_ignition_plot_data <- final_data %>% 
  select(leaf_moisture_content, leaf_mass_per_area, 
         ignition_delay, analysis_group)


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
         ignition_delay, analysis_group, predicted_ignition_delay)

############################################################################################
# Summarising the fixed effects by group and making sure that the name of the mean value 
# after summarising them
############################################################################################

leaf_ignition_delay_by_group <- final_data %>% 
  group_by(analysis_group) %>%
  summarize(leaf_moisture_content = mean(leaf_moisture_content), 
            leaf_mass_per_area = mean(leaf_mass_per_area),
            ignition_delay = mean(ignition_delay))


LMA_ignition <- ggplot(best_leaf_ignition_plot_data, aes(leaf_mass_per_area,ignition_delay, 
                                         color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = leaf_ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab("Ignition delay (s)") +
  xlab(expression(paste("Leaf mass per area (", g/cm^2, ")")))  + 
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/LMA_ignition.pdf",
       plot = LMA_ignition, height = beamer_height,
       width = 10, units = "cm")

leaf_moisture_ignition <- ggplot(best_leaf_ignition_plot_data, aes(leaf_moisture_content,ignition_delay, 
                                         color = analysis_group)) +
  geom_point(size = 2.5, alpha = 0.5, shape = 16) + 
  geom_blank(data = predicted_ignition_leaf_plot_data) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black") +
  geom_point(data = leaf_ignition_delay_by_group, size = 4.5 , alpha = 1, shape = 16,
             aes(color = analysis_group)) +
  ylab("Ignition delay (s)") +
  xlab("Leaf moisture content (%)")  + 
  pubtheme +
  theme(legend.position = "none") +
  scale_color_manual(values = c("maroon2", "#7CD5D9", "yellow", "#6FA3CE","red",
                                "#D6E2A6", "#73E17B", "lightblue",
                                "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                                "black")) +
  scale_fill_manual(values = c("maroon2","#7CD5D9", "yellow", "#6FA3CE", "red",
                               "#D6E2A6", "#73E17B", "lightblue",
                               "#D1A7D6", "#DA61C2", "#C9E558", "blue", "orange",
                               "black"))

ggsave("./results/leaf_moisture_ignition.pdf",
       plot = leaf_moisture_ignition, height = beamer_height,
       width = 10, units = "cm")


##################################################################################
##########################################################################################
# Renamed pca plot
# The problem is there are nine variables.
# First problem is overlapping into the plot, even repel function
# can't fix it. Decided to use the abbreviation here.
# Second problem is I had to rename the variables either in this script
# or flam_pca.R script. Thought, doing it at the end of everything
# might not be a bad idea because can fix it easily later.
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


 
