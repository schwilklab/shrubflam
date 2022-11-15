#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022
# PCA analysis
# Used prcomp() for PCA and factoextra for graphs.

# All scripts that are in source() required to run before running 
# flam_pca_2022.R script

library(factoextra)

source("./read_data_2022.R") # script that read, clean and merged all the morphological and flammability traits data.

source("./read_hobos_2022.R") # script that reads the thermocouple data logger data during burning.

####################################################################
## PCA analysis, unimputed data. Merging the hobo data with alldata_2022 
# data set to do the PCA.
# Decided to drop some samples, descriptions in the exploratory_figures_2022.R
#####################################################################

pca_data_2022 <- alldata_2022 %>%
  filter(! sample_id %in% c("KD18", "DK34", "KD15", "UV04",
                            "DK30")) %>% # outliers in a sense something went wrong during measurement
  # for those samples since I measured some traits for some species manually.
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label") %>%
  select(label, heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay) # Dropping the self_ignition
# since it is a binary variable.



names(pca_data_2022)[7] <- "Duration over (100\u00B0C)"

dim(pca_data_2022)

any(is.na(pca_data_2022)) 

####################################################################
# PCA by prcomp, correlation matrix since I am using scale is TRUE
####################################################################

flam_pca_2022 <- prcomp(pca_data_2022[,-1], 
                   scale=TRUE)


summary(flam_pca_2022) # standard deviation for PC2 is 0.94029

flam_loadings <- flam_pca_2022$rotation[ ,1:2] 

flam_loadings

biplot(flam_pca_2022)

####################################################################
## Scree plot, eigenvalue and variables info
####################################################################

eig_val <- get_eigenvalue(flam_pca_2022) 


eig_val # eignevalue for PC2 is 0.88414214

variables_info <- get_pca_var(flam_pca_2022) # Variables information

variables_info$coord[ ,1:2] # Coordinates of variables

head(variables_info$contrib) # Contributions of variables

####################################################################
## Plot based on cos2 values of variables
####################################################################

# Will remove the plots and take them to results_2022.R later.

var_contr_by_cos2 <- fviz_pca_var(flam_pca_2022,col.var = "cos2",
                                           gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                                          repel = TRUE, col.circle = "white") +
  xlab("Principle component 1") +
  ylab("Principle component 2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(size = 1.6),
        plot.title = element_blank())



####################################################################
## Contributions of variables in Principle components
####################################################################

contributor_pc1_2022 <-  fviz_contrib(flam_pca_2022, choice = "var",
                                 axes = 1,
                                 fill = "lightgray",
                                 color = "black") 



contributor_pc2_2022 <- fviz_contrib(flam_pca_2022, choice = "var", 
                                     axes = 2,
                                     fill = "lightgray",
                                     color = "black") 

#####################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
#####################################################################

pca_data_2022$PC1 <- flam_pca_2022$x[ ,1]

pca_data_2022$PC2 <- flam_pca_2022$x[ ,2]

final_data <- alldata_2022 %>%
  filter( ! sample_id %in% c("KD18", "DK34", "KD15", "UV04",
                             "DK30")) %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(select(pca_data_2022, label, PC1, PC2, degsec_100), by = "label") 

dim(final_data)


## Will use the final_data to do the rest of the analysis regarding mixed effect model.


###############################################################################
# Cleaning the environment
###############################################################################

rm("pca_data_2022", "flam_pca_2022", "flam_loadings",
   "eig_val", "variables_info")

