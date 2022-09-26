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
## PCA analysis, unimputed data. Merging the hobo data with alldata_2022 dataset to do 
# the PCA.
#####################################################################

pca_data_2022 <- alldata_2022 %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label") %>%
  select(label, heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition)



names(pca_data_2022)[7] <- "Duration over (100\u00B0C)"

dim(pca_data_2022)

any(is.na(pca_data_2022)) 

####################################################################
# PCA by prcomp, correlation matrix since I am using scale is TRUE
####################################################################

flam_pca_2022 <- prcomp(pca_data_2022[,-1], 
                   scale=TRUE)


summary(flam_pca_2022)

flam_loadings <- flam_pca_2022$rotation[ ,1:2] 

flam_loadings

biplot(flam_pca_2022)

####################################################################
## Scree plot, eigenvalue and variables info
####################################################################

eig_val <- get_eigenvalue(flam_pca_2022) 

eig_val

variables_info <- get_pca_var(flam_pca_2022) # Variables information

variables_info$coord[ ,1:2] # Coordinates of variables

head(variables_info$contrib) # Contributions of variables

####################################################################
## Plot based on cos2 values of variables
####################################################################

# Will remove the plots and take them to results_2022.R later.

var_contr_by_cos2 <- fviz_pca_var(flam_pca_2022,col.var = "cos2",
                                           gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                                          repel = TRUE) 

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
# dataset for doing rest of the analysis.
#####################################################################

pca_data_2022$PC1 <- flam_pca_2022$x[ ,1]

pca_data_2022$PC2 <- flam_pca_2022$x[ ,2]

final_data <- alldata_2022 %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(select(pca_data_2022, label, PC1, PC2), by = "label") 

dim(final_data) 

## Will use the final_data to do the rest of the analysis.

########################################################################
# Creating a separate data set with flammability traits and morphological
# traits to do the correlation test
########################################################################


cor_data <- alldata_2022 %>%
  mutate(label=paste(sample_id,species_id,sep = "_")) %>%
  left_join(hobos_wider_2022, by = "label") %>%
  select(heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay, self_ignition,
         total_dry_mass_gm, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, leaf_area_per_leaflet,
         leaf_length_per_leaflet, leaf_moisture_content)


###############################################################################
# Cleaning the environment
###############################################################################

rm("pca_data_2022", "flam_pca_2022", "flam_loadings",
   "eig_val", "varibales_info")

