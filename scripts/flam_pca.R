# flam_pca.R

# Shrub flammability project using summer 2022 data. PCA analysis uses prcomp()
# for PCA and factoextra for figures

library(factoextra)

###############################################################################
# PCA analysis. Merging the hobo data with alldata_2022 by label data set to do
# the PCA.
###############################################################################

pca_data_2022 <- alldata_2022 %>%
  left_join(hobos_wider_2022, by = "label") %>%
  dplyr::select(sample_id, heat_release_j, massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay)


dim(pca_data_2022) # 116
any(is.na(pca_data_2022)) 

###############################################################################
# PCA by prcomp, correlation matrix since I am using scale is TRUE
###############################################################################

flam_pca_2022 <- prcomp(pca_data_2022[,-1], 
                   scale=TRUE)

summary(flam_pca_2022) # standard deviation for PC2 is 0.918
flam_loadings <- flam_pca_2022$rotation[ ,1:2] 
flam_loadings
biplot(flam_pca_2022)

##############################################################################
## Scree plot, eigenvalue and variables info
###############################################################################

eig_val <- get_eigenvalue(flam_pca_2022) 
eig_val # eignevalue for PC2 is 0.842
variables_info <- get_pca_var(flam_pca_2022) # Variables information
variables_info$coord[ ,1:2] # Coordinates of variables
head(variables_info$contrib) # Contributions of variables

##############################################################################
## Plot based on cos2 values of variables
###############################################################################

# Will remove the plots and take them to results_2022.R later.

var_contr_by_cos2 <- fviz_pca_var(flam_pca_2022,col.var = "cos2",
                                           gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
                                          repel = TRUE, col.circle = "white") +
  xlab("Principle component 1") +
  ylab("Principle component 2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(),
        plot.title = element_blank())




###############################################################################
## Contributions of variables in Principle components
###############################################################################

contributor_pc1_2022 <-  fviz_contrib(flam_pca_2022, choice = "var",
                                 axes = 1,
                                 fill = "lightgray",
                                 color = "black") 



contributor_pc2_2022 <- fviz_contrib(flam_pca_2022, choice = "var", 
                                     axes = 2,
                                     fill = "lightgray",
                                     color = "black") 

###############################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
###############################################################################

pca_data_2022$PC1 <- flam_pca_2022$x[ ,1]
pca_data_2022$PC2 <- flam_pca_2022$x[ ,2]

final_data <- alldata_2022 %>%
  left_join(dplyr::select(pca_data_2022, sample_id, PC1, PC2, degsec_100), by = "sample_id")
  

dim(final_data)



###############################################################################
# Cleaning the environment
###############################################################################

rm( "flam_pca_2022", "flam_loadings",
   "eig_val", "variables_info" ,"contributor_pc1_2022",
   "contributor_pc2_2022", 
   "var_contr_by_cos2")

