# flam_pca.R

# Shrub flammability project using summer 2022 data. PCA analysis uses prcomp()
# for PCA and factoextra for figures


###############################################################################
# PCA analysis. Merging the hobo data with alldata_2022 by label data set to do
# the PCA.
###############################################################################

pca_data_2022 <- alldata_2022 %>%
  left_join(hobos_wider_2022, by = "label") %>%
  dplyr::select(sample_id, massconsumed,
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


###############################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
###############################################################################

pca_data_2022$PC1 <- flam_pca_2022$x[ ,1]
pca_data_2022$PC2 <- flam_pca_2022$x[ ,2]

final_data <- alldata_2022 %>%
  left_join(dplyr::select(pca_data_2022, sample_id, PC1, PC2, degsec_100), by = "sample_id")
  

dim(final_data)





