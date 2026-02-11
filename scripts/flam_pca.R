# PCA analysis

###############################################################################
# PCA analysis. Merging the hobo data with alldata_2022 by label data set to do
# the PCA.
###############################################################################

pca_data_2022 <- alldata_2022 %>%
  left_join(hobos_wider_2022, by = "label") %>%
  dplyr::select(specific_epithet, #massconsumed,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay) %>%
  group_by(specific_epithet) %>%
  summarise(#massconsumed = mean(massconsumed),
            vol_burned = mean( vol_burned),
            flame_height = mean(flame_height),
            flame_duration = mean(flame_duration),
            dur_100 = mean(dur_100),
            peak_temp = mean(peak_temp),
            degsec_100 = mean(degsec_100),
            ignition_delay = mean(ignition_delay))


dim(pca_data_2022) # 116
any(is.na(pca_data_2022)) 

###############################################################################
# PCA by prcomp, correlation matrix since I am using scale is TRUE
###############################################################################

flam_pca_2022 <- prcomp(pca_data_2022[,-1], 
                   scale=TRUE)

summary(flam_pca_2022) 
flam_loadings <- flam_pca_2022$rotation[ ,1:2] 
flam_loadings
#biplot(flam_pca_2022)

###############################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
###############################################################################

pca_data_2022$PC1 <- flam_pca_2022$x[ ,1]
pca_data_2022$PC2 <- flam_pca_2022$x[ ,2]

final_data <- alldata_2022 %>%
  select(specific_epithet, total_dry_mass_g, canopy_density_gm_cm3, leaf_stem_mass_ratio,
         canopy_moisture_content, leaf_mass_per_area, 
         leaf_area_per_leaflet, leaf_length_per_leaflet, mass_pre) %>%
  group_by(specific_epithet) %>%
  summarise(total_dry_mass_g = mean(total_dry_mass_g),
            canopy_density_gm_cm3 = mean(canopy_density_gm_cm3),
            leaf_stem_mass_ratio = mean(leaf_stem_mass_ratio),
            canopy_moisture_content = mean( canopy_moisture_content),
            leaf_mass_per_area = mean(leaf_mass_per_area),
            leaf_area_per_leaflet = mean(leaf_area_per_leaflet),
            leaf_length_per_leaflet = mean(leaf_length_per_leaflet),
            mass_pre = mean(mass_pre)) %>%
  left_join(pca_data_2022, by = "specific_epithet")
  

dim(final_data)

final_data[-1] <- round(final_data[-1], 3)
#View(final_data)

############################################################################
# Cleasning environments
############################################################################
rm(flam_loadings, pca_data_2022)



