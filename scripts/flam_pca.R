#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

## PCA analysis
## Used prcomp() for PCA and factoextra for graphics.
library(dplyr)
library(FactoMineR)
library(factoextra)
source("./read_hobos.R")



## PCA analysis, unimputed data ( Some NAs and need to
# deal with later)


pca_data <- alldata%>% 
  mutate(label=paste(sample_id,species_id,sep = "_"))%>%
  right_join(hobo_temp_sum, by ="label")%>%
  select(label,heat_release_J,massconsumed,vol.burned, 
  flame.ht,flame.dur,dur.100,peak.temp)%>%
  na.omit()


## pca by prcomp, correlation matrix since I am using scale is TRUE

flam_pca <- prcomp(pca_data[,-1], 
                        scale=TRUE)
biplot(flam_pca)
summary(flam_pca)
flam_loadings <- flam_pca$rotation[,1:2] # Loadings
# are positive for all variables in PC1 and dur.100
# negatively correlated with PC2.


## Scree plot
fviz_eig(flam_pca,addlabels = TRUE)

eig.val2 <- get_eigenvalue(flam_pca) # Eigenvalue > 1
# is only for PC1


variables_info2 <- get_pca_var(flam_pca)

variables_info2$coord[,1:2]

head(variables_info2$contrib)

fviz_pca_var(flam_pca,col.var = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)


fviz_contrib(flam_pca,choice = "var",axes=1)

fviz_contrib(flam_pca,choice = "var",axes = 2)


#fviz_pca_ind(flam_pca,
             #geom.ind = "points",
             #col.ind = pca_data$display_name,
             #addEllipses = TRUE,
             #legend.title="Groups") # Nonsense plot

## vol.burned,massconsumed,heat_release_J and flame.dur
## contributed the most in PC1 and dur.100 contributed the most in PC2.
## PC1 and PC2 explains 75.27% and 8.398% variation respectively.

pca_data$PC1 <- flam_pca$x[,1]

## Will use this dataset for rest of the stuff.
model_data <- pca_data%>%
  select(label,PC1)%>%
  right_join(plot_trials,by="label")%>%
  na.omit()
