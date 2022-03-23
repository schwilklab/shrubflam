#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

## PCA analysis
## Used prcomp() for PCA and factoextra for graphics.

library(dplyr)
library(FactoMineR)
library(factoextra)
library(missMDA)
source("./read_hobos.R")
source("./read_data.R")



## PCA analysis, unimputed data ( Some NAs and need to
# deal with later)

####################################################################
pca_data <- samples_more_than_three%>%
  right_join(hobo_temp_sum, by ="label")%>%
  select(label,heat_release_J,massconsumed,vol.burned, 
  flame.ht,flame.dur,dur.100,peak.temp)%>%
  na.omit()
####################################################################

####################################################################
## pca by prcomp, correlation matrix since I am using scale is TRUE

flam_pca <- prcomp(pca_data[,-1], 
                        scale=TRUE)
biplot(flam_pca)

summary(flam_pca)

flam_loadings <- flam_pca$rotation[,1:2] 
flam_loadings

# Loadings are negative for all variables in PC1 
# and dur.100  negative correlated with PC2.
####################################################################


####################################################################
## Scree plot, eigenvalue and variables info

fviz_eig(flam_pca,addlabels = TRUE)

# Eigenvalue > 1 is only for PC1

eig.val <- get_eigenvalue(flam_pca) 
eig.val

# Variables information

variables_info <- get_pca_var(flam_pca)

# Coordinates of variables

variables_info$coord[,1:2] 

# Contributions of variables

head(variables_info$contrib) 

####################################################################

####################################################################
## Plot based on cos2 values of variables

fviz_pca_var(flam_pca,col.var = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)

####################################################################

####################################################################
## Contributions of variables in Principle components

fviz_contrib(flam_pca,choice = "var",axes=1)

# Contributions of variables in PC2

fviz_contrib(flam_pca,choice = "var",axes = 2)

#####################################################################

#####################################################################
#fviz_pca_ind(flam_pca,
             #geom.ind = "points",
             #col.ind = pca_data$display_name,
             #addEllipses = TRUE,
             #legend.title="Groups") # Nonsense plot
#####################################################################


#####################################################################
## Assigning PC1 to pca_data and then merged with samples_more_than_three
## dataset for doing rest of the analysis.

pca_data$PC1 <- 0-flam_pca$x[,1] #PC1 is negative
model_data <- samples_more_than_three%>%
  right_join(select(pca_data,label,PC1),by="label")

######################################################################

######################################################################

## vol.burned,massconsumed,heat_release_J and flame.dur
## contributed the most in PC1 and dur.100 contributed the most in PC2.
## PC1 and PC2 explains 75.53% and 8.375% variation respectively.

######################################################################