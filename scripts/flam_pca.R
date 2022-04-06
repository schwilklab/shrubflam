#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021

# Need to set the directory as setwd("../scripts") to read the scripts
# Need to make sure that the number of observations of pca_data and
# samples_more_than_three are same.
## PCA analysis
## Used prcomp() for PCA and factoextra for graphics.


library(dplyr)
library(FactoMineR)
library(factoextra)
source("./read_data.R")
source("./read_hobos.R")

####################################################################
## PCA analysis, unimputed data.
# Merging the hobo data with samples_more_than_three dataset to do 
# the PCA.
#####################################################################

pca_data <- samples_more_than_three%>%
  left_join(hobos_wider, by ="label")%>%
  select(label,heat_release_J,massconsumed,vol.burned, 
  flame.ht,flame.dur,dur.100,peak.temp)

dim(pca_data) # 97 rows same as samples_more_than_three
#unique(pca_data$label)

#pca_data_check <- tidyr::separate(pca_data, label, into = c("sample_id","species_id"),
                                  #sep = "_")
#species_id_count <- pca_data_check%>%
  #count(species_id)%>%
  #View()

any(is.na(pca_data)) # No NA
#View(pca_data)

  
####################################################################
## pca by prcomp, correlation matrix since I am using scale is TRUE
####################################################################

flam_pca <- prcomp(pca_data[,-1], 
                        scale=TRUE)
biplot(flam_pca)
summary(flam_pca)
flam_loadings <- flam_pca$rotation[,1:2] 
flam_loadings



####################################################################
## Scree plot, eigenvalue and variables info
####################################################################

fviz_eig(flam_pca,addlabels = TRUE) 
eig.val <- get_eigenvalue(flam_pca) 

eig.val

variables_info <- get_pca_var(flam_pca) # Variables information

variables_info$coord[,1:2] # Coordinates of variables

head(variables_info$contrib) # Contributions of variables

####################################################################
## Plot based on cos2 values of variables
####################################################################

fviz_pca_var(flam_pca,col.var = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE)

####################################################################
## Contributions of variables in Principle components
####################################################################

fviz_contrib(flam_pca,choice = "var",axes=1) # Contributions of variables in PC1

fviz_contrib(flam_pca,choice = "var",axes = 2) # # Contributions of variables in PC2

#####################################################################
# Assigning PC1 to pca_data and then merging with samples_more_than_three
# dataset for doing rest of the analysis.
#####################################################################

pca_data$PC1 <- flam_pca$x[,1]*(-1) #PC1 is negative

model_data <- samples_more_than_three%>%
  right_join(select(pca_data,label,PC1),by="label")

dim(model_data) # 97 rows
#View(model_data)

######################################################################
# All the variables are negative with PC1 and PC1 and PC2 accounts
# for 75.25% and 9.34% variation respectively.
# vol.burned, heat_release_J, peak.temp and massconsumed
# contributed the most in PC1 and dur.100 and flame.dur 
# contributed the most in PC2
######################################################################

