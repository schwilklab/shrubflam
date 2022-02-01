#!/usr/bin/Rscript --vanilla

# Azaj Mahmud
# Shrub Flammability
# Summer 2021

library(ggplot2)
source("./stats_figs.R")

azaj_boxplot <- geom_boxplot(aes(color=genus), outlier.colour = "red",
                             outlier.size = 2)
species_x_theme <- theme(axis.text.x = element_text(angle = 30,
                                                    hjust = 1,
                                                    face = "italic"))

# Does moisture content influence ignition?
alldata <- alldata%>%
  mutate(ignition=ifelse(flame.dur==0,0,1))

ggplot(alldata,aes(moisture_content,ignition,
                   color=display_name))+
  geom_point(size=3)+
  geom_smooth(aes(group=display_name),method="lm",se=FALSE)+
  theme_bw()+
  labs(x="Moisture content (%)",
       y="Ignition")

# How about only juniperus ashei since 4 sample didn't get ignited

only_juniperus_ashei <- alldata%>%
  filter(species_id=="1022")
  
ggplot(only_juniperus_ashei,aes(moisture_content,ignition,
                                ))+
  geom_point()

# Looks like moisture content didn't affect in ignition in 
# Juniperus ashei!!!

# Does leaf area influence heat release?
alldata <- alldata%>%
  filter(! is.na(leaf_area))

ggplot(alldata,aes(leaf_area,heat_release_J,color=display_name))+
  geom_point(size=3)+
  geom_smooth(aes(group=display_name),method = "lm",se=FALSE)+
  xlab(expression(paste("Leaf area(",cm^2 ,")"))) +
  ylab("Heat Release (J)")



# canopy density by genus
ggplot(alldata, aes(x=reorder(genus,canopy_density),
                                y=canopy_density)) + 
  xlab("Genus") +
  ylab(expression(paste("Canopy density (", g / cm^3, ")"))) +
  azaj_boxplot +
  species_x_theme

# Plot of moisture content in dry basis by genus
ggplot(alldata,aes(x=reorder(genus,moisture_content),
                               y=moisture_content)) +
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic")) +
  xlab("Genus") +
  ylab("Moisture content (%)")

# Plot of moisture content in dry basis by species
ggplot(alldata,aes(x=reorder(species,moisture_content), y=moisture_content)) +
  xlab("Species") +
  ylab("Moisture content (%)") +
  azaj_boxplot +
  species_x_theme


# Plot of LMA by genus
ggplot(alldata, aes(x=reorder(genus,leaf_mass_area), y=leaf_mass_area))+
  xlab("Genus")+
  ylab(expression(paste("LMA (", g / cm^3, ")"))) +
  azaj_boxplot +
  species_x_theme

## DWS: well that is useless. Why are you plowing ahead when the plot looks
## like that. THINK. ## DWS: how did you calculate LMA for Juniperus and Pinus?
## Did you take caliper measurements? that leaf area meter won't work for
## those, right?

# Plot of dry matter percent by genus
ggplot(alldata, aes(x=reorder(genus, dry_material_perc), y=dry_material_perc))+
     geom_boxplot(aes(color=genus), outlier.color = "red", outlier.size = 2)+
     theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
     xlab("Genus")+
     ylab("Dry mass (%)")



# Plot of flame duration by genus

## DWS: what are you doing with all these figures? I have no clue about the
## purpose? What questions do you have about genus as a category? What are you
## trying to do?

ggplot(alldata, aes(genus,flame.dur)) + geom_jitter() +
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Flame duration (s)")

# DWS: Why did you change your graph type? What is the plan here? This is just
# a lot of work for no good reason it seems.

# DWS: Use SI unit standard abbreviations.

# Plot of mass consumed by genus
ggplot(alldata, aes(genus, massconsumed))+
  geom_jitter(width = 0.2, size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
          xlab("Genus")+
  ylab("Mass consumed (g)")


# Plot of mass consumed by species
ggplot(alldata, aes(species, 100*massconsumed))+
  geom_jitter(width = 0.2,size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Mass consumed (%)") #Is that a percent? Check. You did not give units.

# Plot of flame height by genus
ggplot(alldata, aes(x=reorder(genus,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Flame height (cm)")

## DWS: and we are back to boxplots with no explanation :).

# Plot of flame height by species
ggplot(alldata, aes(x=reorder(species,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Flame height (cm)")



##: DWS: ok, I'm tired and bored of this. What is the point? I'm commenting out
## the rest since I'm unconvinced any of this means anything.


## # Plot of volume burned by genus
## ggplot(burning_trials,aes(x=reorder(genus,vol.burned),y=vol.burned))+
##   geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
##   xlab("Genus")+
##   ylab("Volume Burned In Percentage")

## # Plot of volume burned by species

## ggplot(burning_trials,aes(x=reorder(species,vol.burned),y=vol.burned))+
##   geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
##   xlab("Species")+
##   ylab("Volume Burned In Percentage")

## # Plot of maximum temperature by genus

## ## DWS: maximum temperature of what?

## ggplot(burning_trials,aes(x=reorder(genus,max.temp),y=max.temp))+
##   geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
##   xlab("Genus")+
##   ylab("Maximum Temperature In Celsius")

## # Plot of maximum temperature by species

## ggplot(burning_trials,aes(x=reorder(species,max.temp),y=max.temp))+
##   geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
##   xlab("Species")+
##   ylab("Maximum Temperature In Celsius")

## # Plot of heat release by genus
## ggplot(burning_trials,aes(x=reorder(genus,heat_release_j),y=heat_release_j))+
##   geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
##   xlab("Genus")+
##   ylab("Heat Release In Joule")

## # Plot of heat release by species

## ggplot(burning_trials,aes(x=reorder(species,heat_release_j),y=heat_release_j))+
##   geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
##   theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
##   xlab("Species")+
##   ylab("Heat Release In Joule")
#set.seed(100)
## # PCA analysis
#library(ggplot2)
#pca <- prcomp(select(alldata,massconsumed, heat_release_J, vol.burned, flame.dur,flame.ht),scale=TRUE)
#summary(pca)
#plot(pca,type="l")

#biplot(pca,scale = 0)
#alldata$PC1 <- pca$x[,1]


## # ggplot of flammability score (PC1) by species

#ggplot(alldata,aes(x=reorder(species,PC1),-PC1))+
   geom_boxplot(aes(color=species,fill=species),outlier.colour = "red",outlier.size = 2)+
   theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
   xlab("Species")+
   ylab("Flammability Score (PC1)")


#new<-with(alldata, data.frame(flame.ht, flame.dur,vol.burned))

#hist(new$flame.ht)

library(vegan)

#new<-decostand(new, method = "hellinger")

#hist(new$vol.burned)
#pc<-prcomp(new)
#pcnew<-data.frame(pc$x)



#EvenNewer<-cbind(pcnew, new)

#plot(EvenNewer$vol.burned ~ EvenNewer$PC1)




#flam_lm <- lm(PC1~massconsumed+heat_release_J+vol.burned+flame.ht+flame.dur,data = alldata)
#residuals <- resid(flam_lm)
library(ggpubr)
#ggdensity(alldata, x = "vol.burned", fill = "lightgray", title = "vol.burned") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#transformed_data <- alldata%>%
  select(sample_id,species,canopy_density,dry_material_perc,leaf_mass_area,moisture_content,
         massconsumed,heat_release_J,vol.burned,flame.dur,flame.ht)%>%
  na.omit()%>%
  mutate(canopy_density=log10(canopy_density),
  dry_material_perc=log10(dry_material_perc),
  leaf_mass_area=log10(leaf_mass_area),
  moisture_content=sqrt(moisture_content))







