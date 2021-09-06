# Azaj Mahmud
# Shrub Flammability
# Summer 2021

# Getting canopy measurement data
canopy_measurement <- read.csv("C://Users//user//shrubflam//data//year_2021//canopy_measurements.csv")

# Getting burn_trials data
burn_trials <- read.csv("C://Users//user//shrubflam//data//year_2021//burn_trials.csv")

# Getting site data
site_data <- read.csv("C://Users//user//shrubflam//data//year_2021//sites.csv")

# Getting leaf measurement data

leaf_measurement <- read.csv("C://Users//user//shrubflam//data//year_2021//leaf_measurements.csv")

# Separate the species into genus and species of site data
site_data <- separate(site_data,species,into = c("genus","species"),sep = " ")

# Changing the canopy width at bottom,middle and top of inches to cm and 
# determine average width.

canopy_measurement <- canopy_measurement%>%
  mutate(bottom_width_in=bottom_width_in*2.54,
         middle_width_in=middle_width_in*2.54,
         top_width_in=top_width_in*2.54,
         average_width_cm=(bottom_width_in+middle_width_in+top_width_in)/3)

# Measuring moisture content on dry basis and canopy volume.
# Assumed the canopy is a cylinder whose radius and height are the average width
# and the length of the branch respectively.

canopy_measurement <- canopy_measurement%>%
  mutate(canopy_volume_cm3=3.1416*(average_width_cm)^2*length_cm,
    moisture_content=((fresh_mass_g-dry_mass_g)/dry_mass_g)*100)
  

# Get the pre mass(mass before burning) out of burn trials data to determine the canopy density (Total mass/volume)
# since a tiny part of the sample separated from the sample to determine the
# moisture content.So the total mass is the addition of pre.mass
# and the mass of tiny part that used for measuring moisture content.

pre_mass <- burn_trials%>%
  select(sample_id,mass.pre)

# Merge pre_mass with canopy measurement by sample_id

canopy_density_data <- merge(canopy_measurement,pre_mass,by="sample_id")

# Determining canopy density as gram per centimeter cube.

canopy_density_data <- canopy_density_data%>%
  mutate(total_mass_g=fresh_mass_g+mass.pre,
         canopy_density=total_mass_g/canopy_volume_cm3)

# Merging the canopy density data with the site data to create plot
# group by genus and species.

canopy_density_data <- left_join(site_data,canopy_density_data,by=c("sample_id","species_id"))

# select the samples which are 70 cm

canopy_density_data <- canopy_density_data%>%
  filter(length_cm=="70")

## Creating plot
# canopy density by genus

ggplot(canopy_density_data,aes(x=reorder(genus,canopy_density),y=canopy_density))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "italic"))+
  xlab("Genus")+
  ylab("Canopy density in gmcm^-3")


# canopy density by species

ggplot(canopy_density_data,aes(x=reorder(species,canopy_density),y=canopy_density))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face = "italic"))+
  xlab("Species")+
  ylab("Canopy density in gmcm^-3")



# Plot of moisture content in dry basis by genus

ggplot(canopy_density_data,aes(x=reorder(genus,moisture_content),y=moisture_content))+
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Moisture Content In Percentage")

# Plot of moisture content in dry basis by species

ggplot(canopy_density_data,aes(x=reorder(species,moisture_content),y=moisture_content))+
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Moisture Content In Percentage")

# Measuring Leaf Mass Area.

lma_data <- leaf_measurement%>%
  mutate(leaf_mass_area=lma_dry_mass_g/leaf_area_cm2)

# Get the length column from the canopy measurement

length_data <- select(canopy_measurement,sample_id,species_id,length_cm)

# Merging lma_data with length data

lma_data <- merge(lma_data,length_data,by=c("sample_id","species_id"))

# Merge the leaf measurement data with the site by sample and species id
# for creating plot create plot of LMA by genus and species.

lma_data <- left_join(site_data,lma_data,by=c("sample_id","species_id"))

# Filter samples which are 70 cm

lma_data <- lma_data%>%
  filter(length_cm=="70")%>%
  na.omit()

# Plot of LMA by genus

ggplot(lma_data,aes(x=reorder(genus,leaf_mass_area),y=leaf_mass_area))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x  = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Leaf Mass Per Area In gmcm^-2")

# Plot of LMA by species

ggplot(lma_data,aes(x=reorder(species,leaf_mass_area),y=leaf_mass_area))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Leaf Mass Area In gmcm^-2")

# Plot of dry matter percent by genus

ggplot(lma_data,aes(x=reorder(genus,dry_material_perc),y=dry_material_perc))+
     geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
     theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
     xlab("Genus")+
     ylab("Dry Material In Percentage")

# Plot of dry matter percent by species

ggplot(lma_data,aes(x=reorder(species,dry_material_perc),y=dry_material_perc))+
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Dry Material In Percentage")

# Measurements of Burning Trials
# Determining temperature difference of discs,
#  and mass consumed during burning.

burning_trials <- burn_trials%>%
  mutate(burn.date=mdy(burn.date),
         tempdiff=(temp.d1.post-temp.d1.pre+temp.d2.post-temp.d2.pre)/2,
         massconsumed=(mass.pre-mass.post)/mass.pre)

# Get the genus and species by doing left join the site data with burning
# trials data

burning_trials <- left_join(site_data,burning_trials,by=c("sample_id","species_id"))

# Merge the length data with burning trials

burning_trials <- merge(burning_trials,length_data,by=c("sample_id","species_id"))

# Filter samples which are 70 cm

burning_trials <- burning_trials%>%
  filter(length_cm=="70")

# Plot of flame duration by genus

ggplot(burning_trials,aes(genus,flame.dur))+geom_jitter()+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Flame Duration In Seconds")

# Plot of flame duration by species

ggplot(burning_trials,aes(species,flame.dur))+geom_jitter()+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Flame Duration In Seconds")

# Plot of mass consumed by genus

ggplot(burning_trials,aes(genus,massconsumed))+
  geom_jitter(width = 0.2,size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
          xlab("Genus")+
          ylab("Mass Consumed")

# Plot of massconsumed by species

ggplot(burning_trials,aes(species,massconsumed))+
  geom_jitter(width = 0.2,size=3)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Mass Consumed")

# Plot of tempdiff by genus

ggplot(burning_trials,aes(x=reorder(genus,tempdiff),y=tempdiff))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Temperature Difference In Celsius")

# Plot of tempdiff by species

ggplot(burning_trials,aes(x=reorder(species,tempdiff),y=tempdiff))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Temperature Difference In Celsius")

# Plot of flame height by genus

ggplot(burning_trials,aes(x=reorder(genus,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Flame Duration In Seconds")

# Plot of flame height by species

ggplot(burning_trials,aes(x=reorder(species,flame.ht),y=flame.ht))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("species")+
  ylab("Flame Height In Centimeter")


# Plot of volume burned by genus

ggplot(burning_trials,aes(x=reorder(genus,vol.burned),y=vol.burned))+
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Volume Burned In Percentage")

# Plot of volume burned by species

ggplot(burning_trials,aes(x=reorder(species,vol.burned),y=vol.burned))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Volume Burned In Percentage")

# Plot of maximum temperature by genus

ggplot(burning_trials,aes(x=reorder(genus,max.temp),y=max.temp))+
  geom_boxplot(aes(color=genus),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Maximum Temperature In Celsius")

# Plot of maximum temperature by species

ggplot(burning_trials,aes(x=reorder(species,max.temp),y=max.temp))+
  geom_boxplot(aes(color=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=30,hjust=1,face="italic"))+
  xlab("Species")+
  ylab("Maximum Temperature In Celsius")

# Determining heat release
# Average mass of two disc is 53.17065
# The specific heat of aluminium is 0.89 Joule/gram times degree celsius
# Heat release = Mass*difference in temperature*specific heat

burning_trials <- burning_trials%>%
  mutate(heat_release_j=tempdiff*53.1765*0.89)

# Plot of heat release by genus

ggplot(burning_trials,aes(x=reorder(genus,heat_release_j),y=heat_release_j))+
  geom_boxplot(aes(color=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust=1,face="italic"))+
  xlab("Genus")+
  ylab("Heat Release In Joule")

# Plot of heat release by species

ggplot(burning_trials,aes(x=reorder(species,heat_release_j),y=heat_release_j))+
  geom_boxplot(aes(color=species),outlier.color = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Heat Release In Joule")

# PCA analysis

pca <- prcomp(select(burning_trials,massconsumed, tempdiff, vol.burned, flame.ht, flame.dur),scale=TRUE)

summary(pca)

plot(pca,type="l")

biplot(pca,scale = 0)

burning_trials$PC1 <- pca$x[,1]

# ggplot of flammability score (PC1) by genus

ggplot(burning_trials,aes(x=reorder(genus,PC1),PC1))+
  geom_boxplot(aes(color=genus,fill=genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Genus")+
  ylab("Flammability Score (PC1)")
ggsave("Flammability Score (PC1) By Genus.pdf")

# ggplot of flammability score (PC1) by species

ggplot(burning_trials,aes(x=reorder(species,PC1),PC1))+
  geom_boxplot(aes(color=species,fill=species),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,face = "italic"))+
  xlab("Species")+
  ylab("Flammability Score (PC1)")
ggsave("Flammability Score (PC1) By Species.pdf")


# Hobo data for drying temperature

Hobo_dry1 <- read.csv("C://Users//user//shrubflam//data//year_2021//lab_temperatures_during_drydown//Second Trip//20966279 2021-06-23 08_35_10 CST (Data CST).csv")
Hobo_dry1 <- Hobo_dry1%>%
  select(2:5)

Hobo_dry2 <- read.csv("C://Users//user//shrubflam//data//year_2021//lab_temperatures_during_drydown//Third Trip//20966279 2021-07-18 09_08_46 CST (Data CST).csv")
Hobo_dry2 <- Hobo_dry2%>%
  select(2:5)

hobo_dry3 <- read.csv("C://Users//user//shrubflam//data//year_2021//lab_temperatures_during_drydown//Dickens Park//20966279 2021-08-16 09_27_30 CST (Data CST)(1).csv")
hobo_dry3 <- hobo_dry3%>%
  select(2:5)

hobo_dry <- rbind(Hobo_dry1,Hobo_dry2,hobo_dry3)
hobo_dry <- hobo_dry%>%
  rename(date=Date.Time..CST. ,
         temperature=Ch..1...Temperature....Â.C.,
         rh=Ch..2...RH......,
         dew.point=Dew.Point....Â.C.)%>%
  na.omit()%>%
  summarise(mean_temp=mean(temperature),
            mean_rh=mean(rh),
            mean_dew_point=mean(dew.point))
