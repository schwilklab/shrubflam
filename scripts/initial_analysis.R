# Azaj Mahmud
# Shrub Flammability
# Summer 2021

## DWS edits 2021-08-12


library(readr)
library(stringr)
library(dplyr)
#library(tidyverse) ## DWS: redundant with much above!
library(ggplot2)
library(lubridate)
library(cluster)



# Getting the Epicollect data

## DWS: Why is the epicollect data in multiple files by trip? The database
## itself on epicollect knows nothing about "trips". This makes no sense.


# Get the epicollect data from the second trip

Epi_second <- read.csv("../data/year_2021/epicollect_data_dump/Epicollect5.csv")
## DWS: I could not find the file "Epicollect5_second_Trip.csv" anywhere in
## your data on that branch.

## DWS: why are you loading "readr" then using
## read.csv? Do one or the other.

# Shape the data and select some specific column
Epi_second <- Epi_second%>%
  select(date,site,sample_ID,species,species_ID)%>%
  rename(Site=site,
         Sample_ID=sample_ID,
         Species_ID=species_ID,
         Species=species,
         Date=date)

# Get the epicollect data from first and third trip

# Epi_first_third <- read.csv("Epicollect5_Third_First_Trip.csv")

## DWS: again, I cannot find this file. You code doesn't make any sense with your data.

## DWS; I'm stopping now. FIX this.

# shape the data, rename the column name and select some specific column

Epi_first_third <- Epi_first_third%>%
  select(date,site,sample_ID,species,species_ID)%>%
  rename(Site=site,
         Sample_ID=sample_ID,
         Species_ID=species_ID,
         Species=species,
         Date=date)

# Turning those three dataset into one
Epi_data <- rbind(Epi_first_third,Epi_second)

# Getting all the column except Species_ID to merge with burning and moisture content
# data
Epi_data <- Epi_data%>%
  select(-c(5))

# Canopy density and moisture content data

# Getting Canopy density data from the first trip
canopy_first <- read.csv("MC_Canopy_Density_length.csv")

# Remove Average_Width_inch column since it is empty.
canopy_first <- canopy_first%>%
  select(-c(7))

# Canopy density data from the second trip
canopy_second <- read.csv("MC_Canopy_Density_length_2nd_trip.csv")

# Remove Average_Width_inch  since it is empty 
canopy_second <- canopy_second%>%
  select(-c(7))

# Canopy density data from third trip
canopy_third <- read.csv("MC_Canopy_Density_Third_trip.csv")

# Remove Average_Width_inch since it is empty. 
canopy_third <- canopy_third%>%
  select(-c(7))

# Turn three data set into a single one by rbind
canopy_density <- rbind(canopy_first,canopy_second,canopy_third)

# Extracting Leaf mass area data

# Getting LMA and percentage of dead material data from first trip
Lma_first <- read.csv("LMA_Per_Dead_Material.csv")

# Getting LMA and percentage of dead material data from second trip
Lma_second <- read.csv("LMA_second_trip.csv")

# Getting LMA and percentage of dead material data from the third trip
Lma_third <- read.csv("LMA_Third_trip.csv")

# Getting those three data set into a single data set by using rbind
Lma_data <- rbind(Lma_first,Lma_second,Lma_third)

# Shaping Lma_data by dplyr, remove LMA and speciesid column to merge
Lma_data <- Lma_data%>%
  select(-c(6))%>%
  rename(Dry_Mass_lma=Dry_Mass_gm,
         Fresh_Mass_lma=Fresh_Mass_gm)

# Extracting the burning data
# Get the burning data from the first trip

Burn_first <- read.csv("First_Trip_Burn_20210601_04.csv")



# Get the burning data from the second trip

Burn_second <- read.csv("Second_Trip_Burn.csv")



# get the burning data from the third trip
Burn_third <- read.csv("Burning_Data_Third_Trip.csv")



# Shape the data by dplyr

Burn_first <- Burn_first%>%
  select(-c(19))
Burn_second <- Burn_second%>%
  select(-c(19))

# Turn three dataset into one
Burning_data <- rbind(Burn_first,Burn_second,Burn_third)

# Shaping burning data
Burning_data <- Burning_data%>%
  select(-c(3))%>%
  rename(Burn_Date=Date)

# Merging canopy and epicollect data to determine the canopy denisty

Epi_canopy <- merge(Epi_data,canopy_density,by=c("Sample_ID"))

# Merge Epi_canopy data with Burning_data
Epi_canopy_burn <- merge(Epi_canopy,Burning_data,by=c("Sample_ID"))

# Get the samples which are 70 cm in length
Epi_canopy_burn <- Epi_canopy_burn%>%
  filter(Length_cm=="70")

# Shaping the Epi_canopy data by dplyr,turn Species into genus and species,
# change the width and length unit from inch and centimeter to meter.
# Separate the Species column into genus and species columns
Epi_canopy_burn <- separate(Epi_canopy_burn,Species, into = c("Genus","Species"),sep = " ")

# Changing the unit and get the total mass of each sample
# The width measured in inch and length in cm.
# Total mass is the addition of the mass of sample before burning
# plus the mass of the small portion which used to measure the moisture content
# on dry basis.
Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Bottom_Width_inch=Bottom_Width_inch*0.0254,
         Middle_Width_inch=Middle_Width_inch*0.0254,
         Top_Width_inch=Top_Width_inch*0.0254,
         Length_cm=Length_cm/100,
         Total_Mass=Fresh_Mass_gm+Pre_Mass_gm)



# Get the average width which is gonna be the radius of a cylinder.

Epi_canopy_burn$Average_Width <- apply(Epi_canopy_burn[,(8:10)],1,mean)

# Determine canopy volume( though unnecessary) and Canopy_Density
Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Canopy_Volume=pi*(Average_Width)^2*Length_cm)%>%
  mutate(Canopy_Density=Total_Mass/Canopy_Volume)

# Mean Canopy density by genus though not compulsory
Epi_canopy_burn_by_genus <- Epi_canopy_burn%>%
  group_by(Genus)%>%
  summarise(Mean_CD=mean(Canopy_Density))%>%
  mutate(Order=Mean_CD>3184.1337,
         order=if_else(Order==TRUE,"More_Than_Juniper","Less_Than_Juniper"))

# Creating plot by using ggplot2, Canopy density by species_ID
p <- ggplot(Epi_canopy_burn,aes(Species_ID,Canopy_Density))+
  geom_boxplot(outlier.colour = "red",outlier.shape = 2)+
  geom_boxplot(aes(color=Species_ID))+
  ggtitle("Canopy Density By Species ID")+
  xlab("Species ID")+
  ylab("Canopy Density(gram per meter cube)")+
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))
# ggplot of canopy density by Genus
cp <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Canopy_Density),y=Canopy_Density))+
  geom_boxplot(aes(color=Genus), outlier.colour = "red", outlier.size = 2)+
  ggtitle("Canopy Density BY Genus")+
  xlab("Genus")+
  ylab("Canopy Density(gram per meter cube)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "italic"),
        plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))

# ggplot of mean canopy density by genus
cpg <- ggplot(Epi_canopy_burn_by_genus,aes(x=reorder(Genus,Mean_CD),y=Mean_CD,fill=Genus))+
  geom_bar(stat = "identity")+
  ggtitle("Mean Canopy Density BY Genus")+
  xlab("Genus")+
  ylab("Canopy Density(gram per meter cube)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "italic"),
        plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"))

# Moisture content determination by dry basis
Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Moisture_Content=((Fresh_Mass_gm-Dry_Mass_gm)/Dry_Mass_gm)*100)

# ggplot of Moisture COntent

# Plot by species ID
q <- ggplot(Epi_canopy_burn,aes(x=reorder(Species_ID,Moisture_Content),y=Moisture_Content,Species_ID))+
  geom_boxplot(aes(color=Species_ID),outlier.colour = "red",outlier.size = 2)+
  ggtitle("Moisture Content By Species ID")+
  xlab("Species ID")+
  ylab("Moisture Content in %")+
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
       axis.title.y = element_text(color="#993333", size=14, face="bold"))
# Plot by Genus
qmc <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Moisture_Content),y=Moisture_Content,Genus))+
  geom_boxplot(aes(color=Genus),outlier.colour = "red",outlier.size = 2)+
  ggtitle("Mositure Content By Genus")+
  xlab("Genus")+
  ylab("Moisture Content in %")+
  theme(plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue",size=14,face="bold"),
        axis.title.y = element_text(color = "#993333",size=14,face = "bold" ),
        axis.text.x = element_text(angle = 30, hjust = 1, face="italic"))


# LMA data
# Merging Lma data with epicollect data
Epi_lma <- merge(Epi_data,Lma_data,by="Sample_ID")
# Separate Species into Genus and Species
Epi_lma <- separate(Epi_lma,Species,into = c("Genus","Species"))

# Remove Species_ID data for merging with canopy data
Epi_lma <- Epi_lma%>%
  select(-c(6))

# Merging Epi_lma with canopy_density data
Epi_lma_canopy <- merge(Epi_lma,canopy_density,by="Sample_ID")

# get samples which are 70 cm in length
Epi_lma_canopy <- Epi_lma_canopy%>%
  filter(Length_cm=="70")


# Plot by ggplot of dry material by Genus
pdm <- ggplot(Epi_lma_canopy,aes(x=reorder(Genus,Dry_Material),y=Dry_Material,Genus))+
  geom_boxplot(aes(color=Genus),outlier.colour = "red",outlier.size = 2)+
  theme(axis.text.x = element_text(angle=45,hjust=1,face="italic") )
  
# Plot by ggplot of dry material by species_ID
pdms <- ggplot(Epi_lma_canopy,aes(x=reorder(Species_ID,Dry_Material),y=Dry_Material,Species_ID))+
  geom_boxplot(aes(color=Genus),outlier.colour = "red",outlier.size = 2)+
  ggtitle("Percent of Dry Material by Species ID")+
  xlab("Species ID")+
  ylab("Percent of Dry Material")+
  theme(plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue",size=14,face="bold"),
        axis.title.y = element_text(color = "#993333",size=14,face = "bold" ),
        axis.text.x = element_text(angle = 30, hjust = 1, face="italic"))

# Determining mean dry material by Genus
Epi_dry_material_by_genus <- Epi_lma_canopy%>%
  group_by(Genus)%>%
  summarise(Mean_DM=mean(Dry_Material))

# Bar plot of mean dry matter by genus
pmdm <- ggplot(Epi_dry_material_by_genus,aes(x=reorder(Genus,Mean_DM),y=Mean_DM,Genus))+
  geom_bar(stat = "identity",aes(color=Genus,fill=Genus))+
  xlab("Genus")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,face = "italic"))

# Determining LMA, change leaf area unit from centimeter square to meter square
Epi_lma_canopy <- Epi_lma_canopy%>%
  mutate(Leaf_Area=Leaf_Area*0.0001)%>%
  mutate(LMA=Dry_Mass_gm/Leaf_Area)%>%
  na.omit()

# Plot by ggplot of leaf mass area by Genus
plma <- ggplot(Epi_lma_canopy,aes(x=reorder(Genus,LMA),y=LMA,Genus))+
  geom_boxplot(aes(color=Genus),outlier.colour = "red",outlier.size = 2)+
  ggtitle("Leaf Mass Area By Genus")+
  xlab("Genus")+
  ylab("Leaf Mass Area(gmm^-2)")+
  theme(plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue",size=14,face="bold"),
        axis.title.y = element_text(color = "#993333",size=14,face = "bold" ),
        axis.text.x = element_text(angle = 30, hjust = 1, face="italic"))

# plot by ggplot of leaf mass area by species ID
plma <- ggplot(Epi_lma_canopy,aes(x=reorder(Species_ID,LMA),y=LMA,Species_ID))+
  geom_boxplot(aes(color=Genus),outlier.colour = "red",outlier.size = 2)+
  ggtitle("Leaf Mass Area By Genus")+
  xlab("Genus")+
  ylab("Leaf Mass Area(gmm^-2)")+
  theme(plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
        axis.title.x = element_text(color = "blue",size=14,face="bold"),
        axis.title.y = element_text(color = "#993333",size=14,face = "bold" ),
        axis.text.x = element_text(angle = 30, hjust = 1, face="italic"))


# Burning Data
# Getting Temperature Difference and amount of mass consumed during burning

Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Temp_Diff=(Post_Temp_D1-Pre_Temp_D1+Post_Temp_D2-Pre_Temp_D2)/2,
         Mass_Consumed=(Pre_Mass_gm-Post_Mass_gm)/Pre_Mass_gm)%>%
  na.omit()

# Create plot in ggplot
# ggplot of Temperature Difference By Genus
pb <- ggplot(Epi_canopy_burn,aes(Genus,Temp_Diff))+
  geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))
# ggplot of Temperature Difference By Species_ID
pbs <- ggplot(Epi_canopy_burn,aes(Species_ID,Temp_Diff))+
  geom_jitter(width=0.2, size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))
# Geom_boxplot  of Temperature Difference By Genus
tdbg <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Temp_Diff),y=Temp_Diff,Genus))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle=45,hjust = 1,face="italic"))+
  xlab("Genus")


# ggplot of mass consumed
# By genus
mcg <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Mass_Consumed),y=Mass_Consumed,Genus))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"))+
  xlab("Genus")

# By Species ID and colored Genus
mcs <- ggplot(Epi_canopy_burn,aes(x=reorder(Species_ID,Mass_Consumed),y=Mass_Consumed,Species_ID))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"))+
  xlab("Species_ID")

# Burning time by Genus
pbt <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Flame_Dur_Sec),y=Flame_Dur_Sec,Genus))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Genus")
# Burning time by species ID
pbs <- ggplot(Epi_canopy_burn,aes(x=reorder(Species_ID,Flame_Dur_Sec),y=Flame_Dur_Sec,Species_ID))+
  geom_boxplot(aes(color=Species_ID),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Species_ID")
  
# ggplot of Max Temp by Genus
pmtg <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Max_Temp),y=Max_Temp,Genus))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Genus")
# ggplot of Max Temp by Species ID
pmts <- ggplot(Epi_canopy_burn,aes(x=reorder(Species_ID,Max_Temp),y=Max_Temp,Species_ID))+
  geom_boxplot(aes(color=Species_ID),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Genus")


# ggplot of volume burn by genus
pvbg <- ggplot(Epi_canopy_burn,aes(x=reorder(Genus,Vol_Burn),y=Vol_Burn,Genus))+
  geom_boxplot(aes(color=Genus),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Genus")
# ggplot of volume burn by species ID
pvbs <- ggplot(Epi_canopy_burn,aes(x=reorder(Species_ID,Vol_Burn),y=Vol_Burn,Species_ID))+
  geom_boxplot(aes(color=Species_ID),outlier.color = "red",outlier.alpha = 0.5,outlier.size = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, face="italic"))+
  xlab("Species_ID")
# Mean volume burn by genus and speciesid
Mean_volb_by_genus <- Epi_canopy_burn%>%
  group_by(Genus)%>%
  summarise(Mean_Volb=mean(Vol_Burn))
ggplot(Mean_volb_by_genus,aes(x=reorder(Genus,Mean_Volb),y=Mean_Volb,Genus))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45,hjust=1,face="italic"))+
  xlab("Genus")
# Mean volume burn by speciesid
Mean_volb_by_speciesid <- Epi_canopy_burn%>%
  group_by(Species_ID)%>%
  summarise(Mean_Volb=mean(Vol_Burn))
ggplot(Mean_volb_by_speciesid,aes(x=reorder(Species_ID,Mean_Volb),y=Mean_Volb,Species_ID))+
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle = 45,hjust=1,face="italic"))+
  xlab("Species_ID")


# Determining ignitibility
Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Ignitibility=if_else(Flame_Dur_Sec==0, "0","1"))

# Getting ignitibility as integer
Epi_canopy_burn$Ignitibility <- as.integer(Epi_canopy_burn$Ignitibility)

# Mean ignitbility by genus 
Ignitibility_by_genus <- Epi_canopy_burn%>%
  group_by(Genus)%>%
  summarise(Mean_Ignitibility=mean(Ignitibility))

# Mean ignitibility by species ID
Ignitibility_by_speciesid <- Epi_canopy_burn%>%
  group_by(Species_ID)%>%
  summarise(Mean_Ignitibility=mean(Ignitibility))
# ggplot of ignitibility by genus
pig <- ggplot(Ignitibility_by_genus,aes(x=reorder(Genus, Mean_Ignitibility),y=Mean_Ignitibility,Genus))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"))+
  xlab("Genus")
# ggplot of ignitibility by species ID
pis <- ggplot(Ignitibility_by_speciesid,aes(x=reorder(Species_ID, Mean_Ignitibility),y=Mean_Ignitibility,Species_ID))+
  geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"))+
  xlab("Species_ID")


# Heat release of disc one
Epi_canopy_burn <- Epi_canopy_burn%>%
  mutate(Heat_release_D1=(Post_Temp_D1-Pre_Temp_D1)*0.897*50.3054,
         Heat_release_D2=(Post_Temp_D2-Pre_Temp_D2)*0.897*50.3054,
         Heat_release=(Heat_release_D1+Heat_release_D2)/2)

# Mass loss of those samples which are not ignited
Epi_canopy_burn_ignitibility <- Epi_canopy_burn%>%
  filter(Ignitibility=="0")
Epi_canopy_burn_ignitibility<- Epi_canopy_burn_ignitibility%>%
  mutate(Mass_loss=Pre_Mass_gm-Post_Mass_gm)

ggplot(Epi_canopy_burn_ignitibility,aes(Genus,Mass_loss))+
  geom_boxplot(aes(color=Genus))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face="italic"))
  


# Scoring flammability as Dr. Tim Curran's lab
# Flammability by ignitibility,sustainability as flame duration, consumability as 
# volume burn, combustibility as maximum temperature.
Flammability_data <- Epi_canopy_burn%>%
  select(Sample_ID,Species_ID,Genus,Flame_Dur_Sec,Vol_Burn,Max_Temp,Ignitibility)

# Asigning 0 to volume burn, Max Temp, and Ignitibility if the flame_duration is zero
Flammability_data$Vol_Burn[Flammability_data$Ignitibility==0] <- 0
Flammability_data$Max_Temp[Flammability_data$Flame_Dur_Sec==0] <- 0  

# Flammability scoring
Flammability <- Flammability_data%>% 
  group_by(Genus)%>%
  summarise(Mean_Flamd=mean(Flame_Dur_Sec),
            Mean_Volb=mean(Vol_Burn),
            Mean_Maxt=mean(Max_Temp),
            Mean_Ign=mean(Ignitibility))
Flammability$Flammability <- apply(Flammability[,c(2:5)],1,sum)
ggplot(Flammability,aes(x=reorder(Genus,Flammability),y=Flammability,Genus))+
  geom_bar(stat = "identity",aes(color=Genus,fill="white"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face = "italic"))+
  xlab("Genus")

# Flammability by speciesID
Flammability_by_speciesid <- Flammability_data%>%
  group_by(Species_ID)%>%
  summarise(Mean_Flamd=mean(Flame_Dur_Sec),
            Mean_Volb=mean(Vol_Burn),
            Mean_Maxt=mean(Max_Temp),
            Mean_Ign=mean(Ignitibility))
  
Flammability_by_speciesid$Flammability <- apply(Flammability_by_speciesid[,c(2:5)],1,sum)
ggplot(Flammability_by_speciesid,aes(x=reorder(Species_ID,Flammability),y=Flammability,Species_ID))+
  geom_bar(stat = "identity",aes(color=Species_ID,fill="white"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,face="italic"))+
  xlab("Species_ID")



# Doing K-means clustering analysis
Flam_score <- Flammability_data%>%
  select(-c(1:2))%>%
  na.omit()
Flam_score <- scale(Flam_score[,c(2:5)])
Flam_score <- as.data.frame(Flam_score)
Flam_score <- Flam_score%>%
  na.omit()
k_mean <- kmeans(Flam_score,6)


plot(Flam_score[c("Max_Temp","Flame_Dur_Sec","Vol_Burn","Ignitibility")],col=k_mean$cluster)
# I couldn't understand anything about this plot.

# PCA analysis
flam1 <- Flammability_data%>%
  select(c(3:7))
pca <- prcomp(flam1[,-1],scale=TRUE)
summary(pca)
plot(pca,type="l")
# First PCA explains 64 percent and secodn PCA explains 23 percent variation
biplot(pca,scale = 0)

flam2 <- cbind(flam1,pca$x[,1:2])
ggplot(flam2,aes(PC1,PC2,col=Genus,fill=Genus))+
  stat_ellipse(geom = "polygon",col="black",alpha=0.5)+
geom_point(shape=21,col="black")
# All I understand so far is that lots of overlapping

# Hobo drying data
Hobo_one <- read.csv("Hobo_drying_data.csv")
Hobo_two <- read.csv("20966279 2021-07-18 09_08_46 CST (Data CST).csv")
Hobo_three <- read.csv("Hobo_Drying_Third_Trip.csv")

Hobo_one <- Hobo_one%>%
  select(c(3:5))
Hobo_two <- Hobo_two%>%
  select(c(3:5))
Hobo_three <- Hobo_three%>%
  select(c(3:5))
Hobo_data <- bind_rows(Hobo_one,Hobo_two,Hobo_three)
Hobo_data <- Hobo_data%>%
  select(c(1:3))%>%
  na.omit()
Drying_data <- apply(Hobo_data[(1:3),],2,mean)

# Temperature Relative_Humidity         Dew_Point 
# 23.82333          50.44667          12.92667 



  
