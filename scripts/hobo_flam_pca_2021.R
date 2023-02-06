#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2021
## read hobos and PCA analysis from 2021, just in a single
## script.


source("./read_data_2021.R") # The script where the data from 2021 were cleaned 

TZ = "CST6CDT"

library(tidyr)
library(stringr)

## DWS: Do you need all those packages?
# AM: removed unnecessary packages.

####################################################################
# Reading trials data in order to grab the temperatures from hobo
# data loggers
####################################################################

trials <- read.csv("../data/year_2021/burn_trials.csv",
                   stringsAsFactors = FALSE)

#####################################################################
# Getting the end trial time, converting the time zone of trials data
#####################################################################

trials <- trials%>%
  mutate(start.time = mdy_hm(str_c(burn.date, " ",
                                   start.time),tz=TZ))%>%
  mutate(end.time=as.POSIXct(start.time+130+flame.dur),
         format="%m-%d-%y %H:%M:%S",tz=TZ)%>% # Two minutes
  #pre-heating and ten seconds ignition period, 130s in total
  mutate(intervals=interval(start.time,end.time))%>%
  mutate(label=paste(sample_id,species_id,sep = "_"))

unique(trials$label) # 118 labels
class(trials$start.time) # as POSIXct
class(trials$end.time) # as POSIXct
class(trials$intervals) # as Interval


####################################################################
# Function for reading a single hobo csv file
####################################################################

read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=2, header=FALSE)
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
  return(hobo)
}

#####################################################################
# Function for concatenating all the hobo files
#####################################################################

concat_hobo_files <- function(filelist, label){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  names(r) <- c("time", label)
  return(r)
}

######################################################################
# We could grab the column name from the file name and do this in one go, but
# this also works.
#######################################################################

########################################################################
# Grabbing all the hobo files from left
########################################################################

flam.left <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                          full.names=TRUE, recursive=TRUE,
                                          pattern = "flam.left*.csv"),
                               "flam.left")

class(flam.left$time) # As POSIXct
any(is.na(flam.left$flam.left)) # No missing value in flam.left
any(is.na(flam.left$time)) # No missing value in in time

#####################################################################
# Grabbing all the hobo files from mid
#####################################################################

flam.mid <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                         full.names=TRUE, recursive=TRUE,
                                         pattern = "flam.mid*.csv"),
                              "flam.mid")

class(flam.mid$time) # As POSIXct
any(is.na(flam.mid$flam.mid)) # No NA in flam.mid
any(is.na(flam.mid$time)) # No NA 
flam.mid.separate <- separate(flam.mid,time, into = c("date","time"),
                              sep = " ")
unique(flam.mid.separate$date) # Nine trials date, 
# data from mid.hobo from 2021-06-04 is absent

#####################################################################
# Grabbing all the hobo files from right
#####################################################################

flam.right <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                           full.names=TRUE, recursive=TRUE,
                                           pattern = "flam.right*.csv"),
                                "flam.right")

class(flam.right$time) # As POSIXct
any(is.na(flam.right$flam.right)) # No NA
any(is.na(flam.right$time)) # No NA

#####################################################################
# Getting all the hob files in a single data frame
#####################################################################

hobos <- full_join(flam.left,flam.mid,by = "time") %>% 
  full_join(flam.right,by="time")

class(hobos$time) # As POSIXct
any(is.na(hobos)) # Yes, hobos has NA
#View(hobos) 
hobos_separate <- separate(hobos,time, into = c("date","time"),
                           sep= " ")


hobos_separate <- hobos_separate %>%
  filter(date == "2021-06-04")


#####################################################################
# Function to assign the labels after matching the trails time hobos
#####################################################################

get_trial_label <- function(time) {
  matches <- time %within% trials$intervals
  if(! any(matches)) return(NA)
  return(trials$label[which.max(matches)])
}

#####################################################################
# Assigning the labels
#####################################################################

hobos$label <- unlist(sapply(hobos$time, get_trial_label))

unique(hobos$label) # 119 unique labels with one NA, now make sense.


#unique(trials$burn.date) 
#unique(new_hobos$burn.date) # 06/01/2021,
# 06/04/2021 and 08/16/2021 data is  entirely 
# missing in hobos


#####################################################################
# Getting the hobos as long format to summarise the data by 
# three thermocouples
#####################################################################

hobos_long <- hobos%>%
  gather(key = "position",
         value = "temperature",-time,-label)

#####################################################################
# Summarising the hobo data
#####################################################################

hobo_temp_sum <- hobos_long %>% group_by(label, position) %>%
  summarise(dur.100 = sum(temperature > 100),
            degsec.100= sum(temperature[temperature >100]),
            peak.temp = max(temperature),
            peak.time = time[which(peak.temp == temperature)[1]],
            num.NA = sum(is.na(temperature))) %>% ungroup()%>%
  filter(label != "NA")

# hobo.mid is missing for all trails on 2021-06-04.

########################################################################
# Need to make the summarise data wider in order to merge with
# the samples_more_than_three to perform the PCA. Need to make 
# sure it has equal number of observations of samples_more_than_three.
########################################################################

hobos_wider <- hobo_temp_sum%>%
  group_by(label)%>%
  summarise(dur.100=mean(dur.100, na.rm = TRUE),
            peak.temp=max(peak.temp, na.rm = TRUE),
            degsec.100=max(degsec.100, na.rm = TRUE))

# 16 samples from day 2021-06-04 showed NA for missing

unique(hobos_wider$label) # 118 unique labels
dim(hobos_wider)
#View(hobos_wider)





####################################################################
## PCA analysis, just to make sure that temperature
# integration is highly correlated with all the variables related to
# heat release for 2021.
# Merging the hobo data with alldata to do 
# the PCA.
#####################################################################

pca_data <- alldata %>%
  left_join(hobos_wider, by ="label")%>%
  select(label, heat_release_J, massconsumed, vol.burned, 
  flame.ht, flame.dur, dur.100, peak.temp, degsec.100)

dim(pca_data)

any(is.na(pca_data)) # No NA
#View(pca_data)

  
####################################################################
## pca by prcomp, correlation matrix since I am using scale is TRUE
####################################################################

flam_pca <- prcomp(pca_data[,-1], 
                        scale=TRUE)

summary(flam_pca)
flam_loadings <- flam_pca$rotation[,1:2] 
flam_loadings  # degsec.100 highly correlated with all the variables related to heat release.



herbivore_2021 <- alldata %>%
  left_join(hobos_wider, by ="label") %>%
  rename(degsec_100 = degsec.100) %>%
  mutate(display_name = ifelse(display_name == "F. reticulata", "F. pubescens", display_name),
         display_name = ifelse(display_name == "Z. obtusifolia", "S. obtusifolia", display_name),
         display_name = ifelse(display_name == "Q. fusiformis", "Q. virginiana", display_name)) %>%
  dplyr::select(display_name, degsec_100, herbivore_preference,
                herbivore_defense, site)  
  
  

unique(herbivore_2021$display_name)
dim(herbivore_2021) # 98

## This dataset will be used for herbivore analysis.



######################################################################

rm("concat_hobo_files", "get_trial_label", "read_hobo_file",
   "hobos_long", "flam.right","flam.mid","flam.left",
   "pca_data", "flam_pca", "flam_loadings")
