#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud

# This script is for reading the temperature and humidity for each trial from hobo data loggers and
# to test whether the temperature and humidity during trials did influence the flammability score or not!!!

# All scripts in source are required to run before running
# this script.


source("../read_data_2022.R") # script that read, clean and merged all the traits data.
source("../read_hobos_2022.R") # script that reads the thermocouples data logger data during burning.
source("../flam_pca_2022.R") # script that did the pca analysis.




TZ = "CST6CDT"

library(readr)
library(dplyr)
library(lubridate) 
library(ggplot2)
library(stringr)

####################################################################
# Reading trials data in order to grab the temperatures from hobo
# data loggers
####################################################################

trials_2022 <- read.csv("../data/year_2022/flam_trials_2022.csv",
                        stringsAsFactors = FALSE)


#####################################################################
# Getting the end trial time, converting the time zone of trials data
#####################################################################

trials_2022 <- trials_2022 %>%
  filter( ! sample_id %in% c("KD07", "UV01")) %>% # KD07 has missing values of flammability measurements and mistakenly, UV01 has ignited with blow torch though 
  # it has self ignition and has existing flame(descriptions on notes in flam_trials_2022,csv.)
  mutate(start_time = mdy_hm(str_c(date, " ",
                                   trial_time), tz = TZ)) %>%
  mutate(end_time = as.POSIXct(start_time + 120 + ignition_delay + flame_duration),
         format = "%m-%d-%y %H:%M:%S", tz = TZ) %>% # Two minutes
  #pre-heating and ignition period,  plus flame_duration
  mutate(intervals = interval(start_time, end_time)) %>%
  mutate(label = paste(sample_id,species_id, sep = "_")) %>%
  filter( date %in% c("5/29/2022", "6/21/2022", "6/30/2022", "7/8/2022")) # Getting only those dates which has hobo weather data during trials

dim(trials_2022)


class(trials_2022$start_time)


class(trials_2022$end_time)

####################################################################
# Function for reading a single hobo csv file
####################################################################

read_hobo_weather_file <- function(filename) {
  hobo <- read.csv(filename, skip=3, header=FALSE)
  names(hobo)[2:5] <- c("time", "temp", "rh", "dewpt")
  hobo <- hobo %>% select(time, temp, rh, dewpt) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
  mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
  return(hobo)
  
}    


#####################################################################
# Function for concatenating all the hobo files
#####################################################################

concat_hobo_weather_files <- function(filelist){
  l <- lapply(filelist, read_hobo_weather_file)
  r <- bind_rows(l)
  return(r)
}


#####################################################################
# Function to assign the labels after matching the trails time hobos
#####################################################################

get_trial_label <- function(time) {
  matches <- time %within% trials_2022$intervals
  if(! any(matches)) return(NA)
  return(trials_2022$label[which.max(matches)])
}


# I will read weather data from each trial date separately since the weather data from the last date is in different unit and
# It's just four trial date data.

#####################################################################################################################################
# Reading weather data from 05_29_2022 trials
#####################################################################################################################################

first_trials_weather_hobo <- read_hobo_weather_file("../data/year_2022/flam_trials_weather_hobos_2022/05_29_2022_flam_trials_weather.csv")

class(first_trials_weather_hobo$time)

class(first_trials_weather_hobo$temp)

class(first_trials_weather_hobo$rh)

class(first_trials_weather_hobo$dewpt)

range(first_trials_weather_hobo$temp, na.rm = T)

range(first_trials_weather_hobo$rh, na.rm = T)

range(first_trials_weather_hobo$dewpt, na.rm = T)

first_trials_weather_hobo$label <- unlist(sapply(first_trials_weather_hobo$time, get_trial_label)) # Assigning the labels


#######################################################################################################################################
# Reading weather data from the 06_21_2022 trials
######################################################################################################################################


second_trials_weather_hobo <- read_hobo_weather_file("../data/year_2022/flam_trials_weather_hobos_2022/06_21_2022_flam_trials_weather.csv")

class(second_trials_weather_hobo$time)

class(second_trials_weather_hobo$temp)

class(second_trials_weather_hobo$rh)

class(second_trials_weather_hobo$dewpt)

range(second_trials_weather_hobo$temp, na.rm = T)

range(second_trials_weather_hobo$rh, na.rm = T)

range(second_trials_weather_hobo$dewpt, na.rm = T)

second_trials_weather_hobo$label <- unlist(sapply(second_trials_weather_hobo$time, get_trial_label)) # Assigning the labels



###########################################################################################################################################
# Reading weather data from 06_30_2022 trials
############################################################################################################################################

third_trials_weather_hobo <- read_hobo_weather_file("../data/year_2022/flam_trials_weather_hobos_2022/06_30_2022_flam_trials_weather.csv")

class(third_trials_weather_hobo$time)

class(third_trials_weather_hobo$temp)

class(third_trials_weather_hobo$rh)

class(third_trials_weather_hobo$dewpt)

range(third_trials_weather_hobo$temp, na.rm = T)

range(third_trials_weather_hobo$rh, na.rm = T)

range(third_trials_weather_hobo$dewpt, na.rm = T)

third_trials_weather_hobo$label <- unlist(sapply(third_trials_weather_hobo$time, get_trial_label))


###########################################################################################################################################
# Reading weather data from 07_08_2022 trials
##########################################################################################################################################

fourth_trials_weather_hobo <- read_hobo_weather_file("../data/year_2022/flam_trials_weather_hobos_2022/07_08_2022_flam_trials_weather.csv")



fourth_trials_weather_hobo <- fourth_trials_weather_hobo %>%
  mutate(temp = (temp-32) * 5/9)  #  somehow the temperature for this days were in F 
#  and converted to Degree Celsius


class(fourth_trials_weather_hobo$time)

class(fourth_trials_weather_hobo$temp)

class(fourth_trials_weather_hobo$rh)


range(fourth_trials_weather_hobo$temp, na.rm = T)

range(fourth_trials_weather_hobo$rh, na.rm = T)


fourth_trials_weather_hobo$label <- unlist(sapply(fourth_trials_weather_hobo$time, get_trial_label)) # Assigning the labels


################################################################################################################################
# Binding all the trials all together
#################################################################################################################################


trials_weather_hobos_2022 <- first_trials_weather_hobo %>%
  rbind(second_trials_weather_hobo) %>%
  rbind(third_trials_weather_hobo) %>%
  rbind(fourth_trials_weather_hobo)


###################################################################################################################################
# Summarising the temperature and humidity by group_by() of labels
##################################################################################################################################

trials_weather_hobos_2022 <- trials_weather_hobos_2022[,-4] %>% # Removing dew points
  na.omit() %>%
  group_by(label) %>%
  summarise(hobo_temp = max(temp),
            hobo_rh = max(rh))

####################################################################################################################################
# Merging with the final data by label
###################################################################################################################################

trials_weather_data <- final_data %>%
  left_join(trials_weather_hobos_2022, by = "label") 


####################################################################################################################################
# Does temperature and humididty during each trial influenced the flammability score?
#####################################################################################################################################

ggplot(trials_weather_data, aes(hobo_temp, PC1)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

summary(lm(PC1 ~ hobo_temp, data = trials_weather_data)) # P value 0.863


ggplot(trials_weather_data, aes(hobo_temp, PC2))  +
  geom_point() +
  geom_smooth(method = "lm", se = F)

summary(lm(PC2 ~ hobo_temp, data = trials_weather_data)) #  p value   0.412  

ggplot(trials_weather_data, aes(hobo_rh, PC1)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

summary(lm(PC1 ~ hobo_rh, data = trials_weather_data)) # p value 0.118

ggplot(trials_weather_data, aes(hobo_rh, PC2)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

summary(lm(PC2 ~ hobo_rh, data = trials_weather_data)) # p value 0.12634


##########################################################################################################################################
# Cleaning the environment
###########################################################################################################################################

rm("trials_2022", "read_hobo_weather_file", "concat_hobo_weather_files ", "get_trial_label", "first_trials_weather_hobo",
   "second_trials_weather_hobo", "third_trials_weather_hobo", "trials_weather_hobos_2022", "trials_weather_data")









