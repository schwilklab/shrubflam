# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2021

# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

# TODO: read GMT offset or specify timezone explicitly
TZ = "CST6CDT"
 
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

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

  
#rename(trial.time=start.time)%>%
#mutate(start.time=as.POSIXct(paste(burn.date,
                                     #trial.time,sep = " "),
                               #format="%m/%d/%Y %H:%M:%S",tz=TZ))%>%
#mutate(end.time=start.time+130+flame.dur)%>% # Two minutes
# pre-heating and ten seconds ignition period, 130s in total
#mutate(intervals=interval(start.time,end.time))%>%
#mutate(label=paste(sample_id,species_id,sep = "_"))
  

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
#files = list.files("../data/year_2021/burn_trial_hobo_temps",
                   #full.names=TRUE, recursive=TRUE,
                   #pattern = "left")
#files = files[grepl(".csv", files)]

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
#View(hobos) # flam.mid and flam.righ shows missing
# values in 06/22/2021 but it is not missing in raw data!!!

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

#unique(hobos$label) # Only 77 unique label!!!!
unique(hobos$label) # 119 unique labels with one NA, now make sense.

#june.first.trials <- hobos[1:11271,] # at 06-22-2021
#unique(june.first.trials$label) # 14 label, it's ok

#june.second.trials <- hobos[11272:22538,] # at 06_23_2021
#unique(june.second.trials$label) # 15 labels and it's ok

#july.first.trials <- hobos[22539:33211,] # at 07-17-2021
#unique(july.first.trials$label) # 13 labels and it's ok

#july.second.trials <- hobos[33212:42481,]
#unique(july.second.trials$label) # 12 labels and it's ok

#new_hobos <- separate(hobos,time, into = c("burn.date","burn.time"),
                      #sep = " ")

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
  summarise(dur.100 = sum(temperature > 100,na.rm = TRUE),
            degsec.100= sum(temperature[temperature >100],na.rm = TRUE),
            peak.temp = max(temperature, na.rm=TRUE),
            peak.time = time[which(peak.temp == temperature)[1]],
            num.NA = sum(is.na(temperature))) %>% ungroup()

#dim(hobo_temp_sum)
#hobo_temp_sum_without.na <- hobo_temp_sum%>%
  #na.omit()
#dim(hobo_temp_sum_without.na)
#names(hobo_temp_sum)

########################################################################
# Need to make the summarise data wider in order to merge with
# the samples_more_than_three to perform the PCA. Need to make 
# sure it has equal number of observations of samples_more_than_three.
########################################################################

hobos_wider <- hobo_temp_sum%>%
  pivot_wider(names_from = position)%>%
  group_by(label)%>%
  summarise(dur.100=mean(dur.100),
            peak.temp=max(peak.temp),
            degsec.100=max(degsec.100))

unique(hobos_wider$label) # 119 unique labels with one NA
dim(hobos_wider) 

#####################################################################
# Plotting the summarized data
# Merge the trials data with hobo summary data and alldata
#####################################################################

hobo_plots <- alldata %>%
  mutate(label=paste(sample_id,species_id,sep = "_"))%>%
     right_join(hobo_temp_sum, by ="label")

########################################################################
# Plot for the summary
########################################################################

ggplot(hobo_plots,aes(display_name,dur.100,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = "italic"))+
  labs(x="Display name",
       y=expression(paste("Duration over ",100^degree*C, " in (s)")))
  
       



ggplot(hobo_plots,aes(display_name,degsec.100,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = "italic"))+
  labs(x="Display name")

ggplot(hobo_plots,aes(display_name,peak.temp,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   face = "italic"))+
  labs(x="Display name",
       y=expression("Peak temperature " ( degree*C)))

########################################################################
# Cleaning the environment
########################################################################

rm("concat_hobo_files", "get_trial_label", "read_hobo_file",
"hobos_long","flam.right","flam.mid","flam.left")

