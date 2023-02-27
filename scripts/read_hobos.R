#!/usr/bin/Rscript --vanilla

# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2022

# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

TZ = "CST6CDT"

library(tidyr)
library(ggplot2)
library(stringr)

####################################################################

trials_2022 <- read.csv("./data/year_2022/flam_trials_2022.csv",
                        stringsAsFactors = FALSE)


#####################################################################
# Getting the end trial time, converting the time zone of trials data
#####################################################################

trials_2022 <- trials_2022 %>%
  filter( ! sample_id %in% c("KD07", "UV01")) %>% # KD07 has missing values of flammability measurements and mistakenly, UV01 has ignited with blow torch though 
  # it has self ignition and has existing flame(descriptions on notes in flam_trials_2022,csv.)
  mutate(start_time = mdy_hm(str_c(date, " ",
                                   trial_time), tz = TZ))%>%
  mutate(end_time = as.POSIXct(start_time + 120 + ignition_delay + flame_duration),
         format = "%m-%d-%y %H:%M:%S", tz = TZ)%>% # Two minutes
  #pre-heating and ignition period,  plus flame_duration
  mutate(intervals = interval(start_time, end_time))%>%
  mutate(label = paste(sample_id, trials, sep = "_"))

dim(trials_2022)



################################################################################
# Getting alldata_2022 from read_data.R, labeled by sample_id and trials
# since I already removed the species_id. I hope it will make sense!
################################################################################

#hobo_thermocouples_data <- burn_trials_2022 %>%
  #mutate(burn_date <- as.Date(burn_date, "%Y-%m-%d")) %>%
  #mutate(trial_time <- hms(trial_time)) %>%
  #mutate(start_time = as.POSIXct(paste(burn_date, trial_time), tz = TZ)) %>%
  #mutate(end_time = as.POSIXct(start_time + 120 + ignition_delay + flame_duration),
         #format = "%m-%d-%y %H:%M:%S", tz = TZ)%>% # Two minutes
  #pre-heating and ignition period,  plus flame_duration
  #mutate(intervals = interval(start_time, end_time))%>%
  #mutate(label = paste(sample_id, trials, sep = "_"))


#class(hobo_thermocouples_data$start_time)


#class(hobo_thermocouples_data$end_time)




####################################################################
# Function for reading a single hobo csv file
####################################################################

read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip = 2, header = FALSE)
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% dplyr::select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time, tz = TZ), "second"))
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
# Grabbing all the hobo files from left
########################################################################

flam_left <- concat_hobo_files(list.files("./data/year_2022/hobos_2022",
                                          full.names = TRUE, recursive = TRUE,
                                          pattern = "flam.left*.csv"),
                               "flam_left")



class(flam_left$time) 
any(is.na(flam_left$flam.left))
any(is.na(flam_left$time)) 
flam.left <- separate(flam_left, time, into = c("date", "time"), sep = " ")
unique(flam.left$date)


#####################################################################
# Grabbing all the hobo files from mid
#####################################################################

flam_mid <- concat_hobo_files(list.files("./data/year_2022/hobos_2022",
                                         full.names = TRUE, recursive = TRUE,
                                         pattern = "flam.mid*.csv"),
                              "flam_mid")

class(flam_mid$time) 
any(is.na(flam_mid$flam.mid))
any(is.na(flam_mid$time)) 
flam.mid <- separate(flam_mid,time, into = c("date","time"),
                     sep = " ")
unique(flam.mid$date)

#####################################################################
# Grabbing all the hobo files from right
#####################################################################

flam_right <- concat_hobo_files(list.files("./data/year_2022/hobos_2022",
                                           full.names = TRUE, recursive = TRUE,
                                           pattern = "flam.right*.csv"),
                                "flam_right")


class(flam_right$time)
any(is.na(flam_right$flam.right))
any(is.na(flam_right$time))
flam.right <- separate(flam_right, time, into =  c("date", "time"), 
                       sep = " ")
unique(flam.right$date)

#####################################################################
# Getting all the hob files in a single data frame
#####################################################################

hobos <- full_join(flam_left, flam_mid, by = "time") %>% 
  full_join(flam_right,  by = "time")
class(hobos$time)
hobos_separate <- separate(hobos, time, into = c("date", "time"),
                           sep= " ")

unique(hobos_separate$date)



#####################################################################
# Function to assign the labels after matching the trails time hobos
#####################################################################

get_trial_label <- function(time) {
  matches <- time %within% trials_2022$intervals
  if(! any(matches)) return(NA)
  return(trials_2022$label[which.max(matches)])
}


#####################################################################
# Assigning the labels
#####################################################################

hobos$label <- unlist(sapply(hobos$time, get_trial_label))


unique(hobos$label) 
length(unique(hobos$label)) #138, one NA

#####################################################################
# Getting the hobos as long format to summarise the data by 
# three thermocouples
#####################################################################

hobos_long <- hobos %>%
  gather(key = "position",
         value = "temperature", -time, -label)

#####################################################################
# Summarising the hobo data
#####################################################################

hobo_temp_sum <- hobos_long %>% group_by(label, position) %>%
  summarise(dur_100 = sum(temperature > 100),
            degsec_100= sum(temperature[temperature >100]),
            peak_temp = max(temperature),
            peak_time = time[which(peak_temp == temperature)[1]],
            hobo.left = max(temperature),
            hobo.right = max(temperature),
            hob.mid = max(temperature),
            num_NA = sum(is.na(temperature))) %>% ungroup()%>%
  filter(label != "NA")

dim(trials_2022) #137
dim(hobo_temp_sum) # 137*3 = 411


###############################################################################
# Need to make the summarise data wider in order to merge with the alldata_2022
# to perform the PCA. Need to make sure it has equal number of observations of
# alldata_2022.
###############################################################################
hobos_wider_2022 <- hobo_temp_sum %>%
  group_by(label) %>%
  summarise(dur_100=mean(dur_100),
            peak_temp=max(peak_temp),
            degsec_100=max(degsec_100))

dim(hobos_wider_2022)

##############################################################################
# The remaining code is only for plotting purpose Plotting the summarized data
# Merge the hobo summary data with alldata_2022
#############################################################################

hobo_plots <- alldata_2022 %>%
  right_join(hobo_temp_sum, by = "label") %>%
  filter( sample_id != "NA")

## DWS: this produces a warning.



dim(alldata_2022) # 116
dim(hobo_plots) # 116*3 = 348
any(is.na(hobo_plots$dur_100)) #FALSE
any(is.na(hobo_plots$degsec_100)) #FALSE
any(is.na(hobo_plots$peak_temp)) #FALSE

########################################################################
# Plot for the summary
########################################################################

## ggplot(hobo_plots,aes(field_taxon, dur_100, color = field_taxon))+
##   geom_jitter(width = 0)+
##   facet_grid(.~position)+
##   theme_bw()+
##   theme(axis.text.x = element_text(angle = 45,
##                                    hjust = 1,
##                                    face = "italic"))+
##   labs(x = "Display name",
##        y=expression(paste("Duration over ",100^degree*C, " in (s)")))


## ggplot(hobo_plots,aes(field_taxon, degsec_100, color = field_taxon))+
##   geom_jitter(width = 0)+
##   facet_grid(.~position)+
##   theme_bw()+
##   theme(axis.text.x = element_text(angle = 45,
##                                    hjust = 1,
##                                    face = "italic"))+
##   labs(x = "Display name",
##   y = expression(Temperature ~ integration ~ (degree~C %.% s ) ) )

## ggplot(hobo_plots,aes(field_taxon, peak_temp, color = field_taxon))+
##   geom_jitter(width = 0)+
##   facet_grid(.~position)+
##   theme_bw()+ 
##   theme(axis.text.x = element_text(angle = 45,
##                                    hjust = 1, 
##                                    face = "italic"))+
##   labs(x="Display name",
##        y=expression("Peak temperature " ( degree*C)))


## Save RDS data
saveRDS(hobos_wider_2022, file.path(DATA_CACHE_DIR, "hobos_wider_2022"))

########################################################################
# Cleaning the environment
########################################################################
rm("concat_hobo_files", "get_trial_label", "read_hobo_file",
   "hobos_long","hobo_plots", "flam.right","flam.mid","flam.left",
   "flam_right","flam_mid","flam_left","trials_2022","TZ","hobos_separate",
   "hobo_temp_sum", "hobos")

