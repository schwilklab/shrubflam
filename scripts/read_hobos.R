# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2021

# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

# TODO: read GMT offset or specify timezone explicitly
TZ = "CST6CDT"

library(dplyr)
library(lubridate)

# Read trials data

trials <- read.csv("../data/year_2021/burn_trials.csv",
                   stringsAsFactors = FALSE)

# Get the end trial time
trials <- trials%>%
  rename(trial.time=start.time)%>%
  mutate(start.time=as.POSIXct(paste(burn.date,
                                     trial.time,sep = " "),
                               format="%m/%d/%Y %H:%M:%S",tz=TZ))%>%
  mutate(end.time=start.time+130+flame.dur)%>% # Two minutes
  # pre-heating and ten seconds ignition period, 130s in total
  mutate(intervals=interval(start.time,end.time))%>%
  mutate(label=paste(sample_id,species_id,sep = "_"))



# Read a single hobo csv file
read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=2, header=FALSE)
  names(hobo)[1:3] <- c("row", "time", "temp")
  hobo <- hobo %>% select(time, temp) %>%
    # we use floor_date() below to round to seconds so we can line up our
    # measurements across HOBOs
    mutate(time = floor_date(mdy_hms(time), "second"))
  return(hobo)
}

concat_hobo_files <- function(filelist, label){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  names(r) <- c("time", label)
  return(r)
}

# We could grab the column name from the file name and do this in one go, but
# this also works.
flam.left <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                     full.names=TRUE, recursive=TRUE,
                                     pattern = "flam.left*.csv"),
                               "flam.left")
 
flam.mid <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                     full.names=TRUE, recursive=TRUE,
                                     pattern = "flam.mid*.csv"),
                               "flam.mid")
flam.right <- concat_hobo_files(list.files("../data/year_2021/burn_trial_hobo_temps",
                                     full.names=TRUE, recursive=TRUE,
                                     pattern = "flam.right*.csv"),
                               "flam.right")
 
hobos <- full_join(flam.left,flam.mid,by = "time") %>% 
  full_join(flam.right,by="time")


hobos <-  hobos%>% mutate_at(c("flam.left", "flam.mid",
                               "flam.right"),list(~ round(.,2)))



get_trial_label <- function(time) {
  matches <- time %within% trials$intervals
  if(! any(matches)) return(NA)
  return(trials$label[which.max(matches)])
}



# assign labels
hobos$label <- unlist(sapply(hobos$time, get_trial_label))





