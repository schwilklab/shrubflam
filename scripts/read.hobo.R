# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

# TODO: read GMT offset or specify timezone explicitly
TZ = "CST6CDT"

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# Read trials data

trials <- read.csv("../data/year_2021/burn_trials.csv",
                   stringsAsFactors = FALSE)

# Get the end trial time
trials <- trials%>%
  rename(trial.time=start.time)%>%
  mutate(start.time=as.POSIXct(paste(burn.date,
                                     trial.time,sep = " "),
                               format="%m/%d/%Y %H:%M:%S",tz=TZ))%>%
  mutate(end.time=start.time+130+flame.dur)%>% 
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

# Get matching for each trial and write 
# separate csv file for each trial

# for(i in c(1:nrow(trials))){
  individual.trials = hobos[hobos$time >= trials$start_time & 
                              hobos$time <= trials$end_time,]
  individual.trials$label = trials$label[i]
  write.csv(individual.trials, paste0("hobo_trial", i, ".csv"), 
            row.names = FALSE)
#}  # Once done, moved all the files to burn_trial_hobo_temps

# concatenate individual trials
concat_all_trials <- function(filelist){
  l <- lapply(filelist, read.csv) # read.csv since reading files as it is
  r <- bind_rows(l)
  return(r)
}
all_hobo_trials <- concat_all_trials(list.files("../data/year_2021/burn_trial_hobo_temps",
                                                recursive = TRUE,
                                                pattern = "hobo_trial"))

# Making all_hobo_trials long
all_hobo_trials_long <- all_hobo_trials%>%
  na.omit()%>%
  gather(key="position",
         value="temperature",-time,-label)

hobo_temp_sum <- all_hobo_trials_long %>% group_by(label, position) %>%
  summarise(dur.100 = sum(temperature > 100),
            peak.temp = max(temperature, na.rm=TRUE),
            peak.time = time[which(peak.temp == temperature)[1]],
            num.NA = sum(is.na(temperature))) %>% ungroup()

# Exploratory plots
trials_plot <- trials %>% select(c("species_id", "label")) %>%
  filter(! species_id %in% c("1070","1078","1085","1094",
                             "1100","2009","QS","UN"))%>%
    right_join(hobo_temp_sum, by ="label")


ggplot(trials_plot,aes(species_id,dur.100,color=species_id))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = "italic"))





