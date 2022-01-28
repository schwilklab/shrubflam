# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2021

# Read the thermocouple data, in multiple csv files saved from the HOBO
# software.

# TODO: read GMT offset or specify timezone explicitly
TZ = "CST6CDT"

library(dplyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)

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
    mutate(time = floor_date(mdy_hms(time, tz=TZ), "second"))
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
 
# Get them all in a single data frame

hobos <- full_join(flam.left,flam.mid,by = "time") %>% 
  full_join(flam.right,by="time")




get_trial_label <- function(time) {
  matches <- time %within% trials$intervals
  if(! any(matches)) return(NA)
  return(trials$label[which.max(matches)])
}



# assign labels

hobos$label <- unlist(sapply(hobos$time, get_trial_label))

# Get the hobos as long format

hobos_long <- hobos%>%
  gather(key = "position",
         value = "temperature",-time,-label)

# Summarise the hobo data

hobo_temp_sum <- hobos_long %>% group_by(label, position) %>%
  summarise(dur.100 = sum(temperature > 100),
            degsec.100= sum(temperature[temperature >100]),
            peak.temp = max(temperature, na.rm=TRUE),
            peak.time = time[which(peak.temp == temperature)[1]],
            num.NA = sum(is.na(temperature))) %>% ungroup()

# Merge the trials data with hobo summary data and alldata

plot_trials <- trials %>% select(c("sample_id","species_id","label")) %>% 
     right_join(hobo_temp_sum, by ="label")%>%
  left_join(select(alldata,"sample_id","species_id","display_name"),
            by=c("sample_id","species_id"))


# Plot for the summary

ggplot(plot_trials,aes(display_name,dur.100,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = "italic"))+
  labs(x="Display name",
       y=expression(paste("Duration over ",100^degree*C, " in (s)")))+
  
       



ggplot(plot_trials,aes(display_name,degsec.100,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   face = "italic"))+
  labs(x="Display name")

ggplot(plot_trials,aes(display_name,peak.temp,color=display_name))+
  geom_jitter(width = 0)+
  facet_grid(.~position)+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   face = "italic"))+
  labs(x="Display name",
       y=expression("Peak temperature " ( degree*C)))

# Clean the environment

rm("concat_hobo_files", "get_trial_label", "read_hobo_file",
"hobos_long","flam.right","flam.mid","flam.left")

