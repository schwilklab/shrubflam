#!/usr/bin/Rscript --vanilla

## script to read and clean up hobo data 
## for temperature, relative humidity and dew point data 
## measured during drying
## Summer 2021 and summer 2022

library(dplyr)

###################################################################################################
# read weather data from files produced by hobo logger
###################################################################################################
read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=1, header=FALSE)
  names(hobo)[2:5] <- c("date_time", "temp", "rh", "dewpt") # eliminated 
  # the row column and grabbed the next four columns
  hobo <- hobo %>% select(date_time, temp, rh, dewpt) %>%
    return(hobo) 
}

######################################################################################################
# Concatenating hobo files
######################################################################################################

concat_hobo_files <- function(filelist){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  return(r)
}


###########################################################################
# Summer 2021
#############################################################################

hobo_bench_drying <- concat_hobo_files(list.files("../data/year_2021/hobo_data_during_bench_drying",
                                                  full.names = TRUE,recursive = TRUE,
                                                  pattern="bench.drying.*csv"))



# Summary of weather data

bench_drying_summary <- hobo_bench_drying %>%
  select(1:4)%>%
  na.omit()%>%
  summarise(mean.temp=mean(temp),max.temp=max(temp),min.temp=min(temp),
            mean.rh=mean(rh),max.rh=max(rh),min.rh=min(rh),
            mean.dewpt=mean(dewpt),max.dewpt=max(dewpt),min.dewpt=min(dewpt))



######################################################################################################
# Reading all the files at a time, summer 2022
######################################################################################################

hobo_bench_drying_2022 <- concat_hobo_files(list.files("../data/year_2022/sample_drying_hobos_2022",
                                                  full.names = TRUE,recursive = TRUE,
                                                  pattern=".*csv"))

#########################################################################################################
# separating date_time into date and time
##########################################################################################################

hobo_bench_drying_2022 <- hobo_bench_drying_2022 %>%
  mutate(date_time = mdy_hms(date_time), tz = TZ) %>%
  separate(date_time, into = c("date", "time"), sep = " ")

unique(hobo_bench_drying_2022$date)

##########################################################################################################
# Summarising  weather data
##########################################################################################################

bench_drying_summary_2022 <- hobo_bench_drying_2022 %>%
  select(1:5)%>%
  na.omit()%>%
  summarise(mean.temp=mean(temp),max.temp=max(temp),min.temp=min(temp),
            mean.rh=mean(rh),max.rh=max(rh),min.rh=min(rh),
            mean.dewpt=mean(dewpt),max.dewpt=max(dewpt),min.dewpt=min(dewpt))

############################################################################################################
# Cleaning the environment
############################################################################################################

rm("read_hobo_file","concat_hobo_files","hobo_bench_drying", "hobo_bench_drying_2022")




