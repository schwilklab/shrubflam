## script to read and clean up hobo data 
## for temperature, relative humidity and dew point data 
## measured during drying

## read weather data from files produced by hobo logger
read_hobo_file <- function(filename) {
  hobo <- read.csv(filename, skip=1, header=FALSE)
  names(hobo)[2:5] <- c("datet", "temp", "rh", "dewpt") # eliminated 
  # the row column and grabbed the next four columns
  hobo <- hobo %>% select(datet, temp, rh, dewpt) %>%
  return(hobo) 
}



concat_hobo_files <- function(filelist){
  l <- lapply(filelist, read_hobo_file)
  r <- bind_rows(l)
  return(r)
}

hobo_bench_drying <- concat_hobo_files(list.files("..data/
                                                  year_2021/
                                                  hobo_data_during_bench_drying",
                                                  recursive = TRUE,full.names = TRUE,
                                              pattern="bench.drying*.csv"))

hobo_bench_drying # A tibble: 0 x 0!!!! Couldn't figure it out

# Summary of weather data
bench_dying_summary <- hobo_bench_drying%>%
  select(1:4)%>%
  na.omit()%>%
  summarise(mean.temp=mean(temp),max.temp=max(temp),min.temp=min(temp),
            mean.rh=mean(rh),max.rh=max(rh),min.rh=min(rh),
            mean.dewpt=mean(dewpt),max.dewpt=max(dewpt),min.dewpt=min(dewpt))


