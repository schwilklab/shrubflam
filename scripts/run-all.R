#!/usr/bin/Rscript --vanilla

# run-all.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

## DWS: Note: 2021 data is not read right now for the reasons explained below:

## This can't be run yet ebcause it overwrites the data file names used by
## read_data.R for 2022 data. So this needs to be fixed if the 2021 data is
## used. Same applies to 2021 hobo data

# source("./scripts/read_data_2021.R")
# source("./scripts/read_hobos_2021.R") 

## DWS: The read data and read hobo data scripts 2021 have a lot of repeated code.


## Code below for 2022 data all runs cleanly

# read, clean and merged all the morphological and flammability traits data.
# Produces "alldata_2022"
source("./scripts/read_data.R")

# script that reads the thermocouple data logger data during burning. Produces
# "hobos_wider_2022"

DATA_CACHE_DIR <- "./results/tempdata"
source("./scripts/read_hobos.R")  ## Do this once
## After running the above once, just read saved data:
# hobos_wider_2022 <- readRDS(file.path(DATA_CACHE_DIR, "hobos_wider_2022"))

# run PCAs requires alldata_2022 and hobos_wider_2022 to exist:
source("./scripts/flam_pca.R") 
source("./scripts/analysis.R")

# run alldata from the 2021
#source("./scripts/read_data_2021.R")

# read hobos data for 2021
#source("./scripts/hobo_flam_pca_2021.R")

# read herbivore analysis script

#source("./scripts/herbivore_analysis.R")

# figures
source("./scripts/ms_figures.R")

# Supplimentary info
source("./scripts/supplementary_analysis_2022.R")
