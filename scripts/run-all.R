#!/usr/bin/Rscript --vanilla

# run-all.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

# read, clean and merged all the morphological and flammability traits data.
# Produces "alldata_2022"
source("./scripts/read_data.R")

# script that reads the thermocouple data logger data during burning. Produces
# "hobos_wider_2022"

DATA_CACHE_DIR <- "./results/tempdata"
# source("./scripts/read_hobos.R")  ## Do this once
#after running the above once, just read saved data:
hobos_wider_2022 <- readRDS(file.path(DATA_CACHE_DIR, "hobos_wider_2022"))

# run PCAs requires alldata_2022 and hobos_wider_2022 to exist:
source("./scripts/flam_pca.R") 
source("./scripts/analysis.R")


# figures

