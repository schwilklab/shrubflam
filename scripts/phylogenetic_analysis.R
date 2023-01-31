#!/usr/bin/Rscript --vanilla
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# 2022

# This script is for doing phylogenetic 
# mixed effect model, possibility to test
# whether flammability is phylogenetically conserved or not in this 
# ecosystem!!

#install.packages("ape")

library(ape)

#devtools::install_github("jinyizju/V.PhyloMaker")

library("V.PhyloMaker")

source("./read_data.R") # The script that used to clean the dataset of 2021

##########################################################################################
#########################################################################################
species_list <- read.csv("../data/year_2021/species_table.csv")

species_list <- species_list %>%
  mutate(species = ifelse(species == "Forestiera reticulata", 
                          "Forestiera pubescens", species)) %>%
  mutate(species = ifelse(species == "Juniperus pinchottii", 
                          "Juniperus pinchotii", species)) %>%
  mutate(species = ifelse(species == "Quercus fusiformis",
                          "Quercus virginiana", species)) %>%
  mutate(species = ifelse(species == "Acacia berlandieri" ,
                          "Senegalia berlandieri" , species)) %>%
  filter(species %in% herbivore_data$species) %>%
  mutate(species = ifelse(species ==  "Ziziphus obtusifolia",
                         "Sarcomphalus obtusifolia", species))

unique(species_list$species)

species_list <- tidyr::separate(species_list, species, into = c("genus","species"),
                                sep = " ")


species_list <- species_list %>%
  dplyr::select(1:3)

#####################################################################################
# Creating two optional columns
####################################################################################

species_list[, "species_relative"] <- NA

species_list[, "genus_relative"] <- NA


write_csv(species_list, "species_list.csv")


species_list <- read.csv("species_list.csv")

View(species_list)


############################################################################
# Getting the nodes.info.1.rad into local environment from global environment
# Getting the GBOTB.extended.rda into Local environment
############################################################################



nodes.info.1 <- get("nodes.info.1")



nodes.info.1.TPL <- nodes.info.1

GBOTB.extended.TPL <- get("GBOTB.extended")

#####################################################################################
# generate a phylogeny for the sample species list
# source: https://www.sciencedirect.com/science/article/pii/S2468265922000580
#####################################################################################
tree <- phylo.maker(sp.list = species_list, nodes = nodes.info.1.TPL, tree = GBOTB.extended.TPL, scenarios = "S3")

write.tree(tree$scenario.3, "sample.tre")

# Working on how to use this tree in phylogenetic mixed effect model!!!!
# Currently, no idea!!!


