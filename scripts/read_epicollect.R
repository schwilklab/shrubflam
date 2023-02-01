#!/usr/bin/Rscript --vanilla

# Shrub Flammability
# Summer 2022
# 06_10_2022

# Use epicollect API to fetch data. Provide read_epicollect_data() function
# Note that we use mapping index 1, the "R_mapping" for column names

library(httr)
library(jsonlite) # if needing json format


## Epicollect project values 
cID <-"3226"      # client ID for the "shrubflam_r_code" app created via the epicollect web interface
secret <- "qjSsDpeiRXbaUnc8tPnlh9qBCNcMbLH7ddybdESx" # client secret
proj.slug <- "shrub-flam-2022" # project slug
form.ref <- "b4ebd423cee542e0886a24c62f5b9c07" # form reference
#branch.ref<- "YourFromRef+BranchExtension" # branch reference

check_connection_result <- function(res) {
  status <- http_status(res)
  if(status$category != "Success") warning(paste("Unable to make https connection\n", status$message))
}

read_epicollect_data <- function() {
  res <- POST("https://five.epicollect.net/api/oauth/token",
              body = list(grant_type = "client_credentials",
                          client_id = cID,
                          client_secret = secret))
  check_connection_result(res)
  
  token <- content(res)$access_token
  
  # url.form<- paste("https://five.epicollect.net/api/export/entries/", proj.slug, "?map_index=1&form_ref=", form.ref, "&format=json", sep= "") ## if using json
  url.form <- paste("https://five.epicollect.net/project/shrub-flam-2022",
                    proj.slug, "?map_index=1&form_ref=",
                    form.ref, "&format=csv&headers=true", sep= "")
  
  res1 <- GET(url.form, add_headers("Authorization" = paste("Bearer", token)))
  check_connection_result(res1)
  
  # ct1<- fromJSON(rawToChar(content(res1))) ## if using json
  csv_result <- read.csv(res1$url)
  #str(ct1)
  return(csv_result)
}

View(read_epicollect_data())
