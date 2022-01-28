# 2021-06-07

# Use epicollect API to fetch data. Provide read_epicollect_data() function
# Note that we use mapping index 1, the "R_mapping" for column names

library(httr)
library(jsonlite) # if needing json format


## Epicollect project values 
cID <-"2474"      # client ID for the "shrubflam_r_code" app created via the epicollect web interface
secret <- "s5zC1pWGsY1t29X52sgmaqRF9RJUESiqCDLfcd6C" # client secret
proj.slug <- "ecolab-flam" # project slug
form.ref <- "31082cb5777546f389bffd8ed5c3cb02_609feac467aaa" # form reference
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
  url.form <- paste("https://five.epicollect.net/api/export/entries/",
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

# code below only for branches, not used for shrubflam project

## # url.branch<- paste("https://five.epicollect.net/api/export/entries/", proj.slug, "?map_index=0&branch_ref=", branch.ref, "&format=json&per_page=1000", sep= "") ## if using json; pushing max number of records from default 50 to 1000
## url.branch<- paste("https://five.epicollect.net/api/export/entries/", proj.slug, "?map_index=0&branch_ref=", branch.ref, "&format=csv&headers=true", sep= "")

## res2<- GET(url.branch, add_headers("Authorization" = paste("Bearer", token)))
## http_status(res2)
## ct2<- read.csv(res2$url)
## # ct2<- fromJSON(rawToChar(content(res2))) ## if using json
## str(ct2)

