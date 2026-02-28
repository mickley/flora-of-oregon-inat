# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
library(googlesheets4) # Needed to access google sheet
source("iNat-API.R") # Our iNaturalist API wrapper function

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- ""
