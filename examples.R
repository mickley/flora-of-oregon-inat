# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
source("iNat-API.R") # Our iNaturalist API wrapper function

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- ""

##################################################


# First test: see if you are logged in
out <- iNatQuery("GET", "v1", "users/me", api_token)
content(out) # Converts the JSON output to an R object

# Get Flora of Oregon project details
out <- iNatQuery("GET", "v1", "projects/168279", api_token)

# Get a list of project members
project_users <- out$data$results$user_ids

# Get project members
out <- iNatQuery("GET", "v1", "projects/168279/members", api_token, 
    per_page = 200)
out$data$results
# Dataframe of members
do.call(data.frame, out$data$results) %>% 
    select(role, user.id, user.login, user.name, join_date = created_at)

# Send a message to a user
body <- toJSON(list(message = list(to_user_id = 6650, 
    subject = "Test", body = "Test Message")), auto_unbox = TRUE)
out <- iNatQuery("POST", "v1", "messages", api_token, body)
content(out)
    
# Delete messages by authenticated user in thread ID
out <- iNatQuery("DELETE", "v1", "messages/4113294", api_token)
out

# Get top 500 observers for the project
out <- iNatQuery("GET", "v1", "observations/observers", api_token, 
    captive = "false", project_id = 168279)
out$data$results %>% do.call(data.frame, .) %>% 
    select(user_id, observation_count, species_count, user.login, user.name)

# Get top identifiers with v1 API
out <- iNatQuery("GET", "v1", "identifications/identifiers", api_token, 
    place_id = 10, observation_taxon_id = 211194, own_observation = "false", 
    is_change = "false", per_page=100)
out$data$results %>% do.call(data.frame, .) %>%
    select(user_id, count, user.login, user.name)

# Get top identifiers with v2 API
out <- iNatQuery("GET", "v2", "identifications/identifiers", api_token, 
    place_id = 10, observation_taxon_id = 211194, own_observation = "false", 
    is_change = "false", per_page=100,
    fields = "(count:!t,user:(login:!t,name:!t))")
out$data$results %>% do.call(data.frame, .)
