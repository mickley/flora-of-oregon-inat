# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
source("iNat-API.R") # Our iNaturalist API wrapper function

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- "eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjo2NjUwLCJleHAiOjE3MDU2MjkxNTF9.J0WwXtbkaK9YNLXD_i5oRphZDlnOQi_GvLYvkL7cXRuC5jtynB1PbCP-3h2Rizhsjos0aFt92w692AFYvYE1MQ"

##### iNat API wrapper syntax #####

# iNatAPI(HTTP_Request_Type, API_Version, API_Endpoint, API_auth_token, [body], [args...])
# Returns an httr::response object
# See documentation:
# - https://api.inaturalist.org/v1/docs/
# - https://api.inaturalist.org/v2/docs/

##################################################


# First test: see if you are logged in
out <- iNatAPI("GET", "v1", "users/me", api_token)
content(out) # Converts the JSON output to an R object
out$data
out$results

# Get Flora of Oregon project details
out <- iNatAPI("GET", "v1", "projects/168279", api_token)

# Get a list of project members
project_users <- out$data$results$user_ids

# Get project members
out <- iNatAPI("GET", "v1", "projects/168279/members", api_token, 
    per_page = 200)

out$results %>% select(created_at, role, user.id, user.login, user.name)

# Dataframe of members
members <- out$results %>% 
    select(role, user.id, user.login, user.name, join_date = created_at)

# Send a message to a user
body <- toJSON(list(message = list(to_user_id = 6650, 
    subject = "Test", body = "Test Message")), auto_unbox = TRUE)
out <- iNatAPI("POST", "v1", "messages", api_token, body)
content(out)
    
# Delete messages by authenticated user in thread ID
out <- iNatAPI("DELETE", "v1", "messages/4113294", api_token)
out

# Get top 500 observers for the project
out <- iNatAPI("GET", "v1", "observations/observers", api_token, 
    captive = "false", project_id = 168279)
observers <- out$results %>%
    select(user_id, observation_count, species_count, user.login, user.name)

observers %>% filter(observation_count > 500) %>% pull(observation_count) %>% sum

# Top observers who haven't joined
observers %>% 
    left_join(members %>% select(user.login, join_date)) %>% 
    filter(is.na(join_date)) %>% 
    write.csv("observers.csv")

# Get top 500 identifiers for the project
fields = toRISON(list("count", "user" = list("login", "name")))
out <- iNatAPI("GET", "v2", "observations/identifiers", api_token, 
    project_id = 168279, fields = fields)
out$results

# Get top identifiers with v1 API
out <- iNatAPI("GET", "v1", "identifications/identifiers", api_token, 
    place_id = 10, observation_taxon_id = 211194, own_observation = "false", 
    is_change = "false", per_page=100)
out$data$results %>% do.call(data.frame, .) %>%
    select(user_id, count, user.login, user.name)

# Get top identifiers with v2 API
out <- iNatAPI("GET", "v2", "identifications/identifiers", api_token, 
    place_id = 10, observation_taxon_id = 211194, own_observation = "false", 
    is_change = "false", per_page=100,
    fields = "(count:!t,user:(login:!t,name:!t))")
out$data$results %>% do.call(data.frame, .)


# Get project members
out <- iNatAPI("GET", "v1", "observations", api_token, project_id = 168279,
    per_page = 2, geoprivacy = "obscured", user_login = "sedgequeen")

(out$data$results$non_traditional_projects[[1]])$project_user.prefers_curator_coordinate_access_for


private_geojson.type
private_geojson.coordinates 
obscured = true
geoprivacy = obscured
private_location

test <- c("jessicalodwick", "mickley")

get_users("jessicalodwick")


fields = toRISON(list("count", "taxon" = list("id"), 
    "default_photo" = list("url"), "is_active", "name", 
    "preferred_common_name", "rank", "rank_level"))

# Fields doesn't work with api v2, but order works with v1, though not documented
out <- iNatAPI("GET", "v1", "observations/species_counts", api_token, 
    project_id = 168279, order = "asc", hrank = "species", 
    per_page=500, page = 5)

out$results %>% pull(count)


#Get the identifiers not in the top 500 observers

out <- iNatAPI("GET", "v1", "observations/observers", api_token, 
               captive = "false", project_id = 168279)
observers <- out$results %>%
    select(user_id, observation_count, species_count, user.login, user.name)


fields = toRISON(list("count", "user" = list("login", "name")))
out <- iNatAPI("GET", "v2", "observations/identifiers", api_token, 
               project_id = 168279, fields = fields)
identifiers <- out$results

identifiers %>% 
    left_join(observers) %>%
    filter(is.na(observation_count) | observation_count < 500) %>% 
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    select(-user_id, -species_count) %>%
    write.csv("identifiers.csv")


######### Tracking observations in project #########

# Get project members
members <- data.frame()
for (i in 1:2) {
    out <- iNatAPI("GET", "v2", "projects/168279/members", api_token, 
                   per_page = 100, page = i, fields="all")
    members <- rbind(members, out$results)
}

# Dataframe of members
members <- members %>% 
    select(role, user.id, user.login, user.name, join_date = created_at)

members

# Get top observers for the project
fields = toRISON(list("observation_count", "species_count", 
                      "user" = list("id", "login", "name", "roles")))
out <- iNatAPI("GET", "v2", "observations/observers", api_token, 
               captive = "false", project_id = 168279, fields = fields, per_page = 500)
observers <- out$results
observers 

# Total observations by project members
members %>%
    left_join(observers) %>% 
    filter(!is.na(observation_count)) %>% 
    pull(observation_count) %>%
    sum()

# Total observations by non-project members
observers %>% 
    left_join(members) %>% 
    filter(is.na(join_date)) %>%
    select(user.name, user.login, observation_count) %>%
    filter(observation_count > 500) %>%
    pull(observation_count) %>%
    sum()



