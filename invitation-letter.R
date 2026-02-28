# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
library(googlesheets4) # Needed to access google sheet
source("iNat-API.R") # Our iNaturalist API wrapper function

# Add your name here to sign the letter, and to cross-reference the google sheet
your_name <- "James Mickley"

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- "eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjo2NjUwLCJleHAiOjE3MTAyNTg2NDd9.-QXX0feXGUD_HhOybMqPZFyvZNf4D-szHkre21Wp7xFVRusuMGqvLU0Xfe7JqWxDFEWRDhlrSoLEsscn6ktpzA"

##### iNat API wrapper syntax #####

# iNatAPI(HTTP_Request_Type, API_Version, API_Endpoint, API_auth_token, [body], [args...])
# Returns an httr::response object
# See documentation:
# - https://api.inaturalist.org/v1/docs/
# - https://api.inaturalist.org/v2/docs/

# Add the subject
subject <- "Invitation to join the Flora of Oregon project"

invitation <- paste0("As a prominent observer or identifier of Oregon plants on iNaturalist, we are inviting you to join us at the **[Flora of Oregon: Vascular Plants](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants)** iNaturalist project, a collaboration of the [Oregon State University Herbarium](https://bpp.oregonstate.edu/herbarium), [OregonFlora](https://oregonflora.org/),and the [Native Plant Society of Oregon](https://www.npsoregon.org/).  By joining the project you will become a member of the community of naturalists advancing botanical knowledge in Oregon, and we hope that you will learn with us. Your observations and identifications are automatically included, and become research data that will help us discover, track, and conserve plants in Oregon. Your participation is greatly appreciated and a contribution to our knowledge of our state flora!

To join the project, visit the [project page](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants), and click on “Join” in the upper right corner.

* As you join the project, please select \"Yes\" in response to the question \"Trust this project with hidden coordinates?\" (see image below). You can choose to share coordinates for any of your observations or only for threatened species. Your observations are already automatically added to the project, but trusting our project admins with coordinates will allow more accurate mapping of species distribution for scientific study and to facilitate conservation of sensitive species.
* We are expecting to start regular journal posts, so we hope that you also opt to receive project updates.
* You can find more detailed information and instructions on our [about page](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants?tab=about).

![Screenshot of the \"trust this project with hidden coordinates\" menu](https://lh3.googleusercontent.com/pw/ABLVV866wY9BXEycZ-afF_ShcO0GW3ZNiuRAioVG3W1InEQ9MVm_BVXQi1JNgtK-gYgJ60b0NcastdN7i2L3bERJ4FgYJCBQOlFjrEhpe1o6KpM9IyLTF9SEDBMqaO3McDjsAvo2-822t9CbjQIQMNq4v5Fd=w795)

Sincerely,
	", your_name, ", for

* **The iNat Flora of Oregon Team (@mickley, @twainwright, @wisel, @ribes2018, and members of the Native Plant Society of Oregon and OregonFlora)**")

##################################################


##################################################

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
fields
# Get top identifiers for the project
fields = toRISON(list("count", "user" = list("login", "name", "roles")))
out <- iNatAPI("GET", "v2", "observations/identifiers", api_token, 
               project_id = 168279, fields = fields)
identifiers <- out$results

users <- identifiers %>% 
    full_join(observers) %>%
    select(user.id, user.login, user.roles)
users

# Get google sheet
invited <- read_sheet('https://docs.google.com/spreadsheets/d/1blzZVI8RffUFvUVB3ENNJS3inmBsxXh6pdXCQizSY1Q/edit?usp=sharing')
invited

# People who need to be contacted
uncontacted <- invited %>%
    
    # Filter out users who have already been invited
    select(user.name = `Name (if specified)`, user.login = `iNat Username`, 
        invite = `Invited Date`, Contacter) %>% 
    filter(invite == "NULL", Contacter == "James Mickley") %>%
    
    # Filter out users who already joined the project
    left_join(members %>% select(user.login, join_date)) %>% 
    filter(is.na(join_date)) %>%
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    # Add user IDs
    left_join(users) %>%
    
    # Select columns
    select(user.id, user.login, user.name) %>%
    
    # Make a first name column
    mutate(first.name = gsub("^(.*?)\\s.*", "\\1", user.name))

uncontacted

# Send messages to users
send_message(uncontacted, subject, invitation, 1, 10)


specific_users <- get_users(c("donmansfield", "kentonchambers"))
send_message(specific_users, subject, invitation, 1, nrow(specific_users))
"", , "cpfeuillet", "lesliedavis"
"tbreezy10"
get_users(c("lesliedavis"))





# Get top 500 observers for the project
out <- iNatAPI("GET", "v1", "observations/observers", api_token, 
    captive = "false", project_id = 168279)
observers <- out$results %>%
    select(user_id, observation_count, species_count, user.login, user.name)
    

admins <- c("schallle", "mickley", "ribes2018", "judisanders", "lindahardison", "stephen_meyers", "twainwright")

# Top observers who haven't joined
observers %>% 
    left_join(invited %>% select(user.login=`iNat Username`, 
        invite=`Invited Date`, Contacter)) %>% 
    filter(is.na(Contacter), !(user.login %in% admins)) %>% 
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    select(user_id, observation_count, user.login, user.name) %>%
    write.csv("observers.csv")


# Curators
members <- data.frame()
for (i in 1:2) {
    out <- iNatAPI("GET", "v2", "projects/168279/members", api_token, 
                   per_page = 100, page = i, fields="all")
    members <- rbind(members, out$results)
}

members %>% 
    select(user.name, user.login) %>% 
    left_join(users) %>% 
    filter(user.roles == "curator")


identifiers %>% 
    left_join(observers) %>% 
    select(user.login, user.name, user.roles, identifications = count, observation_count) %>%
    filter(user.roles == "curator", observation_count > 300 | identifications > 1500)
