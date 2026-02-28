# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
library(googlesheets4) # Needed to access google sheet
source("iNat-API.R") # Our iNaturalist API wrapper function

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- "eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjo2NjUwLCJleHAiOjE3NjI1NDQwMTR9.FdwFk0EKVYNDsAUc3TbkpALoPezeGqBElktIh5leSDndGPKH6v2Xc7iXnh6MOIshuNqM1l-OFpN7zLWDvoyYPQ"


##### Get project members
members <- data.frame()
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "projects/168279/members", api_token, 
                   per_page = 100, page = i, fields="(created_at:!t,user:(id:!t,login:!t,name:!t))")
    members <- rbind(members, out$results)
    
    if(ceiling(out$data$total_results/100) == i) {
        break
    } else {
        i <- i+1
    }
}

members <- members %>% 
    select(created_at, user.id, user.login, user.name)

##### Get top observers

# Get top 500 observers for the project
fields = toRISON(list("observation_count", "species_count", 
    "user" = list("id", "login", "name", "roles")))

out <- iNatAPI("GET", "v2", "observations/observers", api_token, 
    captive = "false", project_id = 168279, page = 1, fields = fields)

observers <- out$results %>%
    select(user.id, user.login, user.name, user.roles, observation_count, 
    species_count)

observers %>% head

##### Get top identifiers

fields = toRISON(list("count", "user" = list("id", "login", "name", "roles")))

out <- iNatAPI("GET", "v2", "observations/identifiers", api_token, 
               project_id = 168279, page = 1, fields = fields)

identifiers <- out$results %>% 
    
    select(user.id, user.login, user.name, user.roles, 
        identification_count = count)
    
identifiers %>% head


##### Combine user lists

# Combine observers and identifiers
users <- observers %>% filter(observation_count > 500) %>%
    full_join(identifiers %>% filter(identification_count > 1200)) %>%
    select(user.id, user.login, user.name)

users

# Combine users, identifiers, and project members
everyone <- members %>% select(user.id, user.login, user.name) %>% 
    
    full_join(users %>% select(user.id, user.login, user.name))

everyone

##### Tracking

gsheet <- 'https://docs.google.com/spreadsheets/d/1blzZVI8RffUFvUVB3ENNJS3inmBsxXh6pdXCQizSY1Q/edit?usp=sharing'

# Get google sheet data and rename columns
outreach <- read_sheet(gsheet, sheet="primary") %>% 
    select(user.id, user.login = `iNat Username`, 
        user.name = `Name (if specified)`, invite.date = `Invited Date`, Joined, 
        welcome.date = `Welcome Letter Date`, coords = `Coordinates shared`, 
        Notes)

outreach

##### Get new members

new.members <- members %>% 
    select(-created_at) %>%
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    # Make a first name column
    mutate(first.name = gsub("^(.*?)\\s.*", "\\1", user.name)) %>% 
    
    # Pull in member outreach info and filter to those who haven't joined
    left_join(outreach %>% select(user.id, Joined, invite.date, welcome.date), by = "user.id") %>%
    filter(is.na(Joined)) %>% 

    # Add join data
    mutate(Joined = "X", 
       invite.date = if_else(is.na(invite.date), Sys.Date(), as.Date(invite.date)),
       welcome.date = Sys.Date())

new.members   

#### Welcome new members

# Send messages to users
send_message(new.members, "Welcome to Flora of Oregon: Vascular Plants", 
    welcome, 1, nrow(new.members))

# tmp <- filter(new.members, user.login %in% c('zackadoodle', 'volcanoesandbugs', 'christinags'))
# send_message(tmp, "Welcome to Flora of Oregon: Vascular Plants", 
#              welcome, 1, nrow(tmp))

#### Get invitees

invitees <- users %>% 
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    # Make a first name column
    mutate(first.name = gsub("^(.*?)\\s.*", "\\1", user.name)) %>% 
    
    # Pull in member outreach info and filter to those who haven't been invited
    left_join(outreach %>% select(user.id, invite.date), by = "user.id") %>%
    filter(is.na(invite.date)) %>% 
    
    # Filter out names on the new members list who have just joined
    filter(!(user.id %in% new.members$user.id)) %>%
    
    # Add invite data
    mutate(invite.date = Sys.Date())

invitees

# specific_users <- get_users(c("kupaoa"))
# specific_users$user.name <- "Gerry Carr"
# specific_users$first.name <- "Gerry"
# specific_users$invite.date <- Sys.Date()
# invitees <- invitees %>% rbind(specific_users)

# Send messages to users
send_message(invitees, "Invitation to join the Flora of Oregon project", 
    invitation, 1, nrow(invitees))

#### Update the Google Sheet
outreach %>%
    
    # Update the username and name if we can
    rows_update(everyone %>% right_join(outreach %>% select(user.id)), 
                by = "user.id") %>%
    
    # Update the invitation data
    rows_upsert(invitees %>% select(-first.name)) %>%

    # Update the Joined data
    rows_upsert(new.members %>% select(-first.name)) %>%
    
    # Reorder columns
    select(user.id, user.name, user.login, invite.date, Joined, welcome.date, 
           coords, Notes) %>% 
    
    # Write data
    range_write(gsheet, data = ., sheet="primary", range = "A2", 
        col_names = FALSE, reformat = FALSE)


##### Delete old messages

messages <- data.frame()
for(i in 1:4){
    
    out <- iNatAPI("GET", "v1", "messages", box = "any", threads = "true", page = i, 
        api_token)
    
    messages <- rbind(messages, out$data$results, make.row.names = FALSE)
    
}

messages

#out$data$results

subjects <- c("Invitation to join the Flora of Oregon project", 
    "Welcome to Flora of Oregon: Vascular Plants")


delete <- out$data$results %>% 
    select(id, thread_id, thread_messages_count, subject, read_at, created_at, 
           to_user) %>%
    do.call(data.frame, .) %>%
    select(id, thread_id, thread_messages_count, subject, created_at, 
           to_user.id, to_user.login, to_user.name) %>%
    filter(subject %in% subjects, thread_messages_count == 1) %>%
    filter(created_at < Sys.Date() - 10) 

delete

delete_message(delete)



##########################
# Invited Non-members

observers %>% 
    full_join(identifiers) %>%
    select(user.id, user.login, user.name, observation_count) %>%
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>% 
    
    right_join(outreach) %>% 
    filter(is.na(Joined)) %>%
    
    select(-Joined, -welcome.date, -coords, -Notes) %>%
    filter(observation_count > 500) %>%
    arrange(desc(observation_count)) %>% 
    
    summarize(obs = sum(observation_count))
    

##########################
# Total obs in project (according to google sheet)

members %>% 
    left_join(observers) %>% 
    left_join(outreach) %>% 
    
    group_by(coords) %>% 
    summarize(obs = sum(observation_count, na.rm = T)) %>% 
    summarize(coords, obs, total_obs = sum(obs))


##########################
    
##########################
# Not sharing coords (according to google sheet)

outreach %>% left_join(observers) %>% 
    filter(coords == "No", observation_count > 0) %>% 
    select(user.id, user.login, user.name, observation_count) %>% 
    arrange(desc(observation_count)) %>% 
    data.frame()

##########################
# Analysis of RTE species and whether we have access to coords via the project

fields = toRISON(list("id", "taxon_geoprivacy", "obscured", "geoprivacy", 
    "positional_accuracy", "taxon" = list("name", "threatened"), 
    "user" = list("login"), "private_location", "viewer_trusted_by_observer")) 
fields

# Get all the obscured observations, splitting up by observation ID windows 
# to get under the max 10000 results per API query limitation
id.breaks <- format(seq(from = 0, to = 360000000, by = 40000000), 
    scientific = FALSE, trim = TRUE)

rte.obs <- data.frame()
results = 0
for(i in 1:(length(id.breaks) - 1)){
    
    cat(paste0("\n", "ID Start: ", id.breaks[i]), "\n")
    
    j <- 1
    while(j > 0) {
        
        # API query id_above or id_below
        out <- iNatAPI("GET", "v2", "observations", api_token, 
            id_above = id.breaks[i], id_below = id.breaks[i+1], 
            project_id = 168279, taxon_geoprivacy = "obscured", per_page = 200, 
            page = j, fields = fields)
        
        # Add to obscured.obs dataframe
        rte.obs <- out$results %>% 
            
        # Make sure private_location is present in the dataset output
        mutate(private_location = ifelse("private_location" %in% names(.), 
            private_location, NA)) %>% 
            
        #Prepend the existing obscured.obs dataframe
        rbind(rte.obs, .)
        
        # Sleep for a second
        Sys.sleep(1) 
        
        # Status update
        cat(paste0((floor(results/200) + j) * 200, "..."))
        
        # Go to the next page, unless we're finished
        if(ceiling(out$data$total_results/200) == j) {
            break
        } else {
            j <- j+1
        }
    }
    cat(paste0("\nResults Total: ", out$data$total_results, "\n"))
    results <- results + out$data$total_results
}


# All obscured observations
#obscured.obs
#str(obscured.obs)

# All RTE obs for FOVP
rte.obs <- obscured.obs %>% filter(taxon_geoprivacy == "obscured")
str(rte.obs)

# All non-RTE obscured observations
obscured.obs %>% pull(taxon_geoprivacy) %>% unique
obscured.obs %>% pull(geoprivacy) %>% unique
nonrte.obs <- obscured.obs %>% filter(taxon_geoprivacy != "obscured")
str(nonrte.obs)

rte.obs <- data.frame()
j <- 1
while(j > 0) {
    
    # API query id_above or id_below
    out <- iNatAPI("GET", "v2", "observations", api_token, 
       project_id = 168279, taxon_geoprivacy = "obscured", per_page = 200, 
       page = j, fields = fields)
    
    # Add to obscured.obs dataframe
    rte.obs <- out$results %>% 
        
        # Make sure private_location is present in the dataset output
        #mutate(private_location = ifelse("private_location" %in% names(.), 
        #    private_location, "")) %>% 
        
        #Prepend the existing obscured.obs dataframe
        rbind(rte.obs, .)
    
    # Sleep for a second
    #Sys.sleep(1) 
    
    # Status update
    cat(paste0(j * 200, "..."))
    
    # Go to the next page, unless we're finished
    if(ceiling(out$data$total_results/200) == j) {
        break
    } else {
        j <- j+1
    }
}

# Filters
rte.obs %>% filter(taxon.name == "Darlingtonia californica")
rte.obs %>% filter(user.login == "elly22")

rte.users %>% 
    distinct(user.login, .keep_all = TRUE) %>%
    group_by(membership) %>%
    summarize(n = n())

1# Categorize by user
rte.users <- rte.obs %>%
    mutate(shared = ifelse(!is.na(private_location), "shared", "withheld")) %>%
    left_join(members %>% select(created_at, user.id)) %>%
    mutate(membership = ifelse(is.na(created_at), "non-member", "member" )) %>%
    group_by(membership, user.login, shared) %>%
    summarize(n = n()) %>% data.frame()

# Overall Summary
rte.users %>%
    arrange(membership, user.login, desc(shared)) %>%
    group_by(membership, shared) %>% 
    summarize(n = sum(n))

# Members who withhold, sorted by # of RTE obs
rte.users %>%
    filter(membership == "member", shared == "withheld") %>% 
    arrange(desc(n))

# Non members, sorted by # of RTE obs
rte.users %>%
    filter(membership == "non-member") %>% 
    arrange(desc(n))

# Non members, who are sharing coordinates (other projects of mine)
rte.users %>%
    filter(membership == "non-member", shared == "shared")


    