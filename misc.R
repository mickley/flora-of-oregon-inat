


# OregonBee Atlas Crosswalk


# Get project members
members <- data.frame()
for (i in 1:4) {
    out <- iNatAPI("GET", "v2", "projects/168279/members", api_token, 
                   per_page = 100, page = i, fields="all")
    members <- rbind(members, out$results)
}

members <- members %>% 
    select(user.name, user.login, created_at)

members

beepro <- data.frame()
for (i in 1:5) {
    out <- iNatAPI("GET", "v2", "projects/18521/members", api_token, 
                   per_page = 100, page = i, fields="all")
    beepro <- rbind(beepro, out$results)
}
beepro <- beepro %>% 
    select(bee=created_at, user.login)
beepro


beeam <- data.frame()
for (i in 1:4) {
    out <- iNatAPI("GET", "v2", "projects/13014/members", api_token, 
                   per_page = 100, page = i, fields="all")
    beeam <- rbind(beeam, out$results)
}
beeam <- beeam %>% 
    select(bee=created_at, user.login)
beeam

# Oregon Bee Atlas (Plant Images/SampleID)
members %>% 
    left_join(beepro) %>% 
    filter(!is.na(bee)) %>%
    select(-bee, -created_at)

# Oregon Bee Atlas (Bee Pictures)
members %>% 
    left_join(beeam) %>% 
    filter(!is.na(bee)) %>%
    select(-bee, -created_at)

# Both
members %>% 
    left_join(beeam) %>% 
    filter(!is.na(bee)) %>%
    select(-bee, -created_at) %>%
    left_join(beepro) %>% 
    filter(!is.na(bee)) %>%
    select(-bee)


out <- iNatAPI("GET", "v2", "observations/observers", api_token, 
    geoprivacy="obscured,private", project_id=168279, fields="all")


out$results %>% 
    select(user.name, user.login, observation_count, species_count) %>%
    left_join(members) #%>%
    #filter(!is.na(created_at)) %>%
    #pull(observation_count) %>% sum


## Show top projects within 1000 km

fields = toRISON(list("title", "user_ids"))

out <- iNatAPI("GET", "v2", "projects", api_token, lat = 44.56, lng = -123.25, 
               radius = 1000, order_by = "distance", page = 1, per_page = 200, fields = fields)

projects = out$data$total_results
results = out$data$results

for (i in 2:ceiling(projects/200)) {
    
    out <- iNatAPI("GET", "v2", "projects", api_token, lat = 45, lng = -119, 
                   radius = 1000, order_by = "distance", page = i, per_page = 200, fields = fields)
    
    results <- rbind(results, out$data$results)
    
}
str(results)

for (i in 1:nrow(results)) {
    results$users[i] = length(results$user_ids[[i]])
}

results <- results %>% select(-user_ids) %>% arrange(desc(users))
results
