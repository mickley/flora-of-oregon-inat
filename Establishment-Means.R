# Packages
library(httr) # Needed for working with HTTP requests
library(jsonlite) # For working with JSON
library(dplyr) # Needed for data wrangling
source("iNat-API.R") # Our iNaturalist API wrapper function

# Get API Token by going to this link and authenticating as yourself
# https://www.inaturalist.org/users/api_token

# Paste API token from line above here
api_token <- "eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjo2NjUwLCJleHAiOjE3NzA3NDc5ODl9.tK1SLLBzcYfudXebEEMN71scdly4vR4vx6tgwqkxWKYikV8PdDFFdT-vAHG3rufR0UDddfMIxzpV_GxPcwSw_A"


# Get Native
taxa <- data.frame()
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "observations/species_counts", api_token, 
        per_page = 200, page = i, captive = "false",
        taxon_id=211194, place_id = 10, quality_grade = "research", 
        include_ancestors = "false", native = "true",
        fields="(taxon:(rank:!t,name:!t))")
    taxa <- rbind(taxa, out$results %>% mutate(establishment = "native"))
    
    if(ceiling(out$data$total_results/200) == i) {
        break
    } else {
        i <- i+1
    }
}

# Get Introduced
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "observations/species_counts", api_token, 
       per_page = 200, page = i, captive = "false", taxon_is_active = "true", 
       project_id = 168279, place_id = 10, quality_grade = "research", 
       include_ancestors = "false", introduced = "true",
       fields="(taxon:(rank:!t,name:!t))")
    taxa <- rbind(taxa, out$results %>% mutate(establishment = "introduced"))
    
    if(ceiling(out$data$total_results/200) == i) {
        break
    } else {
        i <- i+1
    }
}

# Get unknown establishment
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "observations/species_counts", api_token, 
       per_page = 200, page = i, captive = "false", taxon_is_active = "true", 
       project_id = 168279, place_id = 10, quality_grade = "research", 
       include_ancestors = "false", native = "false", introduced = "false",
       fields="(taxon:(rank:!t,name:!t))")
    taxa <- rbind(taxa, out$results %>% mutate(establishment = "unknown"))
    
    if(ceiling(out$data$total_results/200) == i) {
        break
    } else {
        i <- i+1
    }
}

# Remove duplicate unknowns
taxa <- taxa %>%
    add_count(taxon.name) %>%
    filter(!(n > 1 & establishment != "unknown")) %>% 
    filter(taxon.rank == "species" | taxon.rank == "hybrid" | taxon.rank == "complex") %>%
    select(-n)

taxa1 <- taxa %>% mutate(endemic = NA_character_)

# Get endemic
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "observations/species_counts", api_token, 
       per_page = 200, page = i, captive = "false", taxon_is_active = "true", 
       project_id = 168279, place_id = 10, quality_grade = "research", 
       include_ancestors = "false", endemic = "true",
       fields="(taxon:(rank:!t,name:!t))")
    taxa1 <- rows_upsert(taxa1, out$results %>% select(-count) %>%
        mutate(endemic = "yes"), by = "taxon.id")
    
    if(ceiling(out$data$total_results/200) == i) {
        break
    } else {
        i <- i+1
    }
}

right_join(taxa1, out$results %>% select(-count) %>%
    mutate(endemic = "yes"), 
    by = c("taxon.id", "taxon.rank", "taxon.name"))

right_join(taxa1, out$results %>% select(-count) %>% 
          mutate(endemic = "yes"), 
      by = c("taxon.id", "taxon.rank", "taxon.name")) %>% 
    filter(endemic == "yes")

str(taxa1)

taxa1
taxa %>%
    add_count(taxon.name) %>%
    arrange(taxon.name) %>%
    filter(n > 1)

taxa %>% filter(establishment == "unknown")

taxa %>% filter(str_detect(taxon.name, "^Calochortus"))

taxa1 %>% filter(endemic == "yes") %>% tail(132)
    
    group_by(establishment) %>% summarize(n = n())

tail(taxa, 300)

taxa %>% group_by(establishment) %>% summarize(n = n()) 

oregonflora <- read.csv("oregonflora-list.csv")

taxa1 %>% group_by(establishment) %>% summarize(n = n())
taxa1 %>% group_by(endemic) %>% summarize(n = n())

taxa1 %>% 
    left_join(oregonflora, by = "taxon.name") %>%
    arrange(endemic, establishment) %>% 
    mutate(endemic = ifelse(is.na(endemic), "", endemic), 
        OregonFlora = ifelse(is.na(OregonFlora), "", OregonFlora),
        Group = ifelse(is.na(Group), "", Group), 
        Family = ifelse(is.na(Family), "", Family)) %>%
    select(Group, Family, taxon.rank, taxon.id, taxon.name, establishment, endemic, OregonFlora) %>%
    write.csv("inat-OregonFlora-establishment-means.csv")

# endemic = true/false
# out_of_range
# threatened = true/false