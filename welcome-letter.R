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
api_token <- "eyJhbGciOiJIUzUxMiJ9.eyJ1c2VyX2lkIjo2NjUwLCJleHAiOjE3MjE4NDExNzZ9.J2qZ4YVH4hezKOdON5YFW43xNj83uD9nz1wJiQzXyUCpK_tRETuyRqfY6RnuGLlpJPH4QDkiDR8euUwd8YprWQ"

##### iNat API wrapper syntax #####

# iNatAPI(HTTP_Request_Type, API_Version, API_Endpoint, API_auth_token, [body], [args...])
# Returns an httr::response object
# - object output in response$data
# - dataframe output in response$results
# See documentation:
# - https://api.inaturalist.org/v1/docs/
# - https://api.inaturalist.org/v2/docs/


# Add the subject
subject <- "Welcome to Flora of Oregon: Vascular Plants"

welcome <- paste0("Thank you for joining the [Flora of Oregon: Vascular Plants](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants) project! Your participation is greatly  appreciated and a contribution to our knowledge of our state flora! This project is a collaboration among the <a href=\"https://bpp.oregonstate.edu/herbarium\"  target=\"_blank\">Oregon State University Herbarium</a>, <a href=\"https://oregonflora.org/\"  target=\"_blank\">OregonFlora</a>, and the <a href=\"https://www.npsoregon.org/\" target=\"_blank\">Native Plant Society of Oregon</a>.

Please introduce yourself in our [Introductions](https://www.inaturalist.org/posts/83757-welcome-please-introduce-yourselves-in-the-comments) post. Would you like to help contribute? Check out our [About](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants?tab=about) page for ideas. You’ll also find instructions on how to trust our project admins with hidden coordinates there, which we hope you will do to allow scientists and OregonFlora access to your data. 

Observers like you have aided us in recording hundreds of plants new to Oregon. From common to rare species, together we’ve been mapping and documenting the ranges and habits of many species for science and conservation. More than 37,000 people have contributed over 740,000 plant observations, exceeding 4,800 species; and the numbers keep growing! 

As human activity profoundly alters the map of life on local and global scales, our response requires knowledge of plant distributions and habitats across vast landscapes and over long periods of time.  We cannot respond effectively to climate change, natural disasters, invasive species, and other environmental and economic threats without an in-depth understanding of our state’s natural heritage. 

Here are a few resources that you may find helpful for improving your observations and knowledge for identification, and conservation: 

* New to iNaturalist? Here are some [infographics](https://www.inaturalist.org/pages/getting+started), and [video tutorials](https://www.inaturalist.org/pages/video+tutorials) to get you started. 
* Check out iNaturalist’s [Guide to Photographing Plants](https://www.inaturalist.org/guide_taxa/355708).
* How to [improve the quality](https://www.inaturalist.org/projects/flora-of-oregon-vascular-plants/journal/80155-creating-high-quality-inaturalist-observations) of your observations. 
* Learn about our plant species and how to identify them with [OregonFlora’s tools](https://oregonflora.org/pages/tutorials.php). 
* Learn how to contribute as an identifier on iNaturalist with this [webinar](https://media.oregonstate.edu/media/t/1_r2ci5et0) that we produced. 



We look forward to seeing all of your interesting observations! Thank you for joining us.

Sincerely,
	", your_name, ", for

* **The iNat Flora of Oregon Team (@mickley, @twainwright, @wisel, @ribes2018, and members of the Native Plant Society of Oregon and OregonFlora)**")

##################################################


##################################################



# Get project members
members <- data.frame()
i <- 1
while(i > 0) {
    out <- iNatAPI("GET", "v2", "projects/168279/members", api_token, 
        per_page = 100, page = i, fields="all")
    members <- rbind(members, out$results)
    
    if(ceiling(out$data$total_results/100) == i) {
        break
    } else {
        i <- i+1
    }
}


members <- members %>% 
    select(created_at, user.id, user.login)

members

members %>% filter(user.login == "castilliajosie")

# Get google sheet
joined <- read_sheet('https://docs.google.com/spreadsheets/d/1blzZVI8RffUFvUVB3ENNJS3inmBsxXh6pdXCQizSY1Q/edit?usp=sharing')

# Joinees who haven't been welcomed
uncontacted <- joined %>% 
    
    rename(Welcome = `Welcome Letter Date`, user.login = `iNat Username`, 
           user.name = `Name (if specified)`) %>% 
    
    right_join(members) %>% 
    
    # Get rid of <NA> usernames
    mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
    
    # Make a first name column
    mutate(first.name = gsub("^(.*?)\\s.*", "\\1", user.name)) %>% 

    # Filter to only my contactees who have joined and not been welcomed
    filter(is.na(Welcome)) %>%
        
    # Select columns
    select(user.id, user.login, user.name, first.name)
        
uncontacted

# Send messages to users
send_message(uncontacted, subject, welcome, 1, 12)




