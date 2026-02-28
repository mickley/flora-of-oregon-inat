iNatAPI <- function(request_type, api_version, endpoint, api_key = NULL, body = NULL, ...) {
    
    require(httr)
    
    # Get a list of URL options from the additional parameters passed
    options <- list(...)

    # Set up the base URL
    base_url <- "https://api.inaturalist.org/"
    
    # Prepend the API version to the endpoint
    endpoint <- paste(api_version, endpoint, sep = "/")
    
    # Check for an internet connection
    if (!curl::has_internet()) {
        message("API request failed. No internet connection.")
        return()
    }

    # Check that the API url is reachable
    if (httr::http_error(base_url)) {
        message("API request failed. The iNaturalist API cannot be reached.")
        return()
    }
    
    # Add HTTP headers
    headers <- add_headers(.headers = c(
        'Authorization' = api_key, 
        'Accept' = "application/json", 
        'Content-Type' = "application/json"
        ))
    
    # Run HTTP requests
    if (request_type == "GET") {
        response <- GET(url = base_url, config = headers, path = endpoint, 
            query = options);
    } else if (request_type == "POST") {
        response <- POST(url = base_url, config = headers, path = endpoint, 
            body = body)
    } else if (request_type == "PUT") {
        response <- PUT(url = base_url, config = headers, path = endpoint, 
            body = body, query = options)
    } else if (request_type == "DELETE") {
        response <- DELETE(url = base_url, config = headers, path = endpoint, 
            query=options)
    } else {
        # Bad request type
    }

    # Add the content in convenience dataframe form to the response object
    response$data <- jsonlite::fromJSON(content(response, as = "text"))

    # Attempt to un-nest the dataframe, if possible. 
    # Will only work with one nesting
    if(length(response$data) > 1) {
        if (!is.null(response$data$results)) {
            tryCatch({
                response$results <- do.call(data.frame, response$data$results)
            }, error = function(cond){})
        }
    }

    # Return the response object, use content(response), to access content
    return(response)
}

# Convenience function to turn nested R lists to RISON strings for API V2
# Example: toRISON(list("count", "user" = list("login", "name"))) returns
# "(count:!t,user:(login:!t,name:!t))"
# See: https://api.inaturalist.org/v2/docs/
toRISON <- function(list) {
    str <- c()
    
    # Iterate over the list
    for (i in 1:length(list)) {
        
        # Get key/value pair for list
        key <- ifelse(names(list)[i] == "" || is.null(names(list)), i, names(list)[i])
        val <- list[[key]]
        
        # If the value is character, add it
        if (typeof(val) == "character") {
            str <- c(str, paste0(val, ":!t"))
            
        # If value is a list, recurse
        } else if (typeof(val) == "list") {
            str <- c(str, paste0(key, ":", toRISON(val)))
        }
    }
    
    # Return formatted RISON
    return(paste0("(", paste(str, collapse = ","), ")"))
}

send_message <- function(users, subject, message, first, last){
    
    for(i in first:last) {
        
        # Construct greeting
        if(users$first.name[i] == "") {
            greeting = "Greetings,\n\n"
        } else {
            greeting = paste0("Greetings ", users$first.name[i], ",\n\n")
        }
        
        # Construct message
        msg <- toJSON(list(message = list(
            to_user_id = users$user.id[i], 
            subject = subject, 
            body = paste0(greeting, message))), auto_unbox = TRUE)
        
        # Send message
        out <- iNatAPI("POST", "v1", "messages", api_token, msg)
        
        # Status report
        if(!is.null(out$data$created_at)) {
            print(paste0("Successfully messaged ", users$user.login[i]))
        } else {
            print(paste0("Error messaging ", users$user.login[i]))
            print(out)
        }
        
        # Wait 5 seconds before sending another message
        Sys.sleep(5)
    }
}

delete_message <- function(messages) {
    
    for(i in 1:nrow(messages)) {
        
        # Delete message
        out <- iNatAPI("DELETE", "v1", paste0("messages/", messages$id[i]), 
            api_token)
        
        # Status report
        if(out$status_code) {
            print(paste0("Successfully deleted message id: ", messages$id[i], 
                  " to ", messages$to_user.login[i]))
        } else {
            print(paste0("Error deleting message id: ", messages$id[i], 
                  " to ", messages$to_user.login[i]))
        }
        
        # Wait 5 seconds before deleting another message
        Sys.sleep(5) 
    }

}

get_users <- function(usernames) {
    
    users <- data.frame()

    for(user in usernames) {
        
        # Get user
        out <- iNatAPI("GET", "v2", "users/autocomplete", q = user, 
            per_page = 1, fields = "(id:!t,login:!t,name:!t)")
        
        row <- out$results %>% 
            
            # Rename columns
            rename(user.id = id, user.login = login, user.name = name) %>%
            
            # Get rid of <NA> usernames
            mutate(user.name = ifelse(is.na(user.name), "", user.name)) %>%
            
            # Make a first name column
            mutate(first.name = gsub("^(.*?)\\s.*", "\\1", user.name))
        
        users <- rbind(users, row)
        
        # Wait 1 second
        Sys.sleep(1)
    }
    
    return(users)
}
