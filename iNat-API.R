iNatAPI <- function(request_type, api_version, endpoint, api_key, body = NULL, ...) {
    
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
    if (!is.null(response$data$results)) {
        tryCatch({
            response$results <- do.call(data.frame, response$data$results)
        }, error = function(cond){})
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
