iNatAPI <- function(request_type, api_version, endpoint, api_key, body = NULL, ...) {
    
    require(httr)
    
    # Get a list of URL options from the additional parameters passed
    options <- list(...)

    # Set up the base URL
    base_url <- "https://api.inaturalist.org/"
    
    # Prepend the API version to the endpoint
    endpoint <- paste(api_version, endpoint, sep = "/")
    
    # check Internet connection
    if (!curl::has_internet()) {
        message("No Internet connection.")
        return(invisible(NULL))
    }

    # check that iNat can be reached
    if (httr::http_error(base_url)) { # TRUE: 400 or above
        message("iNaturalist API is unavailable.")
        return(invisible(NULL))
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
    
    # Return the response object, use content(response), to access content
    return(response)
}
