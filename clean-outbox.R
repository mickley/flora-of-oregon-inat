

    subjects <- c("Invitation to join the Flora of Oregon project", 
                  "Welcome to Flora of Oregon: Vascular Plants", 
                  "Invitation from the Flora of Oregon project")
    
out <- iNatAPI("GET", "v1", "messages", box = "any", threads = "true", page = 3, 
    api_token)



delete <- out$data$results %>% 
    select(id, thread_id, thread_messages_count, subject, read_at, created_at, 
        to_user) %>%
    do.call(data.frame, .) %>%
    select(id, thread_id, thread_messages_count, subject, created_at, 
           to_user.id, to_user.login, to_user.name) %>%
    filter(subject %in% subjects, thread_messages_count == 1) %>%
    filter(created_at < '2024-04-30') 

delete

delete_message(delete)
