library(httr)
library(tidyverse)
library(jsonlite)

get_url <- function(subreddits, keywords, time_frame, sort_parm, limit) {
  # Construct the search query
  query <- paste0("subreddit:", subreddits[1], "+AND+(", paste(keywords, collapse=" OR "), ")")
  
  # Initialize a vector to store post URLs
  permalinks <- c()
  
  # Loop through each subreddit
  for(subreddit in subreddits) {
    search_url <- paste0("https://oauth.reddit.com/r/", subreddits[1], "/search.json?q=", URLencode(query), "&sort=", sort_parm, "&t=", time_frame, "&limit=", limit)
    
    # Send the request
    response <- GET(search_url, add_headers(Authorization = paste("Bearer", reddit_token$credentials$access_token)))
    
    # Check if the request was successful
    if (http_status(response)$category == "Success") {
      # Parse the response
      content_response <- content(response, as = "text")
      content_list <- fromJSON(content_response, flatten = TRUE)
      
      # Extract URLs from content_list
      for (i in 1:nrow(content_list$data$children)) {
        # Extract the permalink
        permalink <- content_list$data$children[i, "data.permalink"]
        # Append the new permalink to the existing vector
        permalinks <- c(permalinks, permalink)
      }
    }
  }
  
  # Modify URLs to the correct format
  corrected_urls <- lapply(permalinks, function(link) {
    # Construct the full URL and add it to the list
    post_urls <- paste0("https://oauth.reddit.com", link)
    
  })
  
  # Flatten the list to a vector
  corrected_urls <- unlist(corrected_urls)
  # Return a list containing both sets of URLs
  return(list(permalinks = permalinks, corrected_urls = corrected_urls))
  
}



get_content <- function(url_vec) {
  result <- list()
  for (i in 1:length(url_vec)) {
    # Fetch the comments using the GET request
    response <- GET(url_vec[i], add_headers(Authorization = paste("Bearer", reddit_token$credentials$access_token)))
    #Extract the content from the response
    content_response <- content(response)
    # Extract post info from the content
    post_list <- content_response[[1]]
    # Extract the comment info from the content
    comments_list <- content_response[[2]]
    # Store post and comment info 
    result[[i]] <- list(post_list = post_list, comments_list = comments_list)
  }
  return(result)
}

# Define function to recursively extract features from nested list structure 
get_comments <- function(comments, parent_id = "ROOT", depth = 0) {
  results <- data.frame(
    ups = integer(),
    downs = integer(),
    comment_id = character(),
    author = character(),
    parent_id = character(),
    subreddit = character(),
    link_id = character(),
    score = integer(),
    is_submitter = logical(),
    body = character(),
    depth = integer(),
    controversiality = integer(),
    created_utc = integer(),
    stringsAsFactors = FALSE
  )
  print(paste("Processing depth:", depth, "with", length(comments$data$children), "comments"))
  
  for (i in 1:length(comments$data$children)) {
    comment <- comments$data$children[[i]]$data
    
    # Check for NULL values and replace with default values if necessary
    ups <- ifelse(is.null(comment$ups), 0, comment$ups)
    downs <- ifelse(is.null(comment$downs), 0, comment$downs)
    comment_id <- ifelse(is.null(comment$id), NA, comment$id)
    author <- ifelse(is.null(comment$author), NA, comment$author)
    parent_id <- ifelse(is.null(comment$parent_id), NA, comment$parent_id)
    subreddit <- ifelse(is.null(comment$subreddit_name_prefixed), NA, comment$subreddit_name_prefixed)
    link_id <- ifelse(is.null(comment$link_id), NA, comment$link_id)
    score <- ifelse(is.null(comment$score), 0, comment$score)
    is_submitter <- ifelse(is.null(comment$is_submitter), FALSE, comment$is_submitter)
    body <- ifelse(is.null(comment$body), "", comment$body)
    depth <- ifelse(is.null(comment$depth), 0, comment$depth)
    controversiality <- ifelse(is.null(comment$controversiality), 0, comment$controversiality)
    created_utc <- ifelse(is.null(comment$created_utc), 0, comment$created_utc)
    
    results <- rbind(results, data.frame(
      ups = ups,
      downs = downs,
      comment_id = comment_id,
      author = author,
      parent_id = parent_id,
      subreddit = subreddit,
      link_id = link_id,
      score = score,
      is_submitter = is_submitter,
      body = body,
      depth = depth,
      controversiality = controversiality,
      created_utc = created_utc,
      stringsAsFactors = FALSE
    ))
    
    # Check if replies exist and contain actual data
    if (!is.null(comment$replies) && is.list(comment$replies) &&
        !is.null(comment$replies$data) && length(comment$replies$data$children) > 0) {
      print(paste("Found replies at depth:", depth, "for comment ID:", comment$id))
      replies_results <- extract_comments(comment$replies, comment$id, depth +1)
      
      # Only bind if replies_results has rows
      if (nrow(replies_results) > 0) {
        results <- rbind(results, replies_results)
      }
    }
  }
  
  return(results)
}


get_data <- function(subreddits, keywords, time_frame, sort_parm, limit) {
  
  urls_vec <- get_url(subreddits, keywords, time_frame, sort_parm, limit)
  content <- get_content(urls_vec)
  df <- list()
  
  for(i in 1:(length(content))) {
    df[[i]] <- get_comments(content[[2]])
  }
  return(df)
}





subreddits <- c("News")
keywords <- c("Gaza", "Israel", "Palestine","Jerusalem",
               "West Bank","Hamas")
sort_parm <- "relevant"
time_frame <- "top"
limit <- 50

urls_rel <- get_url(subreddits, keywords, time_frame, sort_parm, limit)
urls_top <- get_url(subreddits, keywords, time_frame, sort_parm, limit)
saveRDS(urls, file = "news_top_year_50.rds")



####





