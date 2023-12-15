library(httr)
library(tidyverse)
library(jsonlite)

# Define API credentials
client_id <- "q1vvgIe6E8n7rgfcovg7KQ"
client_secret <- "MFkw2VP8S76lRJ7nLA_D2Msp7RLcpA"

reddit_endpoints <- oauth_endpoint(
  authorize = "https://www.reddit.com/api/v1/authorize",
  access = "https://www.reddit.com/api/v1/access_token"
)


# Authenticate
my_app <- oauth_app("reddit", key = client_id, secret = client_secret)

reddit_token <- oauth2.0_token(
  reddit_endpoints, 
  my_app, 
  scope = c("read"),
  use_basic_auth = TRUE,
  config_init = user_agent("Mac:UMAP:v1.0 (by /u/Forkman3939)")
)
