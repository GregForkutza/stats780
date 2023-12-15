library(tm)
library(wordcloud)

###### Create Word Clouds in each cluster
  # Duplicate funtion adpated to fit concatenated comments
  # Function to extract frequent terms
  extract_terms <- function(cluster_number) {
    corpus <- Corpus(VectorSource(df_comments[df_comments$cluster == cluster_number, ]$comment))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing=TRUE)
    d <- data.frame(word = names(v), freq=v)
    return(d)
  }

  # Extract and view terms for each cluster (adjust range as per your number of clusters)
  for (i in 1:3) {
    print(paste("Cluster", i))
    wordcloud(words = extract_terms(i)$word, freq = extract_terms(i)$freq, min.freq = 10, random.order=FALSE)
  }

####




