library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)
# Function to create and save a plot for each dataset
plot_sentiment_by_topic_cluster <- function(data, n, save_plot = FALSE) {
  
  data <- data %>% filter(!is.na(topic))
  
   if (is.numeric(data$date)) {
    data$date <- as.Date(as.POSIXct(data$date, origin = "1970-01-01", tz = "UTC"))
  } else if (is.character(data$date)) {
    data$date <- as.Date(data$date)
  }
  
  date_min <- min(data$date)
  
  if (n < length(results)) {
    next_data_date <- as.Date(as.POSIXct(min(results[[n + 1]]$all_data$date), origin = "1970-01-01", tz = "UTC"))
    date_max <- next_data_date - days(1)
  } else {
    date_max <- max(data$date)
  }
  
  date_min <- format(date_min, "%Y-%m-%d")
  date_max <- format(date_max, "%Y-%m-%d")
  
  plot_title <- paste("Sentiment Scores by Topic and Cluster -", date_min, "to", date_max)
  plot <- ggplot(data, aes(x = as.factor(topic), y = comment_vader_sentiment, color = as.factor(cluster))) +
    geom_point() +
    labs(title = plot_title, x = "Topic", y = "Sentiment Score", color = "Cluster") +
    theme_minimal() +
    theme(plot.title = element_text(size = 8))  
  
  if (save_plot) {
    ggsave(filename = paste("plots/sentiment_topic_cluster_plot_", n, ".png", sep = ""), plot = plot, width = 10, height = 6)
  }
  
  return(plot)
}

# Apply the function to each element in the results list
plots_list <- lapply(seq_along(results), function(i) {
  data <- results[[i]]$all_data
  plot_sentiment_by_topic_cluster(data, i, save_plot = TRUE)
})

plots_dir <- "plots"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Function to arrange and save 5 plots
arrange_save_5_plots <- function(plots, filename) {
  combined_plot <- arrangeGrob(grobs = plots, ncol = 2, nrow = 3)
  ggsave(file.path(plots_dir, filename), combined_plot, width = 10, height = 15)
}

# Function to arrange and save 4 plots
arrange_save_4_plots <- function(plots, filename) {
  combined_plot <- arrangeGrob(grobs = plots, ncol = 2, nrow = 2)
  ggsave(file.path(plots_dir, filename), combined_plot, width = 10, height = 10)
}

# Create and save the visuals
arrange_save_5_plots(plots_list[1:6], "composite_plot_1_6.png")
arrange_save_4_plots(plots_list[7:10], "composite_plot_7_10.png")