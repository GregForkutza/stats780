

# Libraries 
require(ggplot2)
require(dplyr)
require(ggrepel)
require(readr)
require(shiny)
require(rsconnect)
require(stringr)


#  Download the zip file from the URL and convert to CSV
url <- "https://www150.statcan.gc.ca/n1/tbl/csv/13100416-eng.zip"
destfile <- "13100416-eng.zip"
download.file(url, destfile)

unzip(destfile)

data3 <- read_csv("13100416.csv")  %>%
  mutate(GEO = str_extract(GEO, "^[^,]+")) %>%
  mutate(GEO = case_when(
    GEO == "Newfoundland and Labrador" ~ "NL",
    GEO == "Prince Edward Island" ~ "PE",
    GEO == "Nova Scotia" ~ "NS",
    GEO == "New Brunswick" ~ "NB",
    GEO == "Quebec" ~ "QC",
    GEO == "Ontario" ~ "ON",
    GEO == "Manitoba" ~ "MB",
    GEO == "Saskatchewan" ~ "SK",
    GEO == "Alberta" ~ "AB",
    GEO == "British Columbia" ~ "BC",
    GEO == "Yukon" ~ "YT",
    GEO == "Northwest Territories" ~ "NT",
    GEO == "Nunavut" ~ "NU",
    TRUE ~ GEO
  )) %>%
  mutate(`Age of mother` = sub("Age of mother, ", "", `Age of mother`))

data4 <- data3 %>%
  arrange(REF_DATE) 

data4$REF_DATE <- as.numeric(as.character(data3$REF_DATE))

# UI
ui <- fluidPage(
  selectInput("province", "Choose a Province:", 
              choices = unique(data4$GEO)),
  selectInput("age", "Choose an Age Group:", 
              choices = unique(data4$`Age of mother`)),
  plotOutput("plot")
)

# Convert REF_DATE to numeric
data3$REF_DATE <- as.numeric(data3$REF_DATE)

# Server logic
server <- function(input, output) {
  
  output$plot <- renderPlot({
    filtered_data <- data4 %>%
      filter(GEO == input$province & `Age of mother` == input$age) %>%
      filter(grepl("Number of live births", `Characteristics`)) 
    ggplot(filtered_data, aes(x = REF_DATE, y = VALUE, group = `Age of mother`, color = `Age of mother`)) +
      geom_line() +
      labs(title = "Number of Live Births by Age of Mother",
           x = "Year",
           y = "Number of Live Births") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)

