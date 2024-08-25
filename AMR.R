library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(httr)

# Define the URL for the Excel file
url <- "https://github.com/olneyjR/all_data/raw/master/Animal_metabolic_rates.xlsx"

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readxl)
library(httr)

# Define the URL for the Excel file
url <- "https://github.com/olneyjR/all_data/raw/master/Animal_metabolic_rates.xlsx"

# Download the file to a temporary location
temp_file <- tempfile(fileext = ".xlsx")
response <- GET(url, write_disk(temp_file, overwrite = TRUE))

# Check if the download was successful
if (status_code(response) != 200) {
  stop("Failed to download file: ", status_code(response))
}

# Load the data from the downloaded file
data2 <- read_excel(temp_file)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Species Metabolic Rates Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Species:", 
                  choices = unique(data2$Species), 
                  selected = unique(data2$Species)[1],
                  multiple = TRUE),
      selectInput("met_type", "Select Metabolic Rate Type:",
                  choices = unique(data2$`Type of Metabolic Rate`),
                  selected = unique(data2$`Type of Metabolic Rate`)[1],
                  multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("data_table")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Boxplot", plotOutput("boxplot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    data2 %>%
      filter(Species %in% input$species,
             `Type of Metabolic Rate` %in% input$met_type)
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = `Wet Mass (g)`, y = `Metabolic Rate (W, at 25C)`)) +
      geom_point() +
      labs(title = "Wet Mass vs. Metabolic Rate (at 25C)",
           x = "Wet Mass (g)",
           y = "Metabolic Rate (W, at 25C)")
  })
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = `Wet Mass (g)`)) +
      geom_histogram(binwidth = 10) +
      labs(title = "Histogram of Wet Mass",
           x = "Wet Mass (g)",
           y = "Frequency")
  })
  
  output$boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = Species, y = `Wet Mass (g)`)) +
      geom_boxplot() +
      labs(title = "Boxplot of Wet Mass by Species",
           x = "Species",
           y = "Wet Mass (g)") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Run the application 
shinyApp(ui, server)
