#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# Define UI for application that draws a histogram
library(shiny)
library(dplyr)
library(DT)
library(grant)


# Define UI for Shiny App
# Define UI for Shiny App
ui <- fluidPage(
  titlePanel("Grant Opportunities"),

  # Use fluidRow for layout
  fluidRow(
    column(4,  # Left sidebar for inputs
           sidebarPanel(
             # Selectize input to choose a grant category
             selectizeInput("category_input", "Select a Grant Category:",
                            choices = unique(grant_opp$category),
                            selected = "Science",  # Default to Science
                            multiple = TRUE,
                            options = list(placeholder = 'Select a category...')),

             # Selectize input to choose a year range
             selectizeInput("year_input", "Select a Year:",
                            choices = c("2024", "2025", "2026", "2027", "2028", "2029", "2030 onwards"),
                            selected = "2024",  # Default to 2024
                            multiple = TRUE,
                            options = list(placeholder = 'Select a year...'))
           )
    ),

    column(8,  # Right column for the table
           mainPanel(
             # Display the filtered grants in a sortable DT table
             DTOutput("grant_table")
           )
    )
  )
)

# Define server logic for Shiny App
server <- function(input, output) {

  # Reactive expression to filter grants based on the selected category and year
  filtered_grants <- reactive({
    # Ensure category input is not NULL
    req(input$category_input)

    # Start with the entire dataset
    data <- grant_opp

    # Filter by selected categories (if multiple, use %in% operator)
    data <- data %>%
      filter(category %in% input$category_input)

    # If year input is selected, filter accordingly
    if (length(input$year_input) > 0) {
      # Handle the "2030 onwards" case
      if ("2030 onwards" %in% input$year_input) {
        data <- data %>% filter(as.numeric(format(current_closing_date_for_applications, "%Y")) >= 2030)
      } else {
        selected_years <- as.numeric(input$year_input)
        data <- data %>% filter(as.numeric(format(current_closing_date_for_applications, "%Y")) %in% selected_years)
      }
    }

    data
  })

  # Render the filtered grants in a sortable and searchable DT table
  output$grant_table <- renderDT({
    datatable(
      filtered_grants(),
      options = list(pageLength = 10, autoWidth = TRUE, order = list(list(0, 'asc')))
    )
  })
}

# Run the Shiny App
shinyApp(ui, server)
