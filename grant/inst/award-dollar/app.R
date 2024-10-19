#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(grant)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Grant Opportunities - Award Ceilings and Floors"),

  fluidRow(
    column(3,  # Sidebar
           sidebarPanel(
             # Slider input to filter award ceiling, with wider width
             sliderInput("ceiling_range", "Award Ceiling Range:",
                         min = 50000, max = 337000000,
                         value = c(0, 337000000), step = 100000,
                         width = "100%"),  # Make the slider wider

             # Slider input to filter award floor, with wider width
             sliderInput("floor_range", "Award Floor Range:",
                         min = 33500, max = 8000000,
                         value = c(0, 260000), step = 10000,
                         width = "100%"),  # Make the slider wider

             # Dropdown to filter by category, with wider width
             selectizeInput("category_input", "Select Category:",
                            choices = unique(grant_opp$category),
                            selected = unique(grant_opp$category),
                            multiple = TRUE,
                            options = list(placeholder = 'Select a category...'),
                            width = "100%")  # Make the dropdown wider
           )
    ),

    column(9,  # Main panel for plot and table output
           mainPanel(
             br(),  # Add a line break to move the plot down
             plotlyOutput("bar_plot"),  # Plotly output for the ggplot
             br(),  # Another break to add spacing below the plot
             DTOutput("grant_table")    # DataTable output for category summary
           )
    )
  )
)

# Define server logic for Shiny App
server <- function(input, output) {

  # Reactive expression to filter grants based on user inputs
  filtered_grants <- reactive({
    grant_opp |>
      filter(
        award_ceiling >= input$ceiling_range[1],
        award_ceiling <= input$ceiling_range[2],
        award_floor >= input$floor_range[1],
        award_floor <= input$floor_range[2],
        category %in% input$category_input
      )
  })

  # Summarize the filtered data for plotting and table
  grant_means <- reactive({
    filtered_grants() |>
      group_by(category) |>
      summarise(
        mean_ceiling = mean(award_ceiling, na.rm = TRUE),
        mean_floor = mean(award_floor, na.rm = TRUE)
      ) |>
      mutate(difference = mean_ceiling - mean_floor)
  })

  # Render the bar plot
  output$bar_plot <- renderPlotly({
    p1 <- ggplot(grant_means(), aes(x = category, y = difference)) +
      geom_bar(stat = "identity", fill = "#dab1da") +
      labs(title = "Difference Between Award Ceilings and Floors by Category",
           x = "Category",
           y = "Difference (Ceiling - Floor)",
           fill = "Category") +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove the legend for a cleaner look

    ggplotly(p1)  # Convert ggplot to plotly for interactivity
  })

  # Render the data table showing mean award ceiling and floor
  output$grant_table <- renderDT({
    datatable(
      grant_means(),
      options = list(pageLength = 5, autoWidth = TRUE, order = list(list(0, 'asc'))),
      rownames = FALSE
    )
  })
}

# Run the Shiny App
shinyApp(ui, server)
