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
library(shinydashboard)
library(ggplot2)


# Define UI for Shiny App using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Grant Opportunities Dashboard"),

  # Sidebar for inputs
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filter Grants", tabName = "filter_grants", icon = icon("filter")),
      menuItem("Analyze Awards", tabName = "analyze_awards", icon = icon("chart-bar"))
    )
  ),

  # Main content
  dashboardBody(
    tabItems(

      # Tab for filtering grants by category and year
      tabItem(tabName = "filter_grants",
              fluidRow(
                column(4,
                       selectizeInput("category_input", "Select a Grant Category:",
                                      choices = unique(grant_opp$category),
                                      selected = "Science",
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a category...')),

                       selectizeInput("year_input", "Select a Year:",
                                      choices = c("2024", "2025", "2026", "2027", "2028", "2029", "2030 onwards"),
                                      selected = "2024",
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a year...'))
                ),
                column(8,
                       DTOutput("grant_table")
                )
              )
      ),

      # Tab for analyzing award ceilings and floors
      tabItem(tabName = "analyze_awards",
              fluidRow(
                column(3,
                       sliderInput("ceiling_range", "Award Ceiling Range:",
                                   min = 50000, max = 337000000,
                                   value = c(0, 337000000), step = 100000,
                                   width = "100%"),

                       sliderInput("floor_range", "Award Floor Range:",
                                   min = 33500, max = 8000000,
                                   value = c(0, 260000), step = 10000,
                                   width = "100%"),

                       selectizeInput("category_input_awards", "Select Category:",
                                      choices = unique(grant_opp$category),
                                      selected = unique(grant_opp$category),
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a category...'),
                                      width = "100%")
                ),

                column(9,
                       plotlyOutput("bar_plot"),
                       DTOutput("award_table")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Filter grants based on category and year for the first tab
  filtered_grants <- reactive({
    req(input$category_input)

    data <- grant_opp %>%
      filter(category %in% input$category_input)

    if (length(input$year_input) > 0) {
      if ("2030 onwards" %in% input$year_input) {
        data <- data %>% filter(as.numeric(format(current_closing_date_for_applications, "%Y")) >= 2030)
      } else {
        selected_years <- as.numeric(input$year_input)
        data <- data %>% filter(as.numeric(format(current_closing_date_for_applications, "%Y")) %in% selected_years)
      }
    }
    data
  })

  # Render the filtered grants table
  output$grant_table <- renderDT({
    datatable(
      filtered_grants(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,  # Enable horizontal scroll
        columnDefs = list(list(width = '100px', targets = "_all")),  # Set all column widths to 100px
        initComplete = JS("function(settings, json) {",
                          "$('table').css({'font-size': '12px'});",  # Reduce font size for compact view
                          "}")),
      class = 'compact stripe hover nowrap',  # Compact and nowrap to prevent breaking lines
      rownames = FALSE
    )
  })


  # Filter grants based on award ceiling and floor for the second tab
  filtered_awards <- reactive({
    grant_opp %>%
      filter(
        award_ceiling >= input$ceiling_range[1],
        award_ceiling <= input$ceiling_range[2],
        award_floor >= input$floor_range[1],
        award_floor <= input$floor_range[2],
        category %in% input$category_input_awards
      )
  })

  # Summarize the filtered data for plotting and table
  award_summary <- reactive({
    filtered_awards() %>%
      group_by(category) %>%
      summarise(
        mean_ceiling = mean(award_ceiling, na.rm = TRUE),
        mean_floor = mean(award_floor, na.rm = TRUE)
      ) %>%
      mutate(difference = mean_ceiling - mean_floor)
  })

  # Render the bar plot
  output$bar_plot <- renderPlotly({
    p1 <- ggplot(award_summary(), aes(x = category, y = difference)) +
      geom_bar(stat = "identity", fill = "#dab1da") +
      labs(title = "Difference Between Award Ceilings and Floors by Category",
           x = "Category", y = "Difference (Ceiling - Floor)") +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p1)
  })

  # Render the award summary table
  output$award_table <- renderDT({
    datatable(
      award_summary(),
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  })
}

# Run the Shiny App
shinyApp(ui, server)
