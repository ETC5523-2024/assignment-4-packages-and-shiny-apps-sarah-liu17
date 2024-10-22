#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



# Define UI for application that draws a histogram
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Define UI for application that draws a histogram
# Load necessary libraries
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(dplyr)
library(DT)
library(grant)
library(shinydashboard)
library(ggplot2)
library(plotly)

# Define UI for Shiny App using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Grant Opportunities Dashboard"),

  # Sidebar for inputs
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Filter Grants", tabName = "filter_grants", icon = icon("filter")),
      menuItem("Analyze Awards", tabName = "analyze_awards", icon = icon("chart-bar"))
    )
  ),

  # Main content
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              h2("Welcome to the Grant Opportunities Dashboard"),
              p("This application is designed to help you explore and analyze various grant opportunities."),
              p("You can filter grants based on categories, years, and eligibility criteria, as well as analyze award ceilings and floors."),
              p("Use the tabs on the left to navigate between filtering grants, analyzing awards, and viewing this introductory dashboard.")
      ),

      # Tab for filtering grants by category, year, and eligibility
      tabItem(tabName = "filter_grants",
              fluidRow(
                column(4,
                       selectizeInput("category_input", "Select a Grant Category:",
                                      choices = unique(grant_opp$category),
                                      selected = "science",
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a category...')),
                       helpText("Choose one or more categories from the list to filter grants."),

                       selectizeInput("year_input", "Select a Year:",
                                      choices = c("2024", "2025", "2026", "2027", "2028", "2029", "2030 onwards"),
                                      selected = "2024",
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a year...')),
                       helpText("Select the year(s) for which you want to view grant opportunities."),

                       # Add checkboxes for each eligibility criteria
                       tags$h4("Eligibility Criteria"),
                       helpText("Select the eligibility criteria to filter grants by eligible organizations."),
                       checkboxInput("eligibility_individuals", "Individuals Eligible", value = FALSE),
                       checkboxInput("eligibility_state_governments", "State Governments Eligible", value = FALSE),
                       checkboxInput("eligibility_county_governments", "County Governments Eligible", value = FALSE),
                       checkboxInput("eligibility_independent_school_districts", "Independent School Districts Eligible", value = FALSE),
                       checkboxInput("eligibility_city_or_township_governments", "City or Township Governments Eligible", value = FALSE),
                       checkboxInput("eligibility_special_district_governments", "Special District Governments Eligible", value = FALSE),
                       checkboxInput("eligibility_native_american_tribal_governments_federally_recognized", "Native American Tribal Governments (Federally Recognized) Eligible", value = FALSE),
                       checkboxInput("eligibility_native_american_tribal_organizations_other", "Native American Tribal Organizations (Other than Federally Recognized) Eligible", value = FALSE),
                       checkboxInput("eligibility_nonprofits_501c3", "Nonprofits 501(c)(3) Eligible", value = FALSE),
                       checkboxInput("eligibility_nonprofits_non_501c3", "Nonprofits (Non 501(c)(3)) Eligible", value = FALSE),
                       checkboxInput("eligibility_for_profit", "For-Profit Organizations Eligible", value = FALSE),
                       checkboxInput("eligibility_small_businesses", "Small Businesses Eligible", value = FALSE),
                       checkboxInput("eligibility_private_institutions_of_higher_education", "Private Institutions of Higher Education Eligible", value = FALSE),
                       checkboxInput("eligibility_public_institutions_of_higher_education", "Public Institutions of Higher Education Eligible", value = FALSE),
                       checkboxInput("eligibility_public_indian_housing_authorities", "Public or Indian Housing Authorities Eligible", value = FALSE),
                       checkboxInput("eligibility_others", "Other Groups Eligible", value = FALSE),
                       checkboxInput("eligibility_unrestricted", "Eligibility Unrestricted", value = FALSE)
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
                       helpText("Select the range of award ceilings for which you want to filter to present in the plot and table."),

                       sliderInput("floor_range", "Award Floor Range:",
                                   min = 33500, max = 8000000,
                                   value = c(0, 260000), step = 10000,
                                   width = "100%"),
                       helpText("Select the range of award ceilings for which you want to filter to present in the plot and table."),

                       selectizeInput("category_input_awards", "Select Category:",
                                      choices = unique(grant_opp$category),
                                      selected = unique(grant_opp$category),
                                      multiple = TRUE,
                                      options = list(placeholder = 'Select a category...'),
                                      width = "100%"),
                       helpText("Select the category of grants for which you want to filter to present in the plot and table."),
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

  # Filter grants based on category, year, and eligibility for the first tab
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

    # Apply eligibility filters
    if (input$eligibility_individuals) {
      data <- data %>% filter(eligibility_individuals == TRUE)
    }
    if (input$eligibility_state_governments) {
      data <- data %>% filter(eligibility_state_governments == TRUE)
    }
    if (input$eligibility_county_governments) {
      data <- data %>% filter(eligibility_county_governments == TRUE)
    }
    if (input$eligibility_independent_school_districts) {
      data <- data %>% filter(eligibility_independent_school_districts == TRUE)
    }
    if (input$eligibility_city_or_township_governments) {
      data <- data %>% filter(eligibility_city_or_township_governments == TRUE)
    }
    if (input$eligibility_special_district_governments) {
      data <- data %>% filter(eligibility_special_district_governments == TRUE)
    }
    if (input$eligibility_native_american_tribal_governments_federally_recognized) {
      data <- data %>% filter(eligibility_native_american_tribal_governments_federally_recognized == TRUE)
    }
    if (input$eligibility_native_american_tribal_organizations_other) {
      data <- data %>% filter(eligibility_native_american_tribal_organizations_other == TRUE)
    }
    if (input$eligibility_nonprofits_501c3) {
      data <- data %>% filter(eligibility_nonprofits_501c3 == TRUE)
    }
    if (input$eligibility_nonprofits_non_501c3) {
      data <- data %>% filter(eligibility_nonprofits_non_501c3 == TRUE)
    }
    if (input$eligibility_for_profit) {
      data <- data %>% filter(eligibility_for_profit == TRUE)
    }
    if (input$eligibility_small_businesses) {
      data <- data %>% filter(eligibility_small_businesses == TRUE)
    }
    if (input$eligibility_private_institutions_of_higher_education) {
      data <- data %>% filter(eligibility_private_institutions_of_higher_education == TRUE)
    }
    if (input$eligibility_public_institutions_of_higher_education) {
      data <- data %>% filter(eligibility_public_institutions_of_higher_education == TRUE)
    }
    if (input$eligibility_public_indian_housing_authorities) {
      data <- data %>% filter(eligibility_public_indian_housing_authorities == TRUE)
    }
    if (input$eligibility_others) {
      data <- data %>% filter(eligibility_others == TRUE)
    }
    if (input$eligibility_unrestricted) {
      data <- data %>% filter(eligibility_unrestricted == TRUE)
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
                          "$('table').css({'font-size': '12px'});",  # Adjust font size
                          "}")
      )
    )
  })

  # Award analysis plots and tables
  output$bar_plot <- renderPlotly({
    filtered_awards <- grant_opp %>%
      filter(category %in% input$category_input_awards,
             award_ceiling >= input$ceiling_range[1],
             award_ceiling <= input$ceiling_range[2],
             award_floor >= input$floor_range[1],
             award_floor <= input$floor_range[2])

    p <- ggplot(filtered_awards, aes(x = category, y = award_ceiling)) +
      geom_bar(stat = "identity", fill = "#dab1da") +
      labs(title = "Difference Between Award Ceilings and Floors by Category",
           x = "Category", y = "Difference (Ceiling - Floor)") +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  output$award_table <- renderDT({
    # Filter the grants based on user input
    filtered_awards <- grant_opp %>%
      filter(category %in% input$category_input_awards,
             award_ceiling >= input$ceiling_range[1],
             award_ceiling <= input$ceiling_range[2],
             award_floor >= input$floor_range[1],
             award_floor <= input$floor_range[2]) %>%
      # Select only the required columns and calculate the award difference
      select(
        funding_opportunity_title,
        current_closing_date_for_applications,
        award_ceiling,
        award_floor,
        category
      ) %>%
      mutate(award_difference = award_ceiling - award_floor)  # Calculate the difference

    # Include the calculated difference in the datatable
    datatable(filtered_awards, options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,  # Enable horizontal scroll
      columnDefs = list(list(width = '100px', targets = "_all")),  # Set all column widths to 100px
      initComplete = JS("function(settings, json) {",
                        "$('table').css({'font-size': '12px'});",  # Adjust font size
                        "}")
    ))
  })


}

# Run the application
shinyApp(ui = ui, server = server)
