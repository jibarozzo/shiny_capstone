#
library(tidyverse)
library(shiny)
library(bslib)
source("R/imp_clean_funs.R")


# Loading data
load("data/output/storms.rda")

storm_vars <- c(
    "Crop Damage" = "damage_crops",
    "Property Damage" = "damage_property"
    
)

ui <- page_sidebar(
    title = "Storm Damage in the US", sidebar = sidebar(
        selectInput(
            "region",
            "Select a region",
            choices = c("All Regions", "West", "North Central", "Northeast", "South"),
            selected = "All Regions"
        ),
        selectInput(
            "var",
            "Select a variable",
            choices = storm_vars,
            selected = "damage_property"
        )
        # ,
        # dateRangeInput("dates",
        #                label = "Select dates")
        # ,
        # downloadButton("download")
        
    ),
    card(
        card_header(
            textOutput("title"),
        ),
        card_body(
            plotOutput("plot")
        )
    )
)


server <- function(input, output, session) {
    
    # Reactive function to filter data by region or return all data
    storm_region <- reactive({
        req(input$region)  # Ensure region is selected
        
        if(input$region == "All Regions") {
            return(storms)  # Return all storms if "All Regions" is selected
        } else {
            return(storms %>%
                       filter(region == input$region))  # Filter by selected region
        }
    })

    
    # Update event types when region changes
    observe({
        updateSelectInput(
            session, "event_type",
            choices = storms %>%
                filter(region == input$region | input$region == "All Regions") %>%
                distinct(event_type) %>%
                pull(event_type)
        )
    })
    
    # Render dynamic title based on selected variable
    output$title <- renderText({
        names(storm_vars)[storm_vars == input$var]
    })
    
    # Render the plot
    output$plot <- renderPlot({
        storm_region() |>
            # ggplot(aes(x = year, y = .data[[input$var]])) +
            # geom_line()
            group_by(year, event_type) |>
            summarise(
                sum_value = sum(!!sym(input$var), na.rm = TRUE), 
                .groups = "drop"
            ) |>
            ggplot(aes(x = year, y = sum_value, colour = event_type)) +
            geom_line() +
            scale_y_continuous(labels = scales::label_currency(
                prefix = "$",
                scale_cut = c(0, K = 1e3, M = 1e6, B = 1e9)
            )) +
            theme_light()
    })
}


shinyApp(ui = ui, server = server)


