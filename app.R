library(tidyverse)
library(shiny)
library(bslib)
source("R/imp_clean_funs.R")


# Loading data
# App sources files in R/ and loads necesary objects
# load("data/output/storms.rda")


storm_vars <- c(
    "Crop Damage" = "damage_crops",
    "Property Damage" = "damage_property"
    
)

ui <- page_sidebar(
    title = "Storm Damage in the US", 
    sidebar = sidebar(
        actionButton("btn_all_region", "All Regions", class = "btn-danger"),
        actionButton("btn_west", "West"),
        actionButton("btn_north_central", "North Central"),
        actionButton("btn_northeast", "Northeast"),
        actionButton("btn_south", "South"),
        hr(),
        
        selectInput(
            "var",
            "Select a variable",
            choices = storm_vars,
            selected = "damage_property"
        )
        
        # Old logic
        # selectInput(
        #     "region",
        #     "Select a region",
        #     choices = c("All Regions", "West", "North Central", "Northeast", "South"),
        #     selected = "All Regions"
        # ),
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
    
    # Reactive value to store the selected region
    selected_region <- reactiveVal("All Regions")
    
    # Update selected region when buttons are clicked
    observeEvent(input$btn_all_region, {
        selected_region("All Regions")
    })
    
    observeEvent(input$btn_west, {
        selected_region("West")
    })
    
    observeEvent(input$btn_north_central, {
        selected_region("North Central")
    })
    
    observeEvent(input$btn_northeast, {
        selected_region("Northeast")
    })
    
    observeEvent(input$btn_south, {
        selected_region("South")
    })
    
    
    # Reactive function to filter data by region or return all data
    storm_region <- reactive({
        req(selected_region())  # Ensure region is selected
        
        if(selected_region() == "All Regions") {
            return(storms)  # Return all storms if "All Regions" is selected
        } else {
            return(storms %>%
                       filter(region == selected_region()))  # Filter by selected region
        }
    })

    
    # observe({
    #     updateSelectInput(
    #         session,
    #         "event_type",
    #         choices = storms %>%
    #             filter(region == selected_region() | selected_region() == "All Regions") %>%
    #             distinct(event_type) %>%
    #             pull(event_type)
    #     )
    # })
    # 
    # Render dynamic title based on selected variable
    output$title <- renderText({
        names(storm_vars)[storm_vars == input$var]
    })
    
    # Render the plot
    output$plot <- renderPlot({
        req(input$var)
        
        # Validate that the selected column exists in the dataset
        if (!(input$var %in% names(storms))) {
            return(NULL)  # If column does not exist, return NULL
        }
        
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


