library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(shiny) # Web Application Framework for R
library(bslib) # Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'
source("R/imp_clean_funs.R")


# Loading data
# App sources files in R/ and loads necesary objects
load("data/output/storms.rda")


storm_vars <- c(
    "Crop Damage" = "damage_crops",
    "Property Damage" = "damage_property"
    
)

ui <- page_fluid(
    theme = bs_theme(version = 5, preset = "sandstone"),
    title = "Storm Damage in the US",
    card(
        full_screen = FALSE,
        card_header("Storm Damage in the US"),
        layout_sidebar(
            sidebar = sidebar(
                #width = 550,
                actionButton("btn_all_region", "United States", class = "btn-primary"),
                actionButton("btn_west", "West", class = "btn-secondary"),
                actionButton("btn_north_central", "North Central", class = "btn-success"),
                actionButton("btn_northeast", "Northeast", class = "btn-warning"),
                actionButton("btn_south", "South", class = "btn-dark"),
                hr(),
                
                selectInput(
                    "var",
                    "Select a variable",
                    choices = storm_vars,
                    selected = "damage_property"
                ),
                
            ),
            
            sliderInput(
                "cost",
                "Cost of Damage",
                min = 0,
                max = 50 * 1e9,
                value = c(0, 40 * 1e9),
                width = '100%',
                step = 1 * 1e6,
                pre = "$"
                
            ),
            
            # sliderInput(
            #     "years",
            #     "Years",
            #     min = min(storms$year),
            #     max = max(storms$year),
            #     sep = "",
            #     value = c(1996, 2024),
            # ),
            card_header(textOutput("title"), ),
            card_body(plotOutput("plot"))
        ),
    )
)

server <- function(input, output, session) {
    #bs_themer()
    
    # Reactive value to store the selected region
    selected_region <- reactiveVal("United States")
    
    # Update selected region when buttons are clicked
    observeEvent(input$btn_all_region, {
        selected_region("United States")
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
        
        if(selected_region() == "United States") {
            return(storms)  # Return all storms if "United States" is selected
        } else {
            return(storms %>%
                       filter(region == selected_region()))  # Filter by selected region
        }
    })


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
            group_by(year, event_type) |>
            filter(.data[[input$var]] >= input$cost[1] &
                      .data[[input$var]] <= input$cost[2]) |>
            # filter(.data[[input$var]] >= input$years[1] &
            #           .data[[input$var]] <= input$years[2]) |>
            summarise(sum_value = sum(!!sym(input$var), na.rm = TRUE), .groups = "drop") |>
            ggplot(aes(x = year, y = sum_value, color = event_type)) +
            geom_line(size = 1.2) +
            scale_y_continuous(labels = scales::label_currency(
                prefix = "$",
                scale_cut = c(0, K = 1e3, M = 1e6, B = 1e9)
            )) +
            theme_light() +
            guides(color = guide_legend(title = "Storm Type")) +
            theme(
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14, face = "bold"),
                axis.text = element_text(size = 12, face = "bold")
            )
    })
}


shinyApp(ui = ui, server = server)



