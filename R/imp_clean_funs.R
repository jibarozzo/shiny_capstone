# Functions for importing and cleaning data for "shiny_captone" project

read_data <- function(x, pattern) {
    # Creating a list of files
    list.files(
        path = x,
        pattern = pattern,
        full.names = TRUE,
        recursive = FALSE
    ) %>%
        # Reading and binding list of files into an R object
        vroom(.)
}

# Modified from https://stackoverflow.com/questions/63601709/is-there-a-way-to-convert-1-00m-to-1-000-000
# This function creates a named number vector FYI
convert_str_num <- function(data, column) {
    suffs <- c(K=1e3, M=1e6, B=1e9)
    
    nc <- nchar(data[[column]])
    suff <- substr(data[[column]],nc,nc) ## select last char
    suffval <- suffs[suff]  ## find corresponding numeric value
    num <- as.numeric(substr(data[[column]],1,nc-1))  ## select all but last char
    num*suffval
}

# String to number formatter (e.g., "1M" to 1000000)
str_num_formatter <- function(data, column) {
    dplyr::case_when(
        grepl("K$", data[[column]]) ~ as.numeric(sub("K$", "", data[[column]])) * 1e3,
        grepl("M$", data[[column]]) ~ as.numeric(sub("M$", "", data[[column]])) * 1e6,
        grepl("B$", data[[column]]) ~ as.numeric(sub("B$", "", data[[column]])) * 1e9,
        TRUE ~ as.numeric(data[[column]])
    )
}

num_str_formatter <- function(x) {
    dplyr::case_when(
        x < 1e3 ~ as.character(x),
        x < 1e6 ~ paste0(as.character(x/1e3), "K"),
        x < 1e9 ~ paste0(as.character(x/1e6), "M"),
        x < 1e12 ~ paste0(as.character(x/1e9), "B"),
        TRUE ~ as.numeric(x)
    )
}


# Summarizing damage and fatalities
summarize_effects <- function(data, .groups, var){
    data %>%
        group_by(across({{ .groups }})) %>% # Should convert to variables in future
        summarize("sum_{{ var }}" := sum({{ var }}, na.rm = TRUE), .groups = "drop")
}

# Plotting damages and fatalities
storm_damages_ggplot <- function(df, y) {
    .data <- df %>%
        summarize_effects(.groups = c(year, event_type), var = {{ y }})
    
    # Generate the new column name dynamically using glue
    col_name <- glue::glue("sum_{deparse(substitute(y))}")
    
    # Use !!sym() to refer to the new dynamic column
    .data %>%
        ggplot(aes(
            x = year,
            y = !!sym(col_name),
            colour = event_type
        )) +
    geom_line() +
    scale_y_continuous(labels = scales::label_currency(
        prefix = "$",
        scale_cut = c(0, K = 1e3, M = 1e6, B = 1e9)
    )) +
    theme_light()
}
