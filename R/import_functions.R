# Useful functions for importing data

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

so_inverter <- function(data, column) {
    dplyr::case_when(
        grepl("K$", data[[column]]) ~ as.numeric(sub("K$", "", data[[column]])) * 1e3,
        grepl("M$", data[[column]]) ~ as.numeric(sub("M$", "", data[[column]])) * 1e6,
        grepl("B$", data[[column]]) ~ as.numeric(sub("B$", "", data[[column]])) * 1e9,
        TRUE ~ as.numeric(data[[column]])
    )
}
