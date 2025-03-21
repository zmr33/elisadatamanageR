#'
#'                          Format and Clean ELISA Data
#'
#' This function renames columns, cleans numeric values, and standardizes ELISA plate data.
#'
#' @param all_data_combined Dataframe. The raw ELISA data.
#'
#' @return A cleaned dataframe with numeric values properly formatted.
#'
#' @examples
#' formatted_data <- format_elisa_data(all_data_combined)
#' head(formatted_data)
#'
#' @export
#'


format_elisa_data <- function(all_data_combined) {
  new_column_names <- c("Row_Letter", paste0("column_", 1:12))

  formatted_data <- all_data_combined %>%
    rename_with(~ new_column_names, .cols = 1:13) %>%
    select(plate_number, date, table_id, everything()) %>%

    # Filter for numeric data only
    filter(table_id %in% c("450", "Blank 450", "Concentration")) %>%

    # Remove invalid characters (<, >, OVRFLW, ????)
    mutate(across(starts_with("column_"),
                  ~ ifelse(grepl("^[<>]", .), gsub("^[<>]", "", .),
                           ifelse(. == "OVRFLW" | grepl("\\?\\?\\?\\?\\?", .), NA, .)),
                  .names = "cleaned_{col}")) %>%

    # Remove '*' symbols before numeric conversion
    mutate(across(starts_with("cleaned_"),
                  ~ gsub("\\*", "", .)))  # Remove asterisks

  # Count NAs before conversion
  na_before <- sum(is.na(formatted_data))

  # Convert cleaned columns to numeric, suppressing warnings
  formatted_data <- formatted_data %>%
    mutate(across(starts_with("cleaned_"), ~ suppressWarnings(round(as.numeric(.), 4))))

  # Count NAs after conversion
  na_after <- sum(is.na(formatted_data))

  # Extract rows where new NAs were introduced
  na_rows <- formatted_data %>%
    filter(if_any(starts_with("cleaned_"), is.na)) # Select rows where any cleaned column has NA

  # If new NAs are introduced, print a warning and assign the NA rows to a new dataframe
  if (na_after > na_before) {
    message("Warning: ", na_after - na_before, " new NAs introduced during numeric conversion.")
    assign("na_rows_dataframe", na_rows, envir = .GlobalEnv) # Store NA rows in global environment
    message("A new dataframe `na_rows_dataframe` has been created for review.")
  }

  formatted_data <- formatted_data %>%
    rename(row_letter = Row_Letter) %>%
    select(-starts_with('column_')) %>%
    rename_with(~ gsub("cleaned_", "", .), starts_with("cleaned_"))

  return(formatted_data)
}
