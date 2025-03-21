#'
#'                        generate a dynamic save file name
#'
#' This function generates a save file name that includes the country,
#' study purpose, the current date (MMDDYYYY format), and an optional suffix.
#' It ensures that required inputs are provided and removes invalid characters.
#'
#' @param country Character. The country where the study is conducted (spaces will be replaced with `_`).
#' @param purpose Character. The purpose of the study (spaces will be replaced with `_`).
#' @param other Character. (Optional) Additional suffix for the file name (e.g., "_Batch1"). Default is "".
#'
#' @return A character string with the formatted file name.
#'
#' @details
#' - The function ensures that `country` and `purpose` are required.
#' - **Spaces in `country`, `purpose`, and `other` are automatically replaced with underscores (`_`)**.
#' - **The current date (`MMDDYYYY`) is automatically appended** to the filename.
#' - The `other` argument is optional and can be left blank.
#' - The final file name format is: `"Country_Purpose_MMDDYYYY[Other].xlsx"`.
#' - Any invalid characters (`\ / : * ? " < > |`) are **automatically removed**.
#'
#' @examples
#' # Generate a standard file name
#' generate_file_name("Uganda", "OV16 Survey")
#' # Output: "Uganda_OV16_Survey_03122025.xlsx"
#'
#' # Generate a file name with an additional suffix
#' generate_file_name("Kenya", "ELISA Study", "_Batch1")
#' # Output: "Kenya_ELISA_Study_03122025_Batch1.xlsx"
#'
#' # Attempting to call the function without required arguments will return an error
#' \dontrun{
#' generate_file_name()
#' # Error: 'country' and 'purpose' are required arguments.
#' }
#'
#' @export
#'


generate_file_name <- function(country, purpose, other = "") {
  # Function to remove invalid characters and replace spaces with underscores
  sanitize_file_name <- function(name) {
    name <- gsub(" ", "_", name)  # Replace spaces with underscores
    name <- gsub("[\\\\/:*?\"<>|]", "", name)  # Remove invalid characters
    return(name)
  }

  # Ensure inputs are character strings
  if (!is.character(country) | !is.character(purpose) | !is.character(other)) {
    stop("Error: 'country', 'purpose', and 'other' must be character strings.")
  }

  # Format the current date as MMDDYYYY
  current_date <- format(Sys.Date(), "%m%d%Y")

  # Sanitize inputs
  country <- sanitize_file_name(country)
  purpose <- sanitize_file_name(purpose)
  other <- sanitize_file_name(other)

  # Construct file name
  file_name <- paste0(country, "_", purpose, "_", current_date, other, ".xlsx")

  return(file_name)
}
