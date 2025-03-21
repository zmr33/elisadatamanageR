#'
#'                     Check Errors in Data Files
#'
#' This function scans files for errors related to missing sample IDs and structure issues.
#' It verifies that each file contains the required keywords (`"Layout"`, `"Blank 450"`, `"[Concentration]"`).
#' If any of these elements are missing, the file is flagged in the error log.
#'
#' @param file_list Character vector. A list of file paths to be processed.
#'
#' @return A dataframe containing an error summary of files with issues.
#'
#' @details
#' - This function first checks for **missing keywords** in each file.
#' - It then scans files for **row structure issues** to detect missing sample IDs.
#' - If **missing sample IDs** are detected, the file is **logged in the error summary**.
#' - A **progress bar** is included to track processing.
#' - Finally, the function returns a **combined error log** with all detected issues.
#'
#' @examples
#' file_list <- list.files(path = "data/", full.names = TRUE)
#' errors_found <- check_errors(file_list)
#'
#' print(errors_found)
#'
#' @export
#'


check_errors <- function(file_list) {

  # Initialize error tracking lists
  structure_results <- list()
  error_log <- list()

  # Display message before scanning begins
  if (length(file_list) > 0) {
    cat("Scanning files for errors...\n")
    flush.console()  # Ensure immediate console output
  }

  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Checking files [:bar] :percent in :elapsed",
    total = length(file_list),
    width = 60
  )

  # Scan each file for missing keywords & structure issues
  for (i in seq_along(file_list)) {
    file <- file_list[i]
    tryCatch({
      # Read the Excel file without column names
      file1 <- suppressMessages(read_excel(file, col_names = FALSE))

      # Clean and standardize the first column to avoid issues with spacing/case
      col1_clean <- tolower(trimws(file1[[1]]))

      # Locate required keywords in the dataframe
      layout_row <- which(col1_clean == "layout")
      table_450_row <- which(col1_clean == "450")
      blank_450_row <- which(col1_clean == "blank 450")
      concentration_row <- which(col1_clean == "[concentration]")

      # Check for missing keywords
      missing_keywords <- c()
      if (length(layout_row) == 0) missing_keywords <- c(missing_keywords, "'Layout'")
      if (length(table_450_row) == 0) missing_keywords <- c(missing_keywords, "'450'")
      if (length(blank_450_row) == 0) missing_keywords <- c(missing_keywords, "'Blank 450'")
      if (length(concentration_row) == 0) missing_keywords <- c(missing_keywords, "'[Concentration]'")

      # If missing any keywords, log the error (without duplicate warnings)
      if (length(missing_keywords) > 0) {
        error_log <- append(error_log, list(data.frame(
          file_name = basename(file),
          error = paste("Missing:", paste(missing_keywords, collapse = ", "))
        )))
      } else {

 #### -----------------------------------------------------------
 #### NEW SECTION: Check if any table region is all NA
 #### -----------------------------------------------------------

        # (1) Blank 450 region check
        if (length(blank_450_row) > 0) {
          blank_450_region <- file1[(blank_450_row + 3):(blank_450_row + 10), 2:14]
          if (all(is.na(blank_450_region))) {
            error_log <- append(error_log, list(data.frame(
              file_name = basename(file),
              error = "Keyword 'Blank 450' found but data region is empty (all NA)."
            )))
          }
        }

        # (2) 450 region check
        if (length(table_450_row) > 0) {
          region_450 <- file1[(table_450_row + 3):(table_450_row + 10), 2:14]
          if (all(is.na(region_450))) {
            error_log <- append(error_log, list(data.frame(
              file_name = basename(file),
              error = "Keyword '450' found but data region is empty (all NA)."
            )))
          }
        }

        # (3) [Concentration] region check
        if (length(concentration_row) > 0) {
          concentration_region <- file1[(concentration_row + 3):(concentration_row + 10), 2:14]
          if (all(is.na(concentration_region))) {
            error_log <- append(error_log, list(data.frame(
              file_name = basename(file),
              error = "Keyword '[Concentration]' found but data region is empty (all NA)."
            )))
          }
        }

        # (4) You can do a Layout region check if you need to:
        # For example, if "Layout" must have data in the next 8 rows x 13 columns.
        # if (length(layout_row) > 0) {
        #   layout_region <- file1[(layout_row + 3):(layout_row + 10), 2:14]
        #   if (all(is.na(layout_region))) {
        #     error_log[[length(error_log) + 1]] <- data.frame(
        #       file_name = basename(file),
        #       error = "Keyword 'Layout' found but data region is empty (all NA)."
        #     )
        #   }
        # }

  #### END NEW SECTION
  #### -----------------------------------------------------------

        # If you want to continue checking for sample IDs only if the region checks passed,
        # you could detect if the file was flagged in error_log above. For simplicity, we'll
        # always run detect_row_structure() below.

        # Now check for missing sample IDs
        # (This is your existing logic to detect structure issues.)
        # Now check for missing sample IDs
        structure <- detect_row_structure(file)

        # Store structure result
        structure_results[[basename(file)]] <- ifelse(
          structure == 2,
          "Missing sample ids",
          "Contains sample ids"
        )
      }

      # Update progress bar
      pb$tick()

    }, error = function(e) {
      # Log general errors (without printing message in console)
      error_log <- append(error_log, list(data.frame(
        file_name = basename(file),
        error = conditionMessage(e)
      )))
    })
  }

  # Identify files with missing sample IDs
  missing_sample_ids <- names(structure_results)[structure_results == "Missing sample ids"]

  # Create a summary of missing sample ID errors
  error_summary_sample_ids <- if (length(missing_sample_ids) > 0) {
    data.frame(
      file_name = missing_sample_ids,
      error = "Missing sample ids"
    )
  } else {
    data.frame(file_name = character(), error = character())  # Empty case handling
  }

  # Ensure all errors are combined correctly
  if (length(error_log) > 0) {
    error_log_df <- do.call(rbind, error_log)
  } else {
    error_log_df <- data.frame(file_name = character(), error = character())  # Empty case
  }

  # Merge error logs
  combined_error_summary <- rbind(error_log_df, error_summary_sample_ids)

  # Display clean error summary without redundant messages
  if (nrow(combined_error_summary) > 0) {
    cat("\n ---   ---   ---   ---   ERROR SUMMARY   ---   ---   ---   --- \n\n")
    print(combined_error_summary, row.names = FALSE)
    message("\nErrors were detected in file processing. Fix the errors in the source files
            and re-run the check_errors() function before proceeding. \n
            \nFiles present in the error summary will be filtered out by the extraction functions.")
  } else {
    message("\nNo errors found!")
  }

  return(combined_error_summary)
}
