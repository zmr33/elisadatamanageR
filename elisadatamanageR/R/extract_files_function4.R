#'
#'                  extract data from multiple files (w/ progress bar)
#'
#' This function extracts tables and sample IDs from valid files and automatically
#' assigns them to named variables in the global environment.
#'
#' @param file_list Character vector. A list of file paths to be processed.
#' @param error_summary Dataframe. The output from `check_errors(file_list)`, containing files with errors.
#'
#' @return None. The extracted tables and sample IDs are assigned as separate dataframes.
#'
#' @examples
#' file_list <- list.files(path = "data/", full.names = TRUE)
#' error_summary <- check_errors(file_list)
#' extract_files(file_list, error_summary)
#'
#' @export
#'


extract_files <- function(file_list, error_summary) {
  # Ensure error_summary is provided
  if (missing(error_summary)) {
    stop("You must run `check_errors(file_list)` first and pass `error_summary`.")
  }

  # Step 1: Identify and remove files with errors
  error_files <- basename(error_summary$file_name)
  valid_files <- file_list[!(basename(file_list) %in% error_files)]

  # Step 2: Calculate number of filtered files
  num_filtered_out <- length(file_list) - length(valid_files)

  # Step 3: Check if there are valid files left
  if (length(valid_files) == 0) {
    stop("All files contained errors. No valid files to process.")
  } else {
    cat("Processing", length(valid_files), "valid files...\n")

    # Singular vs. Plural file message cause im like that
    if (num_filtered_out == 1) {
      cat(num_filtered_out, "file was filtered out due to errors. You might want to check on that.\n")
    } else if (num_filtered_out > 1) {
      cat(num_filtered_out, "files were filtered out due to errors. You might want to check on that.\n")
    }
  }

  # Step 4: Initialize storage for extracted data
  all_data <- list()
  all_sample_ids <- list()

  # Step 5: Progress bar for extraction
  pb <- progress_bar$new(
    format = "  Extracting data [:bar] :percent in :elapsed",
    total = length(valid_files),
    width = 60
  )

  # Step 6: Extract data from each valid file
  for (i in seq_along(valid_files)) {
    tryCatch({
      # Extract table data
      table_data <- extract_tables(valid_files[i])
      all_data[[i]] <- table_data

      # Extract sample IDs
      sample_data <- extract_sample_ids(valid_files[i])
      all_sample_ids[[i]] <- sample_data

      # Update progress bar
      pb$tick()
    }, error = function(e) {
      warning(paste("Error processing file:", valid_files[i], " - ", e$message))
    })
  }

  # Step 7: Combine extracted data into final dataframes
  all_data_combined <- bind_rows(all_data[!sapply(all_data, is.null)])
  all_sample_ids_combined <- bind_rows(all_sample_ids[!sapply(all_sample_ids, is.null)])

  # Step 8: Assign the extracted data to global environment
  assign("all_data_combined", all_data_combined, envir = .GlobalEnv)
  assign("all_sample_ids_combined", all_sample_ids_combined, envir = .GlobalEnv)

  # Step 9: Message confirming the export
  message("\n Data saved as 'all_data_combined' and 'all_sample_ids_combined' in the environment.")

  return(list(
    all_data_combined = all_data_combined,
    all_sample_ids_combined = all_sample_ids_combined
  ))

}
