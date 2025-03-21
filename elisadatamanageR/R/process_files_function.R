#'
#'                         process samples function
#'
#' This loop processes each plate using our two previous functions
#' and outputs our data into two dataframes (sample ids and plate data) and one list (error log)
#
#' process_elisa_files(file_list)
#'
#'


process_elisa_files <- function(file_list) {

  #initialize progress bar so its you can see progress when importing files
  pb <- progress_bar$new(
    format = "  Importing files [:bar] :percent in :elapsed",
    total = length(file_list),
    width = 60
  )

  #create lists to store results and error log
  all_data <- list()
  all_sample_ids <- list()
  error_log <- list()

  #process each file in the file_list
  for (i in seq_along(file_list)) {
    tryCatch({
      #extract tables
      table_data <- extract_tables(file_list[i])
      all_data[[i]] <- table_data

      #extract sample ids and track errors
      sample_data <- extract_sample_ids(file_list[i])
      if (!is.na(sample_data$error[1])) {  # Check if the first error value is not NA
        # If error detected, log it
        error_log[[length(error_log) + 1]] <- sample_data
      } else {
        # If no error, store in all_sample_ids
        all_sample_ids[[i]] <- sample_data
      }

      #update progress bar percentage after each file
      pb$tick()

      #print a message after every 10 files
      if (i %% 10 == 0) {
        message(paste(" Imported", i, "plates..."))
      }
    }, error = function(e) {
      #log processing errors (sample ids/layout missing)
      warning(paste("Error processing file:", file_list[i], " - ", e$message))
      error_log[[length(error_log) + 1]] <- list(file = file_list[i], error = e$message)
    })
  }

  #combine all data and sample ids into two separate dataframes
  all_data_combined2 <- bind_rows(all_data[!sapply(all_data, is.null)])
  all_sample_ids_combined2 <- bind_rows(all_sample_ids[!sapply(all_sample_ids, is.null)])

  # Assign outputs directly to the global environment
  assign("all_data_combined2", all_data_combined2, envir = .GlobalEnv)
  assign("all_sample_ids_combined2", all_sample_ids_combined2, envir = .GlobalEnv)
}


