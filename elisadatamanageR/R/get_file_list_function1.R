#'
#'            Select and list data files from a specified folder
#'
#' This function searches for`.xlsx` files in a given folder.
#' It returns the full file paths of all matching files.
#'
#' @param folderpath Character. Path to the folder where your ELISA files are stored.
#'        Example: `"C:/Users/OneDrive - CDC/surveys/country/year_1"`
#'
#' @param file_type Character. File extension to search for.
#'              Only calls excel files, `"xlsx"`, from within selected folder.
#'
#' @return A character vector of full file paths matching the given type.
#'         If no matching files are found, a warning is returned.
#'
#' @examples
#' # Specify the path to your data folder (use quotes!)
#' folderpath <- "C:/Users/youruserid/documents/elisa_data"
#'
#' #get excel files
#' file_list <- get_files(folderpath)  #or
#' file_list <- get_files("C:/Users/youruserid/documents/elisa_data")
#'
#' @export


get_files <- function(folderpath, file_type = "xlsx") {
  #check to ensure the folder path exists
  if (!dir.exists(folderpath)) {
    stop("Error: The specified folder path does not exist.")
  }

  #ensure file_type does not contain extra characters like '.' or wildcards
  file_type <- gsub("^\\.+|[*]", "", file_type) #remove leading dots or wildcards

  #create the regex pattern for the specified file type
  pattern <- paste0("\\.", file_type, "$")  #match files ending with the given extension

  #list files of the specified type
  file_list <- list.files(
    path = folderpath,
    pattern = pattern,
    full.names = TRUE
  )

  #check if any files are found
  if (length(file_list) == 0) {
    warning(paste("No files of type", file_type, "found in the specified folder."))
  }

  return(file_list)
}
