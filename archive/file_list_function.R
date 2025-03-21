#'
#'
#'               select folder and file path for samples
#'
#'
#' #specify a folder path
#' folderpath <- "path/to/folder"
#'
#'
#'          attempt to maintain one filetype
#'
#' #identify your folder containing elisa excel files
#' file_path <- get_files_by_type(folderpath, file_type = "xlsx")
#'
#' #dentify your folder containing elisa csv if they're not excel files
#' file_path <- get_files_by_type(folderpath, file_type = "csv")
#'


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
