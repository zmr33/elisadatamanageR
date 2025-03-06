#'
#'                          detect row structure function
#'
#' This function searches the files (file_path) to see if sample ids were not included/missing per plate
#' it calculates the distance between the 'Layout' row and the '450' row
#'    if the distance is too close together, we know there are no sample ids
#'    and it gives a value of 2, instead of 3 (we expect three data-rows per plate-row)
#'
#' '
#############      ###############

#function to detect row structure
detect_row_structure <- function(file_path) {
  #read the excel file
  file_data <- suppressMessages(read_excel(file_path, col_names = FALSE))

  #locate the "Layout" and "450" rows
  layout_row <- which(file_data[[1]] == "Layout")
  table_450_row <- which(file_data[[1]] == "450")

  if (length(table_450_row) == 0) {
    stop("Error: '450' keyword not found")
  }

  #use the first occurrence of each
  layout_row <- layout_row[1]
  table_450_row <- table_450_row[1]

  #calculate the number of rows between "Layout" and "450"
  rows_between <- table_450_row - layout_row

  #determine row structure
  if (rows_between > 25) {
    return(3)  #3 rows per logical row (sample IDs present)
  } else {
    return(2)  #2 rows per logical row (no sample IDs)
  }
}
