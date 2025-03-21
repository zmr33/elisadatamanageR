#'
#'                           extract sample ids function
#'
#' This function extracts the sample ids from the 'layout' /plate map table
#
#'
#' this code will need the  keyword for this extraction function to work
#'    'Layout' for the platemap table
#'    if layout is missing (even if the sample ids are present it will mark them as NA)
#'
#'  this key word should be in the first column in the excel file,
#'  the sample ids need to be laid out a certain way for this function to work
#'    an example output can be found at this link: [LINK]
#'
#'  the 25x13 data table (24x12 not counting column/row headers) should start:
#'        3 rows down and 1 column right of the layout keyword
#'
#'
#'  we will call this function in the same for loop as our extract tables function
#'
#'  this will output each plate as a dataframe with 8 rows
#'  we will call the file_list and pull each dataframe per plate into one large list in a for loop
#'  then we'll combine all the individual dataframes into one list after the for loop
#'


#function for extracting sample ids
extract_sample_ids <- function(file_path) {
  #read the excel files
  file1 <- suppressMessages(read_excel(file_path, col_names = FALSE))

  #locate the "Layout" row in the dataframe
  layout_row <- which(file1[[1]] == "Layout")

  #ifelse statement looking for 'layout'
  if (length(layout_row) > 0) {
    #23 rows down, 12 columns wide, starting from "Layout"
    layout_table <- file1[(layout_row + 3):(layout_row + 26), 2:14]

    #convert all columns to character for consistency
    layout_table <- layout_table %>%
      mutate(across(everything(), as.character))

    #rename columns
    colnames(layout_table)[1] <- "rowletter"
    colnames(layout_table)[2:13] <- paste0("C", 1:12)

    #fill down values for the "rowletter", "C1", and "C2" columns
    #these are the standards and controls for each elisa plate
    layout_table <- layout_table %>%
      mutate(
        rowletter = ifelse(row_number() %% 3 == 1, rowletter, NA_character_),  #fill down rowletter
        C1 = ifelse(row_number() %% 3 == 1, C1, NA_character_),                #fill down C1
        C2 = ifelse(row_number() %% 3 == 1, C2, NA_character_)                 #fill down C2
      ) %>%
      tidyr::fill(rowletter, C1, C2, .direction = "down")

    #select every third row starting from row 3 to row 24
    #pull the name from each cell
    selected_rows <- seq(3, 24, by = 3)
    filtered_table <- layout_table[selected_rows, ]

    #add the file name as an identifier
    filtered_table <- filtered_table %>%
      mutate(file_name = basename(file_path), error = NA_character_)

    return(filtered_table)

  } else {
    # Generate detailed warning if 'Layout' is not found
    warning(paste("Error: 'Layout' keyword not found in file:", file_path))
    return(data.frame(
      file_name = basename(file_path),
      error = "   'Layout' keyword not found"
    ))
  }
}
