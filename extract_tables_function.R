#'
#'                           extract tables function
#'
#' This function extracts the tables (450, blank 450, concentration) from the excel file output
#' We have a separate function that selects sample ids from the plate map table
#'
#' you need to manually set your folder_path (where your files are) and file_type (.xlsx)
#' make sure you close all the excel files from the selected folder path!
#'
#' make sure your files are named numerically and the first underscore is after the numbers,
#'  in this case:
#'           'Plate 001_date_techinitials.xlsx'
#'           'Plate 002_date_techinitials.xlsx'
#'
#'
#' folderpath <- "C:/Users/tti2/OneDrive - CDC/Data Cleaning/ELISA Data Management/uganda"
#' file_list <- list.files(path = folderpath, pattern = "*.xlsx", full.names = TRUE)
#'
#' each file will need three keywords for this extraction function to work
#'    '450' - for the 450 table
#'    'Blank 450' - for the BLank 450 table, which we might not actually need?
#'    '[Concentration]' - for the conc table, which we definitely need
#'
#'  these three key words should all be in the first column in the excel file,
#'  an example output can be found at this link: [LINK]
#'
#'  the 8x12 data table (9x13 counting column/row headers) should start:
#'        3 rows down and 1 column right of each keyword
#'        in the case of the 450 table, the 'Actual Temperature'
#'         can be present or absent, up to user
#'
#'
#'  this will output each plate as a dataframe with 24 rows
#'  we will call the file_list and pull each dataframe per plate into one large list in a for loop
#'  then we'll combine all the individual dataframes into one list after the for loop


#extract the four tables from each excel file
extract_tables <- function(file) {

  #extract plate number and date from the filename -- ex. filename: Plate 001_230724_MMC.xlsx
  file_name <- basename(file)
  plate_number <- str_extract(file_name, "^[^_]+")        #extracts the text before the first underscore
                # str_extract(file_name, "^[^_]+_[^_]+(?=_)")  #extracts the text before the 2nd underscore (doesn't include underscore)

  date <- str_extract(file_name, "(?<=[_-])[^_-]+(?=[_-])")  #extract date between first two underscores (or dashes)
       #  str_extract(file_name, "(?<=^[^_-]+[-_][^_-]+[-_])[^_-]+")  #extracts date between 2nd and 3rd underscore (or dash)

  #read the whole excel sheet into a data frame
  sheet_data <- suppressMessages(read_excel(file, col_names = FALSE)) #suppress the messages for column naming / dont treat first row as names

  #random filler naming here to remove the new naming message cause no column names
  colnames(sheet_data) <- paste0("Column", seq_len(ncol(sheet_data)))


  #search for "450", and extract the 13x9 table
  table2_start_row <- which(sheet_data[[1]] == "450")
  if (length(table2_start_row) > 0) {
    #check if "Actual Temperature" is in the row immediately below "450"
    next_row <- sheet_data[[1]][table2_start_row + 1]

    if (!is.na(next_row) && next_row == "Actual Temperature:") {
      #use the 4 down over 1 offset when "Actual Temperature" is found
      table2 <- sheet_data[(table2_start_row + 4):(table2_start_row + 11), 2:14]
    } else {
      #use the 3 down over 1 offset when "Actual Temperature" is not found
      table2 <- sheet_data[(table2_start_row + 3):(table2_start_row + 10), 2:14]
    }
    table2 <- table2 %>%
      mutate(source_file = file_name, table_id = "450", plate_number = plate_number, date = date)
  } else {
    table2 <- NULL
  }

  #search for "Blank 450", and extract the 13x9 table, down 3 over 1
  table3_start_row <- which(sheet_data[[1]] == "Blank 450")
  if (length(table3_start_row) > 0) {
    table3 <- sheet_data[(table3_start_row + 3):(table3_start_row + 10), 2:14]
    table3 <- table3 %>%
      mutate(source_file = file_name, table_id = "Blank 450", plate_number = plate_number, date = date)
  } else {
    table3 <- NULL
  }

  #search for "[Concentration]", and extract the 13x9 table, down 3 over 1
  table4_start_row <- which(sheet_data[[1]] == "[Concentration]")
  if (length(table4_start_row) > 0) {
    table4 <- sheet_data[(table4_start_row + 3):(table4_start_row + 10), 2:14]
    table4 <- table4 %>%
      mutate(source_file = file_name, table_id = "Concentration", plate_number = plate_number, date = date)
  } else {
    table4 <- NULL
  }

  #combine all the tables into a single dataframe
  combined_tables <- bind_rows(
    #table1, #was originally the platemap table
    table2,
    table3,
    table4)

  return(combined_tables)
}

