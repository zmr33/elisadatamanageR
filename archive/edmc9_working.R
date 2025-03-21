#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("progress")
#install.packages("plotly)
#install.packages("lubridate")

#highlight and run these
#load packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(progress)
library(plotly)
library(lubridate)

#replace tti2 with your cdc user id
setwd("C:/Users/tti2/OneDrive - CDC/ELISA Data Management")


#############                     pulling the files               ##############

#path to the folder with the data in it
folderpath <- "C:/Users/tti2/OneDrive - CDC/ELISA Data Management/uganda"

#pulling just the excel files out of that data folder
file_list <- list.files(path = folderpath, pattern = "*.xlsx", full.names = TRUE)

###########       add data and country of study for your file output    ########

#dont change
current_date <- format(Sys.Date(), "%m%d%Y")  #gets the current date in YYYYMMDD format

#change (LEAVE THE QUOTATION MARKS)
country <- "Uganda"                           #replace with country of study
purpose <- "OV16_control_tracking"            #add study purpose?
other   <- ""                                 #add additional info for file name here, defaults to blank

#dont change
file_name <- paste0(country, "_", purpose, "_", current_date, other, ".xlsx")

#will save the file like: Uganda_OV16_control_tracking_03122025.xlsx
#it will overwrite a file with the same name and date

###########         extracting the tables from each excel file     ############

#close all the excel files from the selected folder

#extract the four tables from each excel file
extract_tables <- function(file) {

  #extract plate number and date from the filename -- ex. filename: Plate 001_230724_MMC.xlsx
  file_name <- basename(file)
  plate_number <- str_extract(file_name, "^[^_]+")        #extracts the part before the first underscore
  date <- str_extract(file_name, "(?<=[_-])[^_-]+(?=[_-])")  #extract part between the two underscores

  #read the whole sheet into a data frame
  sheet_data <- suppressMessages(read_excel(file, col_names = FALSE)) #suppress the messages for column naming

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
    #table1,
    table2,
    table3,
    table4)

  return(combined_tables)
}


##############            function to call sample ids               ##############

#new function for extracting sample ids
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

#################################################################################



#################################################################################

##############          extract data (use functions above)          ##############

#apply the extract_tables function to all files in the folder
#add progress bar to show file processing cause it takes a while


# Initialize progress bar and error log
pb <- progress_bar$new(
  format = "  Importing files [:bar] :percent in :elapsed",
  total = length(file_list),
  width = 60
)

# Initialize lists to store results and error log
all_data <- list()
all_sample_ids <- list()
error_log <- list()  # For storing errors

# Apply functions to all files with progress bar
for (i in seq_along(file_list)) {
  tryCatch({
    # Extract other tables
    table_data <- extract_tables(file_list[i])
    all_data[[i]] <- table_data

    # Extract sample IDs and track errors
    sample_data <- extract_sample_ids(file_list[i])
    if (!is.na(sample_data$error[1])) {  # Check if the first error value is not NA
      # If error detected, log it
      error_log[[length(error_log) + 1]] <- sample_data
    } else {
      # If no error, store in all_sample_ids
      all_sample_ids[[i]] <- sample_data
    }

    # Update progress bar after each file
    pb$tick()

    # Print a message after every 10 files
    if (i %% 10 == 0) {
      message(paste(" Imported", i, "plates..."))
    }
  }, error = function(e) {
    # Log general processing errors
    warning(paste("Error processing file:", file_list[i], " - ", e$message))
    error_log[[length(error_log) + 1]] <- list(file = file_list[i], error = e$message)
  })
}

# Combine all data and sample IDs into two separate dataframes
all_data_combined1 <- bind_rows(all_data[!sapply(all_data, is.null)])
all_sample_ids_combined1 <- bind_rows(all_sample_ids[!sapply(all_sample_ids, is.null)])


#############      ###############

# Function to detect row structure
detect_row_structure <- function(file_path) {
  # Read the Excel file
  file_data <- suppressMessages(read_excel(file_path, col_names = FALSE))

  # Locate the "Layout" and "450" rows
  layout_row <- which(file_data[[1]] == "Layout")
  table_450_row <- which(file_data[[1]] == "450")

  if (length(table_450_row) == 0) {
    stop("Error: '450' keyword not found")
  }

  # Use the first occurrence of "Layout" and "450"
  layout_row <- layout_row[1]
  table_450_row <- table_450_row[1]

  # Calculate the number of rows between "Layout" and "450"
  rows_between <- table_450_row - layout_row

  # Determine row structure
  if (rows_between > 25) {
    return(3)  # 3 rows per logical row (sample IDs present)
  } else {
    return(2)  # 2 rows per logical row (no sample IDs)
  }
}


##############          generate error summary and save report          ##############


# Combine error log into a dataframe and display it
if (length(error_log) > 0) {
  error_summary <- do.call(rbind, lapply(error_log, as.data.frame))
} else {
  message("\nNo layout errors found!")
  error_summary <- do.call(rbind, lapply(error_log, as.data.frame))
}


layout_errors <- list()
structure_results <- list()

# Assume error_summary is already defined and contains error messages
error_files <- error_summary$file_name

# List of all files to process
all_files <- file_list

# Filter out files that have errors (those present in error_files)
valid_files <- all_files[!(basename(all_files) %in% error_files)]

#########################                   ###############################

message_shown <- FALSE

# Loop through files and check for Layout and Row Structure
for (file in valid_files) {
  # Display the "Please wait" message once at the start of the loop
  if (!message_shown) {
    cat("Please wait ~20 seconds while the files are scanned for errors...\n")
    flush.console()  # Ensure the message is printed immediately
    message_shown <- TRUE  # Set flag to TRUE after showing the message
  }

  tryCatch({
    # Detect row structure
    structure <- detect_row_structure(file)

    # Store structure result (can be customized)
    structure_results[[basename(file)]] <- ifelse(
      structure == 2,
      "Missing Sample IDs",
      "Contains Sample IDs"
    )
  }, error = function(e) {
    # Log errors related to row structure
    layout_errors[[basename(file)]] <- conditionMessage(e)
    message(paste("Error detected in file", basename(file), ":", conditionMessage(e)))
  })
}



# Identify the files that are missing sample IDs (from structure results)
missing_sample_ids <- names(structure_results)[structure_results == "Missing Sample IDs"]

# Create error summary for missing sample IDs
error_summary_2 <- data.frame(file_name = missing_sample_ids, stringsAsFactors = FALSE) %>%
  mutate(error = 'Missing sample ids')

# Combine the two error summaries
combined_error_summary <- rbind(error_summary, error_summary_2)

# Combine error log into a dataframe and display it
if (nrow(combined_error_summary) > 0) {
  cat("\n ---   ---   ---   ---   Error Summary   ---   ---   ---   --- \n")
  cat("\n")
  print(combined_error_summary)
  message("\nErrors were detected in file processing, \nFix the errors in the source files before proceeding.")
} else {
  message("\nNo errors found!")
}



##################################################################################



                          #####   ######    #####   ######
                         ##   ##  # ## #   ##   ##   ##  ##
                         #          ##     ##   ##   ##  ##
                          #####     ##     ##   ##   #####
                              ##    ##     ##   ##   ##
                         ##   ##    ##     ##   ##   ##
                          #####    ####     #####   ####


# check the error log

##################################################################################


#     run the all the code below this after checking the error log
#     identify the file and fix the error in source file


##################################################################################


#filter out the files in file_name in combined_error_summary if we want to keep the code running

files_with_errors <- unique(combined_error_summary$file_name)

all_data_combined1 <- all_data_combined1 %>%
  rename(file_name = source_file) %>%
  filter(!file_name %in% files_with_errors) %>%
  rename(source_file = file_name)

# Filter out the same file names from all_sample_ids_combined1
all_sample_ids_combined1 <- all_sample_ids_combined1 %>%
  filter(!file_name %in% files_with_errors)



##############          formatting sample id column                 ##############

all_sample_ids_long1 <- all_sample_ids_combined1 %>%
  #extract plate number from file name
  mutate(
    plate_number = str_extract(file_name, "Plate \\d+") %>%  #extract 'Plate XX' from file_name
      str_extract("\\d+") %>%  #extract only the digits after 'plate '
      as.integer()
  ) %>%

  #pivot the dataframe longer to create a column of Sample IDs
  pivot_longer(
    cols = starts_with("C"),
    names_to = "column",
    values_to = "sample_id"
  ) %>%

  #numeric version of the column name for sorting
  mutate(
    column_num = str_extract(column, "\\d+") %>% as.integer()  # Extract the numeric part of the column name and convert it to integer
  )


all_sample_ids_long2 <- all_sample_ids_long1 %>%
  group_by(plate_number) %>%  #group by plate number to handle each plate separately
  arrange(plate_number, column_num, rowletter) %>%  #arrange by plate, then by numeric column number, then by row (A-H)
  mutate(
    sample_number = row_number() + (plate_number - 1) * 96  #assign sequential numbering per plate
  ) %>%
  #create a "pair" column to identify pairs of wells (replicates)
  mutate(
    pair = case_when(
      column_num %% 2 == 1 ~ paste0("C", column_num, "_C", column_num + 1),  #if column is odd, create pair name
      column_num %% 2 == 0 ~ paste0("C", column_num - 1, "_C", column_num)   #if column is even, refer back to previous column
    )
  ) %>%
  ungroup() %>%

  #pivot the dataframe wider to combine paired wells into one row per pair
  pivot_wider(
    names_from = column,  #use the original column names to create new columns for paired data
    values_from = sample_id,  #values to spread into new columns
    names_prefix = "replicate_"  #distinguish replicates, e.g., replicate_C1, replicate_C2
  ) %>%

  #select and rename columns
  mutate(
    replicate_1 = coalesce(replicate_C1, replicate_C3, replicate_C5, replicate_C7, replicate_C9, replicate_C11),
    replicate_2 = coalesce(replicate_C2, replicate_C4, replicate_C6, replicate_C8, replicate_C10, replicate_C12)
  ) %>%
  select(-starts_with("replicate_C")) %>%  #remove redundant columns
  rename(sample_id = replicate_1)

#merge the two columns together, fixing the NAs
all_sample_ids_combined_short <- all_sample_ids_long2 %>%
  #group by plate_number, pair, and rowletter to combine information
  group_by(plate_number, rowletter, pair) %>%
  summarize(
    #for sample_id, take the first non-NA value
    sample_id = coalesce(first(na.omit(sample_id)), first(na.omit(replicate_2))),
    #keep the sample number for each row letter
    sample_number = first(sample_number),
    #add other fields if necessary, use summarizing strategy similar to above
    file_name = first(file_name)
  ) %>%
  ungroup()

#filter out constants
all_sample_ids_filtered <- all_sample_ids_combined_short %>%
  filter(
    !(sample_id %in% c("BLK", "STD1", "STD2", "STD3", "STD4", "STD5", "STD6", "STD7",
                       "Med Positive", "High Negative", "Lyo 1",
                       "positive 10ng", "negative 2.5ng", "Lyophilized"))
  )

#sort the dataset by the existing sample_number in ascending order
all_sample_ids_sorted <- all_sample_ids_filtered %>%
  arrange(sample_number)

#create a new column for sample_number_new with a sequential value starting from 1
idsr <- all_sample_ids_sorted %>%
  mutate(sample_number_new = row_number()) %>%
  #reorder columns to bring sample_number_new to the front
  select(plate_number, rowletter, pair, sample_number_new, everything()) %>%
  select(-sample_number) %>%
  rename(sample_number = sample_number_new)





##############                    renaming columns                  ##############

#new column names for easier reference
new_column_names <- c("Row_Letter", paste0("Column ", 1:12))

#rename the ELISA columns in `all_data`
all_data1 <- all_data_combined1 %>%
  rename_with(~ new_column_names, .cols = 1:13) %>%
  select(plate_number, date, table_id, everything())

#check the renamed data
print(all_data1)

##############         separate data into two dataframes                ##############

numeric_data <- all_data1 %>%
  filter(table_id %in% c("450", "Blank 450", "Concentration"))


########################################


#remove leading > or < symbols,  "<0.000" to "0.000" and then
#convert to numeric and try to round the data values so its a little more manageable

numeric_data1 <- numeric_data %>%
  mutate(across(starts_with("Column "),
                ~ ifelse(grepl("^[<>]", .), gsub("^[<>]", "", .),   #remove < or > symbols
                         ifelse(. == "OVRFLW" | grepl("\\?\\?\\?\\?\\?", .), NA, .)),   #handle OVRFLW and ?????
                .names = "cleaned_{col}")) %>%
  mutate(across(starts_with("cleaned_"),
                ~ round(as.numeric(.), 4)))


##############      convert to numeric and round data to 4 decimal places       ##############

convert_and_round <- function(data) {
  data %>%
    mutate(across(
      starts_with("cleaned_"),  #apply to cleaned columns
      ~ ifelse(table_id %in% c("450", "Blank 450", "Concentration"),
               round(as.numeric(.), 4),  #convert to numeric and round to 4 decimal places
               .)
    ))
}

numeric_data2 <- numeric_data1 %>%
  convert_and_round()

numeric_data3 <- numeric_data2 %>%
  rename(row_letter = Row_Letter) %>%
  select(-starts_with('Column ')) %>%
  rename(
    C1 = `cleaned_Column 1`,
    C2 = `cleaned_Column 2`,
    C3 = `cleaned_Column 3`,
    C4 = `cleaned_Column 4`,
    C5 = `cleaned_Column 5`,
    C6 = `cleaned_Column 6`,
    C7 = `cleaned_Column 7`,
    C8 = `cleaned_Column 8`,
    C9 = `cleaned_Column 9`,
    C10 = `cleaned_Column 10`,
    C11 = `cleaned_Column 11`,
    C12 = `cleaned_Column 12`
  )

###############################################################################


#control tracking time


###############################################################################

#select our controls
controls <- numeric_data3 %>%
  filter(row_letter %in% c('A', 'B', 'C', "H")) %>%
  filter(table_id == 'Concentration') %>%
  select(plate_number, date, source_file, row_letter, C1, C2, `C3`, `C4`)

#select our blank values from the 450 table
blanks <- numeric_data3 %>%
  filter(row_letter == 'A') %>%  #select rows with 'A'
  filter(table_id == '450') %>%  #filter for the "450" table
  select(plate_number, date, source_file, row_letter, `C1`, `C2`) %>%
  rename(C3 = "C1",
         C4 = "C2") %>%
  mutate(control_value = "Blank",
         avg_conc = round((C3 + C4) / 2, 4),
         control_range = case_when(
           avg_conc > .11 ~ "above range",
           TRUE ~ "in-range"
         ),
         flag = if_else(control_range != "in-range", "***", "")
  ) %>%
  ungroup() #%>%
#select(-row_letter,-table_id)

#rename and set control ranges
controls1 <- controls %>%
  mutate(control_value = case_when(
    row_letter == "A" ~ "Medium Positive Control",
    row_letter == "B" ~ "High Negative Control",
    row_letter == "C" ~ "CTL 3?",
    row_letter == "H" ~ "Standard 7",
    TRUE ~ NA_character_
  )) %>%
  mutate(
    avg_conc = case_when(
      control_value == "Standard 7" ~ round((C1 + C2) / 2, 4),  #use C1 and C2 for Standard 7
      TRUE ~ round((C3 + C4) / 2, 4)  #use C3 and C4 for all other controls
    ),
    control_range = case_when(
      control_value == "Medium Positive Control" & (avg_conc < 7) ~ "below range",
      control_value == "Medium Positive Control" & (avg_conc > 15) ~ "above range",
      control_value == "High Negative Control" & (avg_conc < 1.5) ~ "below range",
      control_value == "High Negative Control" & (avg_conc > 4.3) ~ "above range",
      control_value == "Standard 7" & (avg_conc > 1.7) ~ "above range",
      TRUE ~ "in-range"
    ),
    flag = if_else(control_range != "in-range", "***", "")
  ) %>%
  ungroup()
#select(-row_letter,-table_id)


#combine our blank data and control data
controls_combined <- bind_rows(controls1, blanks) %>%
  mutate(
    replicate_1 = case_when(
      control_value == "Standard 7" ~ C1,  #use C1 for Standard 7
      TRUE ~ C3  #use C3 for other controls
    ),
    replicate_2 = case_when(
      control_value == "Standard 7" ~ C2,  #use C2 for Standard 7
      TRUE ~ C4  #use C4 for other controls
    )
  ) %>%
  arrange(
    plate_number, source_file,
    factor(control_value, levels = c("Blank", "Medium Positive Control", "High Negative Control", "CTL 3?", "Standard 7"))
  ) %>%
  select(-date, -C1, -C2, -C3, -C4) %>%  #remove original columns
  select(plate_number, source_file, control_value, replicate_1, replicate_2, avg_conc, control_range, flag)


###############################################################################


#plate data tracking


###############################################################################

#filter for conc data
plate_data <- numeric_data3 %>%
  filter(table_id == 'Concentration') %>%
  select(-C1, -C2) %>%
  mutate(
    #set C3 and C4 to NA for rows with row_letter A to C
    #this is where our controls would be
    C3 = if_else(row_letter %in% c('A', 'B', 'C'), NA_real_, C3),
    C4 = if_else(row_letter %in% c('A', 'B', 'C'), NA_real_, C4)
  )

###############################

#reshape from wide to long
test1<- plate_data %>%
  pivot_longer(
    cols = C3:C12,  #specify the columns to pivot (C3 to C12)
    names_to = "well",  #new column that will store the names of C3 to C12
    values_to = "value"  #new column that will store the values from C3 to C12
  )

#creating a pair vairable
test2 <- test1 %>%
  mutate(
    pair = case_when(
      well %in% c("C3", "C4") ~ "C3_C4",
      well %in% c("C5", "C6") ~ "C5_C6",
      well %in% c("C7", "C8") ~ "C7_C8",
      well %in% c("C9", "C10") ~ "C9_C10",
      well %in% c("C11", "C12") ~ "C11_C12"
    )
  ) %>%
  rename(row = row_letter) %>%
  select( plate_number, date, table_id, source_file, row, well, pair, value)

#renaming our individual wells to replicate 1 and replicate 2 for each pair
test3 <- test2 %>%
  mutate(
    replicate = case_when(
      well %in% c("C3", "C5", "C7", "C9", "C11") ~ "Replicate_1",
      well %in% c("C4", "C6", "C8", "C10", "C12") ~ "Replicate_2"
    )
  )

test4 <- test3 %>%
  pivot_wider(
    names_from = replicate,    #create columns based on the replicate type
    values_from = value        #use values from the 'value' column to fill the Replicate_1 and Replicate_2 columns
  )%>%
  select(-well) %>%
  filter(!(row %in% c("A", "B", "C") & pair == "C3_C4"))

#this removes the NA values for each replicate and combines the rows into 1 row
test5 <- test4 %>%
  group_by(plate_number, date, table_id, source_file, row, pair) %>%
  summarize(
    Replicate_1 = max(Replicate_1, na.rm = TRUE),
    Replicate_2 = max(Replicate_2, na.rm = TRUE)
  ) %>%
  ungroup()

test6 <- test5 %>%
  mutate(
    #calculate the mean between Replicate_1 and Replicate_2
    mean = rowMeans(cbind(Replicate_1, Replicate_2), na.rm = TRUE),

    #calculate the %CV for each row
    cv = apply(cbind(Replicate_1, Replicate_2), 1, function(x) {
      sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    }),

    #flag if one is greater than 4.7 and the other is less than 4.7
    flag = case_when(
      (Replicate_1 > 4.7 & Replicate_2 < 4.7) | (Replicate_1 < 4.7 & Replicate_2 > 4.7) ~ "***", #three *** if samples disagree
      (Replicate_1 >= 4.23 & Replicate_1 <= 5.17) | (Replicate_2 >= 4.23 & Replicate_2 <= 5.17) ~ "*", #one * if samples within 10%

      TRUE ~ "" #no flag if everything is normal
    )
  )

row_order_C3_C4 <- c("D", "E", "F", "G", "H")  #start from row D for C3_C4
row_order_other <- c("A", "B", "C", "D", "E", "F", "G", "H")  #start from row A for other pairs
pair_order <- c("C3_C4", "C5_C6", "C7_C8", "C9_C10", "C11_C12")

#assign sample numbers based on the custom order
test7 <- test6 %>%
  mutate(
    #assign row_number based on pair
    row_number = case_when(
      pair == "C3_C4" ~ match(row, row_order_C3_C4),  #use the specific row order for C3_C4
      TRUE ~ match(row, row_order_other)              #use the full row order for other pairs
    ),

    #assign pair_number based on pair order
    pair_number = match(pair, pair_order)
  ) %>%

  #sort data
  arrange(plate_number, pair_number, row_number) %>%

  #assign a sample number across all plates starting from 1
  mutate(sample_number = row_number()) %>%
  ungroup() %>%
  select(-row_number, -pair_number)

#arrange columns
test8 <- test7 %>%
  select(plate_number, sample_number, everything(), -date, -table_id) %>%
  left_join(idsr %>%
              select(sample_number, sample_id), by = "sample_number") %>%
  select(sample_id, everything(), -sample_number)

################################# additional analyses   ##########################

analysis1 <- test8 %>%
  mutate(pos = if_else(
    Replicate_1 >= 4.7 & Replicate_2 >= 4.7, 1, 0)
  )

analysis2 <- analysis1 %>%
  mutate(
    lower10 = 4.23,
    upper10 = 5.17,

    # Create separate flags for each replicate
    rep1_above_pos = ifelse(Replicate_1 > 4.7 & Replicate_1 <= upper10, 1, 0),
    rep2_above_pos = ifelse(Replicate_2 > 4.7 & Replicate_2 <= upper10, 1, 0),

    rep1_below_pos = ifelse(Replicate_1 < 4.7 & Replicate_1 >= lower10, 1, 0),
    rep2_below_pos = ifelse(Replicate_2 < 4.7 & Replicate_2 >= lower10, 1, 0),

    # Determine overall qc_status
    qc_status = case_when(
      (rep1_above_pos == 1 & rep2_below_pos == 1) |
        (rep1_below_pos == 1 & rep2_above_pos == 1) ~ "both_within_10%",

      rep1_above_pos == 1  ~ "rep1 < 10%_above_cutoff",
      rep2_above_pos == 1  ~ "rep2 < 10%_above_cutoff",
      rep1_below_pos == 1  ~ "rep1 < 10%_below_cutoff",
      rep2_below_pos == 1  ~ "rep2 < 10%_below_cutoff",

      TRUE ~ "0"
    )
  ) %>%
  select(-c(lower10, upper10, rep1_above_pos, rep2_above_pos, rep1_below_pos, rep2_below_pos))

totals_df <- analysis2 %>%
  summarise(
    total_samples = n(),
    total_positives = sum(pos),
    within_10percent_of_cutoff = sum(qc_status != '0')
  )

###############################################################################

                    # levey jennings plots of controls

###############################################################################

control_summary <- controls_combined %>%
  group_by(control_value) %>%
  summarise(
    mean_control = mean(avg_conc, na.rm = TRUE),
    sd_control = sd(avg_conc, na.rm = TRUE)
  ) %>%
  mutate(
    lower_3sd = mean_control - (3 * sd_control),
    lower_2sd = mean_control - (2 * sd_control),
    upper_2sd = mean_control + (2 * sd_control),
    upper_3sd = mean_control + (3 * sd_control)
  )

controls_combined1 <- controls_combined %>%
  left_join(control_summary, by = "control_value") %>%
  mutate(plate_num = as.numeric(gsub("Plate ", "", plate_number))) %>%
  rename(control_name = control_value)

leveyjenningsplot <- ggplot(controls_combined1, aes(x = plate_num, y = avg_conc, group = control_name)) +
  geom_point(aes(color = control_name), size = 3) +  # Data points
  geom_line(aes(color = control_name)) +  # Line connecting points

  # Add control limits
  geom_hline(aes(yintercept = mean_control), linetype = "solid", color = "black", linewidth = 1) +
  geom_hline(aes(yintercept = lower_2sd), linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(aes(yintercept = upper_2sd), linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(aes(yintercept = lower_3sd), linetype = "dotted", color = "blue", linewidth = 1) +
  geom_hline(aes(yintercept = upper_3sd), linetype = "dotted", color = "blue", linewidth = 1) +

  facet_wrap(~control_name, scales = "free_y") +
  theme_minimal() +
  labs(title = "Levey-Jennings Plot of mean control values", x = "Plate Number", y = "Control Value Concentration")

leveyjenningsplotly <- ggplotly(leveyjenningsplot)
leveyjenningsplotly


###############################################################################

            # pivoting data wider for levey jennings in excel

###############################################################################


lj_df <- controls_combined1 %>%
  select(plate_num, control_name, replicate_1, replicate_2, avg_conc) %>%  # Keep replicates
  pivot_wider(names_from = control_name, values_from = c(replicate_1, replicate_2, avg_conc))

control_order <- c("Blank", "CTL 3?", "High Negative Control", "Medium Positive Control", "Standard 7")  # Customize as needed

# Build ordered column selection dynamically
ordered_columns <- c("plate_num", unlist(lapply(control_order, function(ctrl) {
  c(paste0("replicate_1_", ctrl), paste0("replicate_2_", ctrl), paste0("avg_conc_", ctrl))
})))

# Reorder the dataframe columns explicitly
plate_controls_wide <- lj_df %>%
  select(all_of(ordered_columns))

###############################################################################

#             creating overview dataframe for our excel sheet output

###############################################################################

overview_df <- tibble(
  Sheet_Name = c(
    "Output_Overview_README",
    "summary_totals",
    "plate_data", "", "",
    "control_tracking", "", "", "", "",
    "controls_wide",
    "error_summary", ""
  ),
  Description = c(
    "This sheet provides an overview of all data included in this workbook.",

    "Summary of total samples, positives, and those that fall within 10% of cutoff.",

    "Sample data (ordered by plate).",
    "     *** indicates one sample is positive, one is negative.",
    "     * indicates at least one replicate falls within 10% of cutoff.",

    "Control data (ordered by plate). Flag '***' indicates sample is out of range.",
    "     Medium Positive Control: *** if avg_conc < 7 or avg_conc > 15.",
    "     High Negative Control: *** if avg_conc < 1.5 or avg_conc > 4.3.",
    "     Standard 7: *** if avg_conc > 1.7.",
    "     Blank: *** if avg_conc > 0.11.",

    "Plate control data in wide format for Excel-based Levey-Jennings plots.",

    if (nrow(combined_error_summary) > 0) "Contains a list of files/plates flagged as having errors." else "No errors found.",
    if (nrow(combined_error_summary) > 0) "     Plates flagged as having errors are not included in the final Excel output." else ""
  ),
  Row_Count = c(
    NA,  # Output_Overview_README doesn't need row counts
    nrow(totals_df),
    nrow(analysis2), NA, NA,
    nrow(controls_combined), NA, NA, NA, NA,
    nrow(plate_controls_wide),
    ifelse(nrow(combined_error_summary) > 0, nrow(combined_error_summary), NA), NA
  ),
  Column_Count = c(
    NA,  # Output_Overview_README doesn't need column counts
    ncol(totals_df),
    ncol(analysis2), NA, NA,
    ncol(controls_combined), NA, NA, NA, NA,
    ncol(plate_controls_wide),
    ifelse(nrow(combined_error_summary) > 0, ncol(combined_error_summary), NA), NA
  )
)

###############################################################################


#write data to xlsx file

#format it too


###############################################################################

wb <- createWorkbook()

addWorksheet(wb, "Output_Overview_README")
writeData(wb, "Output_Overview_README", overview_df)

#add summary_totals
addWorksheet(wb, "summary_totals")
writeData(wb, "summary_totals", totals_df)

#add plate_data sheet
addWorksheet(wb, "plate_data")
writeData(wb, sheet = "plate_data", analysis2)

#add control_tracking sheet
addWorksheet(wb, "control_tracking")
writeData(wb, "control_tracking", controls_combined)

addWorksheet(wb, "controls_wide")
writeData(wb, "controls_wide", plate_controls_wide)

addWorksheet(wb, "error_summary")
writeData(wb, "error_summary", combined_error_summary)

#define styles
white_fill <- createStyle(fgFill = "#FFFFFF")  #white
grey_fill <- createStyle(fgFill = "#D3D3D3")  #light grey
pastel_red_fill <- createStyle(fgFill = "#FFB6C1")  #pastel red for flagged rows
bold_header <- createStyle(textDecoration = "bold", border = "Bottom", borderStyle = "thin")  #bold with underline for headers
center_align <- createStyle(halign = "center")  #center align all columns

#combined style for bold, underline, and center alignment for headers
bold_centered_header <- createStyle(textDecoration = "bold", border = "Bottom", borderStyle = "thin", halign = "center")

#apply alternating white and grey fills in blocks of 5 rows for control_tracking
for (i in seq(2, nrow(controls_combined), by = 10)) {
  addStyle(wb, sheet = "control_tracking", style = white_fill, rows = i:(i + 4), cols = 1:ncol(controls_combined), gridExpand = TRUE)
  addStyle(wb, sheet = "control_tracking", style = grey_fill, rows = (i + 5):(i + 9), cols = 1:ncol(controls_combined), gridExpand = TRUE)
}

#apply pastel red fill for flagged rows in control_tracking
flagged_rows_control <- which(controls_combined$flag == "***")
if (length(flagged_rows_control) > 0) {
  addStyle(wb, sheet = "control_tracking", style = pastel_red_fill, rows = flagged_rows_control + 1, cols = 1:ncol(controls_combined), gridExpand = TRUE)  # +1 for Excel row indexing
}

#apply bold, underline, and center alignment to the header rows in sheets
addStyle(wb, sheet = "Output_Overview_README", style = bold_centered_header, rows = 1, cols = 1:ncol(overview_df), gridExpand = TRUE)
addStyle(wb, sheet = "control_tracking", style = bold_centered_header, rows = 1, cols = 1:ncol(controls_combined), gridExpand = TRUE)
addStyle(wb, sheet = "plate_data", style = bold_centered_header, rows = 1, cols = 1:ncol(analysis2), gridExpand = TRUE)
addStyle(wb, sheet = "summary_totals", style = bold_centered_header, rows = 1, cols = 1:ncol(totals_df), gridExpand = TRUE)
addStyle(wb, sheet = "controls_wide", style = bold_centered_header, rows = 1, cols = 1:ncol(plate_controls_wide), gridExpand = TRUE)
addStyle(wb, sheet = "error_summary", style = bold_centered_header, rows = 1, cols = 1:ncol(combined_error_summary), gridExpand = TRUE)

#set col widths
setColWidths(wb, sheet = "Output_Overview_README", cols = 1, widths = 20)  # Sheet_Name column
setColWidths(wb, sheet = "Output_Overview_README", cols = 2, widths = 70)  # Description column
setColWidths(wb, sheet = "Output_Overview_README", cols = 3:ncol(overview_df), widths = 15)  # Other columns

#set column widths for control_tracking: 25 for 'source_file' and 'control_value', 15 for everything else
setColWidths(wb, sheet = "control_tracking", cols = which(colnames(controls_combined) %in% c("source_file", "control_value")), widths = 25)
setColWidths(wb, sheet = "control_tracking", cols = which(!colnames(controls_combined) %in% c("source_file", "control_value")), widths = 15)

#set column widths for plate_data: 25 for 'source_file', 15 for all other columns
setColWidths(wb, sheet = "plate_data", cols = which(colnames(analysis2) == "source_file"), widths = 25)
setColWidths(wb, sheet = "plate_data", cols = which(colnames(analysis2) != "source_file"), widths = 15)

# Set column widths for summary_totals
setColWidths(wb, sheet = "summary_totals", cols = 1:ncol(totals_df), widths = 20)
setColWidths(wb, sheet = "controls_wide", cols = 1:ncol(plate_controls_wide), widths = 15)

setColWidths(wb, sheet = "error_summary", cols = 1, widths = 30)  # Sheet_Name column
setColWidths(wb, sheet = "error_summary", cols = 2, widths = 30)  # Description column

#apply center alignment to all columns in plate_data
addStyle(wb, sheet = "plate_data", style = center_align, rows = 2:(nrow(analysis2) + 1), cols = 1:ncol(analysis2), gridExpand = TRUE)

#save the workbook
saveWorkbook(wb, file_name, overwrite = TRUE)


###             ###                ###                       ###
