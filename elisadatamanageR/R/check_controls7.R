#' Extract Controls and Blank Values from ELISA Data
#'
#' This function extracts control and blank values from ELISA plate data, calculates 
#' averages, and flags values that fall outside specified ranges. It allows users 
#' to modify threshold values while maintaining default settings.
#'
#' @param numeric_data Dataframe. The processed ELISA data containing numeric values.
#' @param blank_threshold Numeric. The cutoff for blank values in the "450" table. Default is 0.11.
#' @param control_ranges List. Named list defining min/max concentration ranges for controls.
#'                       Default values:
#'                       \itemize{
#'                         \item "Medium Positive Control" = c(7, 15)
#'                         \item "High Negative Control" = c(1.5, 4.3)
#'                         \item "Standard 7" = c(0, 1.7)
#'                       }
#' @param control_names List. Named list defining the control names based on row_letter values.
#'                      Default values:
#'                      \itemize{
#'                        \item "A" = "Medium Positive Control"
#'                        \item "B" = "High Negative Control"
#'                        \item "C" = "CTL 3?"
#'                        \item "H" = "Standard 7"
#'                      }
#'
#' @return A dataframe containing control and blank values with calculated averages,
#'         control ranges, and flagged values if they are out of range.
#'
#' @details
#' - Selects **Medium Positive Control**, **High Negative Control**, **CTL 3?**, and **Standard 7**.
#' - Selects **blank values** from the 450 table and assigns a threshold.
#' - Calculates the **average concentration** for each control.
#' - Flags controls that **fall outside the expected range**.
#' - Returns a dataframe with relevant control information.
#'
#' @examples
#' # Run function with default settings
#' controls_data <- extract_controls_and_blanks(numeric_data)
#'
#' # Change the blank threshold and control ranges
#' controls_data <- check_controls(numeric_data, 
#'                                              blank_threshold = 0.10,  
#'                                              control_ranges = list(
#'                                                "Medium Positive Control" = c(6, 14),
#'                                                "High Negative Control" = c(1, 5),
#'                                                "Standard 7" = c(0, 1.5)
#'                                              ))
#'
#' # Customize control names
#' controls_data <- check_controls(numeric_data, 
#'                                              control_names = list(
#'                                                "A" = "MP Control",
#'                                                "B" = "HN Control",
#'                                                "C" = "CTL 3 Alt",
#'                                                "H" = "Std 7 Mod"
#'                                              ))
#'
#' @export


check_controls <- function(numeric_data, 
                           blank_threshold = 0.11,
                           control_ranges = list(
                             "Medium Positive Control" = c(7, 15),
                             "High Negative Control" = c(1.5, 4.3),
                             "Standard 7" = c(0, 1.7)
                           ),
                           control_names = list(
                             "A" = "Medium Positive Control",
                             "B" = "High Negative Control",
                             "C" = "CTL 3?",
                             "H" = "Standard 7"
                           )) {
  
  # ---- 1. Blanks (from 450 table, row A, columns 1+2) ----
  blanks <- numeric_data %>%
    filter(row_letter == 'A', table_id == '450') %>%
    select(plate_number, date, source_file, row_letter, column_1, column_2) %>%
    rename(column_3 = "column_1", column_4 = "column_2") %>%
    mutate(
      control_value = "Blank",
      avg_conc = round((column_3 + column_4) / 2, 4),
      control_range = if_else(avg_conc > blank_threshold, "above range", "in-range"),
      flag = if_else(control_range != "in-range", "***", "")
    ) %>%
    ungroup()
  
  # ---- 2. Controls (from Concentration table) ----
  controls <- numeric_data %>%
    filter(row_letter %in% names(control_names), table_id == 'Concentration') %>%
    select(plate_number, date, source_file, row_letter, column_1, column_2, column_3, column_4) %>%
    mutate(
      control_value = recode(row_letter, !!!control_names),
      avg_conc = if_else(
        control_value == "Standard 7",
        round((column_1 + column_2) / 2, 4),  # Standard 7 → col 1+2
        round((column_3 + column_4) / 2, 4)   # Others → col 3+4
      ),
      control_range = case_when(
        control_value %in% names(control_ranges) & 
          avg_conc < map_dbl(control_value, ~ control_ranges[[.x]][1]) ~ "below range",
        
        control_value %in% names(control_ranges) & 
          avg_conc > map_dbl(control_value, ~ control_ranges[[.x]][2]) ~ "above range",
        
        TRUE ~ "in-range"
      ),
      flag = if_else(control_range != "in-range", "***", "")
    ) %>%
    ungroup()
  
  # ---- 3. Combine and organize ----
  controls_combined <- bind_rows(controls, blanks) %>%
    mutate(
      replicate_1 = if_else(control_value == "Standard 7", column_1, column_3),
      replicate_2 = if_else(control_value == "Standard 7", column_2, column_4)
    ) %>%
    arrange(
      plate_number, source_file,
      factor(control_value, levels = c("Blank", unname(unlist(control_names))))
    ) %>%
    select(
      plate_number, source_file, control_value,
      replicate_1, replicate_2, avg_conc, control_range, flag
    )
  
  return(controls_combined)
}