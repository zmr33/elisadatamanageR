#'                  
#'                  Write ELISA Output to Formatted Excel Workbook
#'
#' This function writes all processed ELISA results, QC metrics, and metadata into
#' a formatted Excel workbook, with styling and sheet descriptions.
#'
#' @param file_name Name of the Excel file to save (e.g., "ELISA_Output.xlsx").
#' @param totals_df Summary totals dataframe.
#' @param plate_data Main plate result data.
#' @param controls_combined Control summary dataframe.
#' @param plate_controls_wide Controls in wide format for plotting in Excel.
#' @param combined_error_summary Error summary dataframe (can be empty).
#'
#' @return Writes a styled Excel workbook to the specified file path.
#' @export
#'


write_elisa_xlsx <- function(file_name,
                             totals,
                             plate_data,
                             controls,
                             controls_wide,
                             error_summary) {
  
  # Create Overview Sheet Data
  overview_df <- tibble(
    Sheet_Name = c(
      "Output_Overview_README",
      "summary_totals",
      "plate_data", "", "",
      "control_tracking", "", "", "", "", "",
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
      "Controls may be different than listed if changed by user", 
      "     Medium Positive Control: *** if avg_conc < 7 or avg_conc > 15.",
      "     High Negative Control: *** if avg_conc < 1.5 or avg_conc > 4.3.",
      "     Standard 7: *** if avg_conc > 1.7.",
      "     Blank: *** if avg_conc > 0.11.",
      "Plate control data in wide format for Excel-based Levey-Jennings plots.",
      if (nrow(error_summary) > 0) "Contains a list of files/plates flagged as having errors." else "No errors found.",
      if (nrow(error_summary) > 0) "     Plates flagged as having errors are not included in the final Excel output." else ""
    ),
    Row_Count = c(
      NA,
      nrow(totals),
      nrow(plate_data), NA, NA,
      nrow(controls), NA, NA, NA, NA, NA, 
      nrow(controls_wide),
      ifelse(nrow(error_summary) > 0, nrow(error_summary), NA), NA
    ),
    Column_Count = c(
      NA,
      ncol(totals),
      ncol(plate_data), NA, NA,
      ncol(controls), NA, NA, NA, NA, NA, 
      ncol(controls_wide),
      ifelse(nrow(error_summary) > 0, ncol(error_summary), NA), NA
    )
  )
  
  # Create Workbook
  wb <- createWorkbook()
  
  addWorksheet(wb, "Output_Overview_README")
  writeData(wb, "Output_Overview_README", overview_df)
  
  addWorksheet(wb, "summary_totals")
  writeData(wb, "summary_totals", totals)
  
  addWorksheet(wb, "plate_data")
  writeData(wb, "plate_data", plate_data)
  
  addWorksheet(wb, "control_tracking")
  writeData(wb, "control_tracking", controls)
  
  addWorksheet(wb, "controls_wide")
  writeData(wb, "controls_wide", controls_wide)
  
  addWorksheet(wb, "error_summary")
  writeData(wb, "error_summary", error_summary)
  
  # Styles
  white_fill <- createStyle(fgFill = "#FFFFFF")
  grey_fill <- createStyle(fgFill = "#D3D3D3")
  pastel_red_fill <- createStyle(fgFill = "#FFB6C1")
  bold_centered_header <- createStyle(textDecoration = "bold", border = "Bottom", borderStyle = "thin", halign = "center")
  center_align <- createStyle(halign = "center")
  
  # Alternating fill for control_tracking
  for (i in seq(2, nrow(controls), by = 10)) {
    addStyle(wb, "control_tracking", white_fill, rows = i:(i + 4), cols = 1:ncol(controls), gridExpand = TRUE)
    addStyle(wb, "control_tracking", grey_fill, rows = (i + 5):(i + 9), cols = 1:ncol(controls), gridExpand = TRUE)
  }
  
  # Flagged rows
  flagged_rows_control <- which(controls$flag == "***")
  if (length(flagged_rows_control) > 0) {
    addStyle(wb, "control_tracking", pastel_red_fill, rows = flagged_rows_control + 1, cols = 1:ncol(controls), gridExpand = TRUE)
  }
  
  # Bold headers
  addStyle(wb, "Output_Overview_README", bold_centered_header, rows = 1, cols = 1:ncol(overview_df), gridExpand = TRUE)
  addStyle(wb, "summary_totals", bold_centered_header, rows = 1, cols = 1:ncol(totals), gridExpand = TRUE)
  addStyle(wb, "plate_data", bold_centered_header, rows = 1, cols = 1:ncol(plate_data), gridExpand = TRUE)
  addStyle(wb, "control_tracking", bold_centered_header, rows = 1, cols = 1:ncol(controls), gridExpand = TRUE)
  addStyle(wb, "controls_wide", bold_centered_header, rows = 1, cols = 1:ncol(controls_wide), gridExpand = TRUE)
  addStyle(wb, "error_summary", bold_centered_header, rows = 1, cols = 1:ncol(error_summary), gridExpand = TRUE)
  
  # Column widths
  setColWidths(wb, "Output_Overview_README", cols = 1, widths = 20)
  setColWidths(wb, "Output_Overview_README", cols = 2, widths = 70)
  setColWidths(wb, "Output_Overview_README", cols = 3:ncol(overview_df), widths = 15)
  
  setColWidths(wb, "summary_totals", cols = 1:ncol(totals), widths = 20)
  setColWidths(wb, "control_tracking", cols = which(colnames(controls) %in% c("source_file", "control_value")), widths = 25)
  setColWidths(wb, "control_tracking", cols = which(!colnames(controls) %in% c("source_file", "control_value")), widths = 15)
  setColWidths(wb, "plate_data", cols = which(colnames(plate_data) == "source_file"), widths = 25)
  setColWidths(wb, "plate_data", cols = which(colnames(plate_data) != "source_file"), widths = 15)
  setColWidths(wb, "controls_wide", cols = 1:ncol(controls_wide), widths = 15)
  setColWidths(wb, "error_summary", cols = 1:2, widths = 30)
  
  # Center align plate data
  addStyle(wb, "plate_data", center_align, rows = 2:(nrow(plate_data) + 1), cols = 1:ncol(plate_data), gridExpand = TRUE)
  
  # Save
  saveWorkbook(wb, file_name, overwrite = TRUE)
}