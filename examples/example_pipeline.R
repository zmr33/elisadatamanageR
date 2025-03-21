################################################################################

#'                       ELISA Data Management - Example Script
#'
#'  This script demonstrates how to use the `elisadatamanageR` package to:
#'    1. Load raw ELISA excel data
#'    2. Check for file formatting errors
#'    3. Extract raw tables and sample id layouts
#'    4. Format and clean the data
#'    5. Perform control validation and sample qc checks
#'    6. Create summary statistics and interactive levey jennings plot
#'    7. Export results to a formatted excel report
#'
#'
#'
#'        This is my recommended workflow. Run this script top to bottom.
 ################################################################################

# ─────────────────────────────────────────────────────────────────────────────#
#                              Load packages
# ─────────────────────────────────────────────────────────────────────────────#

#load required libraries
library(elisadatamanageR)  #custom ELISA functions (must be installed from GitHub or locally)
library(tidyverse)         #data wrangling
library(readxl)            #read .xlsx files
library(openxlsx)          #write formatted Excel files
library(progress)          #for progress bars
library(plotly)            #interactive plots
library(lubridate)         #working with dates

#disable scientific notation 
options(scipen = 999)

# ─────────────────────────────────────────────────────────────────────────────#
#                     Set working directory and load files
# ─────────────────────────────────────────────────────────────────────────────#

#set your working directory (this is where R will look for files and save output)
#set your user ID here—replace "tti2" with your CDC ID 
setwd("C:/Users/tti2/OneDrive - CDC/ELISA Data Management")

#define the folder containing your ELISA plate .xlsx files
#use double quotes for paths, and use forward slashes (/)
file_list <- get_files("C:/Users/tti2/OneDrive - CDC/ELISA Data Management/uganda")

#check for file errors (missing keywords, sample ids, or whole tables)
error_summary <- check_errors(file_list)

#extract data and combine files that passed error check
extract_results <- extract_files(file_list, error_summary)

#format and clean the sample id layout tables
sample_ids <- format_sample_ids(all_sample_ids_combined)

#format numeric tables (remove symbols, convert to numeric, round, etc.)
formatted_data <- format_elisa_data(all_data_combined)

# ─────────────────────────────────────────────────────────────────────────────#
#                         Plate control set up
# ─────────────────────────────────────────────────────────────────────────────#

#check internal controls (medium positive, high negative, standard 7, etc.)
#you can rename controls here and adjust acceptable ranges
controls_df <- check_controls(
  formatted_data,
  control_names = list(
    "A" = "Medium_Positive_Ctrl",
    "B" = "High_Negative_Ctrl",
    #"C" = "CTL 3",  #optional
    "H" = "Standard 7"
  ),
  control_ranges = list(
    "Medium_Positive_Ctrl" = c(7, 15),
    "High_Negative_Ctrl" = c(1.5, 4.3),
    "Standard 7" = c(0.0, 1.7)
    #"CTL 3" = c(0.5, 2.0)
  )
)

# ─────────────────────────────────────────────────────────────────────────────#
#                  Sample qc + cutoff evaluation (positivity + CVs)
# ─────────────────────────────────────────────────────────────────────────────#

#check test samples from each plate (exclude controls, calculate CV, flag discrepancies)
#adjust the positivity cutoff (e.g. 4.7) and % QC tolerance (e.g. 10%)
plate_data <- check_plate_data(
  formatted_data,
  sample_ids = sample_ids,
  positivity_cutoff = 4.7,
  qc_percent = 10
)

# ─────────────────────────────────────────────────────────────────────────────#
#                        Analyze and Summarize Results
# ─────────────────────────────────────────────────────────────────────────────#

#run summary and analysis of results (plate stats + Levey-Jennings calculations)
results <- analyze_plate_data(plate_data, controls_df)

#plot interactive Levey-Jennings chart of controls across plates
results$lj_plot  #you can click and zoom

#print summary table (total positives, samples flagged, etc.)
results$study_summary


# ─────────────────────────────────────────────────────────────────────────────#
#                        Save Results to Excel File
# ─────────────────────────────────────────────────────────────────────────────#

#generate a filename to save the Excel output (the code automatically adds date)
#replace "uganda" and "ov_16 survey" with your own tags
save_file_name <- generate_file_name("uganda", "ov_16 survey")

#export all results to a styled Excel file
#includes: README, summary, plate data, controls, wide format excel control tracking, and error summary
write_elisa_xlsx(
  file_name = save_file_name,
  totals = results$study_summary,
  plate_data = plate_data,
  controls = controls_df,
  controls_wide = results$lj_data,
  error_summary = error_summary
)

# ─────────────────────────────────────────────────────────────────────────────#
#                            END OF SCRIPT
# ─────────────────────────────────────────────────────────────────────────────#