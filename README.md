# elisadatamanageR

An R package for automated ELISA data management from the Gen 5 microplate reading software for the Ov16 ELIS (and other ELISAs in the future).

## Overview

`elisadatamanageR` is a lightweight R package designed to support end-to-end processing of ELISA plate data exported from the Gen5 Microplate Reading Software as .xlsx files. It was developed specifically for handling *Onchocerca volvulus* (Ov16) serological surveillance data. It might become adaptable to other ELISA-based workflows in the future, we'll see. 

The pipeline automates the import, cleaning, merging, tracking, and analysis of ELISA data, with support for quality control thresholds, control tracking, Levey-Jennings plots, and clean Excel exports.

See example folder for an example pipeline and example excel file layout (with no data in it).

**Note:** This package assumes your Gen5 output is structured in a specific, consistent format, relying on the table names as keywords to locate the tables ('Layout', '450', 'Blank 450', and '[Concentration]'). If your Gen5 exports do not contain these keywords with the tables beneath them, the functions may need to be adjusted. This package was developed without direct access to the Gen5 software, so keep that in mind.

## What it does

- Batch import of ELISA plate files (.xlsx)
- Cleans numeric fields, handles symbols: `<`, `>`, "*" , `OVRFLW`, "?????", and non-numeric values
- Extracts concentration and layout (sample name) tables
- Validates and flags control samples based on user-defined thresholds (default thresholds are for ov16 elisa)
- Performs qc checks with customizable positivity thresholds and qc %
- Generates interactable Levey-Jennings plots and wide data for excel-based analysis
- Outputs a fully formatted Excel workbook with all results, flags, and metadata

## Package Requirements

- R version ≥ 4.0
- Packages:
  - `tidyverse`
  - `readxl`
  - `openxlsx`
  - `progress`
  - `lubridate`
  - `plotly` 

## Installation

This package is currently not on CRAN. To install from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")
library(devtools)

# Then install the package like this 
devtools::install_github(
     repo   = "zmr33/elisadatamanageR",  # repo
     ref    = "main",                    # branch
     subdir = "elisadatamanageR"         # folder that has 'DESCRIPTION'
)

# Load package
library(elisadatamanageR)

# Verify - a pop-up in the help tab should appear 
?check_plate_data()
