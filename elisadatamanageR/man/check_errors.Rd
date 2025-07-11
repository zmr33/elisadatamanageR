% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_errors_function3.R
\name{check_errors}
\alias{check_errors}
\title{Check Errors in Data Files}
\usage{
check_errors(file_list)
}
\arguments{
\item{file_list}{Character vector. A list of file paths to be processed.}
}
\value{
A dataframe containing an error summary of files with issues.
}
\description{
This function scans files for errors related to missing sample IDs and structure issues.
It verifies that each file contains the required keywords (`"Layout"`, `"Blank 450"`, `"[Concentration]"`).
If any of these elements are missing, the file is flagged in the error log.
}
\details{
- This function first checks for **missing keywords** in each file.
- It then scans files for **row structure issues** to detect missing sample IDs.
- If **missing sample IDs** are detected, the file is **logged in the error summary**.
- A **progress bar** is included to track processing.
- Finally, the function returns a **combined error log** with all detected issues.
}
\examples{
file_list <- list.files(path = "data/", full.names = TRUE)
errors_found <- check_errors(file_list)

print(errors_found)

}
