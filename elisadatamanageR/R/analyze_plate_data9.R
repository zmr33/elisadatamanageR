#' Analyze Plate Data for Positivity and QC
#'
#' This function calculates average values, flags quality control issues, tracks sample positivity, and produces
#' a Levey-Jennings summary and wide-format table for export.
#'
#' @param formatted_data Dataframe. Output from `format_elisa_data()`.
#' @param sample_ids Dataframe. The sample ID mapping (must include `sample_number` and `sample_id`).
#' @param positivity_cutoff Numeric. Threshold to define positivity. Default is 4.7.
#' @param qc_percent Numeric. Percent range around the positivity_cutoff used for quality control. Default is 10.
#'
#' @return A list containing:
#' \describe{
#'   \item{plate_data}{Plate-level data with replicate means, CVs, and QC flags.}
#'   \item{totals_df}{Summary of total samples and QC proximity.}
#'   \item{plate_controls_wide}{Wide-format dataframe for export or plotting.}
#'   \item{leveyjenningsplot}{A ggplot2 Levey-Jennings plot object.}
#' }
#'
#' @export
#'


analyze_plate_data <- function(plate_data, controls_combined) {
  
  # Summary totals
  totals_df <- plate_data %>%
    summarise(
      total_samples = n(),
      total_positives = sum(pos, na.rm = TRUE),
      within_10percent_of_cutoff = sum(qc_status != "", na.rm = TRUE)
    )
  
  # ------------------ Levey-Jennings for Controls ------------------
  
  # Summary stats per control group
  control_summary <- controls_combined %>%
    group_by(control_value) %>%
    summarise(
      mean_control = mean(avg_conc, na.rm = TRUE),
      sd_control = sd(avg_conc, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      lower_3sd = mean_control - (3 * sd_control),
      lower_2sd = mean_control - (2 * sd_control),
      upper_2sd = mean_control + (2 * sd_control),
      upper_3sd = mean_control + (3 * sd_control)
    )
  
  # Join stats back to control data
  controls_combined1 <- controls_combined %>%
    left_join(control_summary, by = "control_value") %>%
    mutate(plate_num = as.numeric(gsub("Plate ", "", plate_number))) %>%
    rename(control_name = control_value)
  
  # Create ggplot Levey-Jennings Plot
  leveyjenningsplot <- ggplot(controls_combined1, aes(x = plate_num, y = avg_conc, group = control_name)) +
    geom_point(aes(color = control_name), size = 3) +
    geom_line(aes(color = control_name)) +
    geom_hline(aes(yintercept = mean_control), linetype = "solid", color = "black", linewidth = 1) +
    geom_hline(aes(yintercept = lower_2sd), linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(aes(yintercept = upper_2sd), linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(aes(yintercept = lower_3sd), linetype = "dotted", color = "blue", linewidth = 1) +
    geom_hline(aes(yintercept = upper_3sd), linetype = "dotted", color = "blue", linewidth = 1) +
    facet_wrap(~control_name, scales = "free_y") +
    theme_minimal() +
    labs(title = "Levey-Jennings Plot of Control Means", x = "Plate Number", y = "Average Concentration")
  
  # Interactive Plot
  leveyjenningsplotly <- plotly::ggplotly(leveyjenningsplot)
  
  # Prepare wide version of control data (for Excel export etc.)
  lj_df <- controls_combined1 %>%
    select(plate_num, control_name, replicate_1, replicate_2, avg_conc) %>%
    pivot_wider(names_from = control_name, values_from = c(replicate_1, replicate_2, avg_conc))
  
  # Custom control ordering (optional)
  control_order <- unique(controls_combined1$control_name)
  ordered_columns <- c("plate_num", unlist(lapply(control_order, function(ctrl) {
    c(paste0("replicate_1_", ctrl), paste0("replicate_2_", ctrl), paste0("avg_conc_", ctrl))
  })))
  
  plate_controls_wide <- lj_df %>%
    select(any_of(ordered_columns)) %>%
    as.data.frame()
  
  assign("lj_data", plate_controls_wide, envir = .GlobalEnv)
  
  # Return a list of useful results
  return(list(
    study_summary = totals_df,
    lj_data = plate_controls_wide,
    lj_plot = leveyjenningsplotly
  ))
}
