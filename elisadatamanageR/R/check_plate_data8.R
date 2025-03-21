#'
#'                      Track and Process ELISA Plate Sample Data
#'
#' This function processes cleaned ELISA data to calculate replicate means,
#' CVs, positivity flags, and quality control (QC) drift near the cutoff.
#'
#' @param formatted_data Dataframe. Cleaned ELISA data from `format_elisa_data()`.
#' @param sample_ids Dataframe. Sample ID dataframe with sample_number and sample_id.
#' @param positivity_cutoff Numeric. Default is 4.7.
#' @param qc_percent Numeric. Percent buffer above/below cutoff to flag QC drift. Default is 10 (%).
#'
#' @return A dataframe with replicate values, means, CV, flags, and sample IDs.
#'
#' @export


check_plate_data <- function(formatted_data,
                             sample_ids,
                             positivity_cutoff = 4.7,
                             qc_percent = 10) {
  
  lower_limit <- round(positivity_cutoff - (positivity_cutoff * qc_percent / 100), 4)
  upper_limit <- round(positivity_cutoff + (positivity_cutoff * qc_percent / 100), 4)
  
  # Step 1: Filter out controls
  plate_data <- formatted_data %>%
    filter(table_id == 'Concentration') %>%
    mutate(
      column_3 = if_else(row_letter %in% c("A", "B", "C"), NA_real_, column_3),
      column_4 = if_else(row_letter %in% c("A", "B", "C"), NA_real_, column_4)
    )
  
  # Step 2: Pivot long
  reshaped <- plate_data %>%
    pivot_longer(cols = column_3:column_12, names_to = "well", values_to = "value") %>%
    mutate(
      pair = case_when(
        well %in% c("column_3", "column_4") ~ "C3_C4",
        well %in% c("column_5", "column_6") ~ "C5_C6",
        well %in% c("column_7", "column_8") ~ "C7_C8",
        well %in% c("column_9", "column_10") ~ "C9_C10",
        well %in% c("column_11", "column_12") ~ "C11_C12"
      ),
      replicate = case_when(
        well %in% c("column_3", "column_5", "column_7", "column_9", "column_11") ~ "Replicate_1",
        TRUE ~ "Replicate_2"
      ),
      row = row_letter
    ) %>%
    select(plate_number, date, table_id, source_file, row, well, pair, replicate, value)
  
  # Step 3: Pivot wider and filter out control rows
  wide <- reshaped %>%
    pivot_wider(names_from = replicate, values_from = value) %>%
    filter(!(row %in% c("A", "B", "C") & pair == "C3_C4"))
  
  # Step 4: Aggregate by pair
  summarized <- wide %>%
    group_by(plate_number, date, table_id, source_file, row, pair) %>%
    summarise(
      Replicate_1 = max(Replicate_1, na.rm = TRUE),
      Replicate_2 = max(Replicate_2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean = rowMeans(cbind(Replicate_1, Replicate_2), na.rm = TRUE),
      cv = apply(cbind(Replicate_1, Replicate_2), 1, function(x) {
        sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
      }),
      
      # Flag if one value is above and one below the cutoff
      flag = case_when(
        (Replicate_1 > positivity_cutoff & Replicate_2 < positivity_cutoff) |
          (Replicate_1 < positivity_cutoff & Replicate_2 > positivity_cutoff) ~ "***",
        (Replicate_1 >= lower_limit & Replicate_1 <= upper_limit) |
          (Replicate_2 >= lower_limit & Replicate_2 <= upper_limit) ~ "*",
        TRUE ~ ""
      ),
      
      # Positivity status
      pos = if_else(Replicate_1 >= positivity_cutoff & Replicate_2 >= positivity_cutoff, 1, 0),
      
      # QC status
      qc_status = case_when(
        (Replicate_1 > positivity_cutoff & Replicate_1 <= upper_limit &
           Replicate_2 < positivity_cutoff & Replicate_2 >= lower_limit) |
          (Replicate_2 > positivity_cutoff & Replicate_2 <= upper_limit &
             Replicate_1 < positivity_cutoff & Replicate_1 >= lower_limit) ~ "both_within_qc",
        
        Replicate_1 > positivity_cutoff & Replicate_1 <= upper_limit ~ "rep1 <10% above",
        Replicate_2 > positivity_cutoff & Replicate_2 <= upper_limit ~ "rep2 <10% above",
        Replicate_1 < positivity_cutoff & Replicate_1 >= lower_limit ~ "rep1 <10% below",
        Replicate_2 < positivity_cutoff & Replicate_2 >= lower_limit ~ "rep2 <10% below",
        TRUE ~ ""
      )
    )
  
  # Step 5: Assign sample numbers
  row_order_c3 <- c("D", "E", "F", "G", "H")
  row_order_other <- LETTERS[1:8]
  pair_order <- c("C3_C4", "C5_C6", "C7_C8", "C9_C10", "C11_C12")
  
  numbered <- summarized %>%
    mutate(
      row_number = case_when(
        pair == "C3_C4" ~ match(row, row_order_c3),
        TRUE ~ match(row, row_order_other)
      ),
      pair_number = match(pair, pair_order)
    ) %>%
    arrange(plate_number, pair_number, row_number) %>%
    mutate(sample_number = row_number()) %>%
    select(-row_number, -pair_number)
  
  # Step 6: Join sample ID
  result <- numbered %>%
    left_join(sample_ids %>% select(sample_number, sample_id), by = "sample_number") %>%
    select(sample_id, plate_number, source_file, row, pair, Replicate_1, Replicate_2,
           mean, cv, pos, flag, qc_status)
  
  return(result)
}