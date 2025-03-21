#'                 
#'                  Format Sample IDs into a Structured Dataframe
#'
#' This function processes raw sample ID data, extracts plate numbers, pairs replicates,
#' removes control samples, assigns sequential sample numbers, and checks for duplicate sample IDs.
#'
#' @param all_sample_ids_combined Dataframe. A dataframe containing extracted sample IDs.
#'
#' @return A formatted dataframe with paired replicates and structured sample numbers.
#'         If duplicate sample IDs are found, a warning message is displayed.
#'
#' @examples
#' formatted_sample_ids <- format_sample_ids(all_sample_ids_combined)
#' head(formatted_sample_ids)
#'
#' @export


format_sample_ids <- function(all_sample_ids_combined) {
  # Step 1: Extract plate number from file name
  all_sample_ids_long1 <- all_sample_ids_combined %>%
    mutate(
      plate_number = str_extract(file_name, "Plate \\d+") %>%
        str_extract("\\d+") %>%
        as.integer()
    ) %>%
    
    # Pivot to longer format
    pivot_longer(
      cols = starts_with("C"),
      names_to = "column",
      values_to = "sample_id"
    ) %>%
    
    # Extract numeric column number for sorting
    mutate(
      column_num = str_extract(column, "\\d+") %>% as.integer()
    )
  
  # Step 2: Arrange and assign sample numbers
  all_sample_ids_long2 <- all_sample_ids_long1 %>%
    group_by(plate_number) %>%
    arrange(plate_number, column_num, rowletter) %>%
    mutate(
      sample_number = row_number() + (plate_number - 1) * 96,
      pair = case_when(
        column_num %% 2 == 1 ~ paste0("C", column_num, "_C", column_num + 1),
        column_num %% 2 == 0 ~ paste0("C", column_num - 1, "_C", column_num)
      )
    ) %>%
    ungroup() %>%
    
    # Pivot wider to merge paired wells
    pivot_wider(
      names_from = column,
      values_from = sample_id,
      names_prefix = "replicate_"
    ) %>%
    
    # Merge replicate pairs into one column
    mutate(
      replicate_1 = coalesce(replicate_C1, replicate_C3, replicate_C5, replicate_C7, replicate_C9, replicate_C11),
      replicate_2 = coalesce(replicate_C2, replicate_C4, replicate_C6, replicate_C8, replicate_C10, replicate_C12)
    ) %>%
    select(-starts_with("replicate_C")) %>%
    rename(sample_id = replicate_1)
  
  # Step 3: Merge and clean up paired data
  all_sample_ids_combined_short <- all_sample_ids_long2 %>%
    group_by(plate_number, rowletter, pair) %>%
    summarize(
      sample_id = coalesce(first(na.omit(sample_id)), first(na.omit(replicate_2))),
      sample_number = first(sample_number),
      file_name = first(file_name)
    ) %>%
    ungroup()
  
  # Step 4: Filter out control samples and standards
  all_sample_ids_filtered <- all_sample_ids_combined_short %>%
    filter(
      !(sample_id %in% c("BLK", "STD1", "STD2", "STD3", "STD4", "STD5", "STD6", "STD7",
                         "Med Positive", "High Negative", "Lyo 1",
                         "positive 10ng", "negative 2.5ng", "Lyophilized"))
    )
  
  # Step 5: Sort and assign new sample numbers
  idsr <- all_sample_ids_filtered %>%
    arrange(sample_number) %>%
    mutate(sample_number_new = row_number()) %>%
    select(plate_number, rowletter, pair, sample_number_new, everything()) %>%
    select(-sample_number) %>%
    rename(sample_number = sample_number_new)
  
  duplicate_samples <- idsr %>%
    group_by(sample_id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicate_samples) > 0) {
    cat("\n Duplicate sample ids found in the data.\n")
    print(duplicate_samples)
  }
  
  return(idsr)
}