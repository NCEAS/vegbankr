#' Validate VegBank loader tables for plot observations and related data
#'
#' Performs comprehensive validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings.
#'
#' @param plot_observations A data frame containing details about plot
#'   observations
#' @param projects A data frame containing details about new projects
#' @param parties A data frame containing details about new parties
#' @param references A data frame containing details about new references
#' @param soils A data frame containing details about observed soils
#' @param disturbances A data frame containing details about observed
#'   disturbances
#' @param community_classifications A data frame containing community
#'   classifications of observed plots
#' @param strata A data frame containing details about strata defined for plot
#'   observations
#' @param strata_cover_data A data frame containing details about plant cover as
#' observed in different strata of a plot
#' @param stem_data A data frame containing details about counts and/or
#'   other details about stems observed on a plot
#' @param taxon_interpretations A data frame containing taxon interpretations of
#'   plants observed on a plot
#' @param contributors A data frame associating parties with their contributions
#'   to plot observations, projects, and/or community classifications
#'
#' @return A named list with one element per table, each containing a logical value
#'   (TRUE if all validations passed for that table, FALSE otherwise). For example:
#'   \code{list(parties = TRUE, contributors = FALSE, plot_observations = TRUE)}
#' 
#' @import dplyr
#' @import tidyr
#' @export
#'
vb_validate_plot_observations <- function(plot_observations,
                                        projects = NULL, parties = NULL, references = NULL, soils = NULL,
                                        disturbances = NULL, community_classifications = NULL, strata = NULL,
                                        strata_cover_data = NULL, stem_data = NULL, taxon_interpretations = NULL,
                                        contributors = NULL) {

  fields <- utils::read.csv(system.file("loader-table-fields.csv", package = "vegbankr"))
  
  validation_results <- list()
  
  # projects
  validation_results$projects <- list(
    validate_no_nulls(projects, c("user_pj_code"))
  )
  
  # parties
  validation_results$parties <- list(
    validate_no_nulls(parties, c("user_py_code"))
  )
  
  # TODO: validate all vegbank codes exist in vegbank (optional???)
  # contributors
  validation_results$contributors <- list(
    validate_no_nulls(contributors, c("user_cr_code", "vb_ar_code", "record_identifier", "contributor_type")),
    validate_no_duplicates(contributors, c("user_cr_code")),
    validate_at_least_one_present(contributors, "vb_py_code", "user_py_code"),
    validate_values_exist(child_df = contributors, child_col = "user_py_code", parent_df = parties, parent_col = "user_py_code")
    # TODO: validate the record identifier based on contributor_type
  )
  
  # plot_observations
  validation_results$plot_observations <- list(
    validate_at_least_one_present(plot_observations, "vb_pl_code", "user_pl_code"),
    validate_no_nulls(plot_observations, c("author_plot_code", "user_ob_code", "author_obs_code")),
    validate_no_duplicates(plot_observations, c("author_plot_code", "user_ob_code", "author_obs_code")),
    validate_values_exist(plot_observations, "user_pj_code", projects, "user_pj_code"),
    validate_values_exist(plot_observations, "user_parent_pl_code", plot_observations, "user_pl_code")
  )
  
  # community classifications
  validation_results$community_classifications <- list(
    validate_no_nulls(community_classifications, c("user_cl_code", "user_ob_code", "vb_cc_code")),
    validate_no_duplicates(community_classifications, c("user_cl_code")),
    validate_values_exist(community_classifications, "user_ob_code", plot_observations, "user_ob_code")
    # TODO: user_comm_class_rf_code
  )
  
  # strata cover
  validation_results$strata_cover_data <- list(
    validate_no_nulls(strata_cover_data, c("user_ob_code", "user_to_code", "user_tm_code", "author_plant_name")),
    validate_no_duplicates(strata_cover_data, c("user_to_code")),
    validate_values_exist(strata_cover_data, "user_ob_code", plot_observations, "user_ob_code")
  )
  
  # strata
  validation_results$strata <- list(
    validate_no_nulls(strata, c("user_ob_code", "user_sr_code", "vb_sy_code")),
    validate_values_exist(strata, "user_ob_code", plot_observations, "user_ob_code"),
    validate_values_exist(strata, "user_sr_code", strata_cover_data, "user_sr_code")
  )
  
  # taxon interpretations
  validation_results$taxon_interpretations <- list(
    validate_no_nulls(taxon_interpretations, c("user_ti_code", "user_to_code", "vb_pc_code", "vb_ar_code")),
    validate_no_duplicates(taxon_interpretations, c("user_ti_code")),
    validate_at_least_one_present(taxon_interpretations, "user_py_code", "vb_py_code"),
    validate_values_exist(taxon_interpretations, "user_to_code", strata_cover_data, "user_to_code"),
    validate_values_exist(taxon_interpretations, "user_py_code", parties, "user_py_code")
    # TODO: references
  )
  
  # disturbances
  validation_results$disturbances <- list(
    validate_no_nulls(disturbances, c("user_do_code", "user_ob_code", "type")),
    validate_no_duplicates(disturbances, c("user_do_code")),
    validate_values_exist(disturbances, "user_ob_code", plot_observations, "user_ob_code")
  )
  
  # soils
  validation_results$disturbances <- list(
    validate_no_nulls(soils, c("user_so_code", "user_ob_code", "horizon")),
    validate_no_duplicates(soils, c("user_so_code")),
    validate_values_exist(soils, "user_ob_code", plot_observations, "user_ob_code")
  )
  
  validation_results <- lapply(validation_results, function(x) all(unlist(x)))
  if (all(unlist(validation_results))){
    cli::cli_alert_success("All loader tables valid.")
  }
  return(validation_results)
  
}

#' Validate no NULL values in specified columns
#'
#' @param df Data frame to validate
#' @param columns Character vector of column names to check
#'
#' @return Logical. TRUE if validation passes, FALSE otherwise
#'
#' @import dplyr
#' @import tidyr
#' @import cli
#'
validate_no_nulls <- function(df, columns) {
  table_name <- deparse(substitute(df))
  
  if (is.null(df)) {
    cli::cli_alert_info("{table_name} not provided - skipping NULL validation")
    return(TRUE)
  }
  
  missing_cols <- columns[!columns %in% names(df)]
  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("{table_name}: Missing required columns: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }
  
  na_summary <- df %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    tidyr::pivot_longer(everything(), names_to = "field", values_to = "na_count") %>%
    filter(.data$na_count > 0)
  
  if (nrow(na_summary) > 0) {
    cli::cli_alert_danger("{table_name}: NULL values found in {paste(paste0(na_summary$field, ' (', na_summary$na_count, ')'), collapse = ', ')}")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate no duplicate values in specified columns
#'
#' @param df Data frame to validate
#' @param columns Character vector of column names to check
#'
#' @return Logical. TRUE if validation passes, FALSE otherwise
#' 
#' @import dplyr
#' @import tidyr
#' @import cli
#' 

validate_no_duplicates <- function(df, columns) {
  table_name <- deparse(substitute(df))
  
  if (is.null(df)) {
    cli::cli_alert_info("{table_name} not provided - skipping duplicate validation")
    return(TRUE)
  }
  
  missing_cols <- columns[!columns %in% names(df)]
  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("{table_name}: Missing columns for duplicate check: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }
  
  has_dupes <- df %>%
    select(all_of(columns)) %>%
    summarise(across(everything(), ~any(duplicated(.)))) %>%
    tidyr::pivot_longer(everything(), names_to = "field", values_to = "has_dupe") %>%
    filter(.data$has_dupe) %>%
    pull(.data$field)
  
  if (length(has_dupes) > 0) {
    cli::cli_alert_danger("{table_name}: Duplicate values found in {paste(has_dupes, collapse = ', ')}")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate values exist in parent table
#'
#' @param child_df Child data frame
#' @param child_col Column name in child data frame
#' @param parent_df Parent data frame
#' @param parent_col Column name in parent data frame
#'
#' @import dplyr
#' @import tidyr
#' @import cli
#'
#' @return Logical. TRUE if validation passes, FALSE otherwise

validate_values_exist <- function(child_df, child_col, parent_df, parent_col) {
  child_table <- deparse(substitute(child_df))
  parent_table <- deparse(substitute(parent_df))
  
  if (is.null(child_df) || is.null(parent_df)) {
    cli::cli_alert_info("{child_table} or {parent_table} not provided - skipping foreign key validation")
    return(TRUE)
  }
  
  if (!child_col %in% names(child_df)) {
    cli::cli_alert_danger("{child_table}: Column '{child_col}' not found")
    return(FALSE)
  }
  
  if (!parent_col %in% names(parent_df)) {
    cli::cli_alert_danger("{parent_table}: Column '{parent_col}' not found")
    return(FALSE)
  }
  
  orphaned <- child_df %>%
    select(all_of(child_col)) %>%
    filter(!is.na(.data[[child_col]])) %>%
    anti_join(parent_df %>% select(all_of(parent_col)), 
              by = stats::setNames(parent_col, child_col)) %>%
    pull(!!child_col) %>%
    unique()
  
  if (length(orphaned) > 0) {
    orphaned_msg <- paste(head(orphaned, 5), collapse = ", ")
    if (length(orphaned) > 5) {
      orphaned_msg <- paste0(orphaned_msg, " (and ", length(orphaned) - 5, " more)")
    }
    cli::cli_alert_danger("{child_table}.{child_col} values not found in {parent_table}.{parent_col}: {orphaned_msg}")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate at least one of two columns is present
#'
#' @param df Data frame to validate
#' @param col1 First column name
#' @param col2 Second column name
#'
#' @import dplyr
#' @import tidyr
#' @import cli
#'
#' @return Logical. TRUE if validation passes, FALSE otherwise
#'
validate_at_least_one_present <- function(df, col1, col2) {
  table_name <- deparse(substitute(df))
  
  if (is.null(df)) {
    cli::cli_alert_info("{table_name} not provided - skipping validation")
    return(TRUE)
  }
  
  missing_cols <- c(col1, col2)[!c(col1, col2) %in% names(df)]
  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("{table_name}: Missing columns: {paste(missing_cols, collapse = ', ')}")
    return(FALSE)
  }
  
  both_null_count <- df %>%
    filter(is.na(.data[[col1]]) & is.na(.data[[col2]])) %>%
    nrow()
  
  if (both_null_count > 0) {
    cli::cli_alert_danger("{table_name}: {both_null_count} rows have NULL in both '{col1}' and '{col2}'")
    return(FALSE)
  }
  
  return(TRUE)
}