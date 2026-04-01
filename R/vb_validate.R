#' Validate VegBank loader tables for plot observations and related data
#'
#' Performs validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings. This validation tool is a first pass at catching errors -
#' full validation is only done at upload.
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
#' @examples
#' 
#' plot_observations <- data.frame(
#'   user_ob_code = c("OB001", "OB002", "OB003"),
#'   user_pl_code = c("PL001", "PL002", NA),
#'   vb_pl_code = c(NA, NA, "VB001"),
#'   author_plot_code = c("AP001", "AP002", "AP003"),
#'   author_obs_code = c("AO001", "AO002", "AO003"),
#'   user_pj_code = c("MOJA", "MOJA", "MOJA"),
#'   user_parent_pl_code = c(NA, "PL001", NA),
#'   latitude = c(37.7749, 34.0522, 40.7128),
#'   longitude = c(-122.4194, -118.2437, -74.0060),
#'   obs_start_date = c("2024-01-15", "2024-02-20", "2024-03-10")
#' )
#' 
#' vb_validate_plot_observations(plot_observations = plot_observations)
#'
vb_validate_plot_observations <- function(plot_observations,
                                          projects = NULL, parties = NULL, references = NULL, soils = NULL,
                                          disturbances = NULL, community_classifications = NULL, strata = NULL,
                                          strata_cover_data = NULL, stem_data = NULL, taxon_interpretations = NULL,
                                          contributors = NULL) {
  
  validation_results <- list()
  
  # plot_observations
  validation_results$plot_observations <- list(
    validate_at_least_one_present(plot_observations, "vb_pl_code", "user_pl_code"),
    validate_no_nulls(plot_observations, c("author_plot_code", "user_ob_code", "author_obs_code")),
    validate_no_duplicates(plot_observations, c("user_ob_code", "author_obs_code")),
    validate_values_exist(plot_observations, "user_pj_code", projects, "user_pj_code")
  )
  
  # projects
  if (!is.null(projects)){
    validation_results$projects <- list(
      validate_no_nulls(projects, c("user_pj_code"))
    )
  } else cli::cli_alert_info("projects table not provided - skipping validation")
  
  # parties
  if (!is.null(parties)){
    validation_results$parties <- list(
      validate_no_nulls(parties, c("user_py_code"))
    )
  } else cli::cli_alert_info("parties table not provided - skipping validation")
  
  # contributors
  if (!is.null(contributors)){
    validation_results$contributors <- list(
      validate_no_nulls(contributors, c("user_cr_code", "vb_ar_code", "record_identifier", "contributor_type")),
      validate_no_duplicates(contributors, c("user_cr_code")),
      validate_at_least_one_present(contributors, "vb_py_code", "user_py_code"),
      validate_values_exist(contributors, "user_py_code", parties, parent_col = "user_py_code", optional = TRUE)
    )
    
  } else cli::cli_alert_info("contributors table not provided - skipping validation")
  
  # community classifications
  if (!is.null(community_classifications)){
    validation_results$community_classifications <- list(
      validate_no_nulls(community_classifications, c("user_cl_code", "user_ob_code", "vb_cc_code")),
      validate_no_duplicates(community_classifications, c("user_cl_code")),
      validate_values_exist(community_classifications, "user_ob_code", plot_observations, "user_ob_code")
    )
  } else cli::cli_alert_info("community_classifications table not provided - skipping validation")
  
  # strata cover
  if (!is.null(strata_cover_data)){
    validation_results$strata_cover_data <- list(
      validate_no_nulls(strata_cover_data, c("user_ob_code", "user_to_code", "user_tm_code", "author_plant_name")),
      validate_no_duplicates(strata_cover_data, c("user_tm_code")),
      validate_values_exist(strata_cover_data, "user_ob_code", plot_observations, "user_ob_code"),
      validate_values_exist(strata_cover_data, "user_sr_code", strata, "user_sr_code")
    )
  } else cli::cli_alert_info("strata_cover_data table not provided - skipping validation")
  
  # strata
  if (!is.null(strata)){
    validation_results$strata <- list(
      validate_no_nulls(strata, c("user_ob_code", "user_sr_code", "vb_sy_code")),
      validate_values_exist(strata, "user_ob_code", plot_observations, "user_ob_code")
    )
  } else cli::cli_alert_info("strata table not provided - skipping validation")
  
  # taxon interpretations
  if (!is.null(taxon_interpretations)){
    validation_results$taxon_interpretations <- list(
      validate_no_nulls(taxon_interpretations, c("user_ti_code", "user_to_code", "vb_pc_code", "vb_ar_code")),
      validate_no_duplicates(taxon_interpretations, c("user_ti_code")),
      validate_at_least_one_present(taxon_interpretations, "user_py_code", "vb_py_code"),
      validate_values_exist(taxon_interpretations, "user_to_code", strata_cover_data, "user_to_code"),
      validate_values_exist(taxon_interpretations, "user_py_code", parties, "user_py_code", optional = TRUE)
    )
  } else cli::cli_alert_info("taxon_interpretations table not provided - skipping validation")
  
  # disturbances
  if (!is.null(disturbances)){
    validation_results$disturbances <- list(
      validate_no_nulls(disturbances, c("user_do_code", "user_ob_code", "type")),
      validate_no_duplicates(disturbances, c("user_do_code")),
      validate_values_exist(disturbances, "user_ob_code", plot_observations, "user_ob_code")
    )
  } else cli::cli_alert_info("disturbances table not provided - skipping validation")
  
  # soils
  if (!is.null(soils)){
    validation_results$soils <- list(
      validate_no_nulls(soils, c("user_so_code", "user_ob_code", "horizon")),
      validate_no_duplicates(soils, c("user_so_code")),
      validate_values_exist(soils, "user_ob_code", plot_observations, "user_ob_code")
    )
  } else cli::cli_alert_info("soils table not provided - skipping validation")
  
  # stems
  if (!is.null(stem_data)){
    validation_results$stem_data <- list(
      validate_no_nulls(stem_data, c("user_sc_code", "user_tm_code", "stem_count", "user_sl_code")),
      validate_no_duplicates(stem_data, c("user_sl_code")),
      validate_values_exist(stem_data, "user_tm_code", strata_cover_data, "user_tm_code")
    )
  } else cli::cli_alert_info("stem_data table not provided - skipping validation")
  
  # references
  if (!is.null(references)){
    validation_results$references <- list(
      validate_no_nulls(references, c("user_rf_code")),
      validate_no_duplicates(references, c("user_rf_code"))
    )
  } else cli::cli_alert_info("references table not provided - skipping validation")
  
  
  
  validation_results <- lapply(validation_results, function(x) all(unlist(x)))
  if (all(unlist(validation_results))){
    cli::cli_alert_success("All loader tables valid.")
  }
  return(validation_results)
  
}

#' Validate VegBank loader tables for new plant concepts
#'
#' Performs validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings. This validation tool is a first pass at catching errors -
#' full validation is only done at upload.
#'
#' @param plant_concepts A data frame containing plant concepts as plant names
#'   associated with references, along with with status details and taxonomic
#'   parents
#' @param plant_names A data frame containing plant name usages associated with
#'   specific classification systems for new plant concepts
#' @param plant_correlations A data frame defining correlations between plant
#'   concepts
#' @param parties A data frame containing details about new parties
#' @param references A data frame containing details about new references
#'
#' @return A named list with one element per table, each containing a logical value
#'   (TRUE if all validations passed for that table, FALSE otherwise). For example:
#'   \code{list(parties = TRUE, contributors = FALSE, plot_observations = TRUE)}
#' 
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' 
#' plant_concepts <- data.frame(
#'   user_pc_code = c("pc.91465", "ACER", "ACRU", "ACRUD2", "ACRUD"),
#'   name = c(
#'     "Aceraceae", "Acer L.", "Acer rubrum L.",
#'     "Acer rubrum L. ssp. drummondii (Hook. & Arn. ex Nutt.) A.E. Murray",
#'     "Acer rubrum L. var. drummondii (Hook. & Arn. ex Nutt.) Sarg."),
#'   description = NA_character_,
#'   vb_rf_code = NA_character_,
#'   user_rf_code = "REF001",
#'   user_status_rf_code = "REF001",
#'   plant_concept_status = c("accepted", "accepted", "accepted", "not accepted", "accepted"),
#'   vb_parent_pc_code = rep(c("pc.92372", NA), c(1L, 4L)),
#'   user_parent_pc_code = c(NA, "pc.91465", "ACER", NA, "ACRU"),
#'   plant_level = c("family", "genus", "species", "subspecies", "variety"),
#'   start_date = as.Date("2026-02-04"),
#'   vb_status_py_code = "py.511")
#'   
#' vb_validate_plant_concepts(plant_concepts = plant_concepts)
#' 
#' 
vb_validate_plant_concepts <- function(plant_concepts,
                                       plant_names = NULL, plant_correlations = NULL, parties = NULL,
                                       references = NULL) {
  validation_results <- list()
  
  # plant_concepts
  validation_results$plant_concepts <- list(
    validate_no_nulls(plant_concepts, c("user_pc_code", "name", "start_date", "plant_concept_status")),
    validate_no_duplicates(plant_concepts, c("user_pc_code")),
    validate_at_least_one_present(plant_concepts, "user_rf_code", "vb_rf_code"),
    validate_at_least_one_present(plant_concepts, "user_status_py_code", "vb_status_py_code"),
    validate_values_exist(plant_concepts, "user_rf_code", references, "user_rf_code", optional = TRUE),
    validate_values_exist(plant_concepts, "user_status_py_code", parties, "user_py_code", optional = TRUE)
  )
  
  # plant_correlations
  if (!is.null(plant_correlations)){
    validation_results$plant_correlations <- list(
      validate_no_nulls(plant_correlations, c("convergence_type", "correlation_start")),
      validate_at_least_one_present(plant_correlations, "user_correlated_pc_code", "vb_correlated_pc_code")
    )
  } else cli::cli_alert_info("plant_correlations table not provided - skipping validation")
  
  # plant_names
  if (!is.null(plant_names)){
    validation_results$plant_names <- list(
      validate_no_nulls(plant_names, c("user_pc_code", "name", "name_type", "name_status")),
      validate_at_least_one_present(plant_names, "user_usage_py_code", "vb_usage_py_code"),
      validate_values_exist(plant_names, "user_pc_code", plant_concepts, "user_pc_code"),
      validate_values_exist(plant_names, "user_usage_py_code", parties, "user_py_code", optional = TRUE)
    )
  } else cli::cli_alert_info("plant_names table not provided - skipping validation")
  
  # parties
  if (!is.null(parties)){
    validation_results$parties <- list(
      validate_no_nulls(parties, c("user_py_code")),
      validate_no_duplicates(parties, c("user_py_code"))
    )
  } else cli::cli_alert_info("parties table not provided - skipping validation")
  
  # references
  if (!is.null(references)){
    validation_results$references <- list(
      validate_no_duplicates(references, c("user_rf_code"))
    )
  } else cli::cli_alert_info("references table not provided - skipping validation")
  
  validation_results <- lapply(validation_results, function(x) all(unlist(x)))
  if (all(unlist(validation_results))){
    cli::cli_alert_success("All loader tables valid.")
  }
  return(validation_results)
  
}

#' Validate VegBank loader tables for new community concepts
#'
#' Performs validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings. This validation tool is a first pass at catching errors -
#' full validation is only done at upload.
#' 
#' @param community_concepts A data frame containing community concepts as
#'   community names associated with references, along with with status details
#'   and taxonomic parents
#' @param community_names A data frame containing community name usages
#'   associated with specific classification systems for new community concepts
#' @param community_correlations A data frame defining correlations between
#'   community concepts
#' @param parties A data frame containing details about new parties
#' @param references A data frame containing details about new references
#' @return A named list with one element per table, each containing a logical value
#'   (TRUE if all validations passed for that table, FALSE otherwise). For example:
#'   \code{list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)}
#' 
#' @import dplyr
#' @import tidyr
#' @export
#' 
#' @examples
#' community_concepts <- data.frame(
#'   user_cc_code = "88.100.00",
#'   name = "Abies grandis – Picea sitchensis – Thuja plicata",
#'   user_rf_code = "REF001",
#'   user_status_rf_code = "MCV - CDFW CNPS",
#'   comm_concept_status = "accepted",
#'   user_parent_cc_code = NA,
#'   comm_level = "alliance",
#'   start_date = "1995-11-01",
#'   user_status_py_code = "PY001"
#' )
#' 
#' vb_validate_community_concepts(community_concepts = community_concepts)
vb_validate_community_concepts <- function(community_concepts,
                                           community_names = NULL, community_correlations = NULL, parties = NULL,
                                           references = NULL) {
  
  validation_results <- list()
  # community_concepts
  validation_results$community_concepts <- list(
    validate_no_nulls(community_concepts, c("user_cc_code", "name", "start_date", "comm_concept_status")),
    validate_no_duplicates(community_concepts, c("user_cc_code")),
    validate_at_least_one_present(community_concepts, "user_status_py_code", "vb_status_py_code"),
    validate_values_exist(community_concepts, "user_rf_code", references, "user_rf_code", optional = TRUE),
    validate_values_exist(community_concepts, "user_status_py_code", parties, "user_py_code", optional = TRUE)
  )
  
  # community_names
  if (!is.null(community_names)){
    validation_results$community_names <- list(
      validate_no_nulls(community_names, c("user_cc_code", "name", "name_type", "name_status")),
      validate_values_exist(community_names, "user_usage_py_code", parties, "user_py_code", optional = TRUE)
    )
  } else cli::cli_alert_info("community_names table not provided - skipping validation")
  
  # community_correlations
  if (!is.null(community_correlations)){
    validation_results$community_correlations <- list(
      validate_no_nulls(community_correlations, c("convergence_type", "correlation_start")),
      validate_at_least_one_present(community_correlations, "vb_correlated_cc_code", "user_correlated_cc_code")
    )
  } else cli::cli_alert_info("community_correlations table not provided - skipping validation")
  
  # references
  if (!is.null(references)){
    validation_results$references <- list(
      validate_no_duplicates(references, c("user_rf_code"))
    )
  } else cli::cli_alert_info("references table not provided - skipping validation")
  
  validation_results <- lapply(validation_results, function(x) all(unlist(x)))
  if (all(unlist(validation_results))){
    cli::cli_alert_success("All loader tables valid.")
  }
  return(validation_results)
}

#' Validate VegBank loader tables for new stratum methods
#'
#' Performs validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings. This validation tool is a first pass at catching errors -
#' full validation is only done at upload.
#' 
#' @param stratum_methods A data frame containing stratum methods and their
#'   associated component stratum types
#' @param references A data frame containing details about new references
#' @return A named list with one element per table, each containing a logical value
#'   (TRUE if all validations passed for that table, FALSE otherwise). For example:
#'   \code{list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)}
#' 
#' @import dplyr
#' @import tidyr
#' @export
#' 
#' @examples
#' stratum_methods <- data.frame(
#'   user_sm_code = c("jr_sm_1"),
#'   stratum_method_name = c("my stratum method name"),
#'   stratum_method_description = c("my stratum description"),
#'   stratum_assignment = c("whatever this is"),
#'   user_rf_code = c("REF001"),
#'   stratum_index = c("index 1", "index 2"),
#'   stratum_name = c("name 1", "name 2"),
#'   stratum_description = c("description 1", "description 2")
#' )
#' 
#' vb_validate_stratum_methods(stratum_methods = stratum_methods)
#' 
vb_validate_stratum_methods <- function(stratum_methods, references = NULL){
  
  validation_results <- list()
  
  # stratum_methods
  validation_results$stratum_methods <- list(
    validate_no_nulls(stratum_methods, c("user_sm_code", "stratum_method_name")),
    validate_no_duplicates(stratum_methods, c("user_sm_code")),
    validate_values_exist(stratum_methods, "user_rf_code", references, "user_rf_code")
  )
  
  # references
  if (!is.null(references)){
    validation_results$references <- list(
      validate_no_duplicates(references, c("user_rf_code"))
    )
  } else cli::cli_alert_info("references table not provided - skipping validation")
  
  validation_results <- lapply(validation_results, function(x) all(unlist(x)))
  if (all(unlist(validation_results))){
    cli::cli_alert_success("All loader tables valid.")
  }
  return(validation_results)
  
}

#' Validate VegBank loader tables for new cover methods
#'
#' Performs validation checks on VegBank loader tables to ensure
#' data integrity before upload. Validates required fields, uniqueness constraints,
#' and referential integrity between related tables. Prints validation errors
#' and warnings. This validation tool is a first pass at catching errors -
#' full validation is only done at upload.
#' 
#' @param cover_methods A data frame containing cover methods and their
#'   associated component cover indexes
#' @param references A data frame containing details about new references
#' @return A named list with one element per table, each containing a logical value
#'   (TRUE if all validations passed for that table, FALSE otherwise). For example:
#'   \code{list(community_concepts = TRUE, community_names = FALSE, community_correlations = TRUE)}
#' 
#' @import dplyr
#' @import tidyr
#' @export
#' 
#' @examples
#' 
#' cover_methods <- data.frame(
#'   user_cm_code = c("cm1"),
#'   cover_type = c("My really amazing cover method"),
#'   user_rf_code = "REF001",
#'   cover_code = c("cvr_type_1"),
#'   cover_percent = c(45),
#'   upper_limit = c(10),
#'   lower_limit = c(0)
#' )
#' 
#' vb_validate_cover_methods(cover_methods = cover_methods)
vb_validate_cover_methods <- function(cover_methods, references = NULL){
  
  validation_results <- list()
  
  # cover_methods
  validation_results$cover_methods <- list(
    validate_no_nulls(cover_methods, c("user_cm_code", "cover_type", "cover_code", "cover_percent")),
    validate_no_duplicates(cover_methods, c("user_cm_code")),
    validate_values_exist(cover_methods, "user_rf_code", references, "user_rf_code")
  )
  
  # references
  if (!is.null(references)){
    validation_results$references <- list(
      validate_no_duplicates(references, c("user_rf_code"))
    )
  } else cli::cli_alert_info("references table not provided - skipping validation")
  
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
#' @param optional (Boolean) If FALSE, check will fail if parent column is not found. Otherwise, check will skip.
#'
#' @import dplyr
#' @import tidyr
#' @import cli
#'
#' @return Logical. TRUE if validation passes, FALSE otherwise

validate_values_exist <- function(child_df, child_col, parent_df, parent_col, optional = FALSE) {
  child_table <- deparse(substitute(child_df))
  parent_table <- deparse(substitute(parent_df))
  
  if (is.null(child_df) || is.null(parent_df)) {
    if (optional){
      return(TRUE)
    } else {
      cli::cli_alert_danger("{child_table} or {parent_table} not provided - skipping foreign key validation.")
      return(TRUE) 
    }
  }
  
  if (!(child_col %in% names(child_df))) {
    if (optional) {
      return(TRUE)
    } else {
      cli::cli_alert_danger("{child_table}: Column '{child_col}' not found - skipping foreign key validation.")
      return(FALSE)
    }
  }
  
  if (!(parent_col %in% names(parent_df))) {
    
    if (optional) {
      return(TRUE)
    } else{
      cli::cli_alert_danger("{child_table}: Column '{parent_col}' not found in {parent_table} - skipping foreign key validation")
      return(FALSE)    
    } 
  }
  
  orphaned <- child_df %>%
    select(all_of(child_col)) %>%
    mutate(!!child_col := as.character(.data[[child_col]])) %>%
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
    return(TRUE)
  }
  
  if (!col1 %in% names(df)) {
    df[[col1]] <- NA_character_
  }
  if (!col2 %in% names(df)) {
    df[[col2]] <- NA_character_
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