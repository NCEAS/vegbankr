#' Get a single plant concept
#'
#' Return a single plant concept record from VegBank, using
#' its `pc` code. This includes additional processing to take three
#' columns containing nested JSON objects (`usage_names`,
#' `usage_statuses, and `children`), and parse them into companion
#' columns that contain this information in list format.
#'
#' @param plant_concept_code A plant concept code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_plant_concept("pc.111478")
#' }
#' @export
get_plant_concept <- function(plant_concept_code, parquet = FALSE) {
  resource <- "plant-concepts"
  get_resource_by_code(resource, plant_concept_code, parquet = parquet,
                       clean_names = FALSE) |>
    parse_json_column("children", skip_if_missing = TRUE) |>
    parse_json_column("usage_names", skip_if_missing = TRUE) |>
    parse_json_column("usage_statuses", skip_if_missing = TRUE)
}

#' Get all plant concepts
#'
#' Return a paginated set of plant concept records from VegBank. This
#' includes additional processing to take three columns containing
#' nested JSON objects (`usage_names`, `usage_statuses, and `children`),
#' and parse them into companion columns that contain this
#' information in list format.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_plant_concepts(limit = 10)
#' }
#' @export
get_all_plant_concepts <- function(limit = 100, offset = 0, parquet = TRUE) {
  resource <- "plant-concepts"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail, parquet = parquet,
                       clean_names = FALSE) |>
    parse_json_column("children", skip_if_missing = TRUE) |>
    parse_json_column("usage_names", skip_if_missing = TRUE) |>
    parse_json_column("usage_statuses", skip_if_missing = TRUE)
}

#' Parse JSON column to list column
#'
#' Takes a data frame and the name of a column containing character string
#' representations of JSON objects, and parses them to create a new column
#' with R list representations of the same information. The new column is
#' added to the data frame and named by appending `_list` to the input
#' column name.
#'
#' @param df A data frame
#' @param col_name Character string naming the column to parse
#' @param skip_if_missing Logical. If `TRUE`, skip processing if column is
#'        missing instead of throwing an error. Default is `FALSE`.
#'
#' @returns The data frame with an added list column named `{col_name}_list`
#'
#' @importFrom rlang .data := !!
#' @noRd
parse_json_column <- function(df, col_name, skip_if_missing = FALSE) {
  # Check whether col_name exists
  if (!col_name %in% names(df)) {
    if (skip_if_missing) {
      return(df)
    } else {
      stop(sprintf("Column '%s' not found in data frame", col_name))
    }
  }
  # Create the new column with parsed lists
  new_col_name <- paste0(col_name, "_list")
  df |>
    dplyr::mutate(
      !!new_col_name := purrr::map(
        ifelse(is.na(.data[[!!col_name]]), "{}", .data[[!!col_name]]),
        ~ jsonlite::fromJSON(.x, simplifyVector = FALSE)
      )
    )
}
