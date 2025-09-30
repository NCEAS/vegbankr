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
