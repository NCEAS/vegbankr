#' Get a single plant concept
#'
#' Return a single plant concept record from VegBank, using
#' its `pc` code.
#'
#' @param plant_concept_code A plant concept code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_plant_concept("pc.111478")
#' }
#' @export
get_plant_concept <- function(plant_concept_code, parquet = FALSE, ...) {
  resource <- "plant-concepts"
  get_resource_by_code(resource, plant_concept_code, parquet = parquet, ...)
}

#' Get all plant concepts
#'
#' Return a paginated set of plant concept records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param search An optional text search term
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame
#' @examples \dontrun{
#' get_all_plant_concepts(limit = 10)
#' }
#' @export
get_all_plant_concepts <- function(limit = 100, offset = 0, parquet = TRUE,
                                   search = NULL, ...) {
  resource <- "plant-concepts"
  get_all_resources(resource, limit, offset, parquet = parquet,
                    search = search, ...)
}
