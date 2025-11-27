#' Get a single community classification
#'
#' Return a single community classification record from VegBank, using
#' its `cl` code.
#'
#' @param comm_class_code A community classification code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param detail Desired level of detail ("minimal" or "full")
#' @param with_nested Include nested fields in response?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_community_classification("cl.1553")
#' }
#' @export
get_community_classification <- function(comm_class_code, parquet = FALSE,
                                         detail = "full", with_nested = TRUE,
                                         ...) {
  resource <- "community-classifications"
  get_resource_by_code(resource, comm_class_code, parquet = parquet,
                       detail = detail, with_nested = with_nested, ...)
}

#' Get all community classifications
#'
#' Return a paginated set of community classification records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param detail Desired level of detail ("minimal" or "full")
#' @param with_nested Include nested fields in response?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame
#' @examples \dontrun{
#' get_all_community_classifications()
#' }
#' @export
get_all_community_classifications <- function(limit=100, offset=0,
                                              parquet = TRUE,
                                              detail = "minimal",
                                              with_nested = FALSE, ...) {
  resource <- "community-classifications"
  get_all_resources(resource, limit, offset, parquet = parquet,
                    detail = detail, with_nested = with_nested, ...)
}
