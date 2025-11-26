#' Get a single community classification
#'
#' Return a single community classification record from VegBank, using
#' its `cl` code.
#'
#' @param comm_class_code A community classification code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_community_classification("cl.1553")
#' }
#' @export
get_community_classification <- function(comm_class_code,
                                         parquet = FALSE) {
  resource <- "community-classifications"
  get_resource_by_code(resource, comm_class_code, parquet = parquet)
}

#' Get all community classifications
#'
#' Return a paginated set of community classification records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_community_classifications()
#' }
#' @export
get_all_community_classifications <- function(limit=100, offset=0,
                                              detail = c("minimal", "full"),
                                              parquet = TRUE) {
  resource <- "community-classifications"
  get_all_resources(resource, limit, offset, detail, parquet = parquet)
}
