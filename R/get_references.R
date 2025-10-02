#' Get a single reference
#'
#' Return a single reference record from VegBank, using
#' its `rf` code.
#'
#' @param reference_code A reference code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_reference("rf.1")
#' }
#' @export
get_reference <- function(reference_code, parquet = FALSE) {
  resource <- "references"
  get_resource_by_code(resource, reference_code, parquet = parquet,
                       clean_names = FALSE)
}

#' Get all references
#'
#' Return a paginated set of reference records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_references(limit = 10)
#' }
#' @export
get_all_references <- function(limit = 100, offset = 0, parquet = TRUE) {
  resource <- "references"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail,
                    parquet = parquet, clean_names = FALSE)
}
