#' Get a single cover method
#'
#' Return a single cover method record from VegBank, using
#' its `cm` code.
#'
#' @param cover_method_code A cover method code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_cover_method("cm.1")
#' }
#' @export
get_cover_method <- function(cover_method_code, parquet = FALSE, ...) {
  resource <- "cover-methods"
  get_resource_by_code(resource, cover_method_code, parquet = parquet, ...)
}

#' Get all cover methods
#'
#' Return a paginated set of cover method records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame
#' @examples \dontrun{
#' get_all_cover_methods(limit = 10)
#' }
#' @export
get_all_cover_methods <- function(limit = 100, offset = 0,
                                  parquet = TRUE, ...) {
  resource <- "cover-methods"
  get_all_resources(resource, limit, offset, parquet = parquet, ...)
}
