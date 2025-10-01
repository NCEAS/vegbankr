#' Get a single stratum method
#'
#' Return a single stratum method record from VegBank, using
#' its `sm` code.
#'
#' @param stratum_method_code A stratum method code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_stratum_method("sm.1")
#' }
#' @export
get_stratum_method <- function(stratum_method_code, parquet = FALSE) {
  resource <- "stratum-methods"
  get_resource_by_code(resource, stratum_method_code, parquet = parquet,
                       clean_names = FALSE)
}

#' Get all stratum methods
#'
#' Return a paginated set of stratum method records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_stratum_methods(limit = 10)
#' }
#' @export
get_all_stratum_methods <- function(limit = 100, offset = 0, parquet = TRUE) {
  resource <- "stratum-methods"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail,
                    parquet = parquet, clean_names = FALSE)
}
