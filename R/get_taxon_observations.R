#' Get a single taxon observation
#'
#' Return a single taxon observation record from VegBank, using
#' its `to` code.
#'
#' @param taxon_obs_code A taxon observation code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param with_nested Include nested fields in response?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_taxon_observation("to.64992")
#' }
#' @export
get_taxon_observation <- function(taxon_obs_code, parquet = FALSE,
                                  with_nested = TRUE, ...) {
  resource <- "taxon-observations"
  get_resource_by_code(resource, taxon_obs_code, parquet = parquet,
                       with_nested = with_nested, ...)
}

#' Get all taxon observations
#'
#' Return a paginated set of taxon observation records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param with_nested Include nested fields in response?
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame
#' @examples \dontrun{
#' get_all_taxon_observations()
#' }
#' @export
get_all_taxon_observations <- function(limit=100, offset=0, parquet = TRUE,
                                       with_nested = FALSE, ...) {
  resource <- "taxon-observations"
  get_all_resources(resource, limit, offset, parquet = parquet,
                    with_nested = with_nested, ...)
}
