#' Get a single taxon observation
#'
#' Return a single taxon observation record from VegBank, using
#' its `to` code.
#'
#' @param taxon_obs_code A taxon observation code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_taxon_observation("to.64992")
#' }
#' @import httr2
#' @export
get_taxon_observation <- function(taxon_obs_code, parquet = FALSE) {
  resource <- "taxon-observations"
  get_resource_by_code(resource, taxon_obs_code, parquet = parquet,
                       clean_names = TRUE)
}

#' Get all plot observations
#'
#' Return a paginated set of taxon observation records from VegBank.
#'
#' @param max_taxa_per_plot Max number of taxon observations to return
#' per plot observation, in descending order of reported cover
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_taxon_observations()
#' }
#' @import httr2
#' @export
get_all_taxon_observations <- function(max_taxa_per_plot = 5, limit=100,
                                       offset=0, parquet = TRUE) {
  resource <- "taxon-observations"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail,
                    num_taxa = max_taxa_per_plot, parquet = parquet,
                    clean_names = TRUE)
}
