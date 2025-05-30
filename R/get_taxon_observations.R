#' Get a single taxon observation
#'
#' Return a single taxon observation record from VegBank, using the
#' observation's accession code.
#'
#' @param accession_code A taxon observation accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_taxon_observation("VB.TO.64992.VACCINIUMBOREAL")
#' }
#' @import httr2
#' @export
get_taxon_observation <- function(accession_code) {
  resource <- "taxon-observations"
  get_resource_by_code(resource, accession_code)
}

#' Get all plot observations
#'
#' Return a paginated set of taxon observation records from VegBank.
#'
#' @return A data frame
#' @param max_taxa_per_plot Max number of taxon observations to return
#' per plot observation, in descending order of reported cover
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @examples \dontrun{
#' get_all_taxon_observations()
#' }
#' @import httr2
#' @export
get_all_taxon_observations <- function(max_taxa_per_plot = 5, limit=100, offset=0,
    detail = c("minimal", "full")) {
  resource <- "taxon-observations"
  get_all_resources(resource, limit, offset, detail,
                    numTaxa = max_taxa_per_plot)
}
