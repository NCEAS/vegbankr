#' Get a single party
#'
#' Return a single party record from VegBank, using its accession code.
#'
#' @param accession_code A party accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_party("VB.py.191378.VOLUNTEER")
#' }
#' @import httr2
#' @export
get_party <- function(accession_code) {
  resource <- "parties"
  get_resource_by_code(resource, accession_code)
}

#' Get all parties
#'
#' Return a paginated set of party records from VegBank.
#'
#' @return A data frame
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @examples \dontrun{
#' get_all_parties()
#' }
#' @import httr2
#' @export
get_all_parties <- function(limit=100, offset=0) {
  resource <- "parties"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail)
}
