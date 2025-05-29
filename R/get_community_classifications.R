#' Get a single community classification
#'
#' Return a single community classification record from VegBank, using
#' its accession code.
#'
#' @param accession_code A community classification accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_community_classification("VB.Cl.1553.2948")
#' }
#' @import httr2
#' @export
get_community_classification <- function(accession_code) {
  resource <- "community-classifications"
  get_resource_by_code(resource, accession_code)
}

#' Get all community classifications
#'
#' Return a paginated set of community classification records from VegBank.
#'
#' @return A data frame
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @examples \dontrun{
#' get_all_community_classifications()
#' }
#' @import httr2
#' @export
get_all_community_classifications <- function(limit=100, offset=0,
    detail = c("minimal", "full")) {
  resource <- "community-classifications"
  get_all_resources(resource, limit, offset, detail)
}
