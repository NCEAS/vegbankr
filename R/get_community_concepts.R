#' Get a single community concept
#'
#' Return a single community concept record from VegBank, using
#' its accession code.
#'
#' @param accession_code A community concept accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_community_concept("VB.cc.30617.ARTEMISIATRIDEN")
#' }
#' @import httr2
#' @export
get_community_concept <- function(accession_code) {
  resource <- "community-concepts"
  get_resource_by_code(resource, accession_code)
}

#' Get all community concepts
#'
#' Return a paginated set of community concept records from VegBank.
#'
#' @return A data frame
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @examples \dontrun{
#' get_all_community_concepts()
#' }
#' @import httr2
#' @export
get_all_community_concepts <- function(limit=100, offset=0,
    detail = c("minimal", "full")) {
  resource <- "community-concepts"
  get_all_resources(resource, limit, offset, detail)
}
