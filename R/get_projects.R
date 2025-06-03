#' Get a single project
#'
#' Return a single project record from VegBank, using its accession
#' code.
#'
#' @param accession_code A project accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_project("VB.Pj.340.ACADIANATIONALP")
#' }
#' @import httr2
#' @export
get_project <- function(accession_code) {
  resource <- "projects"
  get_resource_by_code(resource, accession_code)
}

#' Get all projects
#'
#' Return a paginated set of project records from VegBank.
#'
#' @return A data frame
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @examples \dontrun{
#' get_all_projects()
#' }
#' @import httr2
#' @export
get_all_projects <- function(limit=100, offset=0) {
  resource <- "projects"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail)
}
