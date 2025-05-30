#' Get a single plot observation
#'
#' Return a single plot observation record from VegBank, using the
#' observation's accession code.
#'
#' @param accession_code A plot observation accession code
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided accession code
#' @examples \dontrun{
#' get_plot_observation("VB.Ob.2948.ACAD143")
#' }
#' @import httr2
#' @export
get_plot_observation <- function(accession_code) {
  resource <- "plot-observations"
  get_resource_by_code(resource, accession_code)
}

#' Get all plot observations
#'
#' Return a paginated set of plot observation records from VegBank.
#'
#' @return A data frame
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @examples \dontrun{
#' get_all_plot_observations()
#' }
#' @import httr2
#' @export
get_all_plot_observations <- function(limit=100, offset=0,
    detail = c("minimal", "full")) {
  resource <- "plot-observations"
  get_all_resources(resource, limit, offset, detail)
}
