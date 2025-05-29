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
