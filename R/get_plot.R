#' Get a single plot observation
#'
#' Return a single plot observation record from VegBank, using the
#' observation's accession code.
#'
#' @param accession_code A plot observation accession code
#' @returns A data frame (one row), or an empty list if there is no
#' matching record for the provided accession code.
#' @examples \dontrun{
#' get_plot("VB.PL.48373.VZ17QEZ6PVLCDPY")
#' }
#' @import httr2
#' @export
get_plot <- function(accession_code) {
  resource <- "plot"
  response <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(accession_code) |>
    req_headers(Accept = "application/json") |>
    req_perform()
  plot <- response |>
    resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)
  return(plot)
}
