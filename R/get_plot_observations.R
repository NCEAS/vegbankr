#' Get a single plot observation
#'
#' Return a single plot observation record from VegBank, using
#' its `ob` code.
#'
#' @param plot_obs_code A plot observation code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param detail Desired level of detail ("minimal", "full", or "geo")
#' @param with_nested Include nested fields in response?
#' @param num_taxa Max number of taxon observations to return
#' per plot observation, in descending order of reported cover
#' @param num_comms Max number of community classifications to return
#' per plot observation
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_plot_observation("ob.2948")
#' }
#' @export
get_plot_observation <- function(plot_obs_code, parquet = FALSE,
                                 detail = "full", with_nested = TRUE,
                                 num_taxa = 5, num_comms = 5, ...) {
  resource <- "plot-observations"
  get_resource_by_code(resource, plot_obs_code, parquet = parquet,
                       detail = detail, with_nested = with_nested,
                       num_taxa = num_taxa, num_comms = num_comms, ...)
}

#' Get all plot observations
#'
#' Return a paginated set of plot observation records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @param search An optional text search term
#' @param detail Desired level of detail ("minimal", "full", or "geo")
#' @param with_nested Include nested fields in response?
#' @param num_taxa Max number of taxon observations to return
#' per plot observation, in descending order of reported cover
#' @param num_comms Max number of community classifications to return
#' per plot observation
#' @param ... Additional query parameters passed to the VegBank API
#' @return A data frame
#' @examples \dontrun{
#' get_all_plot_observations()
#' }
#' @export
get_all_plot_observations <- function(limit=100, offset=0, parquet = TRUE,
                                      search = NULL, detail = "minimal",
                                      with_nested = FALSE, num_taxa = 5,
                                      num_comms = 5, ...) {
  resource <- "plot-observations"
  get_all_resources(resource, limit, offset, parquet = parquet, search = search,
                    detail = detail, with_nested = with_nested,
                    num_taxa = num_taxa, num_comms = num_comms, ...)
}
