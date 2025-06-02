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

#' Get details for a plot observation
#'
#' Return details about a plot observation record from VegBank,
#' including plot observation details, taxon observation, and community
#' classifications, and more, using the plot observation accession code.
#'
#' @param accession_code A plot observation accession code
#' @return A list of 3 data frames, or an empty list if there is no
#' matching record for the provided accession code
#' @examples \dontrun{
#' get_plot_observation_details("VB.Ob.2948.ACAD143")
#' }
#' @import httr2
#' @export
get_plot_observation_details <- function(accession_code) {
  resource <- "get_observation_details"
  sub_table_names <- c("taxa", "communities")
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(accession_code) |>
    req_headers(Accept = "application/json")
  response <- send(request)
  response_json <- resp_body_json(response)[[1]]
  if (!"error" %in% names(response_json)) {
    df_list <- sapply(sub_table_names, function(table_name) {
        jsonlist2df(response_json[[table_name]])
      }, simplify=FALSE)
    obs_data_jsonlist <- response_json[!names(response_json) %in%
                                       sub_table_names]
    obs_df <- jsonlist2df(list(obs_data_jsonlist))
    df_list <- append(df_list, list(plot_observation=obs_df), after = 0)
    df_list <- sapply(df_list, canonicalize_names)
    return(df_list)
  } else {
    warning(response_json[["error"]], call. = FALSE)
    invisible(NULL)
  }
}
