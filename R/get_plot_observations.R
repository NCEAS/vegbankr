#' Get a single plot observation
#'
#' Return a single plot observation record from VegBank, using
#' its `ob` code.
#'
#' @param plot_obs_code A plot observation code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_plot_observation("ob.2948")
#' }
#' @import httr2
#' @export
get_plot_observation <- function(plot_obs_code, parquet = FALSE) {
  resource <- "plot-observations"
  get_resource_by_code(resource, plot_obs_code, parquet = parquet)
}

#' Get all plot observations
#'
#' Return a paginated set of plot observation records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param detail Desired level of detail ("minimal", "full")
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_plot_observations()
#' }
#' @import httr2
#' @export
get_all_plot_observations <- function(limit=100, offset=0,
    detail = c("minimal", "full"), parquet = TRUE) {
  resource <- "plot-observations"
  get_all_resources(resource, limit, offset, detail, parquet = parquet)
}

#' Get details for a plot observation
#'
#' Return details about a plot observation record from VegBank,
#' including plot observation details, taxon observation, and community
#' classifications, and more, using the plot observation `ob` code.
#'
#' @param plot_obs_code A plot observation code
#' @return A list of 3 data frames, or an empty list if there is no
#' matching record for the provided code
#' @examples \dontrun{
#' get_plot_observation_details("ob.2948")
#' }
#' @import httr2
#' @export
get_plot_observation_details <- function(plot_obs_code) {
  resource <- "get_observation_details"
  sub_table_names <- c("taxa", "communities")
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(plot_obs_code) |>
    req_headers(Accept = "application/json")
  response <- send(request)
  response_json <- resp_body_json(response)
  if (length(response_json[["data"]]) == 0) {
    message("No records returned")
    return(invisible(list()))
  }
  json_record <- response_json[["data"]][[1]]
  df_list <- sapply(sub_table_names, function(table_name) {
      jsonlist2df(json_record[[table_name]])
    }, simplify=FALSE)
  obs_data_jsonlist <- json_record[!names(json_record) %in%
                                   sub_table_names]
  obs_df <- jsonlist2df(list(obs_data_jsonlist))
  df_list <- append(df_list, list(plot_observation=obs_df), after = 0)
  df_list <- sapply(df_list, canonicalize_names)
  return(df_list)
}
