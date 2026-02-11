#' Retrieve VegBank summary stats
#'
#' @description
#' Gets basic summary stats from VegBank as a list of data frames,
#' including a table of high level counts and a few tables with
#' "top N" (by count) or "latest N" (by upload date) reports.
#'
#' @param limit Integer specifying maximum number of records to return
#'    in the "top_n" and "latest_n" tables. Default is 5. Set to `NULL`
#'    to use API default.
#'
#' @return A list of data frames corresponding to the returned summary tables
#'
#' @examples
#' \dontrun{
#' # Retrieve summary stats
#' stats_list <- vb_overview(limit=5)
#' }
#'
#' @export
vb_overview <- function(limit=5) {
  response <- request(vb_get_base_url()) |>
    req_url_path_append("overview") |>
    req_url_query(limit = limit) |>
    req_headers(Accept = "application/json") |>
    send() |>
    resp_body_json()
  sapply(response,
         function(arr) dplyr::bind_rows(arr),
         simplify = FALSE)
}
