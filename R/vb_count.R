#' Get record count for a VegBank resource
#'
#' `vb_count()` and friends (implemented for each of the VegBank
#' resource types) are used to retrieve a record count from the VegBank
#' API, optionally constrained by filtering query parameters. The count
#' is the same as the `count_reported` attribute attached to API data
#' responses (see [get_page_details()]), but is returned here without
#' actually querying for any data.
#'
#' @param resource VegBank API resource (e.g., `plot-observation`)
#' @param ... Additional API query parameters
#' @return Integer count, or NULL if the count cannot be retrieved
#' @examples \dontrun{
#' vb_count("projects")
#' vb_count_projects(search="california")
#' }
#'
#' @rdname vb_count
#' @import httr2
#' @importFrom rlang !!!
#' @export
vb_count <- function(resource, ...) {
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_headers(Accept = "application/json") |>
    req_url_query(count = TRUE) |>
    req_url_query(!!!list(...))
  resp <- send(request)
  msg_warn <- "Failed to obtain count from VegBank"
  count <- tryCatch(
    resp_body_json(resp)$count,
    error = function(msg) NULL)
  if (is.null(count)) {
    warning(msg_warn, call.=FALSE)
  }
  return(count)
}

#' @rdname vb_count
#' @export
vb_count_community_classifications <- function(...) {
  vb_count("community-classifications", ...)
}

#' @rdname vb_count
#' @export
vb_count_community_concepts <- function(...) {
  vb_count("community-concepts", ...)
}

#' @rdname vb_count
#' @export
vb_count_cover_methods <- function(...) {
  vb_count("cover-methods", ...)
}

#' @rdname vb_count
#' @export
vb_count_parties <- function(...) {
  vb_count("parties", ...)
}

#' @rdname vb_count
#' @export
vb_count_plant_concepts <- function(...) {
  vb_count("plant-concepts", ...)
}

#' @rdname vb_count
#' @export
vb_count_plot_observations <- function(...) {
  vb_count("plot-observations", ...)
}

#' @rdname vb_count
#' @export
vb_count_projects <- function(...) {
  vb_count("projects", ...)
}

#' @rdname vb_count
#' @export
vb_count_references <- function(...) {
  vb_count("references", ...)
}

#' @rdname vb_count
#' @export
vb_count_stratum_methods <- function(...) {
  vb_count("stratum-methods", ...)
}

#' @rdname vb_count
#' @export
vb_count_taxon_observations <- function(...) {
  vb_count("taxon-observations", ...)
}
