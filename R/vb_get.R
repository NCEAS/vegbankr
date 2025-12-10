vb_get_projects <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "projects"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get("projects", vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

vb_get_parties <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "parties"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

vb_get_references <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "references"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

vb_get_cover_methods <- function(vb_code = NULL, limit = 100, offset = 0,
                                 parquet = NULL, search = NULL,
                                 with_nested = NULL, ...) {
  resource <- "cover-methods"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

vb_get_stratum_methods <- function(vb_code = NULL, limit = 100, offset = 0,
                                   parquet = NULL, search = NULL,
                                   with_nested = NULL, ...) {
  resource <- "stratum-methods"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

vb_get_plant_concepts <- function(vb_code = NULL, limit = 100, offset = 0,
                                  parquet = NULL, search = NULL,
                                  with_nested = NULL, ...) {
  resource <- "plant-concepts"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

vb_get_taxon_observations <- function(vb_code = NULL, limit = 100,
                                      offset = 0, parquet = NULL,
                                      search = NULL,
                                      with_nested = NULL, ...) {
  resource <- "taxon-observations"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

vb_get_plot_observations <- function(vb_code = NULL, limit = 100, offset = 0,
                                     parquet = NULL, search = NULL,
                                     detail = NULL, with_nested = NULL,
                                     num_taxa = NULL, num_comms = NULL, ...) {
  resource <- "plot-observations"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(detail)) detail <- if (is_collection) "minimal" else "full"
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  if (missing(num_taxa)) num_taxa <- if (is_collection) 5 else 5000
  if (missing(num_comms)) num_comms <- if (is_collection) 5 else 5000
  vb_get(resource, vb_code, parquet = parquet, search = search,
         detail = detail, with_nested = with_nested, num_taxa = num_taxa,
         num_comms = num_comms, limit = limit, offset = offset, ...)
}

vb_get <- function(resource, vb_code = NULL, by = NULL, parquet = TRUE,
                   clean_names = FALSE, limit=100, offset=0, ...) {
  if (!is.logical(parquet)) {
    stop("argument 'parquet' must be TRUE or FALSE", call.=FALSE)
  }
  if (!is.logical(clean_names)) {
    stop("argument 'clean_names' must be TRUE or FALSE", call.=FALSE)
  }
  query_params <- list(...)
  request <- request(get_vb_base_url())
  if (is.null(vb_code)) {
    request <- request |> req_url_path_append(resource)
  } else {
    if (is.null(by)) {
       vb_key <- substr(vb_code, 1, 2)
       if (vb_key %in% names(vb_resource_lookup)) {
           by = vb_resource_lookup[[vb_key]]
       } else {
           by = resource
       }
    }
    if (resource == by) {
      request <- request |>
        req_url_path_append(resource) |>
        req_url_path_append(vb_code)
    } else {
      request <- request |>
        req_url_path_append(by) |>
        req_url_path_append(vb_code) |>
        req_url_path_append(resource)
    }
  }
  request <- request |>
    req_url_query(limit = limit,
                  offset = offset) |>
    req_url_query(!!!query_params) |>
    req_headers(Accept = "application/json")
  if (parquet) {
    request <- request |> req_url_query(create_parquet = TRUE)
    response <- send(request)
    vb_data <- vb_df_from_parquet(response, clean_names)
  } else {
    response <- send(request)
    vb_data <- vb_df_from_json(response, clean_names)
  }
  attr(vb_data, "vb_limit") <- limit
  attr(vb_data, "vb_offset") <- offset
  return(vb_data)
}

# Internal mapping table between vb_code prefixes and resource names
vb_resource_lookup <- c(
  cl = "community-classifications",
  cc = "community-concepts",
  ci = "community-interpretations",
  cm = "cover-methods",
  py = "parties",
  pc = "plant-concepts",
  ob = "plot-observations",
  pj = "projects",
  rf = "references",
  sm = "stratum-methods",
  ti = "taxon-interpretations",
  to = "taxon-observations"
)

#' Lookup vb_code prefix via resource name
#'
#' @param resource A resource name (e.g., "plant-concepts")
#' @return The corresponding vb_code prefix (e.g., "pc")
#'
#' @noRd
get_vb_key <- function(resource) {
   names(vb_resource_lookup)[match(resource, vb_resource_lookup)]
}
