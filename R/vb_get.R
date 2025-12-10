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
