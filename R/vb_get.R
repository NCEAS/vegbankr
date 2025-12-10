#' Retrieve data from VegBank
#'
#' @description
#' Retrieve data from the VegBank REST API. `vb_get()` is the core function
#' that can access any resource type. Resource-specific functions like
#' `vb_get_plot_observations()` and `vb_get_plant_concepts()` are convenience
#' wrappers that provide resource-appropriate defaults. For typical usage, the
#' resource-specific functions should be preferred, but `vb_get()` may be useful
#' when using `vegbankr` in a R package or leveraging any experimental VegBank
#' read API features that may not (yet) supported by `vegbankr`.
#'
#' @param vb_code Optional VegBank code to retrieve a specific record.
#' @param limit Integer specifying maximum number of records to return.
#'   Default is 100. Set to `NULL` to use API default.
#' @param offset Integer specifying the offset for pagination. Default is 0.
#'   Set to `NULL` to use API default.
#' @param parquet Logical indicating whether to request data in Parquet format
#'   instead of JSON. This is transparent to users insofar as data will
#'   be returned as a data frame in either case, but performance may differ
#'   between the two options. If not specified, defaults to `TRUE` for
#'   collection queries and `FALSE` for single-record queries.
#' @param search Optional search string for filtering results based on full-text
#'   search. Available for: plot-observations, plant-concepts, community-concepts,
#'   projects, and parties.
#' @param sort Optional string for sorting results. Prepend with "-" for
#'   descending order (e.g., "-obs_count"). Available for:
#'   \itemize{
#'     \item \strong{plot-observations:} "default", "author_obs_name"
#'     \item \strong{plant-concepts:} "default", "plant_name", "obs_count"
#'     \item \strong{community-concepts:} "default", "comm_name", "obs_count"
#'     \item \strong{projects:} "default", "project_name", "obs_count"
#'     \item \strong{parties:} "default", "surname", "organization_name",
#'           "obs_count"
#'   }
#' @param detail Character string specifying level of detail. All endpoints
#'   support "full" detail. For those that support "minimal" detail, this is the
#'   default for collection queries, otherwise the default is "full". Plot
#'   observations additionally support detail="geo". In all cases, set to `NULL`
#'   to use the API default.
#' @param with_nested Logical indicating whether to include nested data
#'   structures. All endpoints support `FALSE`. For those that support `TRUE`,
#'   this is the default for individual record queries, otherwise the default is
#'   `FALSE`. Set to `NULL` to use the API default.
#' @param num_taxa \emph{Available only for plot-observations.} Integer
#'   specifying maximum number of taxon observations to return as a nested
#'   array. If not specified, defaults to 5 for collection queries and 5000
#'   for single-record queries. Set to `NULL` to use the API default.
#' @param num_comms \emph{Available only for plot-observations.} Integer
#'   specifying maximum number of community classifications records to return as
#'   a nested array.  If not specified, defaults to 5 for collection queries and
#'   5000 for single-record queries. Set to `NULL` to use the API default.
#' @param resource \emph{Available only for `vb_get()`.} Character string
#'   specifying the VegBank resource type to retrieve (e.g.,
#'   "plot-observations", "projects").
#' @param by \emph{Available only for `vb_get()`.} Character string specifying
#'   the VegBank resource type associated with the provided `vb_code` (if
#'   specified). If omitted, this will be inferred based on the format of the
#'   `vb_code`, so in general it is not necessary. However, if using `vb_get()`
#'   in a function, specifying `by` may be useful to ensure that the correct
#'   resource is being queried.
#' @param clean_names \emph{Available only for `vb_get()`.} Logical indicating
#'   whether to canonicalize column names to snake case. Default is `FALSE`.
#' @param ... Additional query parameters passed to the API endpoint as
#'   key-value pairs. E.g., foo="bar" will add a URL query parameter "?foo=bar"
#'   to the API GET request.
#'
#' @return A data frame containing the requested VegBank data.
#'
#' @details
#'
#' The `vb_get*()` family of functions can all be used to perform three types of
#' queries, which differ in terms of the set of records returned as well as the
#' default query parameters applied.
#'
#' ## Available resource-specific functions:
#'
#' * `vb_get_plot_observations()` - Plot observation data
#' * `vb_get_community_concepts()` - Community concepts (assertions) linked to
#' community names through usages
#' * `vb_get_community_classifications()` - Community classification events
#'   wherein one or more community concepts were applied to a plot observation
#' * `vb_get_community_interpretations()` - Assignments of community names and
#'   authorities (i.e., community concepts) to specific plot observations, as
#'   part of a community classification event
#' * `vb_get_plant_concepts()` - Plant concepts (assertions) linked to plant
#'   names through usages
#' * `vb_get_taxon_observations()` - Data provider's determination of taxa
#'   observed on a plot, and the overall cover of those taxa
#' * `vb_get_taxon_interpretations()` - Assignments of taxon names and
#'   authorities (i.e., plant concepts) to specific taxon observations
#' * `vb_get_cover_methods()` - Information about registered coverclass methods
#' * `vb_get_stratum_methods()` - Information about registered strata sampling
#'   protocols
#' * `vb_get_references()` - Information about references cited within VegBank
#' * `vb_get_projects()` - Information about projects established to collect
#'   vegetation plot data
#' * `vb_get_parties()` - Information about people and organizations who have
#'   contributed to the collection or interpretation of a plot
#'
#' ## Query types
#'
#' ### Single-record queries
#'
#' A query is considered a "single-record query" if a `vb_code` is provided with
#' prefix that matches the type of the target resource. For example,
#' `vb_get_plot_observations("ob.2948")` will return the single VegBank plot
#' observation record corresponding to "ob.2948", if one exists.
#'
#' ### Cross-resource collection queries
#'
#' A query is considered a "cross-resource collection query" if a `vb_code` is
#' provided with prefix matching a resource type that _differs_ from the target
#' resource.  For example, `vb_get_plot_observations("pj.340")`, will return the
#' collection of VegBank plot observation records corresponding to project
#' "pj.340", if the project and corresponding plot observations exist.
#'
#' ### Full collection queries
#'
#' A query is considered a "full collection query" if no `vb_code` is provided.
#' For example, `vb_get_plot_observations()` will return all plot observations.
#' Both collection query types are paginated via `limit` and `offset`, and can
#' be further filtered with parameters like `search` when supported.
#'
#' ## Smart defaults:
#'
#' Many parameters have "smart defaults" that change based on whether you're
#' querying a single record or a collection of records. In general,
#' single-record queries automatically use settings optimized for detailed
#' individual records (more nested data, higher limits for related entities),
#' whereas collection queries use leaner settings.  For example,
#' `vb_get_plot_observations()` uses `detail = "minimal"` and `with_nested =
#' FALSE` for collection queries, but `detail = "full"` and `with_nested = TRUE`
#' for single records.
#'
#' To use the API's own defaults instead of these smart defaults, explicitly
#' pass `NULL` for the parameter (e.g., `detail = NULL`).
#'
#' @examples
#' \dontrun{
#' # Collection query - uses minimal detail and excludes nested fields
#' plots <- vb_get_plot_observations(limit = 10)
#'
#' # Single record - uses full detail with nested fields
#' plot <- vb_get_plot_observations(vb_code = "ob.12345")
#'
#' # Override defaults explicitly
#' plots <- vb_get_plot_observations(limit = 10, detail = "full", num_taxa = 50)
#'
#' # Use API defaults instead of smart defaults
#' plots <- vb_get_plot_observations(limit = NULL, detail = NULL, num_taxa = NULL)
#'
#' # Get projects (different available parameters)
#' projects <- vb_get_projects(search = "heritage", limit = 20)
#' }
#'
#' @name vb_get
#' @export
vb_get_projects <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "projects"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get("projects", vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
vb_get_parties <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "parties"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
vb_get_references <- function(vb_code = NULL, limit = 100, offset = 0,
                            parquet = NULL, search = NULL, sort = NULL, ...) {
  resource <- "references"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         sort = sort, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
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

#' @rdname vb_get
#' @export
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

#' @rdname vb_get
#' @export
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

#' @rdname vb_get
#' @export
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

#' @rdname vb_get
#' @export
vb_get_taxon_interpretations <- function(vb_code = NULL, limit = 100,
                                         offset = 0, parquet = NULL,
                                         detail = NULL, ...) {
  resource <- "taxon-interpretations"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(detail)) detail <- if (is_collection) "minimal" else "full"
  vb_get(resource, vb_code, parquet = parquet, detail = detail,
         limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
vb_get_community_concepts <- function(vb_code = NULL, limit = 100, offset = 0,
                                      parquet = NULL, search = NULL,
                                      with_nested = NULL, ...) {
  resource <- "community-concepts"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
vb_get_community_classifications <- function(vb_code = NULL, limit = 100,
                                             offset = 0, parquet = NULL,
                                             search = NULL, detail = NULL,
                                             with_nested = NULL, ...) {
  resource <- "community-classifications"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(detail)) detail <- if (is_collection) "minimal" else "full"
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, search = search, detail = detail,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
vb_get_community_interpretations <- function(vb_code = NULL, limit = 100,
                                             offset = 0, parquet = NULL,
                                             detail = NULL, with_nested = NULL,
                                             ...) {
  resource <- "community-interpretations"
  vb_key <- get_vb_key(resource)
  is_collection <- is.null(vb_code) || substr(vb_code, 1, 2) != vb_key
  if (missing(parquet)) parquet <- if (is_collection) TRUE else FALSE
  if (missing(detail)) detail <- if (is_collection) "minimal" else "full"
  if (missing(with_nested)) with_nested <- if (is_collection) FALSE else TRUE
  vb_get(resource, vb_code, parquet = parquet, detail = detail,
         with_nested = with_nested, limit = limit, offset = offset, ...)
}

#' @rdname vb_get
#' @export
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

#' @rdname vb_get
#' @import httr2
#' @importFrom rlang !!!
#' @export
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
