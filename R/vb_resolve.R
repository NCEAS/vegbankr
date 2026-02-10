#' Resolve a VegBank identifier
#'
#' @description
#' Queries the VegBank API to resolve a public identifier (such as an accession
#' code or DOI) to its internal resource details.
#'
#' @param identifier A character string specifying the VegBank identifier to
#'   resolve. This can be an accession code (e.g., "VB.Ob.2948.ACAD143") or
#'   other supported identifier type.
#'
#' @return A list containing the resolved identifier details with the following
#'   components:
#'   \describe{
#'     \item{identifier_value}{The original identifier value provided}
#'     \item{identifier_type}{Type of identifier (e.g., "accession_code")}
#'     \item{vb_code}{VegBank code for the resource}
#'     \item{vb_resource_type}{VegBank resource type}
#'   }
#'
#' @examples
#' \dontrun{
#' # Resolve an accession code
#' result <- vb_resolve("VB.Ob.2948.ACAD143")
#' result$vb_code  # "ob.2948"
#' }
#'
#' @export
vb_resolve <- function(identifier) {
  response <- request(vb_get_base_url()) |>
    req_url_path_append("identifiers") |>
    req_url_path_append(identifier) |>
    req_headers(Accept = "application/json") |>
    send() |>
    resp_body_json()
  if (response$vb_table_code %in% names(vb_resource_lookup)) {
      vb_resource_type <- vb_resource_lookup[[response$vb_table_code]]
  } else {
    warning("Unknown resource type code ", dQuote(response$vb_table_code))
    vb_resource_type <- "unknown"
  }
  list(
    identifier_value = response$identifier_value,
    identifier_type = response$identifier_type,
    vb_code = response$vb_code,
    vb_resource_type = vb_resource_type
  )
}

#' Retrieve a VegBank resource by identifier
#'
#' @description
#' Fetches a VegBank resource using any supported identifier. This function
#' first resolves the identifier to determine the resource type and internal
#' code, then retrieves the full resource data.
#'
#' @param identifier A character string specifying the VegBank identifier.
#'   This can be an accession code, DOI, or other supported identifier type.
#' @param verbose Logical. If `TRUE`, prints a message indicating which resource
#'   was retrieved. Default is `FALSE`.
#' @param ... Additional query parameters passed to `vb_get()` as
#'   key-value pairs. E.g., foo="bar" will add a URL query parameter
#'   "?foo=bar" to the API GET request.
#'
#' @return A data frame containing the requested resource data. The structure
#'   depends on the resource type.
#'
#' @examples
#' \dontrun{
#' # Retrieve a dataset silently
#' data <- vb_get_by_id("VB.Ob.2948.ACAD143")
#'
#' # Retrieve with informational message
#' data <- vb_get_by_id("VB.Ob.2948.ACAD143", verbose = TRUE)
#' }
#'
#' @seealso [vb_resolve()] for identifier resolution details
#'
#' @export
vb_get_by_id <- function(identifier, ..., verbose = FALSE) {
  id_map <- tryCatch(
    expr = vb_resolve(identifier),
    warning = function(w) {
      stop("Can't retrieve identifier ", dQuote(identifier),
           ": ", conditionMessage(w), call. = FALSE)
    },
    error = function(e) {
      stop(conditionMessage(e), call. = FALSE)
    }
  )
  data <- vb_get(id_map$vb_resource_type, id_map$vb_code, ...)
  if (verbose) {
      message("Retrieved ", id_map$vb_resource_type,
              " record ", id_map$vb_code)
  }
  return(data)
}
