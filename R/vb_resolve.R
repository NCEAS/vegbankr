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
#'     \item{identifier_id}{Internal numeric identifier}
#'     \item{identifier_type}{Type of identifier (e.g., "accession_code")}
#'     \item{identifier_value}{The original identifier value provided}
#'     \item{vb_code}{VegBank code for the resource}
#'     \item{vb_record_id}{Numeric record ID}
#'     \item{vb_table_code}{Table code indicating resource type}
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
  request(vb_get_base_url()) |>
    req_url_path_append("identifiers") |>
    req_url_path_append(identifier) |>
    req_headers(Accept = "application/json") |>
    send() |>
    resp_body_json()
}
