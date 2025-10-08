#' Get a single party
#'
#' Return a single party record from VegBank, using its `py` code.
#'
#' @param party_code A party code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_party("py.191378")
#' }
#' @import httr2
#' @export
get_party <- function(party_code, parquet = FALSE) {
  resource <- "parties"
  get_resource_by_code(resource, party_code, parquet = parquet)
}

#' Get all parties
#'
#' Return a paginated set of party records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_parties()
#' }
#' @import httr2
#' @export
get_all_parties <- function(limit=100, offset=0, parquet = TRUE) {
  resource <- "parties"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail, parquet = parquet)
}
