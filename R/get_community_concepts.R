#' Get a single community concept
#'
#' Return a single community concept record from VegBank, using
#' its `cc` code.
#'
#' @param comm_concept_code A community concept code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_community_concept("cc.30617")
#' }
#' @import httr2
#' @export
get_community_concept <- function(comm_concept_code, parquet = FALSE) {
  resource <- "community-concepts"
  get_resource_by_code(resource, comm_concept_code, parquet = parquet,
                       clean_names = FALSE)
}

#' Get all community concepts
#'
#' Return a paginated set of community concept records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_community_concepts()
#' }
#' @import httr2
#' @export
get_all_community_concepts <- function(limit = 100, offset = 0,
                                       parquet = TRUE) {
  resource <- "community-concepts"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail,
                    parquet = parquet, clean_names = FALSE)
}
