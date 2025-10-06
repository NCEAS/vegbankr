#' Get a single project
#'
#' Return a single project record from VegBank, using its `pj` code.
#'
#' @param project_code A project code
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame (one row), or an empty data frame if there is
#' no matching record for the provided code
#' @examples \dontrun{
#' get_project("pj.340")
#' }
#' @import httr2
#' @export
get_project <- function(project_code, parquet = FALSE) {
  resource <- "projects"
  get_resource_by_code(resource, project_code, parquet = parquet)
}

#' Get all projects
#'
#' Return a paginated set of project records from VegBank.
#'
#' @param limit Number of records to return
#' @param offset Number of records to skip
#' @param parquet Transfer data as Parquet (TRUE) or JSON (FALSE)?
#' @return A data frame
#' @examples \dontrun{
#' get_all_projects()
#' }
#' @import httr2
#' @export
get_all_projects <- function(limit=100, offset=0, parquet = TRUE) {
  resource <- "projects"
  detail <- "full"
  get_all_resources(resource, limit, offset, detail, parquet = parquet)
}
