#' Create a VegBank dataset
#'
#' @description
#' Use the VegBank API to create a user dataset, which defines a
#' collection of plot observations that can be cited in VegBank.
#'
#' @param name A single character string giving the dataset name. Must be 100
#'   characters or fewer.
#' @param description A single character string describing the dataset.
#' @param observations A character vector of observation codes. Each element
#'   must match the pattern \code{"ob.<positive_integer>"} (e.g.
#'   \code{"ob.2948"}), and must refer to a plot observation in VegBank. The
#'   vector must not be empty.
#' @param dry_run Logical indicating whether to perform a dry run. If `TRUE`,
#'   the API will validate the data without committing changes to the database.
#'   Default is `FALSE`.
#'
#' @return The processed response object from the VegBank API documenting what
#'   (if anything) was successfully created in VegBank.
#'
#' @examples
#' \dontrun{
#' vb_create_dataset(
#'   name = "Test Dataset 001",
#'   description = "A test dataset containing 10 observations",
#'   observations = paste0("ob.", 2948:2957)
#' )
#' }
#'
#' @export
vb_create_dataset <- function(name, description, observations,
                              dry_run = FALSE) {

  # Validate inputs
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single non-NA character string.")
  }
  if (nchar(name) > 100L) {
    stop("`name` must be 100 characters or fewer.")
  }

  if (!is.character(description) || length(description) != 1L ||
        is.na(description)) {
    stop("`description` must be a single non-NA character string.")
  }

  if (!is.character(observations) || length(observations) == 0L) {
    stop("`observations` must be a non-empty character vector.")
  }

  ob_pattern <- "^ob\\.[1-9][0-9]*$"
  invalid <- observations[!grepl(ob_pattern, observations)]
  if (length(invalid) > 0L) {
    stop(
      "`observations` contains invalid codes: ",
      paste(invalid, collapse = ", ")
    )
  }

  request <- build_dataset_request(name, description, observations, dry_run)
  response <- send(request)
  handle_vb_upload_response(response)
}

#' Build httr2 request for POST /user-datasets
#'
#' @param name A single character string giving the dataset name.
#' @param description A single character string describing the dataset.
#' @param observations A character vector of VegBank observation codes.
#' @param dry_run Logical indicating whether to perform a dry run. If `TRUE`,
#'   the API will validate the data without committing changes to the database.
#'   Default is `FALSE`.
#'
#' @return An httr2 response
#'
#' @import httr2
#' @importFrom jsonlite toJSON
#' @noRd
build_dataset_request <- function(name, description, observations,
                                  dry_run = FALSE) {
  # Assemble payload
  json_payload <- list(
    name = name,
    description = description,
    data = list(observation = as.list(observations))
  ) |> jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  # Build request
  request <- request(vb_get_base_url()) |>
    req_url_path_append("user-datasets") |>
    req_method("POST") |>
    req_url_query(dry_run = dry_run) |>
    req_headers("Content-Type"  = "application/json") |>
    req_body_raw(json_payload, type = "application/json")

  return(request)
}
