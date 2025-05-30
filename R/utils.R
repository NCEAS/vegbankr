#' Set a base URL for the VegBank API
#'
#' Sets a global option specifying the base URL for the VegBank API
#' using the provided `vb_base_url` string, optionally adding a port as
#' "http(s)://vb_base_url:port".
#'
#' @param vb_base_url (character) The base URL, including protocol and domain
#' @param port (numeric) Optional port value
#' @returns NULL
#' @examples
#' set_vb_base_url("https://api.vegbank.org")
#' set_vb_base_url("http://localhost", port = 8080)
#' @seealso [get_vb_base_url()]
#' @export
set_vb_base_url <- function(vb_base_url, port) {
  if (!missing(port)) {
    vb_base_url <- paste(vb_base_url, port, sep = ":")
  }
  options(vegbank.base_api_url = vb_base_url)
  message("Using ", get_vb_base_url(), " as base URL")
}

#' Get the currently configured base URL for the VegBank API
#'
#' Gets the base URL for the VegBank API. If previously set by the user,
#' e.g. via `set_vb_base_url()`, this will be pulled from the global
#' option (`vegbank.base_api_url`). If this option is unset, the package
#' default value of "https://api.vegbank.org" will be used.
#'
#' @returns A length-one character vector containing the base URL string
#' @seealso [set_vb_base_url()]
#' @export
get_vb_base_url <- function() {
  base_url <- getOption("vegbank.base_api_url")
  if (base_url == "" || is.null(base_url)) {
    options(vegbank.base_api_url = "https://api.vegbank.org")
  }
  getOption("vegbank.base_api_url")
}

#' Canonicalize VegBank column names (i.e. convert to snake_case), using
#' a package-provided lookup table by default
#'
#' Takes a data frame of VegBank records, and canonicalizes the column
#' names by converting them to snake_case via a package-provided
#' lookup_table. If any column names in the input data frame are
#' unmatched in the lookup table, these are left unaltered in the
#' output, and a warning message is displayed.
#'
#' Callers may optionally provide a lookup table
#'
#' @param target_df (dataframe)
#' @param lookup_df (dataframe) Optional custom names lookup table
#' @returns A data frame matching the input data frame, but with
#' canonicalized column names
#' @examples
#' canonicalize_names(data.frame(
#'   stratum_ID = integer(),
#'   stratummethodname = character()
#' ))
#' @importFrom rlang .data
#' @export
canonicalize_names <- function(target_df, lookup_df) {
  if (missing(lookup_df)) {
    lookup_file <- system.file("canonical-name-lookup.txt",
                               package = "vegbankr")
    lookup_df <- utils::read.csv(lookup_file)
  }
  original_names <- tolower(names(target_df))
  augmented_names <- data.frame(lower=original_names) %>%
    dplyr::left_join(lookup_df, by=dplyr::join_by("lower"))
  if (any(is.na(augmented_names$snake))) {
     warning("Unmatched names: ",
       paste(augmented_names %>%
               dplyr::filter(is.na(.data$snake)) %>%
               dplyr::pull(.data$lower),
             collapse=", "))
  }
  canonicalized_names <- augmented_names %>%
    dplyr::mutate(canonical=dplyr::coalesce(.data$snake, .data$lower)) %>%
    dplyr::pull(.data$canonical)
  names(target_df) <- canonicalized_names
  return(target_df)
}

#' Transform VegBank response into a data frame
#'
#' Transforms a VegBank API response into a data frame, canonicalizing
#' names by default. If the API returns an error (indicated by a
#' top-level "error" key in the JSON response), the error message is
#' displayed as an R warning, and `NULL` is returned. If API returns a
#' non-error response with a reported record count of 0, an informative
#' message is displayed, and an empty data frame is returned.
#'
#' @param response VegBank API response object
#' @param clean_names (logical) VegBank API response object
#'
#' @noRd
as_vb_dataframe <- function(response, clean_names = TRUE) {
  response_list <- response |>
    resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)
  if ("error" %in% names(response_list)) {
     warning("API error: ", response_list[["error"]], call. = FALSE)
     return(invisible(NULL))
  } else if ("count" %in% names(response_list) &&
             response_list[["count"]] == 0) {
     message("No records returned")
     return(invisible(data.frame()))
  }
  response_data <- response_list[["data"]]
  if (length(response_data) == 0) {
    message("No records returned")
    return(as.data.frame(response_data))
  }
  if (clean_names) {
    response_data <- canonicalize_names(response_data)
  }
  return(response_data)
}

#' Request a VegBank resource by accession code
#'
#' Transforms a VegBank API response into a data frame, canonicalizing
#' names by default. If the API returns an error (indicated by a
#' top-level "error" key in the JSON response), the error message is
#' displayed as an R warning, and `NULL` is returned. If API returns a
#' non-error response with a reported record count of 0, an informative
#' message is displayed, and an empty data frame is returned.
#'
#' @param resource VegBank API resource (e.g., `plot-observations`)
#' @param accession_code Resource accession code
#' @return VegBank query results as a dataframe
#'
#' @noRd
get_resource_by_code <- function(resource, accession_code) {
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(accession_code) |>
    req_headers(Accept = "application/json")
  response <- request |> req_perform()
  vb_data <- as_vb_dataframe(response)
  return(vb_data)
}

#' Get all records for a VegBank resource
#'
#' Retrieves a dataframe containing "all" returned records (constrained
#' by limit and offset) of the requested resource type, with possible
#' control over the level of detail depending on the API endpoint.
#'
#' @param resource VegBank API resource (e.g., `plot-observation`)
#' @param limit Query result limit
#' @param offset Query result offset
#' @param detail Level of detail ("minimal", "full")
#' @param ... Additional API query parameters
#' @return VegBank query results as a dataframe
#'
#' @noRd
get_all_resources <- function(resource, limit=100, offset=0,
                              detail = c("minimal", "full"), ...) {
  detail <- match.arg(detail)
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_query(detail = detail,
                  limit = limit,
                  offset = offset) |>
    req_url_query(!!!list(...)) |>
    req_headers(Accept = "application/json")
  response <- request |> req_perform()
  vb_data <- as_vb_dataframe(response)
  return(vb_data)
}

#' Coerce a list of JSON-like objects into a data frame
#'
#' Converts a list of named lists (typically parsed from JSON) into a
#' data frame, replacing `NULL` values with `NA` to ensure proper row
#' and column alignment. Used for manually processing API responses
#' containing tabular data in JSON format.
#'
#' @param jsonlist A list of named lists, each representing a row of
#'        data with consistent keys.
#' @return A data frame with one row per element of `jsonlist` and one
#'         column per key. `NULL` values in the input are coerced to
#'         `NA` in the output.
#'
#' @noRd
jsonlist2df <- function(jsonlist) {
  do.call(rbind, lapply(jsonlist, function(x) {
    x[sapply(x, is.null)] <- NA
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
}
