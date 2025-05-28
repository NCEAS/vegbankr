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
#' @export
canonicalize_names <- function(target_df, lookup_df) {
  if (missing(lookup_df)) {
    lookup_file <- system.file("canonical-name-lookup.txt",
                               package = "vegbankr")
    lookup_df <- read.csv(lookup_file)
  }
  original_names <- tolower(names(target_df))
  augmented_names <- data.frame(lower=original_names) %>%
    dplyr::left_join(lookup_df, by=dplyr::join_by(lower))
  if (any(is.na(augmented_names$snake))) {
     warning("Unmatched names: ",
       paste(augmented_names %>%
               dplyr::filter(is.na(snake)) %>%
               dplyr::pull(lower),
             collapse=", "))
  }
  canonicalized_names <- augmented_names %>%
    dplyr::mutate(canonical=dplyr::coalesce(snake, lower)) %>%
    dplyr::pull(canonical)
  names(target_df) <- canonicalized_names
  return(target_df)
}
