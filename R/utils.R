#' Set a base URL for the VegBank API
#'
#' Sets a global option specifying the base URL for the VegBank API
#' using the provided `vb_base_url` string, optionally adding a port as
#' "http(s)://{vb_base_url}:{port}".
#'
#' @param vb_base_url (character) The base URL, including protocol and domain
#' @param port (numeric) Optional port value
#' @returns NULL
#' @examples
#' set_vb_base_url("https://api.vegbank.org")
#' set_vb_base_url("http://localhost", port=8080)
#' @seealso [get_vb_base_url()]
#' @export
set_vb_base_url <- function(vb_base_url, port) {
  if (!missing(port)) {
    vb_base_url <- paste(vb_base_url, port, sep=":")
  }
  options(vegbank.base_api_url = vb_base_url)
  message("Using ", getOption("vegbank.base_api_url"), " as base URL")
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
