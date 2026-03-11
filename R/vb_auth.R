#' Set a Bearer token for authenticated API requests
#'
#' Stores an OAuth2 access token in a global option so it is automatically
#' included in subsequent API requests as an `Authorization: Bearer` header.
#' Call `vb_set_token(NULL)` to clear the token.
#'
#' @param token (character) A Bearer token string, or NULL to clear
#' @returns NULL
#' @examples
#' vb_set_token("eyJhbGciOiJIUzI1NiJ9...")
#' vb_set_token(NULL)  # clear
#' @export
vb_set_token <- function(token) {
  if (!is.null(token) && (!is.character(token) || nchar(token) == 0)) {
    stop("token must be a non-empty string, or NULL to clear")
  }
  options(vegbank.token = token)
  if (is.null(token)) message("VegBank token cleared") else message("VegBank token set")
}

#' Retrieve the currently stored Bearer token
#'
#' @returns The token string, or NULL if none is set
#' @noRd
vb_token <- function() {
  getOption("vegbank.token", default = NULL)
}