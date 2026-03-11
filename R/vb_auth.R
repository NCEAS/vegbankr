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


#' Parse a tokens dict into a normalized named list
#'
#' Accepts a named list or a JSON string with `access_token` and/or
#' `refresh_token` keys and returns a normalized named list. Errors
#' on malformed or unrecognised input.
#'
#' @param tokens Named list or JSON string
#' @return Named list with `access_token` and/or `refresh_token`
#' @noRd
parse_tokens_dict <- function(tokens) {
  if (is.character(tokens) && length(tokens) == 1) {
    tokens <- tryCatch(
      jsonlite::fromJSON(tokens),
      error = function(e) stop("'tokens' could not be parsed as JSON: ", e$message)
    )
  }
  if (!is.list(tokens)) {
    stop("'tokens' must be a named list or JSON string")
  }
  # Unwrap nested 'token' envelope from /authorize and /refresh responses
  if ("token" %in% names(tokens) && is.list(tokens[["token"]])) {
    tokens <- tokens[["token"]]
  }
  if (!any(c("access_token", "refresh_token") %in% names(tokens))) {
    stop("'tokens' must contain at least one of 'access_token' or 'refresh_token'")
  }
  tokens
}

#' Validate that a token value is a non-empty string (or NULL)
#'
#' @param value The token value to check
#' @param name The parameter name, used in the error message
#' @noRd
assert_token_string <- function(value, name) {
  if (!is.null(value) &&
        (!is.character(value) || length(value) != 1 || nchar(value) == 0)) {
    stop(name, " must be a non-empty string")
  }
}
