#' Set Bearer token(s) for authenticated API requests
#'
#' Stores an OAuth2 access token and/or refresh token in global options for use in subsequent authenticated API requests. Use [vb_unset_token()] to clear previously stored tokens.
#'
#' Two input modes are supported — use one or the other, not both:
#' * **Individual strings:** pass `access_token`, `refresh_token`, or both.
#' * **Token dict:** pass `tokens` as a named list or JSON string with `access_token` and/or `refresh_token` keys.
#'
#' @param access_token (character) Access token string.
#' @param refresh_token (character) Refresh token string.
#' @param tokens (list | character) Named list or JSON string containing
#'   `access_token` and/or `refresh_token`. Cannot be combined with
#'   `access_token` or `refresh_token`.
#' @returns NULL
#' @examples
#' vb_set_token(access_token = "eyJhbGciOiJIUzI1NiJ9...")
#' vb_set_token(access_token = "eyJ...", refresh_token = "eyJ...")
#' vb_set_token(tokens = list(access_token = "eyJ...", refresh_token = "eyJ..."))
#' vb_set_token(tokens = '{"access_token": "eyJ...", "refresh_token": "eyJ..."}')
#' @seealso [vb_unset_token()], [vb_refresh_tokens()]
#' @export
vb_set_token <- function(access_token = NULL, refresh_token = NULL, tokens = NULL) {
  # Only accept one input mode: either individual tokens or a tokens dict, not both
  if (!is.null(tokens) && (!is.null(access_token) || !is.null(refresh_token))) {
    stop("provide either 'tokens' or 'access_token'/'refresh_token', not both")
  }

  # If token supplied as a dict, extract individual tokens
  if (!is.null(tokens)) {
    tokens        <- parse_tokens_dict(tokens)
    access_token  <- tokens[["access_token"]]
    refresh_token <- tokens[["refresh_token"]]
  }

  assert_token_string(access_token,  "access_token")
  assert_token_string(refresh_token, "refresh_token")

  if (is.null(access_token) && is.null(refresh_token)) {
    stop("at least one of 'access_token' or 'refresh_token' must be provided")
  }

  # Update global options with provided tokens
  if (!is.null(access_token))  options(vegbank.token = access_token)
  if (!is.null(refresh_token)) options(vegbank.refresh_token = refresh_token)

  set_parts <- c(
    if (!is.null(access_token))  "access token",
    if (!is.null(refresh_token)) "refresh token"
  )
  message("VegBank token(s) updated: ", paste(set_parts, collapse = " and "))
}

#' Clear stored Bearer tokens
#'
#' Removes the OAuth2 access token and refresh token previously stored by
#' [vb_set_token()]. After calling this function, subsequent API requests
#' will be sent without an `Authorization: Bearer` header.
#'
#' @returns NULL
#' @examples
#' vb_unset_token()
#' @seealso [vb_set_token()]
#' @export
vb_unset_token <- function() {
  options(vegbank.token         = NULL)
  options(vegbank.refresh_token = NULL)
  message("VegBank token(s) cleared")
}

#' Retrieve the currently stored Bearer token
#'
#' @returns The token string, or NULL if none is set
#' @noRd
vb_token <- function() {
  getOption("vegbank.token", default = NULL)
}


#' Extract a claim from a JWT payload
#'
#' Decodes the payload segment of a JWT and returns the value of the specified claim. 
#' Returns NULL if the token is NULL, malformed, or the claim is absent.
#'
#' A JWT is a dot-separated triple: `header.payload.signature`. This
#' function base64url-decodes the payload, parses it as JSON,
#' and returns the value of the requested claim. No signature verification is performed.
#'
#' @param token JWT string
#' @param claim (character) Name of the claim to extract (e.g. "exp",  "scope", etc.)
#' @returns The claim value, or NULL
#' @noRd
jwt_claim <- function(token, claim) {
  if (is.null(token)) return(NULL)

  # A JWT has three dot-separated parts: header.payload.signature
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)

  # Base64url → standard Base64: replace URL-safe chars and pad to multiple of 4
  payload_b64 <- parts[[2]]
  padding <- (4 - nchar(payload_b64) %% 4) %% 4
  payload_b64 <- paste0(payload_b64, strrep("=", padding))
  payload_b64 <- chartr("-_", "+/", payload_b64)

  # Decode payload to JSON string, then extract the requested claim
  payload_json <- tryCatch(
    rawToChar(base64enc::base64decode(payload_b64)),
    error = function(e) NULL
  )

  if (is.null(payload_json)) return(NULL)
  tryCatch(jsonlite::fromJSON(payload_json)[[claim]], error = function(e) NULL)
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
