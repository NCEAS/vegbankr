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

#' Enable VegBank API debugging mode
#'
#' Set VegBank debug level used when `send`ing API requests. This
#' currently controls two things:
#'  1. Verbosity of API requests, specifically as handled by
#'     `httr::req_perform()`
#'  2. Reporting of the elapsed time, as a console message
#'
#' @param verbosity Integer between 0-3 (Default: 1). Using 0 is
#' equivalent to setting `vb_undebug()`, in which case no information is
#' reported. Values between 1-3 control verbosity level passed to
#' `httr2::req_perform()`, and in all cases include display of API
#' request time duration.
#'
#' @seealso [vb_undebug()], [httr2::req_perform()]
#' @export
vb_debug <- function(verbosity=1) {
  if (is.null(verbosity) || !is.atomic(verbosity) ||
      length(verbosity) != 1 || is.na(verbosity) ||
      !verbosity %in% 0:3) {
    stop("verbosity must be 0, 1, 2, or 3")
  }
  if (verbosity == 0) {
    vb_undebug()
  } else {
    options(vegbank.debug = verbosity)
    message("Enabling VegBank debugging with verbosity ", verbosity)
  }
}

#' Disable VegBank API debugging mode
#'
#' Unset VegBank debugging. Equivalent to `vb_debug(0)`.
#'
#' @seealso [vb_debug()]
#' @export
vb_undebug <- function() {
  options(vegbank.debug = 0)
  message("Disabling VegBank debugging")
}

#' Get VegBank API verbosity
#'
#' Gets the verbosity level (0-3) as set via the `vb_debug()` and
#' `vb_undebug()` utilities. If the underlying option variable is set to
#' something other than an integer between 0 and 3, this resets it to 0
#' with a warning.
#'
#' @return Current verbosity
#'
#' @noRd
vb_verbosity <- function() {
  verbosity <- getOption("vegbank.debug")
  if (is.null(verbosity)) {
    return(0)
  } else if (!verbosity %in% 0:3) {
    warning("invalid verbosity level; disabling debug mode")
    suppressMessages(vb_undebug())
    return(0)
  } else {
    return(verbosity)
  }
}

#' Send a request
#'
#' Light wrapper of httr::req_perform() that performs the request with
#' higher verbosity if enabled via vb_debug(), and also calculates and
#' reports the time taken to perform the request.
#'
#' If the API responds with an error condition, this wrapper will
#' attempt to extract an error message from the response, else will use
#' a generic message. This will be added to the standard httr2 error
#' messaging, and an R error will be raised.
#'
#' @param request An httr2 request
#' @return An httr2 response
#'
#' @import httr2
#' @noRd
send <- function(request) {
  error_body <- function(resp) {
    tryCatch(resp_body_json(resp)$error$message,
      error = function(msg) {
        return("No additional error details from server.")
      }
    )
  }
  request <- request |> req_error(body = error_body)

  verbosity <- vb_verbosity()
  if (verbosity == 0) {
    response <- req_perform(request)
  } else {
    start_time <- Sys.time()
    response <- req_perform(request, verbosity=verbosity)
    duration <- difftime(Sys.time(), start_time)
    elapsed_time <- paste(format(unclass(duration),
                                 digits = getOption("digits")),
                          attr(duration, "units"))
    message("API response time: ", elapsed_time)
  }
  return(response)
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

#' Transform VegBank JSON response into a data frame
#'
#' Transforms a VegBank API JSON response into a data frame, optionally
#' canonicalizing names. This is intended for use on API responses in JSON
#' format with a top level "data" element containing a list of records
#' coercible to a dataframe. If this element contains zero records
#' (represented as an empty JSON array, an informative message is
#' displayed, and a data frame with zero columns and zero rows is
#' returned. If the API returns count information, this is packaged into
#' data frame attributes that can be retrieved with get_page_details().
#'
#' @param response VegBank API response object
#' @param clean_names (logical) Should names be canonicalized? Defaults
#'        to `FALSE`.
#' @returns A data frame
#'
#' @import httr2
#' @noRd
vb_df_from_json <- function(response, clean_names = FALSE) {
  response_list <- response |>
    resp_body_string() |>
    jsonlite::fromJSON(flatten = TRUE)
  response_data <- response_list[["data"]]
  if (length(response_data) == 0) {
    message("No records returned")
    response_data <- as.data.frame(response_data)
  } else if (clean_names) {
    response_data <- canonicalize_names(response_data)
  }
  response_data <- dplyr::as_tibble(response_data)
  attr(response_data, "vb_count_returned") <- nrow(response_data)
  if ("count" %in% names(response_list)) {
    count <- extract_vb_count(response_list[["count"]])
  } else {
    count <- NA_integer_
  }
  attr(response_data, "vb_count_reported") <- count
  return(response_data)
}

#' Transform VegBank parquet response into a data frame
#'
#' Transforms a VegBank API Parquet response into a data frame, optionally
#' canonicalizing names. This is intended for use on API responses in Parquet
#' format representing a single data table. If this element contains zero
#' records, an informative message is displayed, and the empty data
#' frame is returned. If the API returns count information, this is packaged
#' into data frame attributes that can be retrieved with get_page_details().
#'
#' @param response VegBank API response object
#' @param clean_names (logical) Should names be canonicalized? Defaults
#'        to `FALSE`.
#' @returns A data frame
#'
#' @import httr2
#' @noRd
vb_df_from_parquet <- function(response, clean_names = FALSE) {
  temp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(temp_file))
  writeBin(resp_body_raw(response), temp_file)
  conn <- duckdb::dbConnect(duckdb::duckdb())
  on.exit(duckdb::dbDisconnect(conn), add=TRUE)
  vb_data <- DBI::dbGetQuery(conn,
    paste0("SELECT * FROM read_parquet('", temp_file, "')"))
  if (clean_names) {
    vb_data <- canonicalize_names(vb_data)
  }
  if (nrow(vb_data) == 0) {
    message("No records returned")
  }
  vb_data <- dplyr::as_tibble(vb_data)
  attr(vb_data, "vb_count_returned") <- nrow(vb_data)
  # extract record count reported by VegBank
  raw_count <- DBI::dbGetQuery(conn,
    paste0("SELECT CAST(value AS VARCHAR)::INTEGER AS count",
           "  FROM parquet_kv_metadata('", temp_file, "')",
           "  WHERE key = 'vb_count'"))$count
  count <- extract_vb_count(raw_count)
  attr(vb_data, "vb_count_reported") <- count

  return(vb_data)
}

#' Validate and interpret VegBank reported count
#'
#' Takes a `raw_count` value (intended to be a count returned by
#' VegBank), checks that it meets structural expectations, then either
#' (1) returns it if it's a single non-negative number, (2) silently
#' returns NA if it's a length-0 numeric vector, or (3) returns NULL
#' with a warning otherwise.
#'
#' @param raw_count Count value extracted from an API response
#' @returns A number, which may be `NA` or `NULL`
#'
#' @noRd
extract_vb_count <- function(raw_count) {
    is_valid_structure <- is.atomic(raw_count) &&
                          length(raw_count) <= 1 &&
                          is.numeric(raw_count)

    if (is_valid_structure && length(raw_count) == 0) {
        count <- NA_integer_
    } else if (is_valid_structure && 0 <= raw_count) {
        count <- raw_count
    } else {
        warning("Unable to interpret count metadata returned by API")
        count <- NULL
    }
    return(count)
}

#' Request a VegBank resource by vb code
#'
#' Retrieves a dataframe containing a single record of the requested VegBank
#' resource type, identified by its vb_code.
#'
#' The API response can be requested either as JSON or Parquet, and name
#' canonicalization can optionally be applied to the dataframe (client-side).
#' If the API returns an error (indicated by a top-level "error" key in a JSON
#' response), the error message is displayed as an R warning, and `NULL` is
#' returned. If API returns a non-error response with a reported record count of
#' 0, an informative message is displayed, and an empty data frame is returned.
#'
#' @param resource VegBank API resource (e.g., `plot-observations`)
#' @param vb_code Resource identifier
#' @param parquet Request data in Parquet format? Defaults to `FALSE`.
#' @param clean_names (logical) Should names be canonicalized? Defaults
#'        to `TRUE`.
#' @return VegBank query results as a dataframe
#'
#' @import httr2
#' @noRd
get_resource_by_code <- function(resource, vb_code, parquet = FALSE,
                                 clean_names = FALSE, ...) {
  if (!is.logical(parquet)) {
    stop("argument 'parquet' must be TRUE or FALSE", call.=FALSE)
  }
  if (!is.logical(clean_names)) {
    stop("argument 'clean_names' must be TRUE or FALSE", call.=FALSE)
  }
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(vb_code) |>
    req_url_query(!!!list(...)) |>
    req_headers(Accept = "application/json")
  if (parquet) {
    request <- request |> req_url_query(create_parquet = parquet)
  }
  response <- send(request)
  if (parquet) {
    vb_data <- vb_df_from_parquet(response, clean_names)
  } else {
    vb_data <- vb_df_from_json(response, clean_names)
  }
  return(vb_data)
}

#' Get all records for a VegBank resource
#'
#' Retrieves a dataframe containing a collection of records of the
#' requested VegBank resource type, where collection membership and size
#' are potentially constrained by limit, offset, and any other passed
#' API query parameters (e.g., `search`) that impose server-side filtering.
#'
#' The API response can be requested either as JSON or Parquet, and name
#' canonicalization can optionally be applied to the dataframe (client-side).
#' If the API returns an error (indicated by a top-level "error" key in a JSON
#' response), the error message is displayed as an R warning, and `NULL` is
#' returned. If API returns a non-error response with a reported record count of
#' 0, an informative message is displayed, and an empty data frame is returned.
#'
#' @param resource VegBank API resource (e.g., `plot-observation`)
#' @param limit Query result limit
#' @param offset Query result offset
#' @param parquet Request data in Parquet format? Defaults to `FALSE`.
#' @param clean_names (logical) Should names be canonicalized? Defaults
#'        to `TRUE`.
#' @param ... Additional API query parameters
#' @return VegBank query results as a dataframe
#'
#' @import httr2
#' @noRd
get_all_resources <- function(resource, limit=100, offset=0,
                              parquet = FALSE, clean_names = FALSE, ...) {
  if (!is.logical(parquet)) {
    stop("argument 'parquet' must be TRUE or FALSE", call.=FALSE)
  }
  if (!is.logical(clean_names)) {
    stop("argument 'clean_names' must be TRUE or FALSE", call.=FALSE)
  }
  request <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_headers(Accept = "application/json") |>
    req_url_query(limit = limit,
                  offset = offset) |>
    req_url_query(!!!list(...))
  if (parquet) {
    request <- request |> req_url_query(create_parquet = TRUE)
    response <- send(request)
    vb_data <- vb_df_from_parquet(response, clean_names)
  } else {
    response <- send(request)
    vb_data <- vb_df_from_json(response, clean_names)
  }
  attr(vb_data, "vb_limit") <- limit
  attr(vb_data, "vb_offset") <- offset
  return(vb_data)
}

#' Get paging details for a VegBank dataframe
#'
#' Reports paging details associated with a dataframe produced by
#' querying an VegBank API endpoint with limit/offset-based pagination.
#'
#' @param x Dataframe, presumably returned by a VegBank getter
#' @return Named vector with the following elements, if available (any
#'         of these values not attached to the data frame will simply be
#'         missing from the returned vector):
#'  * count_reported: the full record count reported by the API
#'  * offset: the record offset used in the API query
#'  * limit: the record limit used in the API query
#'  * count_returned: the actual count of returned records
#' @examples \dontrun{
#' parties <- get_all_parties(limit=10, offset=50)
#' get_page_details(parties)
#' }
#' @export
get_page_details <- function(x) {
  return(c(
    count_reported = attr(x, "vb_count_reported", exact = TRUE),
    offset = attr(x, "vb_offset", exact = TRUE),
    limit = attr(x, "vb_limit", exact = TRUE),
    count_returned = attr(x, "vb_count_returned", exact = TRUE)
    ))
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
  do.call(rbind.data.frame,
    lapply(jsonlist, function(record) {
      replace(record, sapply(record, is.null, USE.NAMES=FALSE), NA)
    })
  )
}

#' Parse JSON column to list column
#'
#' Takes a data frame and the name of a column containing character string
#' representations of JSON objects, and parses them to create a new column
#' with R list representations of the same information. The new column is
#' added to the data frame and named by appending `_list` to the input
#' column name.
#'
#' @param df A data frame
#' @param col_name Character string naming the column to parse
#' @param skip_if_missing Logical. If `TRUE`, skip processing if column is
#'        missing instead of throwing an error. Default is `FALSE`.
#'
#' @returns The data frame with an added list column named `{col_name}_list`
#'
#' @importFrom rlang .data := !!
#' @noRd
parse_json_column <- function(df, col_name, skip_if_missing = FALSE) {
  # Check whether col_name exists
  if (!col_name %in% names(df)) {
    if (skip_if_missing) {
      return(df)
    } else {
      stop(sprintf("Column '%s' not found in data frame", col_name))
    }
  }
  # Create the new column with parsed lists
  new_col_name <- paste0(col_name, "_list")
  df |>
    dplyr::mutate(
      !!new_col_name := purrr::map(
        ifelse(is.na(.data[[!!col_name]]), "{}", .data[[!!col_name]]),
        ~ jsonlite::fromJSON(.x, simplifyVector = FALSE)
      )
    )
}
