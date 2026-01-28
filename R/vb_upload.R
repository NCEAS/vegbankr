#' Upload data to VegBank
#'
#' @description
#' Upload data to VegBank via the REST API. `vb_upload()` is the core
#' function that can upload to any of the supported `POST` endpoints.
#'
#' @param resource \emph{Available only for `vb_upload()`.} Character string
#'   specifying the VegBank resource type to upload (e.g., "plot-observations",
#'   "projects").
#' @param ... Named data frames to upload. Each data frame should correspond to
#'   a table expected by the VegBank API for the specified resource. All
#'   arguments must be named, and at least one data frame must be provided.
#' @param dry_run Logical indicating whether to perform a dry run. If `TRUE`,
#'   the API will validate the data without committing changes to the database.
#'   Default is `FALSE`.
#'
#' @return The processed response object from the VegBank API documenting what
#'   (if anything) was successfully uploaded to VegBank.
#'
#' If `vb_debug()` is enabled, additional debugging details will be reported to
#' the console, primarily focused on the data being uploaded.
#'
#' @name vb_upload
#' @import httr2
#' @import curl
#' @import nanoparquet
#' @importFrom rlang !!!
#' @export
vb_upload <- function(resource, ..., dry_run = FALSE) {
  # Capture the named data frames
  dfs <- list(...)

  # Remove NULL entries
  dfs <- Filter(Negate(is.null), dfs)

  # Validate that we have at least one data frame
  if (length(dfs) == 0) {
    stop("At least one named data frame must be provided")
  }

  # Check that all arguments are named
  df_names <- names(dfs)
  if (is.null(df_names) || any(df_names == "")) {
    stop("All data frames must be passed as named arguments")
  }

  # Validate all inputs are data frames with at least one row
  for (i in seq_along(dfs)) {
    if (!is.data.frame(dfs[[i]])) {
      stop(sprintf("Argument '%s' is not a data frame", df_names[i]))
    }
    if (nrow(dfs[[i]]) == 0) {
      stop(sprintf("Data frame '%s' has zero rows", df_names[i]))
    }
  }

  # Create temporary files for all data frames
  tmp_files <- vapply(dfs,
                      function(df) tempfile(fileext = ".parquet"),
                      character(1))
  on.exit(unlink(tmp_files), add = TRUE)

  # Write each data frame to Parquet format
  for (i in seq_along(dfs)) {
    nanoparquet::write_parquet(dfs[[i]], tmp_files[i])
    if (0 < vb_verbosity()) {
      message("======================")
      message(sprintf("Wrote '%s' to Parquet:", df_names[i]))
      print(nanoparquet::read_parquet(tmp_files[i]))
      print(nanoparquet::read_parquet_schema(tmp_files[i])[c("name", "r_type", "type")])
      message("======================")
    }
  }

  # Build multipart form data elements
  form_data <- lapply(tmp_files, curl::form_file, type = "application/octet-stream")
  names(form_data) <- df_names

  # Build and send the request
  request <- request(vb_get_base_url()) |>
    req_url_path_append(resource) |>
    req_method("POST") |>
    req_url_query(dry_run = dry_run) |>
    req_body_multipart(!!!form_data)

  response <- send(request)
  handle_vb_upload_response(response)
}

#' Process VB upload (POST) response
#'
#' @param response VegBank API response object
#'
#' @return A data frame, invisibly
#' @import httr2
#' @importFrom utils head tail
#'
#' @noRd
handle_vb_upload_response <- function(response) {
  resp <- resp_body_string(response) |>
    jsonlite::fromJSON(flatten = TRUE)
  if ("dry_run_data" %in% names(resp)) {
    message(resp$message)
    resp <- resp$dry_run_data
  } else {
    message("Upload complete")
  }
  resp |>
    purrr::pluck("dry_run_data", "resources")

  counts <- resp$counts
  resources <- resp$resources

  action_msgs <- paste("->",
    sapply(counts, function(cnt) paste(names(cnt), cnt)),
    names(counts), "record(s)")

  for (msg in action_msgs) message(msg)

  # Create truncated dataframes to print, showing only first 2 rows
  # and last 2 rows for those with 5+ rows
  resources_peek <- lapply(resources,
    function(df) {
      if (nrow(df) <= 4) {
        df
      } else {
        rbind(head(df, 2),
              "..." = rep("...", ncol(df)),
              tail(df, 2))
      }
    })
  print(resources_peek)

  invisible(response)
}
