#' Upload data to VegBank
#'
#' @description
#' Upload data to VegBank via the REST API. `vb_upload()` is the core
#' function that can upload to any of the supported `POST` endpoints.
#' Resource-specific functions like `vb_upload_plot_observations()` are
#' convenience wrappers that apply resource-specific validation. For
#' typical usage, the resource-specific functions should be preferred.
#'
#' @param resource \emph{Available only for `vb_upload()`.} Character string
#'   specifying the VegBank resource type to upload (e.g., "plot-observations",
#'   "projects").
#' @param ... Named data frames to upload. Each data frame should correspond to
#'   a table expected by the VegBank API for the specified resource. All
#'   arguments must be named, and at least one data frame must be provided.
#' @param query_params A named list of parameter names and values to pass to the
#'   API as query parameters. For example, `list(foo="bar")` will be treated as
#'   `?foo=bar` in the request URL.
#' @param dry_run Logical indicating whether to perform a dry run. If `TRUE`,
#'   the API will validate the data without committing changes to the database.
#'   Default is `FALSE`.
#' @param plot_observations A data frame containing details about plot
#'   observations
#' @param projects A data frame containing details about new projects
#' @param parties A data frame containing details about new parties
#' @param references A data frame containing details about new references
#' @param soils A data frame containing details about observed soils
#' @param disturbances A data frame containing details about observed
#'   disturbances
#' @param community_classifications A data frame containing community
#'   classifications of observed plots
#' @param strata A data frame containing details about strata defined for plot
#'   observations
#' @param strata_cover_data A data frame containing details about plant cover as
#' observed in different strata of a plot
#' @param stem_data A data frame containing details about counts and/or
#'   other details about stems observed on a plot
#' @param taxon_interpretations A data frame containing taxon interpretations of
#'   plants observed on a plot
#' @param contributors A data frame associating parties with their contributions
#'   to plot observations, projects, and/or community classifications
#' @param plant_concepts A data frame containing plant concepts as plant names
#'   associated with references, along with with status details and taxonomic
#'   parents
#' @param plant_names A data frame containing plant name usages associated with
#'   specific classification systems for new plant concepts
#' @param plant_correlations A data frame defining correlations between plant
#'   concepts
#' @param community_concepts A data frame containing community concepts as
#'   community names associated with references, along with with status details
#'   and taxonomic parents
#' @param community_names A data frame containing community name usages
#'   associated with specific classification systems for new community concepts
#' @param community_correlations A data frame defining correlations between
#'   community concepts
#' @param cover_methods A data frame containing cover methods and their
#'   associated component cover indexes
#' @param stratum_methods A data frame containing stratum methods and their
#'   associated component stratum types
#' @param what_to_deactivate \emph{Available only for `vb_upload_plant_concepts()`
#'   and `vb_upload_community_concepts()`.} Character string specifying what
#'   existing concepts to deactivate in VegBank. Supported values are "none" and
#'   "by_party", along with "by_party_below_order" for plant uploads only.
#'   VegBank default is `none`.
#'
#' @return The processed response object from the VegBank API documenting what
#'   (if anything) was successfully uploaded to VegBank.
#'
#' @details
#'
#' The `vb_upload*()` family of functions can all be used to upload data to
#' VegBank queries, with each one differing in terms of what input dataframes
#' are expected (and, in some case, required).
#'
#' ## Available resource-specific functions:
#'
#' 1. `vb_upload_plot_observations()` - Plot observational data, including
#   ' contextual detail (associated projects, parties, references), detailed
#'    soil and disturbance observations, plant taxon observations and importance
#'    assessments (potentially including stem-level details) within any defined
#'    strata, and both individual plant taxon and overall vegetation community
#'    interpretation.
#' 2. `vb_upload_plant_concepts()` - Plant concepts linked to plant names
#'    through usages, with some status designation
#' 3. `vb_upload_community_concepts()` - Community concepts linked to community
#'    names through usages, with some status designation
#' 4. `vb_upload_taxon_interpretations()` - Re-interpretation of existing
# '   VegBank taxon observations (each of which belongs to some VegBank plot
#'    observation), associating them with one or more VegBank plant concepts
#' 5. `vb_upload_community_classifications()` - Re-interpretation of existing
#'    VegBank plot observations, associating them with one or more VegBank
#'    community concepts as part of a classification activity
#' 6. `vb_upload_cover_methods()` - New cover methods, including all component
#'     cover indexes defined by the method
#' 7. `vb_upload_stratum_methods()` - New stratum methods, including all component
#'     stratum types defined by the method
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
vb_upload <- function(resource, ..., query_params = NULL, dry_run = FALSE) {
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

  if (!is.null(query_params)) {
    if (!is.list(query_params) ||
        is.data.frame(query_params) ||
        is.null(names(query_params)) ||
        any(names(query_params) == "")) {
      stop("`query_params` must be a named list, or NULL.")
    }
    request <- request |>
      req_url_query(!!!query_params)
  }

  response <- send(request)
  handle_vb_upload_response(response)
}

#' Process VB upload (POST) response
#'
#' @param response VegBank API response object
#'
#' @return A data frame, invisibly
#' @import httr2
#' @import cli
#' @importFrom utils head tail
#'
#' @noRd
handle_vb_upload_response <- function(response) {
  resp <- resp_body_string(response) |>
    jsonlite::fromJSON(flatten = TRUE)
  
  dry <- FALSE
  if ("dry_run_data" %in% names(resp)) {
    dry <- TRUE
    dry_msg <- resp$message
    resp <- resp$dry_run_data
  }

  counts <- resp$counts
  resources <- resp$resources

  action_msgs <- paste("->",
    sapply(counts, function(cnt) paste(names(cnt), cnt)),
    names(counts), "record(s)")

  # Create truncated dataframes to print, showing only first 2 rows
  # and last 2 rows for those with 5+ rows
  resources_peek <- lapply(resources,
    function(df) {
      if (length(df) == 0) {
        data.frame()
      } else if (nrow(df) <= 4) {
        df
      } else {
        rbind(head(df, 2),
              "..." = rep("...", ncol(df)),
              tail(df, 2))
      }
    })
  print(resources_peek)
  
  for (msg in action_msgs) cli::cli_alert_success(msg)
  
  if (dry) {
    cli::cli_alert_info(dry_msg)
  } else {
    cli::cli_alert_success("Upload complete")
  }

  invisible(response)
}

#' @rdname vb_upload
#' @export
vb_upload_plot_observations <- function(plot_observations,
    projects = NULL, parties = NULL, references = NULL, soils = NULL,
    disturbances = NULL, community_classifications = NULL, strata = NULL,
    strata_cover_data = NULL, stem_data = NULL, taxon_interpretations = NULL,
    contributors = NULL, dry_run = FALSE) {
  vb_upload("plot-observations",
            plot_observations = plot_observations,
            projects = projects,
            parties = parties,
            references = references,
            soils = soils,
            disturbances = disturbances,
            community_classifications = community_classifications,
            strata = strata,
            strata_cover_data = strata_cover_data,
            stem_data = stem_data,
            taxon_interpretations = taxon_interpretations,
            contributors = contributors,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_plant_concepts <- function(plant_concepts,
    plant_names = NULL, plant_correlations = NULL, parties = NULL,
    references = NULL, what_to_deactivate = NULL, dry_run = FALSE) {

  if (!is.null(what_to_deactivate)) {
    query_params = list("deactivation" = what_to_deactivate)
  } else {
    query_params = NULL
  }
  vb_upload("plant-concepts",
            plant_concepts = plant_concepts,
            plant_names = plant_names,
            plant_correlations = plant_correlations,
            parties = parties,
            references = references,
            query_params = query_params,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_community_concepts <- function(community_concepts,
    community_names = NULL, community_correlations = NULL, parties = NULL,
    references = NULL, what_to_deactivate = NULL, dry_run = FALSE) {

  if (!is.null(what_to_deactivate)) {
    query_params = list("deactivation" = what_to_deactivate)
  } else {
    query_params = NULL
  }
  vb_upload("community-concepts",
            community_concepts = community_concepts,
            community_names = community_names,
            community_correlations = community_correlations,
            parties = parties,
            references = references,
            query_params = query_params,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_taxon_interpretations <- function(taxon_interpretations,
    parties = NULL, references = NULL, dry_run = FALSE) {
  vb_upload("taxon-interpretations",
            taxon_interpretations = taxon_interpretations,
            parties = parties,
            references = references,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_community_classifications <- function(community_classifications,
    parties = NULL, references = NULL, contributors = NULL, dry_run = FALSE) {
  vb_upload("community-classifications",
            community_classifications = community_classifications,
            parties = parties,
            references = references,
            contributors = contributors,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_cover_methods <- function(cover_methods, references = NULL,
    dry_run = FALSE) {
  vb_upload("cover-methods",
            cover_methods = cover_methods,
            references = references,
            dry_run = dry_run)
}

#' @rdname vb_upload
#' @export
vb_upload_stratum_methods <- function(stratum_methods, references = NULL,
    dry_run = FALSE) {
  vb_upload("stratum-methods",
            stratum_methods = stratum_methods,
            references = references,
            dry_run = dry_run)
}
