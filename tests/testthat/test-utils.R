test_that("default vb_base_url is used", {
  local_base_url(NULL)
  expect_identical(get_vb_base_url(), "https://api.vegbank.org")
  suppressMessages(set_vb_base_url(""))
  expect_identical(get_vb_base_url(), "https://api.vegbank.org")
})
test_that("vb_base_url option set", {
  local_base_url(NULL)
  suppressMessages(set_vb_base_url("http://localhost", 8080))
  expect_identical(get_vb_base_url(), "http://localhost:8080")
})
test_that("set_vb_base_url(NULL) reports default URL", {
  local_base_url(NULL)
  expect_message(set_vb_base_url(NULL),
                 "Using https://api.vegbank.org as base URL")
})

test_that("canonicalize_names() works", {
  # Input with all names matched in the package lookup table
  input_df <- data.frame(
    stratum_ID = integer(),
    stratummethodname = character()
  )
  output_df <- canonicalize_names(input_df)
  expect_s3_class(output_df, "data.frame")
  expect_named(
    output_df,
    c("stratum_id", "stratum_method_name"),
    ignore.order = TRUE
  )
  # Input with a name missing from the package lookup table
  input_df$unexpectedname <- character()
  expect_warning(
     canonicalize_names(input_df),
     "^Unmatched names: unexpectedname$"
  )
  # Pass in custom name lookup table
  input_df <- data.frame(customfield = character())
  lookup_df <- data.frame(
    lower = 'customfield',
    snake = 'custom_field'
  )
  output_df <- canonicalize_names(input_df, lookup_df)
  expect_s3_class(output_df, "data.frame")
  expect_named(output_df, "custom_field")
})

with_mock_api({
  test_that("as_vb_dataframe() works", {

    local_base_url(NULL)

    # response with record count of 0
    zero_response <- request(get_vb_base_url()) |>
      req_url_path_append('plot') |>
      req_url_path_append('zero_records') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_message(
      zero_records <- as_vb_dataframe(zero_response),
      "No records returned")
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    # response with error
    error_response <- request(get_vb_base_url()) |>
      req_url_path_append('plot') |>
      req_url_path_append('error_response') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_warning(
      error_output <- as_vb_dataframe(error_response),
      "API error: something went wrong")
    expect_null(error_output)

  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("get_resource_by_code() works", {
    response <- get_resource_by_code("plot-observations",
                                     "VB.Ob.41618.50D47AJX5G5U8WY")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("observation_accession_code", "author_obs_code",
        "obs_start_date", "max_slope_aspect", "max_slope_gradient",
        "total_cover"),
      ignore.order = TRUE
    )
    expect_identical(response$observation_accession_code,
                 "VB.Ob.41618.50D47AJX5G5U8WY")
    expect_identical(response$max_slope_aspect,
                 -106.409918856452)
    expect_identical(response$max_slope_gradient,
                 NA)

  })
})
