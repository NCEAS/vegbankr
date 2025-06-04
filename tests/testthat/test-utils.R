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

with_mock_api({
  test_that("send() works", {

    local_base_url(NULL)
    test_request <- request(get_vb_base_url()) |>
      req_url_path_append('test')

    # verbosity 0 -- httr2_response with no message
    local_vb_debug(0)
    expect_no_message(response <- send(test_request))
    expect_s3_class(response, "httr2_response")

    # verbosity 1 -- httr2_response with timing message
    local_vb_debug(1)
    expect_message(
      response <- send(test_request),
      "^API response time: [0-9]")
    expect_s3_class(response, "httr2_response")

  })
})

with_mock_api({
  test_that("send() handles errors", {

    # quick helper to verify the mocked response looks right :)
    get_mock_http_status <- function(request) {
      response_status <- request |>
        req_error(is_error=\(resp) FALSE) |>
        req_perform() |>
        resp_status()
    }

    local_base_url(NULL)
    base_request <- request(get_vb_base_url()) |>
      req_url_path_append('api-error-test')

    # error handling -- HTTP 400 with JSON error message
    error_400_request <- base_request |> req_url_query(limit = "foo")
    expect_identical(get_mock_http_status(error_400_request), 400L)
    expect_error(
      send(error_400_request),
      "When provided, 'offset' and 'limit' must be non-negative integers.")

    # error handling -- HTTP 500 with no error message
    error_500_request <- base_request |> req_url_query(limit = -1)
    expect_identical(get_mock_http_status(error_500_request), 500L)
    expect_error(
      send(error_500_request),
      "No additional error details from server.")

  })
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
      req_url_path_append('plot-observations') |>
      req_url_path_append('zero_records') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_message(
      zero_records <- as_vb_dataframe(zero_response),
      "No records returned")
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    # response with invalid record count
    invalid_count_response <- request(get_vb_base_url()) |>
      req_url_path_append('invalid-count') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_warning(
      vb_df <- as_vb_dataframe(invalid_count_response),
      "API returned an invalid count")
    expect_s3_class(vb_df, "data.frame")
    expect_identical(nrow(vb_df), 3L)
    expect_null(attr(vb_df, "vb_count_reported"))

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

with_mock_api({
  local_base_url(NULL)
  test_that("get_all_resources() works", {
    response <- get_all_resources("plot-observations",
                                  detail="minimal", limit=1)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("obs_accession_code", "author_obs_code", "observation_id",
        "plot_accession_code", "author_plot_code", "plot_id",
        "latitude", "longitude", "country", "state_province"),
      ignore.order = TRUE
    )
    expect_identical(response$obs_accession_code,
                 "VB.Ob.2948.ACAD143")
    expect_identical(response$longitude,
                  -68.229339874)
    expect_identical(response$state_province,
                 NA)
  })
})

test_that("get_page_details() extracts the expected attributes", {
  df <- data.frame()
  attr(df, "vb_offset") <- 100
  attr(df, "vb_limit") <- 50
  attr(df, "vb_count_reported") <- 125
  attr(df, "vb_count_returned") <- 25
  page_details <- get_page_details(df)
  expected <- c(count_reported = 125,
                offset = 100,
                limit = 50,
                count_returned = 25)
  expect_identical(page_details, expected)
})

with_mock_api({
  local_base_url(NULL)
  test_that("get_all_resources() returns expected page details", {
    response <- get_all_resources("test-count",
                                  limit=50, offset=2)
    page_details <- get_page_details(response)
    expected <- c(count_reported = 5,
                  offset = 2,
                  limit = 50,
                  count_returned = 3)
    expect_identical(page_details, expected)
  })
})
