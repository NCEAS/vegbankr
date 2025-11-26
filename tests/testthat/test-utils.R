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
      req_url_path_append('json-test') |>
      req_url_path_append('zero-records')

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
  test_that("vb_df_from_json() works", {

    local_base_url(NULL)

    # responses with record count of 3
    request <- request(get_vb_base_url()) |>
      req_url_path_append('json-test') |>
      req_url_path_append('three-records') |>
      req_headers(Accept = "application/json")
    response <- request |> req_perform()
    # canonicalize names
    records_c <- vb_df_from_json(response, clean_names = TRUE)
    expect_s3_class(records_c, "data.frame")
    expect_identical(nrow(records_c), 3L)
    expect_named(records_c,
      c("surname", "given_name"), ignore.order = TRUE)
    # don't canonicalize names
    records_nc <- vb_df_from_json(response)
    expect_s3_class(records_nc, "data.frame")
    expect_identical(nrow(records_nc), 3L)
    expect_named(records_nc,
      c("surname", "givenname"), ignore.order = TRUE)

    # response with record count of 0
    zero_response <- request(get_vb_base_url()) |>
      req_url_path_append('json-test') |>
      req_url_path_append('zero-records') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_message(
      zero_records <- vb_df_from_json(zero_response),
      "No records returned")
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    # response with invalid record count
    invalid_count_response <- request(get_vb_base_url()) |>
      req_url_path_append('json-test') |>
      req_url_path_append('invalid-count') |>
      req_headers(Accept = "application/json") |>
      req_perform()
    expect_warning(
      vb_df <- vb_df_from_json(invalid_count_response),
      "Unable to interpret count metadata returned by API")
    expect_s3_class(vb_df, "data.frame")
    expect_identical(nrow(vb_df), 3L)
    expect_null(attr(vb_df, "vb_count_reported"))

  })
})

with_mock_api({
  test_that("vb_df_from_parquet() works", {
    local_base_url(NULL)

    # response with record count of 0
    empty_parquet_response <- request(get_vb_base_url()) |>
      req_url_path_append('parquet-test') |>
      req_url_query(limit = 0,
                    offset = 0,
                    create_parquet = TRUE) |>
      req_perform()
    expect_message(
      empty_vb_df <- vb_df_from_parquet(empty_parquet_response),
      "No records returned")
    # response with invalid record count
    expect_s3_class(empty_vb_df, "data.frame")
    expect_identical(ncol(empty_vb_df), 10L)
    expect_identical(nrow(empty_vb_df), 0L)

    # response with record count of 2
    parquet_response <- request(get_vb_base_url()) |>
      req_url_path_append('parquet-test') |>
      req_url_query(limit = 2,
                    offset = 0,
                    create_parquet = TRUE) |>
      req_perform()
    vb_df <- vb_df_from_parquet(parquet_response)
    # response with invalid record count
    expect_s3_class(vb_df, "data.frame")
    expect_identical(ncol(vb_df), 8L)
    expect_identical(nrow(vb_df), 2L)
    expect_true("givenname" %in% names(vb_df))

    # and again, but this time clean names
    vb_df <- vb_df_from_parquet(parquet_response,
                                clean_names=TRUE)
    expect_true("given_name" %in% names(vb_df))
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("get_resource_by_code() works", {
    # Test JSON response
    response_json <- get_resource_by_code("plot-observations",
                                     "ob.41618")
    expect_s3_class(response_json, "data.frame")
    expect_identical(nrow(response_json), 1L)
    expect_named(
      response_json,
      c("ob_code", "author_obs_code",
        "obs_start_date", "max_slope_aspect", "max_slope_gradient",
        "total_cover"),
      ignore.order = TRUE
    )
    expect_identical(response_json$ob_code,
                 "ob.41618")
    expect_identical(response_json$max_slope_aspect,
                 -106.409918856452)
    expect_identical(response_json$max_slope_gradient,
                 NA)

    # Test Parquet response
    response_parquet <- get_resource_by_code("parquet-test", "vb.1",
                                             parquet=TRUE,
                                             clean_names=FALSE)
    expect_s3_class(response_parquet, "data.frame")
    expect_identical(nrow(response_parquet), 40L)
    expect_identical(response_parquet$cm_code[1],
                     "cm.1")
    expect_identical(response_parquet$cover_percent[1],
                     0.05)
    expect_identical(response_parquet$cover_estimation_method[1],
                     NA_integer_)

    # Function parameter error conditions
    expect_error(
      get_resource_by_code("some-endpoint", parquet="not_logical"),
      "argument 'parquet' must be TRUE or FALSE"
    )
    expect_error(
      get_resource_by_code("some-endpoint", clean_names="not_logical"),
      "argument 'clean_names' must be TRUE or FALSE"
    )
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("get_all_resources() works", {
    # Test JSON response
    response_json <- get_all_resources("plot-observations", limit=2)
    expect_s3_class(response_json, "data.frame")
    expect_identical(nrow(response_json), 2L)
    expect_named(
      response_json,
      c("ob_code", "author_obs_code", "pl_code", "author_plot_code",
        "latitude", "longitude", "country", "state_province"),
      ignore.order = TRUE
    )
    expect_identical(response_json$ob_code[2],
                 "ob.2948")
    expect_identical(response_json$longitude[2],
                  -68.229339874)
    expect_identical(response_json$state_province[2],
                 NA)

    # Test Parquet response
    response_parquet <- get_all_resources("parquet-test", limit=2,
                                          parquet=TRUE, clean_names=TRUE)
    expect_s3_class(response_parquet, "data.frame")
    expect_identical(nrow(response_parquet), 2L)
    expect_identical(response_parquet$party_id[2],
                     199335)
    expect_identical(response_parquet$given_name[2],
                     "Chris")
    expect_identical(response_parquet$salutation[1],
                     NA_integer_)

    # Function parameter error conditions
    expect_error(
      get_all_resources("some-endpoint", parquet="not_logical"),
      "argument 'parquet' must be TRUE or FALSE"
    )
    expect_error(
      get_all_resources("some-endpoint", clean_names="not_logical"),
      "argument 'clean_names' must be TRUE or FALSE"
    )
    # API query parameter error conditions
    expect_error(
      get_all_resources("some-endpoint", limit="foo"))
    expect_error(
      get_all_resources("some-endpoint", limit=NULL))
    expect_error(
      get_all_resources("some-endpoint", offset=-1))
    expect_error(
      get_all_resources("some-endpoint", offset=NA_integer_))
    expect_error(
      get_all_resources("some-endpoint", detail="invalid_value"))
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
    response <- get_all_resources("test-count", limit=50, offset=2)
    page_details <- get_page_details(response)
    expected <- c(count_reported = 5,
                  offset = 2,
                  limit = 50,
                  count_returned = 3)
    expect_identical(page_details, expected)
  })
})

test_that("parse_json_column() works", {
  df <- data.frame(
    id = 1:3,
    json_col = c('{"a": 1}', NA, '{"b": 2, "c": 3}'),
    stringsAsFactors = FALSE
  )

  # normal path
  new_df <- parse_json_column(df, "json_col", skip_if_missing = TRUE)
  expect_true("json_col_list" %in% names(new_df))
  expect_equal(names(df), names(new_df)[1:2])
  expect_type(new_df$json_col_list, "list")
  expect_equal(new_df$json_col_list[[1]]$a, 1)
  expect_type(new_df$json_col_list[[2]], "list")
  expect_length(new_df$json_col_list[[2]], 0)
  expect_equal(new_df$json_col_list[[3]]$b, 2)

  # no target column, but skip_if_missing
  same_df <- parse_json_column(df, "missing_col", skip_if_missing = TRUE)
  expect_identical(df, same_df)

  # no target column, and don't skip if missing
  expect_error(
    parse_json_column(df, "missing_col"),
    "Column 'missing_col' not found in data frame"
  )
  expect_error(
    parse_json_column(df, "missing_col", skip_if_missing = FALSE),
    "Column 'missing_col' not found in data frame"
  )

})
