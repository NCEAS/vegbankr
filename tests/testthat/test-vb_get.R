with_mock_api({
  local_base_url(NULL)
  test_that("vb_get() can get one record", {
    # Test JSON response
    response_json <- vb_get("plot-observations", "ob.41618",
      limit=NULL, offset=NULL, parquet=FALSE)
    expect_s3_class(response_json, "data.frame")
    expect_identical(nrow(response_json), 1L)
    expect_named(
      response_json,
      c("ob_code", "author_obs_code",
        "obs_start_date", "max_slope_aspect", "max_slope_gradient",
        "total_cover"),
      ignore.order = TRUE
    )
    expect_identical(response_json$ob_code, "ob.41618")
    expect_identical(response_json$max_slope_aspect, -106.409918856452)
    expect_identical(response_json$max_slope_gradient, NA)

    # Test Parquet response
    response_parquet <- vb_get("parquet-test", "vb.1", parquet=TRUE,
      limit=NULL, offset=NULL)
    expect_s3_class(response_parquet, "data.frame")
    expect_identical(nrow(response_parquet), 40L)
    expect_identical(response_parquet$cm_code[1], "cm.1")
    expect_identical(response_parquet$cover_percent[1], 0.05)
    expect_identical(response_parquet$cover_estimation_method[1], NA_integer_)

    # Function parameter error conditions
    expect_error(
      vb_get("some-endpoint", parquet="not_logical"),
      "argument 'parquet' must be TRUE or FALSE"
    )
    expect_error(
      vb_get("some-endpoint", clean_names="not_logical"),
      "argument 'clean_names' must be TRUE or FALSE"
    )
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("vb_get() can get full collection", {
    # Test JSON response
    response_json <- vb_get("plot-observations", limit=2, parquet=FALSE)
    expect_s3_class(response_json, "data.frame")
    expect_identical(nrow(response_json), 2L)
    expect_named(
      response_json,
      c("ob_code", "author_obs_code", "pl_code", "author_plot_code",
        "latitude", "longitude", "country", "state_province"),
      ignore.order = TRUE
    )
    expect_identical(response_json$ob_code[2], "ob.2948")
    expect_identical(response_json$longitude[2], -68.229339874)
    expect_identical(response_json$state_province[2], NA)

    # Test Parquet response
    response_parquet <- vb_get("parquet-test", limit=2,
      parquet=TRUE, clean_names=TRUE)
    expect_s3_class(response_parquet, "data.frame")
    expect_identical(nrow(response_parquet), 2L)
    expect_identical(response_parquet$party_id[2], 199335)
    expect_identical(response_parquet$given_name[2], "Chris")
    expect_identical(response_parquet$salutation[1], NA_integer_)

    # Function parameter error conditions
    expect_error(
      vb_get("some-endpoint", parquet="not_logical"),
      "argument 'parquet' must be TRUE or FALSE"
    )
    expect_error(
      vb_get("some-endpoint", clean_names="not_logical"),
      "argument 'clean_names' must be TRUE or FALSE"
    )
    # API query parameter error conditions
    expect_error(vb_get("some-endpoint", limit="foo"))
    expect_error(vb_get("some-endpoint", limit=NULL))
    expect_error(vb_get("some-endpoint", offset=-1))
    expect_error(vb_get("some-endpoint", offset=NA_integer_))
    expect_error(vb_get("some-endpoint", detail="invalid_value"))
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("vb_get() can construct a cross-resource request", {
    endpoint <- "https://api.vegbank.org"
    expect_GET(
      vb_get("plot-observations", "pj.11"),
      paste0(endpoint, "/projects/pj.11/plot-observations")
    )
    expect_GET(
      vb_get("plot-observations", "some_project", by="projects"),
      paste0(endpoint, "/projects/some_project/plot-observations")
    )
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("vb_get() returns expected page details", {
    response <- vb_get("test-count", limit=50, offset=2, parquet=FALSE)
    page_details <- get_page_details(response)
    expected <- c(count_reported = 5,
                  offset = 2,
                  limit = 50,
                  count_returned = 3)
    expect_identical(page_details, expected)
  })
})

test_that("vb key lookup works", {
  expect_equal("cl", get_vb_key("community-classifications"))
  expect_equal("cc", get_vb_key("community-concepts"))
  expect_equal("ci", get_vb_key("community-interpretations"))
  expect_equal("cm", get_vb_key("cover-methods"))
  expect_equal("py", get_vb_key("parties"))
  expect_equal("pc", get_vb_key("plant-concepts"))
  expect_equal("ob", get_vb_key("plot-observations"))
  expect_equal("pj", get_vb_key("projects"))
  expect_equal("rf", get_vb_key("references"))
  expect_equal("sm", get_vb_key("stratum-methods"))
  expect_equal("ti", get_vb_key("taxon-interpretations"))
  expect_equal("to", get_vb_key("taxon-observations"))
})
