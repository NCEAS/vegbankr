with_mock_api({
  test_that("get_all_plot_observations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/plot-observations"
    expect_GET(
      get_all_plot_observations(),
      paste0(endpoint,
             "?detail=minimal",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_plot_observations(limit=5, offset=10),
      paste0(endpoint,
             "?detail=minimal",
             "&limit=5",
             "&offset=10")
    )

    expect_message(
       zero_records <- get_all_plot_observations(limit=0, parquet=FALSE),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_plot_observations(detail="minimal", limit=1, parquet=FALSE)
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

    expect_error(
      get_all_plot_observations(limit="foo"),
      "limit must be a finite, non-negative integer")
    expect_error(
      get_all_plot_observations(offset=-1),
      "offset must be a finite, non-negative integer")
    expect_error(
      get_all_plot_observations(detail="invalid_value"),
      "'arg' should be one of \"minimal\", \"full\"")
  })
})
