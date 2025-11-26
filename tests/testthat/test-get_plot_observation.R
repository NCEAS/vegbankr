with_mock_api({
  test_that("get_plot_observation() works", {
    local_base_url(NULL)

    expect_GET(
      get_plot_observation("some_vb_code"),
      "https://api.vegbank.org/plot-observations/some_vb_code"
    )

    expect_message(
       zero_records <- get_plot_observation("zero_records",
                                            detail=NULL, with_nested=NULL,
                                            num_taxa=NULL, num_comms=NULL),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_plot_observation("ob.41618", detail=NULL, with_nested=NULL,
                                     num_taxa=NULL, num_comms=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("ob_code", "author_obs_code",
        "obs_start_date", "max_slope_aspect", "max_slope_gradient",
        "total_cover"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code,
                 "ob.41618")
    expect_identical(response$max_slope_aspect,
                 -106.409918856452)
    expect_identical(response$max_slope_gradient,
                 NA)
  })
})
