with_mock_api({
  test_that("get_plot_observation() works", {
    local_base_url(NULL)

    expect_GET(
      get_plot_observation("some_vb_code"),
      "https://api.vegbank.org/plot-observations/some_vb_code"
    )

    expect_message(
       zero_records <- get_plot_observation("zero_records"),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_plot_observation("VB.Ob.41618.50D47AJX5G5U8WY")
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
