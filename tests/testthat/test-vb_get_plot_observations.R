with_mock_api({
  test_that("vb_get_plot_observations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/plot-observations"
    expect_GET(
      vb_get_plot_observations("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_plot_observations(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_plot_observations(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    expect_message(
      zero_records <- vb_get_plot_observations("zero_records",
        parquet=FALSE, limit=NULL, offset=NULL, detail=NULL,
        with_nested=NULL, num_taxa=NULL, num_comms=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_plot_observations("ob.41618", detail=NULL,
      limit=NULL, offset=NULL, with_nested=NULL, num_taxa=NULL,
      num_comms=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("ob_code", "author_obs_code", "obs_start_date",
        "max_slope_aspect", "max_slope_gradient", "total_cover"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code, "ob.41618")
    expect_identical(response$max_slope_aspect, -106.409918856452)
    expect_identical(response$max_slope_gradient, NA)

    expect_message(
      zero_records <- vb_get_plot_observations(limit=0, parquet=FALSE,
        detail=NULL, with_nested=NULL, num_taxa=NULL, num_comms=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_plot_observations(limit=2, parquet=FALSE,
      detail=NULL, with_nested=NULL, num_taxa=NULL, num_comms=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("ob_code", "author_obs_code", "pl_code", "author_plot_code",
        "latitude", "longitude", "country", "state_province"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code[2], "ob.2948")
    expect_identical(response$longitude[2], -68.229339874)
    expect_identical(response$state_province[2], NA)
  })
})
