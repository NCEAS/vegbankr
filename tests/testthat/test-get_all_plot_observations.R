with_mock_api({
  test_that("get_all_plot_observations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/plot-observations"
    expect_GET(
      get_all_plot_observations(),
      paste0(endpoint,
             "?limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_plot_observations(limit=5, offset=10),
      paste0(endpoint,
             "?limit=5",
             "&offset=10")
    )

    expect_message(
       zero_records <- get_all_plot_observations(limit=0, parquet=FALSE,
                                                 detail=NULL, with_nested=NULL,
                                                 num_taxa=NULL, num_comms=NULL),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_plot_observations(limit=2, parquet=FALSE,
                                          detail=NULL, with_nested=NULL,
                                          num_taxa=NULL, num_comms=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("ob_code", "author_obs_code",
        "pl_code", "author_plot_code",
        "latitude", "longitude", "country", "state_province"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code[2],
                 "ob.2948")
    expect_identical(response$longitude[2],
                  -68.229339874)
    expect_identical(response$state_province[2],
                 NA)
  })
})
