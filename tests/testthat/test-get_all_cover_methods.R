with_mock_api({
  test_that("get_all_cover_methods() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/cover-methods"
    expect_GET(
      get_all_cover_methods(),
      paste0(endpoint,
             "?limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_cover_methods(limit=5, offset=10),
      paste0(endpoint,
             "?limit=5",
             "&offset=10")
    )

    response <- get_all_cover_methods(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cm_code", "cover_code", "cover_estimation_method",
        "cover_percent", "cover_type", "index_description",
        "lower_limit", "rf_code", "rf_name", "upper_limit"),
      ignore.order = TRUE
    )
    expect_identical(response$cm_code[2],
                     "cm.1")
    expect_identical(response$cover_percent[2],
                     0.505)
    expect_identical(response$cover_estimation_method[2],
                     NA)
  })
})
