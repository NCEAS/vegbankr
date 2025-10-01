with_mock_api({
  test_that("get_cover_method() works", {
    local_base_url(NULL)

    expect_GET(
      get_cover_method("cm.0"),
      "https://api.vegbank.org/cover-methods/cm.0"
    )

    response <- get_cover_method("cm.1", parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cm_code", "cover_code", "cover_estimation_method",
        "cover_percent", "cover_type", "index_description",
        "lower_limit", "rf_code", "rf_name", "upper_limit"),
      ignore.order = TRUE
    )
    expect_identical(response$cm_code[1],
                     "cm.1")
    expect_identical(response$cover_percent[1],
                     0.05)
    expect_identical(response$cover_type[1],
                     "Carolina Vegetation Survey")
  })
})
