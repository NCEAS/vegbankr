with_mock_api({
  test_that("vb_get_cover_methods() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/cover-methods"
    expect_GET(
      vb_get_cover_methods("cm.0"),
      paste0(endpoint, "/cm.0")
    )
    expect_GET(
      vb_get_cover_methods(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_cover_methods(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_cover_methods("cm.1", parquet=FALSE,
      limit=NULL, offset=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cm_code", "cover_code", "cover_estimation_method",
        "cover_percent", "cover_type", "index_description",
        "lower_limit", "rf_code", "rf_name", "upper_limit"),
      ignore.order = TRUE
    )
    expect_identical(response$cm_code[1], "cm.1")
    expect_identical(response$cover_percent[1], 0.05)
    expect_identical(response$cover_type[1], "Carolina Vegetation Survey")

    response <- vb_get_cover_methods(limit=2, parquet=FALSE,
      with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cm_code", "cover_code", "cover_estimation_method",
        "cover_percent", "cover_type", "index_description",
        "lower_limit", "rf_code", "rf_name", "upper_limit"),
      ignore.order = TRUE
    )
    expect_identical(response$cm_code[2], "cm.1")
    expect_identical(response$cover_percent[2], 0.505)
    expect_identical(response$cover_estimation_method[2], NA)
  })
})
