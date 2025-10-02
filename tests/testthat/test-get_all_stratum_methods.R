with_mock_api({
  test_that("get_all_stratum_methods() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/stratum-methods"
    expect_GET(
      get_all_stratum_methods(),
      paste0(endpoint,
             "?detail=full",
             "&limit=100",
             "&offset=0",
             "&create_parquet=TRUE")
    )
    expect_GET(
      get_all_stratum_methods(limit=5, offset=10),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10",
             "&create_parquet=TRUE")
    )

    response <- get_all_stratum_methods(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("rf_code", "rf_name", "sm_code", "stratum_assignment",
        "stratum_description", "stratum_index",
        "stratum_method_description", "stratum_method_name",
        "stratum_name", "sy_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sm_code[2],
                     "sm.2038")
    expect_identical(response$stratum_name[2],
                     "Cnpy")
    expect_identical(response$stratum_description[2],
                     NA)
  })
})
