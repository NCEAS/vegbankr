with_mock_api({
  test_that("get_stratum_method() works", {
    local_base_url(NULL)

    expect_GET(
      get_stratum_method("sm.0"),
      "https://api.vegbank.org/stratum-methods/sm.0"
    )

    response <- get_stratum_method("sm.1", parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 3L)
    expect_named(
      response,
      c("rf_code", "rf_name", "sm_code", "stratum_assignment",
        "stratum_description", "stratum_index",
        "stratum_method_description", "stratum_method_name",
        "stratum_name", "sy_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sm_code[1],
                     "sm.1")
    expect_identical(response$stratum_name[1],
                     "Tree")
    expect_identical(response$stratum_description[1],
                     NA)
  })
})
