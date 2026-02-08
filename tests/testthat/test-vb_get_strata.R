with_mock_api({
  test_that("vb_get_strata() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/strata"
    expect_GET(
      vb_get_strata("sr.0"),
      paste0(endpoint, "/sr.0")
    )
    expect_GET(
      vb_get_strata(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_strata(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_strata("sr.1", parquet=FALSE,
      limit=NULL, offset=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("base", "cover", "description", "height", "name", "ob_code",
        "sm_code", "sr_code", "stratum_method_name",
        "stratum_type_name", "sy_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sr_code[1], "sr.1")
    expect_identical(response$cover[1], 0.0)
    expect_identical(response$stratum_type_name[1], "Shrub")

    expect_message(
      zero_records <- vb_get_strata(limit=0, parquet=FALSE),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_strata(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("base", "cover", "description", "height", "name", "ob_code",
        "sm_code", "sr_code", "stratum_method_name",
        "stratum_type_name", "sy_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sr_code[2], "sr.17457")
    expect_identical(response$cover[2], 0.0)
    expect_identical(response$height[2], NA)
    expect_identical(response$stratum_type_name[2], "Short Shrub")
  })
})
