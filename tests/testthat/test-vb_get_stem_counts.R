with_mock_api({
  test_that("vb_get_stem_counts() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/stem-counts"
    expect_GET(
      vb_get_stem_counts("sc.0"),
      paste0(endpoint, "/sc.0")
    )
    expect_GET(
      vb_get_stem_counts(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_stem_counts(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_stem_counts("sc.1", parquet=FALSE,
      limit=NULL, offset=NULL, detail=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("count", "diameter", "diameter_accuracy", "height", "height_accuracy",
        "ob_code", "sc_code", "sr_code", "stratum_name", "taxon_area",
        "tm_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sc_code[1], "sc.1")
    expect_identical(response$count[1], 1L)
    expect_identical(response$diameter[1], 22.0)
    expect_identical(response$stratum_name[1], "Canopy")

    expect_message(
      zero_records <- vb_get_stem_counts(limit=0, parquet=FALSE, detail=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_stem_counts(limit=2, parquet=FALSE, detail=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("count", "diameter", "diameter_accuracy", "height", "height_accuracy",
        "ob_code", "sc_code", "sr_code", "stratum_name", "taxon_area",
        "tm_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$sc_code[2], "sc.2057")
    expect_identical(response$count[2], 1L)
    expect_identical(response$diameter[2], 17.2)
    expect_identical(response$stratum_name[2], "Canopy")
  })
})
