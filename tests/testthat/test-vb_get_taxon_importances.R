with_mock_api({
  test_that("vb_get_taxon_importances() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/taxon-importances"
    expect_GET(
      vb_get_taxon_importances("tm.0"),
      paste0(endpoint, "/tm.0")
    )
    expect_GET(
      vb_get_taxon_importances(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_taxon_importances(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_taxon_importances("tm.1", parquet=FALSE,
      limit=NULL, offset=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("basal_area", "biomass", "cover", "cover_code",
        "inference_area", "ob_code", "sr_code", "stratum_base",
        "stratum_height", "stratum_name", "tm_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$tm_code[1], "tm.1")
    expect_identical(response$cover[1], 63.3125)
    expect_identical(response$stratum_name[1], "Nonvascular")

    response <- vb_get_taxon_importances(limit=2, parquet=FALSE,
      with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("basal_area", "biomass", "cover", "cover_code",
        "inference_area", "ob_code", "sr_code", "stratum_base",
        "stratum_height", "stratum_name", "tm_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$tm_code[2], "tm.67084")
    expect_identical(response$cover[2], 63.3125)
    expect_identical(response$stratum_name[2], "<All>")
  })
})
