with_mock_api({
  test_that("vb_get_taxon_interpretations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/taxon-interpretations"
    expect_GET(
      vb_get_taxon_interpretations("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_taxon_interpretations(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_taxon_interpretations(limit=5),
      paste0(endpoint, "?limit=5&offset=0")
    )

    expect_message(
      zero_records <- vb_get_taxon_interpretations("zero_records",
        parquet=FALSE, limit=NULL, offset=NULL, detail=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_taxon_interpretations("ti.64550", limit=NULL,
      offset=NULL, detail=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("author_obs_code", "author_plant_name", "collection_number",
        "group_type", "interpretation_date", "interpretation_type",
        "is_curr", "is_orig", "notes", "notes_mgt", "notes_public", "ob_code",
        "party", "pc_code", "plant_code", "plant_label", "plant_name",
        "py_code", "revisions", "role", "taxon_confidence", "taxon_fit",
        "ti_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$ti_code, "ti.64550")
    expect_identical(response$author_plant_name, "Lichen")
    expect_identical(response$taxon_fit, NA)

    expect_message(
      zero_records <- vb_get_taxon_interpretations(limit=0, parquet=FALSE,
        detail=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_taxon_interpretations(limit=2, parquet=FALSE,
      detail=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("author_obs_code", "author_plant_name", "collection_number",
        "group_type", "interpretation_date", "interpretation_type",
        "is_curr", "is_orig", "notes", "notes_mgt", "notes_public", "ob_code",
        "party", "pc_code", "plant_code", "plant_label", "plant_name",
        "py_code", "revisions", "role", "taxon_confidence", "taxon_fit",
        "ti_code", "to_code"),
      ignore.order = TRUE
    )
    expect_identical(response$ti_code[2], "ti.64551")
    expect_identical(response$author_plant_name[2], "Bryophyta")
    expect_identical(response$taxon_fit[2], NA)
  })
})
