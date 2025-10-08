with_mock_api({
  test_that("get_taxon_observation() works", {
    local_base_url(NULL)

    expect_GET(
      get_taxon_observation("some_vb_code"),
      "https://api.vegbank.org/taxon-observations/some_vb_code"
    )

    expect_message(
       zero_records <- get_taxon_observation("zero_records"),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_taxon_observation("VB.TO.693826.Q8MM3I559WIQP3D")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("to_code", "author_plant_name", "emb_taxon_observation",
        "int_curr_plant_code", "int_curr_plant_common",
        "int_curr_plant_concept_id", "int_curr_plant_sci_full",
        "int_curr_plant_sci_name_no_auth", "int_orig_plant_code",
        "int_orig_plant_common", "int_orig_plant_concept_id",
        "int_orig_plant_sci_full", "int_orig_plant_sci_name_no_auth",
        "max_cover", "ob_code", "rf_code", "taxon_inference_area"),
      ignore.order = TRUE
    )
    expect_identical(response$to_code,
                     "to.64982")
    expect_identical(response$max_cover,
                     63.3125)
    expect_identical(response$taxon_inference_area,
                     NA)
  })
})
