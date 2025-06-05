with_mock_api({
  test_that("get_taxon_observation() works", {
    local_base_url(NULL)

    expect_GET(
      get_taxon_observation("some_accession_code"),
      "https://api.vegbank.org/taxon-observations/some_accession_code"
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
      c("accession_code", "author_plant_name", "emb_taxon_observation",
        "int_curr_plant_code", "int_curr_plant_common",
        "int_curr_plant_concept_id", "int_curr_plant_sci_full",
        "int_curr_plant_sci_name_no_auth", "int_orig_plant_code",
        "int_orig_plant_common", "int_orig_plant_concept_id",
        "int_orig_plant_sci_full", "int_orig_plant_sci_name_no_auth",
        "max_cover", "observation_id", "reference_id",
        "taxon_inference_area", "taxon_observation_id"),
      ignore.order = TRUE
    )
    expect_identical(response$accession_code,
                     "VB.TO.64982.LICHEN")
    expect_identical(response$max_cover,
                     63.3125)
    expect_identical(response$taxon_inference_area,
                     NA)
  })
})
