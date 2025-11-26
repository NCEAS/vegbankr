with_mock_api({
  test_that("get_taxon_observation() works", {
    local_base_url(NULL)

    expect_GET(
      get_taxon_observation("some_vb_code"),
      "https://api.vegbank.org/taxon-observations/some_vb_code"
    )

    expect_message(
       zero_records <- get_taxon_observation("zero_records",
                                             with_nested = NULL),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_taxon_observation("to.693826",
                                      with_nested = NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("to_code", "author_plant_name", "int_curr_pc_code",
        "int_curr_plant_code", "int_curr_plant_common",
        "int_curr_plant_sci_full", "int_curr_plant_sci_name_no_auth",
        "int_orig_pc_code", "int_orig_plant_code", "int_orig_plant_common",
        "int_orig_plant_sci_full", "int_orig_plant_sci_name_no_auth",
        "ob_code", "rf_code", "rf_label", "taxon_inference_area"),
      ignore.order = TRUE
    )
    expect_identical(response$to_code,
                     "to.64982")
    expect_identical(response$author_plant_name,
                     "Lichen")
    expect_identical(response$taxon_inference_area,
                     NA)
  })
})
