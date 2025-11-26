with_mock_api({
  test_that("get_all_taxon_observations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/taxon-observations"
    expect_GET(
      get_all_taxon_observations(),
      paste0(endpoint,
             "?limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_taxon_observations(limit=5),
      paste0(endpoint,
             "?limit=5",
             "&offset=0")
    )

    expect_message(
       zero_records <- get_all_taxon_observations(limit=0, parquet=FALSE,
                                                  detail=NULL, with_nested=NULL),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_taxon_observations(limit=2, parquet=FALSE,
                                           detail=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
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
    expect_identical(response$to_code[2],
                     "to.64983")
    expect_identical(response$author_plant_name[2],
                     "Bryophyta")
    expect_identical(response$taxon_inference_area[2],
                     NA)
  })
})
