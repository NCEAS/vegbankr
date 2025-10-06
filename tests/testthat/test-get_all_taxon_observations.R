with_mock_api({
  test_that("get_all_taxon_observations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/taxon-observations"
    expect_GET(
      get_all_taxon_observations(),
      paste0(endpoint,
             "?detail=full",
             "&limit=100",
             "&offset=0",
             "&num_taxa=5")
    )
    expect_GET(
      get_all_taxon_observations(max_taxa_per_plot=2, limit=5),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=0",
             "&num_taxa=2")
    )

    expect_message(
       zero_records <- get_all_taxon_observations(limit=0, parquet=FALSE),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_taxon_observations(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
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
    expect_identical(response$accession_code[2],
                     "VB.TO.64990.SIBBALDIOPSISTR")
    expect_identical(response$max_cover[2],
                     5.1875)
    expect_identical(response$int_curr_plant_code[2],
                     NA_character_)
  })
})
