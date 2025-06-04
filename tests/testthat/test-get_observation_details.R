with_mock_api({
  test_that("get_plot_observation_details() works", {
    local_base_url(NULL)

    expect_GET(
      get_plot_observation_details("some_accession_code"),
      "https://api.vegbank.org/get_observation_details/some_accession_code"
    )

    expect_message(
       zero_records <- get_plot_observation_details("zero_records"),
       "No records returned",
       fixed = TRUE
    )
    expect_type(zero_records, "list")
    expect_identical(length(zero_records), 0L)

    response <- get_plot_observation_details("VB.Ob.41618.50D47AJX5G5U8WY")
    expect_type(response, "list")
    expect_identical(length(response), 3L)
    expect_named(
      response,
      c("plot_observation", "taxa", "communities"),
      ignore.order = TRUE
    )
    df_plot_observation <- response[["plot_observation"]]
    expect_s3_class(df_plot_observation, "data.frame")
    expect_identical(nrow(df_plot_observation), 1L)
    expect_named(
      df_plot_observation,
      c("area", "author_obs_code", "author_plot_code",
        "auto_taxon_cover", "bryophyte_quality", "confidentiality_text",
        "country", "cover_method_accession_code", "cover_type",
        "date_entered", "effort_level", "elevation",
        "floristic_quality", "interp_current_partyname", "latitude",
        "lichen_quality", "location_narrative", "longitude",
        "obs_accession_code", "obs_end_date", "obs_start_date",
        "permanence", "plot_accession_code", "plot_validation_level",
        "plot_validation_level_descr", "project_accession_code",
        "project_name", "slope_aspect", "slope_gradient",
        "state_province", "stratum_method_accession_code",
        "stratum_method_description", "stratum_method_name",
        "taxon_observation_area"),
      ignore.order = TRUE
    )
    expect_identical(df_plot_observation$longitude,
                      -106.410502828997)
    expect_identical(df_plot_observation$lichen_quality,
                      NA)
    expect_identical(df_plot_observation$plot_validation_level,
                      1L)

    df_taxa <- response[["taxa"]]
    expect_s3_class(df_taxa, "data.frame")
    expect_identical(nrow(df_taxa), 8L)
    expect_named(
      df_taxa,
      c("author_plant_name", "basal_area", "biomass", "cover",
        "cover_code", "inference_area", "int_curr_plant_code",
        "int_curr_plant_common", "int_curr_plant_sci_full",
        "int_curr_plant_sci_name_no_auth", "int_orig_plant_code",
        "int_orig_plant_common", "int_orig_plant_sci_full",
        "int_orig_plant_sci_name_no_auth", "stratum"),
      ignore.order = TRUE
    )
    expect_identical(df_taxa$int_curr_plant_sci_full[8],
                     "Lupinus L.")

    df_communities <- response[["communities"]]
    expect_s3_class(df_communities, "data.frame")
    expect_identical(nrow(df_communities), 1L)
    expect_named(
      df_communities,
      c("accession_code", "comm_name"),
      ignore.order = TRUE
    )
    expect_identical(df_communities$accession_code,
                     "VB.cc.30617.ARTEMISIATRIDEN")
  })
})
