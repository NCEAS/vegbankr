with_mock_api({
  test_that("get_plot_observation_details() works", {
    local_base_url(NULL)

    expect_GET(
      get_plot_observation_details("ob.0"),
      "https://api.vegbank.org/get_observation_details/ob.0"
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
      c("author_obs_code", "author_plot_code",
        "latitude", "lichen_quality", "longitude",
        "plot_validation_level", "taxon_observation_area"),
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
        "cover_code", "stratum"),
      ignore.order = TRUE
    )
    expect_identical(df_taxa$author_plant_name[8],
                     "Lupinus L.")

    df_communities <- response[["communities"]]
    expect_s3_class(df_communities, "data.frame")
    expect_identical(nrow(df_communities), 1L)
    expect_named(
      df_communities,
      c("cc_code", "comm_name"),
      ignore.order = TRUE
    )
    expect_identical(df_communities$cc_code,
                     "cc.30617")
  })
})
