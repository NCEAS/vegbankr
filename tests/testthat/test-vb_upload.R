with_mock_api({
  local_base_url(NULL)
  test_that("vb_upload() works", {

    # Test messages with debugging enabled
    suppressMessages(vb_debug())
    printed <- capture.output({
      msgs <- capture_messages(
        vb_upload("some-endpoint", data = data.frame(a=1))
      )
    })
    expect_true(any(grepl("Wrote 'data' to Parquet", msgs)))
    suppressMessages(vb_undebug())

    # Test messages with dry_run enabled
    printed_dry_run <- capture.output({
      msgs <- capture_messages(
        vb_upload("some-endpoint", data = data.frame(a=1), dry_run = TRUE)
      )
    })
    expect_true(any(grepl("dry run, transaction was rolled back", msgs)))
    expect_true(all(!grepl("Wrote 'data' to Parquet", msgs)))
    expect_equal(printed_dry_run,
        c("$ob",
          "    action user_ob_code vb_ob_code",
          "1 inserted            1       ob.1",
          "2 inserted            2       ob.2",
          ""))

    # Test that query_params argument works
    endpoint <- "https://api.vegbank.org/some-endpoint"
    expect_POST(
      vb_upload("some-endpoint",
                some_dataset = data.frame(a=1),
                query_params = list(some_param = "some_value")),
      paste0(endpoint, "?dry_run=FALSE&some_param=some_value")
    )
    # Test error with bad query_params argument
    expect_error(
      vb_upload("some-endpoint",
                dat = data.frame(a=1),
                query_params = TRUE),
      "`query_params` must be a named list, or NULL."
    )

    # Test messages without debugging enabled
    printed <- capture.output({
      msgs <- capture_messages(
        vb_upload("some-endpoint", data = data.frame(a=1))
      )
    })
    expect_true(all(!grepl("Wrote 'data' to Parquet", msgs)))
    expect_true(any(grepl("Upload complete", msgs)))
    expect_true(any(grepl("-> inserted 5 ob record(s)", msgs, fixed=TRUE)))

    # Test JSON response
    printed <- capture.output({
      response <- suppressMessages(
        vb_upload("some-endpoint", data = data.frame(a=1))
      )
    })
    response_list <- response |> resp_body_json()
    expect_type(response_list, "list")
    expect_named(
      response_list,
      c("counts", "resources"),
      ignore.order = TRUE
    )
    expect_identical(response_list$counts$ob$inserted, 5L)
    expect_identical(response_list$resources$ob[[2]]$action, "inserted")
    expect_identical(response_list$resources$ob[[2]]$user_ob_code, "2")
    expect_identical(response_list$resources$ob[[2]]$vb_ob_code, "ob.2")
    expect_equal(printed,
        c("$ob",
          "      action user_ob_code vb_ob_code",
          "1   inserted            1       ob.1",
          "2   inserted            2       ob.2",
          "...      ...          ...        ...",
          "4   inserted            4       ob.4",
          "5   inserted            5       ob.5",
          ""))

    # Test JSON response with zero returned records
    printed <- capture.output({
      response <- suppressMessages(
        vb_upload("some-endpoint", data = data.frame(a=0))
      )
    })
    response_list <- response |> resp_body_json()
    expect_type(response_list, "list")
    expect_named(
      response_list,
      c("counts", "resources"),
      ignore.order = TRUE
    )
    expect_identical(response_list$counts$ob$inserted, 0L)
    expect_identical(response_list$resources$ob, list())
    expect_equal(printed,
        c("$ob",
          "data frame with 0 columns and 0 rows",
          ""))

    # Function parameter error conditions
    expect_error(
      vb_upload("some-endpoint", dry_run=TRUE),
      "At least one named data frame must be provided"
    )
    expect_error(
      vb_upload("some-endpoint", "unnamed_argument"),
      "All data frames must be passed as named arguments"
    )
    expect_error(
      vb_upload("some-endpoint", data="not a dataframe!"),
      "Argument 'data' is not a data frame"
    )
    expect_error(
      vb_upload("some-endpoint", data = data.frame()),
      "Data frame 'data' has zero rows"
    )
  })
})

with_mock_api({
  local_base_url(NULL)
  test_that("specific vb_upload_*() methods work", {

    endpoint <- "https://api.vegbank.org/plot-observations"
    expect_error(
      vb_upload_plot_observations(),
      "argument \"plot_observations\" is missing, with no default"
    )
    expect_POST(
      vb_upload_plot_observations(
        plot_observations = data.frame(a=1)),
      paste0(endpoint)
    )

    endpoint <- "https://api.vegbank.org/plant-concepts"
    expect_error(
      vb_upload_plant_concepts(),
      "argument \"plant_concepts\" is missing, with no default"
    )
    expect_POST(
      vb_upload_plant_concepts(
        plant_concepts = data.frame(a=1)),
      paste0(endpoint)
    )
    expect_POST(
      vb_upload_plant_concepts(
        plant_concepts = data.frame(a=1),
        what_to_deactivate = "none"),
      paste0(endpoint, "?dry_run=FALSE&deactivation=none")
    )

    endpoint <- "https://api.vegbank.org/community-concepts"
    expect_error(
      vb_upload_community_concepts(),
      "argument \"community_concepts\" is missing, with no default"
    )
    expect_POST(
      vb_upload_community_concepts(
        community_concepts = data.frame(a=1)),
      paste0(endpoint)
    )
    expect_POST(
      vb_upload_community_concepts(
        community_concepts = data.frame(a=1),
        what_to_deactivate = "none"),
      paste0(endpoint, "?dry_run=FALSE&deactivation=none")
    )

    endpoint <- "https://api.vegbank.org/taxon-interpretations"
    expect_error(
      vb_upload_taxon_interpretations(),
      "argument \"taxon_interpretations\" is missing, with no default"
    )
    expect_POST(
      vb_upload_taxon_interpretations(
        taxon_interpretations = data.frame(a=1)),
      paste0(endpoint)
    )

    endpoint <- "https://api.vegbank.org/community-classifications"
    expect_error(
      vb_upload_community_classifications(),
      "argument \"community_classifications\" is missing, with no default"
    )
    expect_POST(
      vb_upload_community_classifications(
        community_classifications = data.frame(a=1)),
      paste0(endpoint)
    )

    endpoint <- "https://api.vegbank.org/cover-methods"
    expect_error(
      vb_upload_cover_methods(),
      "argument \"cover_methods\" is missing, with no default"
    )
    expect_POST(
      vb_upload_cover_methods(
        cover_methods = data.frame(a=1)),
      paste0(endpoint)
    )

    endpoint <- "https://api.vegbank.org/stratum-methods"
    expect_error(
      vb_upload_stratum_methods(),
      "argument \"stratum_methods\" is missing, with no default"
    )
    expect_POST(
      vb_upload_stratum_methods(
        stratum_methods = data.frame(a=1)),
      paste0(endpoint)
    )

  })
})
