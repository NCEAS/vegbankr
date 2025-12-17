with_mock_api({
  test_that("vb_count() works", {
    local_base_url(NULL)

    # Case 1: Valid count
    valid_count <- vb_count("count-valid")
    expect_identical(valid_count, 999L)

    # Case 2: JSON response missing count field
    expect_warning(
      vb_count("count-missing"),
      "Failed to obtain count from VegBank"
    )

    # Case 3: Unparseable JSON response
    expect_warning(
      vb_count("count-bad-response"),
      "Failed to obtain count from VegBank"
    )
  })
})

with_mock_api({
  test_that("vb_count_* functions make the expected request", {
    local_base_url(NULL)

    # Test endpoint-specific wrappers
    endpoint <- "https://api.vegbank.org/"
    count_param <- "?count"
    expect_GET(
      vb_count_community_classifications(),
      paste0(endpoint, "community-classifications", count_param)
    )
    expect_GET(
      vb_count_community_concepts(),
      paste0(endpoint, "community-concepts", count_param)
    )
    expect_GET(
      vb_count_cover_methods(),
      paste0(endpoint, "cover-methods", count_param)
    )
    expect_GET(
      vb_count_parties(),
      paste0(endpoint, "parties", count_param)
    )
    expect_GET(
      vb_count_plant_concepts(),
      paste0(endpoint, "plant-concepts", count_param)
    )
    expect_GET(
      vb_count_plot_observations(),
      paste0(endpoint, "plot-observations", count_param)
    )
    expect_GET(
      vb_count_projects(),
      paste0(endpoint, "projects", count_param)
    )
    expect_GET(
      vb_count_references(),
      paste0(endpoint, "references", count_param)
    )
    expect_GET(
      vb_count_stratum_methods(),
      paste0(endpoint, "stratum-methods", count_param)
    )
    expect_GET(
      vb_count_taxon_observations(),
      paste0(endpoint, "taxon-observations", count_param)
    )
  })
})
