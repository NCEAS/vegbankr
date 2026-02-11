with_mock_api({
  test_that("vb_overview() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/overview"

    response <- vb_overview(limit=2)
    expect_type(response, "list")
    expect_identical(length(response), 7L)
    expect_true(all(sapply(response, is.data.frame)))
    expect_named(
      response,
      c("core_counts", "latest_n_projects", "top_n_community_concepts",
        "top_n_contributors", "top_n_named_places", "top_n_plant_concepts",
        "top_n_projects"),
      ignore.order = TRUE
    )
    expect_true("Observations" %in% response$core_counts$name)

  })
})
