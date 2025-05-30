with_mock_api({
  test_that("get_all_community_classifications() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-classifications"
    expect_GET(
      get_all_community_classifications(),
      paste0(endpoint,
             "?detail=minimal",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_community_classifications(limit=5, offset=10, detail="full"),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10")
    )

    response <- get_all_community_classifications(detail="minimal", limit=2)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("comm_class_accession_code", "comm_concept_accession_code",
        "comm_name", "obs_accession_code"),
      ignore.order = TRUE
    )
    expect_identical(response$comm_class_accession_code[2],
                     "VB.Cl.1554.2949")
    expect_identical(response$comm_name[2],
                     "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Marsh")
  })
})
