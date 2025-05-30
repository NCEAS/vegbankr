with_mock_api({
  test_that("get_all_community_concepts() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-concepts"
    expect_GET(
      get_all_community_concepts(),
      paste0(endpoint,
             "?detail=minimal",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_community_concepts(limit=5, offset=10, detail="full"),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10")
    )

    response <- get_all_community_concepts(detail="minimal", limit=2)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("accession_code", "comm_description", "comm_name_date_entered",
        "current_accepted", "default_name", "obs_count", "reference_id",
        "reference_accession_code"),
      ignore.order = TRUE
    )
    expect_identical(response$comm_name_date_entered[2],
                     "Fri, 30 May 2003 07:00:00 GMT")
    expect_identical(response$obs_count[2],
                     0L)
    expect_identical(response$current_accepted[2],
                     TRUE)
  })
})
