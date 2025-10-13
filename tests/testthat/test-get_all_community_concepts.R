with_mock_api({
  test_that("get_all_community_concepts() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-concepts"
    expect_GET(
      get_all_community_concepts(),
      paste0(endpoint,
             "?detail=full",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_community_concepts(limit=5, offset=10, parquet=FALSE),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10")
    )

    expect_message(
       zero_records <- get_all_community_concepts(limit=0, parquet=FALSE),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_community_concepts(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cc_code", "children", "comm_description", "obs_count",
        "usages"),
      ignore.order = TRUE
    )
    expect_identical(response$children[[2]]$cc_code[2],
                     "cc.25004")
    expect_identical(response$obs_count[2],
                     100L)
    expect_identical(response$comm_description[2],
                     "Graminoids and stuff")
  })
})
