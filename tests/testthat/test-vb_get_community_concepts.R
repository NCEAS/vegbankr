with_mock_api({
  test_that("vb_get_community_concepts() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-concepts"
    expect_GET(
      vb_get_community_concepts("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_community_concepts(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_community_concepts(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_community_concepts("cc.30617", limit=NULL,
      offset=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("cc_code", "children", "comm_description", "obs_count",
        "usages"),
      ignore.order = TRUE
    )
    expect_identical(response$cc_code, "cc.19437")
    expect_identical(response$obs_count, 0L)
    expect_identical(response$comm_description, NA)

    expect_message(
      zero_records <- vb_get_community_concepts(limit=0, parquet=FALSE,
        with_nested=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_community_concepts(limit=2, parquet=FALSE,
      with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cc_code", "children", "comm_description", "obs_count",
        "usages"),
      ignore.order = TRUE
    )
    expect_identical(response$children[[2]]$cc_code[2], "cc.25004")
    expect_identical(response$obs_count[2], 100L)
    expect_identical(response$comm_description[2], "Graminoids and stuff")
  })
})
