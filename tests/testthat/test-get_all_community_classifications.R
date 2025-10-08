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

    expect_message(
       zero_records <- get_all_community_classifications(limit=0,
                                                         parquet=FALSE),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_community_classifications(detail="minimal",
                                                  limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cl_code", "cc_code", "comm_name", "ob_code"),
      ignore.order = TRUE
    )
    expect_identical(response$cl_code[2],
                     "cl.1554")
    expect_identical(response$comm_name[2],
                     "Typha (angustifolia, latifolia) - (Schoenoplectus spp.) Eastern Marsh")
  })
})
