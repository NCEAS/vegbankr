with_mock_api({
  test_that("get_community_concept() works", {
    local_base_url(NULL)

    expect_GET(
      get_community_concept("some_vb_code"),
      "https://api.vegbank.org/community-concepts/some_vb_code"
    )

    response <- get_community_concept("cc.30617")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("cc_code", "children", "comm_description", "obs_count",
        "usages"),
      ignore.order = TRUE
    )
    expect_identical(response$cc_code,
                     "cc.19437")
    expect_identical(response$obs_count,
                     0L)
    expect_identical(response$comm_description,
                     NA)
  })
})
