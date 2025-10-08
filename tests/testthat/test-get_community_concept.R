with_mock_api({
  test_that("get_community_concept() works", {
    local_base_url(NULL)

    expect_GET(
      get_community_concept("some_vb_code"),
      "https://api.vegbank.org/community-concepts/some_vb_code"
    )

    response <- get_community_concept("VB.cc.30617.ARTEMISIATRIDEN")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("cc_code", "class_system", "comm_description",
        "comm_name", "comm_name_date_entered", "comm_name_status",
        "current_accepted", "default_name", "obs_count",
        "py_code", "rf_code", "usage_start", "usage_stop"),
      ignore.order = TRUE
    )
    expect_identical(response$cc_code,
                     "cc.30617")
    expect_identical(response$obs_count,
                     336L)
    expect_identical(response$comm_description,
                     NA)
  })
})
