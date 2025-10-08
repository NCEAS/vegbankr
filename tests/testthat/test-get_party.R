with_mock_api({
  test_that("get_party() works", {
    local_base_url(NULL)

    expect_GET(
      get_party("some_vb_code"),
      "https://api.vegbank.org/parties/some_vb_code"
    )

    response <- get_party("VB.py.191378.VOLUNTEER")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("py_code", "contact_instructions", "current_name_id",
        "d_obs_count", "email", "given_name", "middle_name",
        "organization_name", "party_public", "party_type",
        "salutation", "surname"),
      ignore.order = TRUE
    )
    expect_identical(response$py_code,
                     "py.191378")
    expect_identical(response$d_obs_count,
                     1123L)
    expect_identical(response$party_public,
                     TRUE)
    expect_identical(response$contact_instructions,
                     NA)
  })
})
