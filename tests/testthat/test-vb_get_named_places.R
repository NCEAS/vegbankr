with_mock_api({
  test_that("vb_get_named_places() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/named-places"
    expect_GET(
      vb_get_named_places("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_named_places(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_named_places(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_named_places("np.1", limit=NULL, offset=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("code", "description", "name", "np_code", "obs_count", "owner",
        "rf_label", "system"),
      ignore.order = TRUE
    )
    expect_identical(response$np_code, "np.1")
    expect_identical(response$name, "Someplace")
    expect_identical(response$description, NA)
    expect_identical(response$obs_count, 8L)

    expect_message(
      zero_records <- vb_get_named_places(limit=0, parquet=FALSE),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_named_places(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("code", "description", "name", "np_code", "obs_count", "owner",
        "rf_label", "system"),
      ignore.order = TRUE
    )
    expect_identical(response$np_code[2], "np.2")
    expect_identical(response$owner[2], NA_character_)
    expect_identical(response$system[2], "area|country|territory")
    expect_identical(response$description[2], NA)
  })
})
