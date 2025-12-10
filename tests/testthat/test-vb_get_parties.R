with_mock_api({
  test_that("vb_get_parties() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/parties"
    expect_GET(
      vb_get_parties("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_parties(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_parties(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_parties("py.191378", limit=NULL, offset=NULL)
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
    expect_identical(response$py_code, "py.191378")
    expect_identical(response$d_obs_count, 1123L)
    expect_identical(response$party_public, TRUE)
    expect_identical(response$contact_instructions, NA)

    expect_message(
      zero_records <- vb_get_parties(limit=0, parquet=FALSE),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_parties(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("py_code", "contact_instructions", "current_name_id",
        "d_obs_count", "email", "given_name", "middle_name",
        "organization_name", "party_public", "party_type",
        "salutation", "surname"),
      ignore.order = TRUE
    )
    expect_identical(response$py_code[2], "py.191378")
    expect_identical(response$d_obs_count[2], 1123L)
    expect_identical(response$party_public[2], TRUE)
    expect_identical(response$contact_instructions[2], NA)
  })
})
