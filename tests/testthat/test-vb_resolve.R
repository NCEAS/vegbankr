with_mock_api({
  test_that("vb_resolve() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/identifiers"
    identifier <- "some_identifier"

    response <- vb_resolve(identifier)
    expect_type(response, "list")
    expect_identical(length(response), 6L)
    expect_named(
      response,
      c("identifier_id", "identifier_type", "identifier_value",
        "vb_code", "vb_record_id", "vb_table_code"),
      ignore.order = TRUE
    )
    expect_identical(response$identifier_value, identifier)
    expect_identical(response$vb_code, "ob.1")

  })
})

with_mock_api({
  test_that("vb_get_by_id() works", {
    local_base_url(NULL)

    identifier <- "some_identifier"

    expect_message(
      vb_get_by_id(identifier, parquet=FALSE, verbose=TRUE),
      "Retrieved plot-observations record ob.1",
      fixed = TRUE)

    response <- vb_get_by_id(identifier, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("ob_code", "author_obs_code"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code, "ob.1")
    expect_identical(response$author_obs_code, "obs1")

  })
})
