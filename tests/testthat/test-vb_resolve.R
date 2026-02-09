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
