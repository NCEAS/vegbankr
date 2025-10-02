with_mock_api({
  test_that("get_reference() works", {
    local_base_url(NULL)

    expect_GET(
      get_reference("rf.0"),
      "https://api.vegbank.org/references/rf.0"
    )

    response <- get_reference("rf.1", parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("degree", "doi", "full_citation", "isbn", "journal",
        "publication_date", "publication_place", "publisher",
        "reference_type", "rf_code", "short_name", "title",
        "total_pages", "url"),
      ignore.order = TRUE
    )
    expect_identical(response$rf_code[1],
                     "rf.1")
    expect_identical(response$total_pages[1],
                     5L)
    expect_identical(response$short_name[1],
                     "USLC place names")
  })
})
