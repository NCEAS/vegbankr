with_mock_api({
  test_that("vb_get_references() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/references"
    expect_GET(
      vb_get_references("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_references(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_references(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_references("rf.1", limit=NULL, offset=NULL)
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
    expect_identical(response$rf_code[1], "rf.1")
    expect_identical(response$total_pages[1], 5L)
    expect_identical(response$short_name[1], "USLC place names")

    response <- vb_get_references(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("degree", "doi", "full_citation", "isbn", "journal",
        "publication_date", "publication_place", "publisher",
        "reference_type", "rf_code", "short_name", "title",
        "total_pages", "url"),
      ignore.order = TRUE
    )
    expect_identical(response$rf_code[2], "rf.50592")
    expect_identical(response$short_name[2], "Weakley Jan 1, 2006")
    expect_identical(response$doi[2], NA)
  })
})
