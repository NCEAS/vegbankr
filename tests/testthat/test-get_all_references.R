with_mock_api({
  test_that("get_all_references() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/references"
    expect_GET(
      get_all_references(),
      paste0(endpoint,
             "?limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_references(limit=5, offset=10),
      paste0(endpoint,
             "?limit=5",
             "&offset=10")
    )

    response <- get_all_references(limit=2, parquet=FALSE)
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
    expect_identical(response$rf_code[2],
                     "rf.50592")
    expect_identical(response$short_name[2],
                     "Weakley Jan 1, 2006")
    expect_identical(response$doi[2],
                     NA)
  })
})
