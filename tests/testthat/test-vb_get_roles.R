with_mock_api({
  test_that("vb_get_roles() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/roles"
    expect_GET(
      vb_get_roles("ar.0"),
      paste0(endpoint, "/ar.0")
    )
    expect_GET(
      vb_get_roles(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_roles(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_roles("ar.1", parquet=FALSE,
      limit=NULL, offset=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("ar_code", "description", "name"),
      ignore.order = TRUE
    )
    expect_identical(response$ar_code[1], "ar.1")
    expect_identical(response$description[1], NA)
    expect_identical(response$name[1], "Classifier")

    expect_message(
      zero_records <- vb_get_roles(limit=0, parquet=FALSE),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_roles(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("ar_code", "description", "name"),
      ignore.order = TRUE
    )
    expect_identical(response$ar_code[2], "ar.17")
    expect_identical(response$name[2],"Contact")
    expect_identical(response$description[2], "VegBank Contact Party")
  })
})
