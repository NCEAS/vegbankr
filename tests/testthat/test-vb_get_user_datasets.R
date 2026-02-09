with_mock_api({
  test_that("vb_get_user_datasets() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/user-datasets"
    expect_GET(
      vb_get_user_datasets("ds.0"),
      paste0(endpoint, "/ds.0")
    )
    expect_GET(
      vb_get_user_datasets(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_user_datasets(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_user_datasets("ds.1", parquet=FALSE,
      limit=NULL, offset=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("accession_code", "description", "ds_code", "name", "obs_count",
        "owner_email", "owner_label", "start", "stop", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$ds_code[1], "ds.1")
    expect_identical(response$description[1], NA)
    expect_identical(response$obs_count[1], 8L)
    expect_identical(response$accession_code[1], "VB.ds.1.xxx")

    expect_message(
      zero_records <- vb_get_user_datasets(limit=0, parquet=FALSE),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_user_datasets(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("accession_code", "description", "ds_code", "name", "obs_count",
        "owner_email", "owner_label", "start", "stop", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$ds_code[2], "ds.123")
    expect_identical(response$obs_count[2], 6L)
    expect_identical(response$owner_label[2], "Owner, The")
    expect_identical(response$type[2], "normal")
  })
})
