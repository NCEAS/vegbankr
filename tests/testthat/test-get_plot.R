with_mock_api({
  test_that("get_plot() works", {
    local_base_url(NULL)

    expect_GET(
      get_plot("some_accession_code"),
      "https://api.vegbank.org/plot/some_accession_code"
    )

    expect_message(
       zero_records <- get_plot("zero_records"),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    expect_warning(
       error_response <- get_plot("error_response"),
       "API error: something went wrong",
       fixed = TRUE
    )
    expect_null(error_response)

    response <- get_plot("VB.PL.48373.VZ17QEZ6PVLCDPY")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("accession_code", "latitude", "longitude", "plot_id"),
      ignore.order = TRUE
    )
  })
})
