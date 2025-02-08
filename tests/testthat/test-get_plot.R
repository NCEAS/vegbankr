with_mock_api({
  test_that("get_plot() works", {
    old_base <- get_vb_base_url()
    set_vb_base_url(NULL)

    expect_GET(
      get_plot("some_accession_code"),
      "https://api.vegbank.org/plot/some_accession_code"
    )

    response <- get_plot("VB.PL.48373.VZ17QEZ6PVLCDPY")
    expect_s3_class(response, "data.frame")
    expect_equal(nrow(response), 1)
    expect_named(response, c("accessioncode", "latitude",
      "longitude", "plot_id"), ignore.order=TRUE)

    set_vb_base_url(old_base)
  })
})
