with_mock_api({
  test_that("get_project() works", {
    local_base_url(NULL)

    expect_GET(
      get_project("some_accession_code"),
      "https://api.vegbank.org/projects/some_accession_code"
    )

    response <- get_project("VB.pj.10508.SOUTHWESTGAPCOL")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("last_plot_added_date", "obs_count", "project_accession_code",
        "project_description", "project_id", "project_name",
        "start_date", "stop_date"),
      ignore.order = TRUE
    )
    expect_identical(response$project_accession_code,
                     "VB.pj.10508.SOUTHWESTGAPCOL")
    expect_identical(response$obs_count,
                     5286L)
    expect_identical(response$start_date,
                     NA)
  })
})
