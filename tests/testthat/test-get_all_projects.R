with_mock_api({
  test_that("get_all_projects() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/projects"
    expect_GET(
      get_all_projects(),
      paste0(endpoint,
             "?detail=full",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_projects(limit=5, offset=10),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10")
    )

    expect_message(
       zero_records <- get_all_projects(limit=0, parquet=FALSE),
       "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- get_all_projects(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("last_plot_added_date", "obs_count", "project_accession_code",
        "project_description", "project_id", "project_name",
        "start_date", "stop_date"),
      ignore.order = TRUE
    )
    expect_identical(response$project_accession_code[2],
                     "urn:lsid:cvs.bio.unc.edu:project:CVS.Pj.CVS-49")
    expect_identical(response$obs_count[2],
                     56L)
    expect_identical(response$last_plot_added_date[2],
                     "Fri, 11 Dec 2015 18:28:33 GMT")
    expect_identical(response$start_date[2],
                     NA)
  })
})
