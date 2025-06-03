with_mock_api({
  test_that("get_all_parties() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/parties"
    expect_GET(
      get_all_parties(),
      paste0(endpoint,
             "?detail=full",
             "&limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_parties(limit=5, offset=10),
      paste0(endpoint,
             "?detail=full",
             "&limit=5",
             "&offset=10")
    )

    response <- get_all_parties(limit=2)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("accession_code", "contact_instructions", "current_name_id",
        "d_obs_count", "email", "given_name", "middle_name",
        "organization_name", "party_id", "party_public", "party_type",
        "salutation", "surname"),
      ignore.order = TRUE
    )
    expect_identical(response$accession_code[2],
                     "VB.py.191378.VOLUNTEER")
    expect_identical(response$d_obs_count[2],
                     1123L)
    expect_identical(response$party_public[2],
                     TRUE)
    expect_identical(response$contact_instructions[2],
                     NA)
  })
})
