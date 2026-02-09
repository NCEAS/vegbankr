with_mock_api({
  test_that("vb_resolve() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/identifiers"
    identifier <- "some_identifier"

    response <- vb_resolve(identifier)
    expect_type(response, "list")
    expect_identical(length(response), 4L)
    expect_named(
      response,
      c("identifier_type", "identifier_value",
        "vb_code", "vb_resource_type"),
      ignore.order = TRUE
    )
    expect_identical(response$identifier_value, identifier)
    expect_identical(response$vb_code, "ob.1")
    expect_identical(response$vb_resource_type, "plot-observations")

    bad_identifier <- "bad_identifier"
    expect_warning(
      bad_response <- vb_resolve(bad_identifier),
      "Unknown resource type code \"xx\"",
      fixed = TRUE)
    expect_type(bad_response, "list")
    expect_identical(length(bad_response), 4L)
    expect_identical(bad_response$identifier_value, bad_identifier)
    expect_identical(bad_response$vb_code, "xx.1")
    expect_identical(bad_response$vb_resource_type, "unknown")
  })
})

with_mock_api({
  test_that("vb_get_by_id() works", {
    local_base_url(NULL)

    identifier <- "some_identifier"

    expect_message(
      vb_get_by_id(identifier, parquet=FALSE, verbose=TRUE),
      "Retrieved plot-observations record ob.1",
      fixed = TRUE)

    response <- vb_get_by_id(identifier, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("ob_code", "author_obs_code"),
      ignore.order = TRUE
    )
    expect_identical(response$ob_code, "ob.1")
    expect_identical(response$author_obs_code, "obs1")

    bad_identifier <- "bad_identifier"
    expect_error(
      bad_response <- vb_get_by_id(bad_identifier, parquet=FALSE),
      paste("Can't retrieve identifier \"bad_identifier\":",
            "Unknown resource type code \"xx\""),
      fixed = TRUE)

  })
})
