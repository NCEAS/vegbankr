with_mock_api({
  test_that("vb_get_community_classifications() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-classifications"
    expect_GET(
      vb_get_community_classifications("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_community_classifications(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_community_classifications(limit=5, offset=10),
      paste0(endpoint, "?limit=5&offset=10")
    )

    response <- vb_get_community_classifications("cl.34809",
      limit=NULL, offset=NULL, detail=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("cl_code", "class_confidence", "class_fit", "class_notes",
        "class_publication_id", "class_start_date", "class_stop_date",
        "cc_code", "comm_code", "comm_framework", "ci_code",
        "comm_level", "comm_name", "expert_system", "inspection",
        "multivariate_analysis", "nomenclatural_type", "notes",
        "ob_code", "table_analysis", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$cl_code, "cl.34809")
    expect_identical(response$class_publication_id, 50596L)
    expect_identical(response$multivariate_analysis, FALSE)
    expect_identical(response$class_confidence, NA)

    expect_message(
      zero_records <- vb_get_community_classifications(limit=0,
        parquet=FALSE, detail=NULL, with_nested=NULL),
      "No records returned",
       fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_community_classifications(limit=2, parquet=FALSE,
      detail=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("cl_code", "cc_code", "comm_name", "ob_code"),
      ignore.order = TRUE
    )
    expect_identical(response$cl_code[2], "cl.1554")
    expect_identical(response$comm_name[2], "Typha")
  })
})
