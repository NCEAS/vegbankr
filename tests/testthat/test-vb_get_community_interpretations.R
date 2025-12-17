with_mock_api({
  test_that("vb_get_community_interpretations() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/community-interpretations"
    expect_GET(
      vb_get_community_interpretations("some_vb_code"),
      paste0(endpoint, "/some_vb_code")
    )
    expect_GET(
      vb_get_community_interpretations(),
      paste0(endpoint, "?limit=100&offset=0")
    )
    expect_GET(
      vb_get_community_interpretations(limit=5),
      paste0(endpoint, "?limit=5&offset=0")
    )

    expect_message(
      zero_records <- vb_get_community_interpretations("zero_records",
        parquet=FALSE, limit=NULL, offset=NULL, detail=NULL, with_nested=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_community_interpretations("ci.297", limit=NULL,
      offset=NULL, detail=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("author_obs_code", "cc_code", "ci_code", "cl_code",
        "class_confidence", "class_fit", "class_notes",
        "class_start_date", "class_stop_date", "comm_authority_name",
        "comm_authority_rf_code", "comm_code", "comm_framework",
        "comm_label", "comm_level", "comm_name", "expert_system",
        "inspection", "multivariate_analysis", "nomenclatural_type",
        "notes", "ob_code", "table_analysis", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$ci_code, "ci.297")
    expect_identical(response$author_obs_code, "ACAD.143")
    expect_identical(response$nomenclatural_type, FALSE)

    expect_message(
      zero_records <- vb_get_community_interpretations(limit=0, parquet=FALSE,
        detail=NULL, with_nested=NULL),
      "No records returned",
      fixed = TRUE
    )
    expect_s3_class(zero_records, "data.frame")
    expect_identical(nrow(zero_records), 0L)

    response <- vb_get_community_interpretations(limit=2, parquet=FALSE,
      detail=NULL, with_nested=NULL)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("author_obs_code", "cc_code", "ci_code", "cl_code",
        "class_confidence", "class_fit", "class_notes",
        "class_start_date", "class_stop_date", "comm_authority_name",
        "comm_authority_rf_code", "comm_code", "comm_framework",
        "comm_label", "comm_level", "comm_name", "expert_system",
        "inspection", "multivariate_analysis", "nomenclatural_type",
        "notes", "ob_code", "table_analysis", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$ci_code[2], "ci.298")
    expect_identical(response$author_obs_code[2], "ACAD.144")
    expect_identical(response$nomenclatural_type[2], FALSE)
  })
})
