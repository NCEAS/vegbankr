with_mock_api({
  test_that("get_community_classification() works", {
    local_base_url(NULL)

    expect_GET(
      get_community_classification("some_accession_code"),
      "https://api.vegbank.org/community-classifications/some_accession_code"
    )

    response <- get_community_classification("VB.Cl.34809.2HZMTQQVR2GWE9P")
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("accession_code", "class_confidence", "class_fit",
        "class_notes", "class_publication_id", "class_start_date",
        "class_stop_date", "comm_authority_id", "comm_class_id",
        "comm_code", "comm_concept_id", "comm_framework",
        "comm_interpretation_id", "comm_level", "comm_name",
        "emb_comm_class", "emb_comm_interpretation", "expert_system",
        "inspection", "multivariate_analysis", "nomenclatural_type",
        "notes", "observation_id", "table_analysis", "type"),
      ignore.order = TRUE
    )
    expect_identical(response$accession_code,
                     "VB.Cl.34809.2HZMTQQVR2GWE9P")
    expect_identical(response$emb_comm_class,
                     0L)
    expect_identical(response$multivariate_analysis,
                     FALSE)
    expect_identical(response$class_confidence,
                     NA)
  })
})
