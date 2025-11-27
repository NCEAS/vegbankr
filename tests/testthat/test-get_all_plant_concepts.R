with_mock_api({
  test_that("get_all_plant_concepts() works", {
    local_base_url(NULL)

    endpoint <- "https://api.vegbank.org/plant-concepts"
    expect_GET(
      get_all_plant_concepts(),
      paste0(endpoint,
             "?limit=100",
             "&offset=0")
    )
    expect_GET(
      get_all_plant_concepts(limit=5, offset=10),
      paste0(endpoint,
             "?limit=5",
             "&offset=10")
    )

    response <- get_all_plant_concepts(limit=2, parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 2L)
    expect_named(
      response,
      c("children", "concept_rf_code", "concept_rf_name", "correlations",
        "current_accepted", "obs_count", "parent_name", "parent_pc_code",
        "party", "pc_code", "plant_code", "plant_description", "plant_level",
        "plant_name", "plant_party_comments", "py_code", "start_date", "status",
        "status_rf_code", "status_rf_name", "stop_date", "usages"),
      ignore.order = TRUE
    )
    expect_identical(response$pc_code[2],
                     "pc.194")
    expect_identical(response$obs_count[2],
                     0L)
    expect_identical(response$plant_description[2],
                     NA)
    expect_identical(response$usages[[2]]$plant_name[1],
                     "CAPA57")
  })
})
