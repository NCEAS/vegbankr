with_mock_api({
  test_that("get_plant_concept() works", {
    local_base_url(NULL)

    expect_GET(
      get_plant_concept("some_pc_code"),
      "https://api.vegbank.org/plant-concepts/some_pc_code"
    )

    response <- get_plant_concept("pc.111478", parquet=FALSE)
    expect_s3_class(response, "data.frame")
    expect_identical(nrow(response), 1L)
    expect_named(
      response,
      c("children", "concept_rf_code", "concept_rf_name", "current_accepted",
        "obs_count", "parent_name", "parent_pc_code", "party", "pc_code",
        "plant_code", "plant_description", "plant_level", "plant_name",
        "plant_party_comments", "py_code", "start_date", "status",
        "status_rf_code", "status_rf_name", "stop_date", "usage_names",
        "usage_statuses", "children_list", "usage_names_list",
        "usage_statuses_list"),
      ignore.order = TRUE
    )
    expect_identical(response$pc_code,
                     "pc.111478")
    expect_identical(response$obs_count,
                     0L)
    expect_identical(response$plant_description,
                     NA)
    expect_identical(response$usage_names_list[[1]][["Code"]],
                     "ACRU")
  })
})
