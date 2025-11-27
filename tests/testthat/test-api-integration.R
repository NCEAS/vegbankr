# Integration tests to be run against an actual instance of the API.
#
# Rather than using the higher level vegbankr wrapper functions, this
# test suite constructs requests using low level httr2 functions, and
# only uses internal vegbankr utility functions to sanely handle the
# API responses to convert data payloads into data frames, print
# information messages, and/or raise errors when applicable.

ENABLED <- FALSE

#
# First define repeatable test wrappers
#

# Test invalid limit parameter
test_error_limit <- function(resource) {
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(limit = "-1") |>
      send(),
    "When provided, limit must be a non-negative integer.")
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(limit = "foo") |>
      send(),
    "When provided, limit must be a non-negative integer.")
}

# Test invalid offset parameter
test_error_offset <- function(resource) {
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(offset = "-1") |>
      send(),
    "When provided, offset must be a non-negative integer.")
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(offset = "foo") |>
      send(),
    "When provided, offset must be a non-negative integer.")
}

# Test invalid detail parameter
test_error_detail <- function(resource, must_be_full = TRUE, msg = NULL) {
  if (!is.null(msg)) {
    error_msg = msg
  } else if (must_be_full) {
    error_msg <- "When provided, 'detail' must be 'full'"
    expect_error(
      request(get_vb_base_url()) |>
        req_url_path_append(resource) |>
        req_url_query(detail = "minimal") |>
        send(),
      error_msg)
  } else {
    error_msg <- "When provided, 'detail' must be 'minimal' or 'full'."
  }
  expect_error(
    resp <- request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(detail = "foo") |>
      send(),
    error_msg)
}

# Test invalid vb code
test_error_vb_code <- function(resource, table_code) {
  error_msg <- paste("Invalid", table_code, "code 'foo.123'.")
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_path_append("foo.123") |>
      send(),
    error_msg)
}

# Test successful query by code, returning JSON
test_success_one_json <- function(resource, vb_code, names, n=1) {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(vb_code) |>
    send() |>
    vb_df_from_json(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), n)
  expect_named(df, names, ignore.order = TRUE)
  expect_equal(get_page_details(df)[["count_reported"]], 1)
}

# Test successful query by code, returning Parquet
test_success_one_parquet <- function(resource, vb_code, names, n=1) {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(vb_code) |>
    req_url_query(create_parquet = TRUE) |>
    send() |>
    vb_df_from_parquet(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), n)
  expect_named(df, names, ignore.order = TRUE)
  expect_equal(get_page_details(df)[["count_reported"]], 1)
}

# Test successful collection query, returning JSON
test_success_collection_json <- function(resource, names, limit = 10,
                                         offset = 5, detail = "full",
                                         with_nested = NULL) {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_query(limit = limit) |>
    req_url_query(offset = offset) |>
    req_url_query(detail = detail) |>
    req_url_query(with_nested = with_nested) |>
    send() |>
    vb_df_from_json(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), limit %||% 1000)
  expect_named(df, names, ignore.order = TRUE)
}

# Test successful collection query, returning Parquet
test_success_collection_parquet <- function(resource, names, limit = 10,
                                            offset = 5, detail = NULL,
                                            with_nested = NULL) {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_query(create_parquet = TRUE) |>
    req_url_query(limit = limit) |>
    req_url_query(offset = offset) |>
    req_url_query(detail = detail) |>
    req_url_query(with_nested = with_nested) |>
    send() |>
    vb_df_from_parquet(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10)
  expect_named(df, names, ignore.order = TRUE)
}

#
# Now define actual tests
#

test_that("plot-observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "plot-observations"
  table_code <- "ob"
  vb_code <- "ob.2948"
  names_one <- c(
    "area",
    "author_datum",
    "author_e",
    "author_location",
    "author_n",
    "author_obs_code",
    "author_plot_code",
    "author_zone",
    "auto_taxon_cover",
    "azimuth",
    "basal_area",
    "bryophyte_quality",
    "cm_code",
    "confidentiality_reason",
    "confidentiality_status",
    "country",
    "cover_dispersion",
    "date_accuracy",
    "date_entered",
    "dominant_stratum",
    "dsg_poly",
    "effort_level",
    "elevation",
    "elevation_accuracy",
    "elevation_range",
    "emb_observation",
    "field_cover",
    "field_ht",
    "floating_cover",
    "floristic_quality",
    "growthform_1_cover",
    "growthform_1_type",
    "growthform_2_cover",
    "growthform_2_type",
    "growthform_3_cover",
    "growthform_3_type",
    "has_observation_synonym",
    "homogeneity",
    "hydrologic_regime",
    "interp_bestfit_cc_code",
    "interp_bestfit_ci_code",
    "interp_bestfit_code",
    "interp_bestfit_partyname",
    "interp_bestfit_py_code",
    "interp_bestfit_sciname",
    "interp_current_cc_code",
    "interp_current_ci_code",
    "interp_current_code",
    "interp_current_partyname",
    "interp_current_py_code",
    "interp_current_sciname",
    "interp_orig_cc_code",
    "interp_orig_ci_code",
    "interp_orig_code",
    "interp_orig_partyname",
    "interp_orig_py_code",
    "interp_orig_sciname",
    "landform",
    "landscape_narrative",
    "latitude",
    "layout_narrative",
    "lichen_quality",
    "location_accuracy",
    "location_narrative",
    "longitude",
    "max_slope_aspect",
    "max_slope_gradient",
    "method_narrative",
    "min_slope_aspect",
    "min_slope_gradient",
    "name_other",
    "nonvascular_cover",
    "nonvascular_ht",
    "number_of_taxa",
    "ob_code",
    "obs_end_date",
    "ob_notes_mgt",
    "ob_notes_public",
    "ob_revisions",
    "obs_start_date",
    "observation_narrative",
    "organic_depth",
    "original_data",
    "parent_pl_code",
    "percent_bare_soil",
    "percent_bed_rock",
    "percent_litter",
    "percent_other",
    "percent_rock_gravel",
    "percent_water",
    "percent_wood",
    "permanence",
    "phenologic_aspect",
    "pj_code",
    "pl_code",
    "placement_method",
    "pl_notes_mgt",
    "pl_notes_public",
    "pl_revisions",
    "plot_validation_level",
    "previous_ob_code",
    "representativeness",
    "rf_code",
    "rock_type",
    "shape",
    "shore_distance",
    "shrub_cover",
    "shrub_ht",
    "slope_aspect",
    "slope_gradient",
    "sm_code",
    "soil_depth",
    "soil_drainage",
    "soil_moisture_regime",
    "soil_taxon_src",
    "st_code",
    "stratum_assignment",
    "stratum_method_description",
    "stratum_method_name",
    "stand_maturity",
    "stand_size",
    "state_province",
    "stem_observation_area",
    "stem_sample_method",
    "stem_size_limit",
    "submerged_cover",
    "submerged_ht",
    "successional_status",
    "surficial_deposits",
    "taxon_observation_area",
    "top_taxon1_name",
    "top_taxon2_name",
    "top_taxon3_name",
    "top_taxon4_name",
    "top_taxon5_name",
    "topo_position",
    "total_cover",
    "tree_cover",
    "tree_ht",
    "water_depth",
    "water_salinity",
    "year"
  )
  names_collection <- names_one
  names_coll_min <- c(
    "area",
    "author_obs_code",
    "author_plot_code",
    "country",
    "elevation",
    "latitude",
    "longitude",
    "ob_code",
    "pl_code",
    "state_province",
    "year"
  )
  names_coll_nested <- c(names_collection, c("taxon_count",
                                             "taxon_importance_count",
                                             "taxon_importance_count_returned",
                                             "top_classifications",
                                             "top_taxon_observations"))
  names_coll_min_nested <- c(names_coll_min, c("taxon_count",
                                               "taxon_count_returned",
                                               "top_classifications",
                                               "top_taxon_observations"))
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource,
      msg = "When provided, 'detail' must be 'minimal', 'full', or 'geo'.")
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_one)
  test_success_one_parquet(resource, vb_code, names_one)
  test_success_collection_json(resource, names_collection)
  test_success_collection_parquet(resource, names_collection)
  test_success_collection_json(resource, names_coll_min, detail = "minimal")
  test_success_collection_parquet(resource, names_coll_min, detail = "minimal")
  test_success_collection_json(resource, names_coll_nested,
                               detail="full", with_nested = TRUE)
  test_success_collection_parquet(resource, names_coll_nested,
                                  detail="full", with_nested = TRUE)
  test_success_collection_json(resource, names_coll_min_nested,
                               detail="minimal", with_nested = TRUE)
  test_success_collection_parquet(resource, names_coll_min_nested,
                                  detail="minimal", with_nested = TRUE)
})

test_that("taxon-observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "taxon-observations"
  table_code <- "to"
  vb_code <- "to.587096"
  names <- c(
    "author_plant_name",
    "int_curr_pc_code",
    "int_curr_plant_code",
    "int_curr_plant_common",
    "int_curr_plant_sci_full",
    "int_curr_plant_sci_name_no_auth",
    "int_orig_pc_code",
    "int_orig_plant_code",
    "int_orig_plant_common",
    "int_orig_plant_sci_full",
    "int_orig_plant_sci_name_no_auth",
    "ob_code",
    "rf_code",
    "rf_label",
    "taxon_inference_area",
    "to_code"
  )
  nested_field_names <- c("taxon_importance")
  names_nested <- c(names, nested_field_names)
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
  test_success_collection_json(resource, names_nested, with_nested = TRUE)
  test_success_collection_parquet(resource, names_nested, with_nested = TRUE)
})

test_that("community-classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-classifications"
  table_code <- "cl"
  vb_code <- "cl.1553"
  names_one <- c(
    "cl_code",
    "class_notes",
    "class_publication_rf_code",
    "class_publication_rf_label",
    "class_start_date",
    "class_stop_date",
    "expert_system",
    "inspection",
    "multivariate_analysis",
    "ob_code",
    "table_analysis"
  )
  names_coll <- names_one
  nested_field_names <- c("interpretations", "contributors")
  names_coll_nested <- c(names_coll, nested_field_names)
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_one)
  test_success_one_parquet(resource, vb_code, names_one)
  test_success_collection_json(resource, names_coll)
  test_success_collection_parquet(resource, names_coll)
  test_success_collection_json(resource, names_coll_nested, with_nested = TRUE)
  test_success_collection_parquet(resource, names_coll_nested, with_nested = TRUE)
})

test_that("community-concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-concepts"
  table_code <- "cc"
  vb_code <- "cc.1324"
  names_one <- c(
    "children",
    "concept_rf_code",
    "concept_rf_name",
    "correlations",
    "current_accepted",
    "obs_count",
    "parent_name",
    "parent_cc_code",
    "party",
    "cc_code",
    "comm_code",
    "comm_description",
    "comm_level",
    "comm_name",
    "comm_party_comments",
    "py_code",
    "start_date",
    "status",
    "status_rf_code",
    "status_rf_name",
    "stop_date",
    "usages"
  )
  names_collection <- names_one
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_one, n=1)
  test_success_one_parquet(resource, vb_code, names_one, n=1)
  test_success_collection_json(resource, names_collection)
  test_success_collection_parquet(resource, names_collection)
})

test_that("plant-concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "plant-concepts"
  table_code <- "pc"
  vb_code <- "pc.193"
  names <- c(
    "children",
    "concept_rf_code",
    "concept_rf_name",
    "correlations",
    "current_accepted",
    "obs_count",
    "parent_name",
    "parent_pc_code",
    "party",
    "pc_code",
    "plant_code",
    "plant_description",
    "plant_level",
    "plant_name",
    "plant_party_comments",
    "py_code",
    "start_date",
    "status",
    "status_rf_code",
    "status_rf_name",
    "stop_date",
    "usages"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("parties works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "parties"
  table_code <- "py"
  vb_code <- "py.11"
  names <- c(
    "contact_instructions",
    "given_name",
    "middle_name",
    "obs_count",
    "organization_name",
    "py_code",
    "salutation",
    "surname"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("projects works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "projects"
  table_code <- "pj"
  vb_code <- "pj.9300"
  names <- c(
    "last_plot_added_date",
    "obs_count",
    "pj_code",
    "project_description",
    "project_name",
    "start_date",
    "stop_date"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("cover-methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "cover-methods"
  table_code <- "cm"
  vb_code <- "cm.1"
  names <- c(
    "cm_code",
    "cover_estimation_method",
    "cover_type",
    "rf_code",
    "rf_name",
    "cover_indexes"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names, n=1)
  test_success_one_parquet(resource, vb_code, names, n=1)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("stratum-methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "stratum-methods"
  table_code <- "sm"
  vb_code <- "sm.622"
  names <- c(
    "rf_code",
    "rf_name",
    "sm_code",
    "stratum_assignment",
    "stratum_method_description",
    "stratum_method_name",
    "stratum_types"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names, n=1)
  test_success_one_parquet(resource, vb_code, names, n=1)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("references works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "references"
  table_code <- "rf"
  vb_code <- "rf.1"
  names <- c(
    "degree",
    "doi",
    "full_citation",
    "isbn",
    "journal",
    "publication_date",
    "publication_place",
    "publisher",
    "reference_type",
    "rf_code",
    "short_name",
    "title",
    "total_pages",
    "url"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})
