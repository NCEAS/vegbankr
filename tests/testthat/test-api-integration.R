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
    request(vb_get_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(limit = "-1") |>
      send(),
    "When provided, limit must be a non-negative integer.")
  expect_error(
    request(vb_get_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(limit = "foo") |>
      send(),
    "When provided, limit must be a non-negative integer.")
}

# Test invalid offset parameter
test_error_offset <- function(resource) {
  expect_error(
    request(vb_get_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(offset = "-1") |>
      send(),
    "When provided, offset must be a non-negative integer.")
  expect_error(
    request(vb_get_base_url()) |>
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
      request(vb_get_base_url()) |>
        req_url_path_append(resource) |>
        req_url_query(detail = "minimal") |>
        send(),
      error_msg)
  } else {
    error_msg <- "When provided, 'detail' must be 'minimal' or 'full'."
  }
  expect_error(
    resp <- request(vb_get_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(detail = "foo") |>
      send(),
    error_msg)
}

# Test invalid vb code
test_error_vb_code <- function(resource, table_code) {
  error_msg <- paste("Invalid", table_code, "code 'foo.123'.")
  expect_error(
    request(vb_get_base_url()) |>
      req_url_path_append(resource) |>
      req_url_path_append("foo.123") |>
      send(),
    error_msg)
}

# Test successful query by code, returning JSON
test_success_one_json <- function(resource, vb_code, names, n=1,
                                  with_nested = NULL) {
  df <- request(vb_get_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(vb_code) |>
    req_url_query(with_nested = with_nested) |>
    send() |>
    vb_df_from_json(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), n)
  expect_named(df, names, ignore.order = TRUE)
  expect_equal(get_page_details(df)[["count_reported"]], 1)
}

# Test successful query by code, returning Parquet
test_success_one_parquet <- function(resource, vb_code, names, n=1,
                                     with_nested = NULL) {
  df <- request(vb_get_base_url()) |>
    req_url_path_append(resource) |>
    req_url_path_append(vb_code) |>
    req_url_query(with_nested = with_nested) |>
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
  df <- request(vb_get_base_url()) |>
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
  df <- request(vb_get_base_url()) |>
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

# Test cross-resource queries
test_cross_resource <- function(target_resource, scoping_resource,
                                scoping_code) {
  json <- request(vb_get_base_url()) |>
    req_url_path_append(scoping_resource) |>
    req_url_path_append(scoping_code) |>
    req_url_path_append(target_resource) |>
    req_url_query(count = TRUE) |>
    req_headers(Accept = "application/json") |>
    send() |>
    resp_body_json()
  expect_named(json, "count")
  expect_gt(json$count, 0)
}

#
# Now define actual tests
#

test_that("projects works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "projects"
  table_code <- "pj"
  vb_code <- "pj.9300"
  names_full <- c(
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
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
})

test_that("parties works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "parties"
  table_code <- "py"
  vb_code <- "py.11"
  names_full <- c(
    "contact_instructions",
    "given_name",
    "middle_name",
    "obs_count",
    "organization_name",
    "party_label",
    "py_code",
    "salutation",
    "surname"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_cross_resource(resource, "plot-observations", "ob.109863")
  test_cross_resource(resource, "community-classifications", "cl.112850")
  test_cross_resource(resource, "projects", "pj.11057")
})

test_that("references works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "references"
  table_code <- "rf"
  vb_code <- "rf.1"
  names_full <- c(
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
    "rf_label",
    "short_name",
    "title",
    "total_pages",
    "url"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
})

test_that("cover-methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "cover-methods"
  table_code <- "cm"
  vb_code <- "cm.1"
  names_full <- c(
    "cm_code",
    "cover_estimation_method",
    "cover_type",
    "rf_code",
    "rf_label"
  )
  names_full_nest <- c(
    names_full,
    "cover_indexes"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
})

test_that("stratum-methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "stratum-methods"
  table_code <- "sm"
  vb_code <- "sm.622"
  names_full <- c(
    "rf_code",
    "rf_label",
    "sm_code",
    "stratum_assignment",
    "stratum_method_description",
    "stratum_method_name"
  )
  names_full_nest <- c(
    names_full,
    "stratum_types"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
})

test_that("plant-concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "plant-concepts"
  table_code <- "pc"
  vb_code <- "pc.193"
  names_full <- c(
    "concept_rf_code",
    "concept_rf_label",
    "current_accepted",
    "obs_count",
    "parent_name",
    "parent_pc_code",
    "party_label",
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
    "status_rf_label",
    "stop_date"
  )
  names_full_nest <- c(
    names_full,
    "children",
    "correlations",
    "usages"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_cross_resource(resource, "plot-observations", "ob.134668")
})

test_that("taxon-observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "taxon-observations"
  table_code <- "to"
  vb_code <- "to.587096"
  names_full <- c(
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
  names_full_nest <- c(
    names_full,
    "taxon_importance"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_cross_resource(resource, "plot-observations", "ob.83802")
  test_cross_resource(resource, "plant-concepts", "pc.110944")
})

test_that("taxon-interpretations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "taxon-interpretations"
  table_code <- "ti"
  vb_code <- "ti.587463"
  names_mini <- c(
    "collection_number",
    "group_type",
    "interpretation_date",
    "interpretation_type",
    "is_curr",
    "is_orig",
    "notes",
    "notes_mgt",
    "notes_public",
    "pc_code",
    "py_code",
    "rf_code",
    "taxon_confidence",
    "taxon_fit",
    "ti_code",
    "to_code"
  )
  names_full <- c(
    names_mini,
    "author_obs_code",
    "author_plant_name",
    "ob_code",
    "party_label",
    "rf_label",
    "plant_code",
    "plant_label",
    "plant_name",
    "revisions",
    "role"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_mini, detail="minimal")
  test_cross_resource(resource, "taxon-observations", "to.587096")
  test_cross_resource(resource, "plot-observations", "ob.134668")
  test_cross_resource(resource, "plant-concepts", "pc.110944")
})

test_that("community-concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-concepts"
  table_code <- "cc"
  vb_code <- "cc.1324"
  names_full <- c(
    "concept_rf_code",
    "concept_rf_label",
    "current_accepted",
    "obs_count",
    "parent_name",
    "parent_cc_code",
    "party_label",
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
    "status_rf_label",
    "stop_date"
  )
  names_full_nest <- c(
    names_full,
    "children",
    "correlations",
    "usages"
  )

  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_cross_resource(resource, "plot-observations", "ob.134668")
})

test_that("community-classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-classifications"
  table_code <- "cl"
  vb_code <- "cl.1553"
  names_mini <- c(
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
  names_full <- c(
    names_mini,
    "author_obs_code"
  )
  names_mini_nest <- c(
    names_mini,
    "contributors",
    "interpretations"
  )
  names_full_nest <- c(
    names_full,
    "contributors",
    "interpretations"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_mini, detail="minimal")
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_success_collection_parquet(resource, names_mini_nest,
                                  detail="minimal", with_nested=TRUE)
  test_cross_resource(resource, "plot-observations", "ob.134668")
  test_cross_resource(resource, "community-concepts", "cc.38712")
})

test_that("community-interpretations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-interpretations"
  table_code <- "ci"
  vb_code <- "ci.297"
  names_mini <- c(
    "cc_code",
    "ci_code",
    "cl_code",
    "class_confidence",
    "class_fit",
    "comm_authority_rf_code",
    "nomenclatural_type",
    "notes",
    "type"
  )
  names_full <- c(
    names_mini,
    "author_obs_code",
    "class_notes",
    "class_start_date",
    "class_stop_date",
    "comm_authority_rf_label",
    "comm_code",
    "comm_framework",
    "comm_label",
    "comm_level",
    "comm_name",
    "expert_system",
    "inspection",
    "multivariate_analysis",
    "ob_code",
    "table_analysis"
  )
  names_mini_nest <- c(
    names_mini
  )
  names_full_nest <- c(
    names_full,
    "class_contributors"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_mini, detail="minimal")
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_success_collection_parquet(resource, names_mini_nest,
                                  detail="minimal", with_nested=TRUE)
  test_cross_resource(resource, "community-classifications", "cl.1553")
  test_cross_resource(resource, "plot-observations", "ob.134668")
  test_cross_resource(resource, "community-concepts", "cc.38712")
})

test_that("plot-observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "plot-observations"
  table_code <- "ob"
  vb_code <- "ob.2948"
  names_mini <- c(
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
  names_full <- c(
    names_mini,
    "author_datum",
    "author_e",
    "author_location",
    "author_n",
    "author_zone",
    "auto_taxon_cover",
    "azimuth",
    "basal_area",
    "bryophyte_quality",
    "cm_code",
    "confidentiality_status",
    "cover_dispersion",
    "cover_method_name",
    "date_accuracy",
    "date_entered",
    "dominant_stratum",
    "dsg_poly",
    "effort_level",
    "elevation_accuracy",
    "elevation_range",
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
    "layout_narrative",
    "lichen_quality",
    "location_accuracy",
    "location_narrative",
    "max_slope_aspect",
    "max_slope_gradient",
    "method_narrative",
    "min_slope_aspect",
    "min_slope_gradient",
    "name_other",
    "nonvascular_cover",
    "nonvascular_ht",
    "number_of_taxa",
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
    "placement_method",
    "pl_notes_mgt",
    "pl_notes_public",
    "pl_revisions",
    "plot_validation_level",
    "previous_ob_code",
    "project_name",
    "representativeness",
    "rf_code",
    "rf_label",
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
    "water_salinity"
  )
  names_mini_nest <- c(
    names_mini,
    "taxon_count",
    "taxon_count_returned",
    "top_classifications",
    "top_taxon_observations"
  )
  names_full_nest <- c(
    names_full,
    "taxon_count",
    "taxon_importance_count",
    "taxon_importance_count_returned",
    "top_classifications",
    "top_taxon_observations"
  )
  names_geo <- c(
    "author_obs_code",
    "latitude",
    "longitude",
    "ob_code"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource,
      msg = "When provided, 'detail' must be 'minimal', 'full', or 'geo'.")
  test_error_vb_code(resource, table_code)
  test_success_one_json(resource, vb_code, names_full)
  test_success_one_parquet(resource, vb_code, names_full)
  test_success_collection_json(resource, names_full)
  test_success_collection_parquet(resource, names_full)
  test_success_collection_parquet(resource, names_mini, detail="minimal")
  test_success_collection_parquet(resource, names_full_nest, with_nested=TRUE)
  test_success_collection_parquet(resource, names_mini_nest,
                                  detail="minimal", with_nested=TRUE)
  test_success_collection_parquet(resource, names_geo, detail="geo")
  test_cross_resource(resource, "projects", "pj.340")
  test_cross_resource(resource, "parties", "py.192036")
  test_cross_resource(resource, "plant-concepts", "pc.110944")
  test_cross_resource(resource, "community-concepts", "cc.24417")
  test_cross_resource(resource, "cover-methods", "cm.1")
  test_cross_resource(resource, "stratum-methods", "sm.1")
})
