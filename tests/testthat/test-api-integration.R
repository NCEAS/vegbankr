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
    "When provided, 'offset' and 'limit' must be non-negative integers.")
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(limit = "foo") |>
      send(),
    "When provided, 'offset' and 'limit' must be non-negative integers.")
}

# Test invalid offset parameter
test_error_offset <- function(resource) {
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(offset = "-1") |>
      send(),
    "When provided, 'offset' and 'limit' must be non-negative integers.")
  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append(resource) |>
      req_url_query(offset = "foo") |>
      send(),
    "When provided, 'offset' and 'limit' must be non-negative integers.")
}

# Test invalid detail parameter
test_error_detail <- function(resource, must_be_full = TRUE) {
  if (must_be_full) {
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
test_error_vb_code <- function(resource, operator) {
  error_msg <- paste("Invalid", operator, "code 'foo.123'.")
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
}

# Test successful collection query, returning JSON
test_success_collection_json <- function(resource, names, limit = 10,
                                         offset = 15, detail = "full") {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_query(limit = limit) |>
    req_url_query(offset = offset) |>
    req_url_query(detail = detail) |>
    send() |>
    vb_df_from_json(clean_names = FALSE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), limit %||% 1000)
  expect_named(df, names, ignore.order = TRUE)
}

# Test successful collection query, returning Parquet
test_success_collection_parquet <- function(resource, names, limit = 10,
                                            offset = 15, detail = NULL) {
  df <- request(get_vb_base_url()) |>
    req_url_path_append(resource) |>
    req_url_query(create_parquet = TRUE) |>
    req_url_query(limit = 10) |>
    req_url_query(offset = 15) |>
    req_url_query(detail = detail) |>
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
  operator <- "plot_observation"
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
    "water_salinity"
  )
  names_collection <- names_one
  names_coll_min <- c(
    "author_obs_code",
    "author_plot_code",
    "country",
    "latitude",
    "longitude",
    "ob_code",
    "pl_code",
    "state_province"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names_one)
  test_success_one_parquet(resource, vb_code, names_one)
  test_success_collection_json(resource, names_collection)
  test_success_collection_parquet(resource, names_collection)
  test_success_collection_json(resource, names_coll_min, detail = "minimal")
  test_success_collection_parquet(resource, names_coll_min, detail = "minimal")
})

test_that("taxon-observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "taxon-observations"
  operator <- "taxon_observation"
  vb_code <- "to.587096"
  names <- c(
    "authorplantname",
    "emb_taxonobservation",
    "int_curr_pc_code",
    "int_currplantcode",
    "int_currplantcommon",
    "int_currplantscifull",
    "int_currplantscinamenoauth",
    "int_orig_pc_code",
    "int_origplantcode",
    "int_origplantcommon",
    "int_origplantscifull",
    "int_origplantscinamenoauth",
    "maxcover",
    "ob_code",
    "rf_code",
    "taxoninferencearea",
    "to_code"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("community-classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-classifications"
  operator <- "community_classification"
  vb_code <- "cl.1553"
  names <- c(
    "cc_code",
    "cl_code",
    "class_confidence",
    "class_fit",
    "class_notes",
    "class_start_date",
    "class_stop_date",
    "comm_authority_rf_code",
    "comm_code",
    "comm_framework",
    "comm_level",
    "comm_name",
    "emb_comm_class",
    "emb_comm_interpretation",
    "expert_system",
    "inspection",
    "interpretation_nomenclatural_type",
    "interpretation_notes",
    "interpretation_type",
    "multivariate_analysis",
    "ob_code",
    "table_analysis"
  )
  names_coll_min <- c(
    "cc_code",
    "cl_code",
    "comm_name",
    "ob_code"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource, must_be_full = FALSE)
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
  test_success_collection_json(resource, names_coll_min, detail = "minimal")
  test_success_collection_parquet(resource, names_coll_min, detail = "minimal")
})

test_that("community-concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "community-concepts"
  operator <- "community_concept"
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
  test_error_vb_code(resource, operator)
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
  operator <- "plant_concept"
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
  test_error_vb_code(resource, operator)
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
  operator <- "party"
  vb_code <- "py.1"
  names <- c(
    "contact_instructions",
    "given_name",
    "middle_name",
    "organization_name",
    "py_code",
    "salutation",
    "surname"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, operator)
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
  operator <- "project"
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
  test_error_vb_code(resource, operator)
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
  operator <- "cover_method"
  vb_code <- "cm.1"
  names <- c(
    "cm_code",
    "cover_code",
    "cover_estimation_method",
    "cover_percent",
    "cover_type",
    "index_description",
    "lower_limit",
    "rf_code",
    "rf_name",
    "upper_limit"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names, n=40)
  test_success_one_parquet(resource, vb_code, names, n=40)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("stratum-methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "stratum-methods"
  operator <- "stratum_method"
  vb_code <- "sm.622"
  names <- c(
    "rf_code",
    "rf_name",
    "sm_code",
    "stratum_assignment",
    "stratum_description",
    "stratum_index",
    "stratum_method_description",
    "stratum_method_name",
    "stratum_name",
    "sy_code"
  )
  test_error_limit(resource)
  test_error_offset(resource)
  test_error_detail(resource)
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names, n=3)
  test_success_one_parquet(resource, vb_code, names, n=3)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})

test_that("references works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  resource <- "references"
  operator <- "reference"
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
  test_error_vb_code(resource, operator)
  test_success_one_json(resource, vb_code, names)
  test_success_one_parquet(resource, vb_code, names)
  test_success_collection_json(resource, names)
  test_success_collection_parquet(resource, names)
})
