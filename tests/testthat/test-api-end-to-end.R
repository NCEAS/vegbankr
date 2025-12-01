# End-to-end tests to be run against an actual instance of the API.
#
# This test suite verifies that higher level API wrapper functions work
# end-to-end as expected in terms of constructing a valid request,
# sending it to an API endpoint, receiving a response, and ultimately
# transforming data payloads into data frames as expected with respect
# to dimensionality and column names.

ENABLED <- FALSE

test_that("Getting plot observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    obs_zero <- get_plot_observation("ob.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(obs_zero), 0L)

  names_obs_one <- c(
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
  names_obs_all_full <- names_obs_one
  names_obs_all_minimal <- c(
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

  obs_one <- get_plot_observation("ob.41618", with_nested=FALSE)
  expect_identical(nrow(obs_one), 1L)
  expect_named(obs_one, names_obs_one, ignore.order = TRUE)

  obs_all_full <- get_all_plot_observations(detail = "full", limit = 5)
  expect_identical(nrow(obs_all_full), 5L)
  expect_named(obs_all_full, names_obs_all_full, ignore.order = TRUE)

  obs_all_minimal <- get_all_plot_observations(detail = "minimal", limit = 5)
  expect_identical(nrow(obs_all_minimal), 5L)
  expect_named(obs_all_minimal, names_obs_all_minimal, ignore.order = TRUE)

})

test_that("Getting taxon observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    txo_zero <- get_taxon_observation("to.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(txo_zero), 0L)

  names_txo_one <- c(
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
  names_txo_all_full <- names_txo_one
  nested_field_names <- c("taxon_importance")
  names_txo_all_full_nested <- c(names_txo_all_full, nested_field_names)

  txo_one <- get_taxon_observation("to.693826", with_nested=FALSE)
  expect_identical(nrow(txo_one), 1L)
  expect_named(txo_one, names_txo_one, ignore.order = TRUE)

  txo_all_full <- get_all_taxon_observations(limit = 5)
  expect_identical(nrow(txo_all_full), 5L)
  expect_named(txo_all_full, names_txo_all_full, ignore.order = TRUE)

  txo_all_full_nested <- get_all_taxon_observations(detail = "full",
      with_nested = TRUE, limit = 5)
  expect_identical(nrow(txo_all_full_nested), 5L)
  expect_named(txo_all_full_nested, names_txo_all_full_nested, ignore.order = TRUE)

})

test_that("Getting community classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    cl_zero <- get_community_classification("cl.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(cl_zero), 0L)

  names_cl_one <- c(
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
  names_cl_all_full <- names_cl_one
  names_cl_all_minimal <- names_cl_all_full
  nested_field_names <- c("interpretations", "contributors")
  names_cl_all_full_nested <- c(names_cl_all_full, nested_field_names)
  names_cl_all_minimal_nested <- names_cl_all_full_nested

  cl_one <- get_community_classification("cl.34809", with_nested=FALSE)
  expect_identical(nrow(cl_one), 1L)
  expect_named(cl_one, names_cl_one, ignore.order = TRUE)

  cl_all_full <- get_all_community_classifications(detail = "full", limit = 5)
  expect_identical(nrow(cl_all_full), 5L)
  expect_named(cl_all_full, names_cl_all_full, ignore.order = TRUE)

  cl_all_full_nested <- get_all_community_classifications(detail = "full",
      with_nested = TRUE, limit = 5)
  expect_identical(nrow(cl_all_full_nested), 5L)
  expect_named(cl_all_full_nested, names_cl_all_full_nested, ignore.order = TRUE)

  cl_all_minimal <- get_all_community_classifications(detail = "minimal", limit = 5)
  expect_identical(nrow(cl_all_minimal), 5L)
  expect_named(cl_all_minimal, names_cl_all_minimal, ignore.order = TRUE)

  cl_all_minimal_nested <- get_all_community_classifications(detail = "minimal",
      with_nested = TRUE, limit = 5)
  expect_identical(nrow(cl_all_minimal_nested), 5L)
  expect_named(cl_all_minimal_nested, names_cl_all_minimal_nested, ignore.order = TRUE)

})

test_that("Getting community concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    cc_zero <- get_community_concept("cc.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(cc_zero), 0L)

  names_cc_one <- c(
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
  names_cc_all <- names_cc_one

  cc_one <- get_community_concept("cc.30617")
  expect_identical(nrow(cc_one), 1L)
  expect_named(cc_one, names_cc_one, ignore.order = TRUE)

  cc_all <- get_all_community_concepts(limit = 5)
  expect_identical(nrow(cc_all), 5L)
  expect_named(cc_all, names_cc_all, ignore.order = TRUE)

})

test_that("Getting cover methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    cm_zero <- get_cover_method("cm.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(cm_zero), 0L)

  names_cm_one <- c(
    "cm_code",
    "cover_estimation_method",
    "cover_type",
    "rf_code",
    "rf_name",
    "cover_indexes"
  )
  names_cm_all <- names_cm_one

  cm_one <- get_cover_method("cm.1")
  expect_identical(nrow(cm_one), 1L)
  expect_named(cm_one, names_cm_one, ignore.order = TRUE)

  cm_all <- get_all_cover_methods(limit = 5)
  expect_identical(nrow(cm_all), 5L)
  expect_named(cm_all, names_cm_all, ignore.order = TRUE)

})

test_that("Getting stratum methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    sm_zero <- get_stratum_method("sm.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(sm_zero), 0L)

  names_sm_one <- c(
    "rf_code",
    "rf_name",
    "sm_code",
    "stratum_assignment",
    "stratum_method_description",
    "stratum_method_name",
    "stratum_types"
  )
  names_sm_all <- names_sm_one

  sm_one <- get_stratum_method("sm.1")
  expect_identical(nrow(sm_one), 1L)
  expect_named(sm_one, names_sm_one, ignore.order = TRUE)

  sm_all <- get_all_stratum_methods(limit = 5)
  expect_identical(nrow(sm_all), 5L)
  expect_named(sm_all, names_sm_all, ignore.order = TRUE)

})

test_that("Getting plant concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    pc_zero <- get_plant_concept("pc.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(pc_zero), 0L)

  names_pc_one <- c(
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
  names_pc_all <- names_pc_one

  pc_one <- get_plant_concept("pc.193")
  expect_identical(nrow(pc_one), 1L)
  expect_named(pc_one, names_pc_one, ignore.order = TRUE)

  pc_all <- get_all_plant_concepts(limit = 5)
  expect_identical(nrow(pc_all), 5L)
  expect_named(pc_all, names_pc_all, ignore.order = TRUE)

})

test_that("Getting parties works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    party_zero <- get_party("py.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(party_zero), 0L)

  names_party_one <- c(
    "contact_instructions",
    "given_name",
    "middle_name",
    "obs_count",
    "organization_name",
    "py_code",
    "salutation",
    "surname"
  )
  names_party_all <- names_party_one

  party_one <- get_party("py.191378")
  expect_identical(nrow(party_one), 1L)
  expect_named(party_one, names_party_one, ignore.order = TRUE)

  party_all <- get_all_parties(limit = 5)
  expect_identical(nrow(party_all), 5L)
  expect_named(party_all, names_party_all, ignore.order = TRUE)

})

test_that("Getting projects works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    proj_zero <- get_project("pj.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(proj_zero), 0L)

  names_proj_one <- c(
    "last_plot_added_date",
    "obs_count",
    "pj_code",
    "project_description",
    "project_name",
    "start_date",
    "stop_date"
  )
  names_proj_all <- names_proj_one

  proj_one <- get_project("pj.10508")
  expect_identical(nrow(proj_one), 1L)
  expect_named(proj_one, names_proj_one, ignore.order = TRUE)

  proj_all <- get_all_projects(limit = 5)
  expect_identical(nrow(proj_all), 5L)
  expect_named(proj_all, names_proj_all, ignore.order = TRUE)

})

test_that("Getting references works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    rf_zero <- get_reference("rf.0"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(rf_zero), 0L)

  names_rf_one <- c(
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
  names_rf_all <- names_rf_one

  rf_one <- get_reference("rf.1")
  expect_identical(nrow(rf_one), 1L)
  expect_named(rf_one, names_rf_one, ignore.order = TRUE)

  rf_all <- get_all_references(limit = 5)
  expect_identical(nrow(rf_all), 5L)
  expect_named(rf_all, names_rf_all, ignore.order = TRUE)

})
