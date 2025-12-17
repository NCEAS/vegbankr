# End-to-end tests to be run against an actual instance of the API.
#
# This test suite verifies that higher level API wrapper functions work
# end-to-end as expected in terms of constructing a valid request,
# sending it to an API endpoint, receiving a response, and ultimately
# transforming data payloads into data frames as expected with respect
# to dimensionality and column names.

ENABLED <- FALSE

test_that("Getting projects works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_projects("pj.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
    "last_plot_added_date",
    "obs_count",
    "pj_code",
    "project_description",
    "project_name",
    "start_date",
    "stop_date"
  )
  one <- vb_get_projects("pj.10508")
  expect_identical(nrow(one), 1L)
  expect_named(one, names, ignore.order = TRUE)
  all <- vb_get_projects(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting parties works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_parties("py.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
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
  one <- vb_get_parties("py.191378")
  expect_identical(nrow(one), 1L)
  expect_named(one, names, ignore.order = TRUE)
  all <- vb_get_parties(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting references works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_references("rf.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
    "rf_label",
    "short_name",
    "title",
    "total_pages",
    "url"
  )
  one <- vb_get_references("rf.1")
  expect_identical(nrow(one), 1L)
  expect_named(one, names, ignore.order = TRUE)
  all <- vb_get_references(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting cover methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_cover_methods("cm.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
    "cm_code",
    "cover_estimation_method",
    "cover_type",
    "rf_code",
    "rf_label"
  )
  names_nest <- c(
    names,
    "cover_indexes"
  )
  one <- vb_get_cover_methods("cm.1")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_nest, ignore.order = TRUE)
  all <- vb_get_cover_methods(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting stratum methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_stratum_methods("sm.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
    "rf_code",
    "rf_label",
    "sm_code",
    "stratum_assignment",
    "stratum_method_description",
    "stratum_method_name"
  )
  names_nest <- c(
    names,
    "stratum_types"
  )
  one <- vb_get_stratum_methods("sm.1")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_nest, ignore.order = TRUE)
  all <- vb_get_stratum_methods(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting plant concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  expect_message(
    zero_records <- vb_get_plant_concepts("pc.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
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
  names_nest <- c(
    names,
    "children",
    "correlations",
    "usages"
  )
  one <- vb_get_plant_concepts("pc.193")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_nest, ignore.order = TRUE)
  all <- vb_get_plant_concepts(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)

})

test_that("Getting taxon observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_taxon_observations("to.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
  names_nest <- c(
    names,
    "taxon_importance"
  )
  one <- vb_get_taxon_observations("to.693826")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_nest, ignore.order = TRUE)
  all <- vb_get_taxon_observations(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting taxon interpretations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_taxon_interpretations("ti.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
  one <- vb_get_taxon_interpretations("ti.64550")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_full, ignore.order = TRUE)
  all <- vb_get_taxon_interpretations(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names_mini, ignore.order = TRUE)
})

test_that("Getting community concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_community_concepts("cc.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
  names <- c(
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
  names_nest <- c(
    names,
    "children",
    "correlations",
    "usages"
  )
  one <- vb_get_community_concepts("cc.30617")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_nest, ignore.order = TRUE)
  all <- vb_get_community_concepts(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names, ignore.order = TRUE)
})

test_that("Getting community classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_community_classifications("cl.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
  names_full_nest <- c(
    names_full,
    "contributors",
    "interpretations"
  )
  one <- vb_get_community_classifications("cl.34809")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_full_nest, ignore.order = TRUE)
  all <- vb_get_community_classifications(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names_mini, ignore.order = TRUE)
})

test_that("Getting community interpretations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_community_interpretations("ci.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
  names_full_nest <- c(
    names_full,
    "class_contributors"
  )
  one <- vb_get_community_interpretations("ci.297")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_full_nest, ignore.order = TRUE)
  all <- vb_get_community_interpretations(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names_mini, ignore.order = TRUE)
})

test_that("Getting plot observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)
  expect_message(
    zero_records <- vb_get_plot_observations("ob.0"),
    "No records returned",
    fixed = TRUE)
  expect_identical(nrow(zero_records), 0L)
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
  names_full_nest <- c(
    names_full,
    "taxon_count",
    "taxon_importance_count",
    "taxon_importance_count_returned",
    "top_classifications",
    "top_taxon_observations"
  )
  one <- vb_get_plot_observations("ob.41618")
  expect_identical(nrow(one), 1L)
  expect_named(one, names_full_nest, ignore.order = TRUE)
  all <- vb_get_plot_observations(limit = 5)
  expect_identical(nrow(all), 5L)
  expect_named(all, names_mini, ignore.order = TRUE)
})
