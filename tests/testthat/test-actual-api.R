ENABLED <- FALSE

test_that("VegBank API error handling works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append("plot-observations") |>
      req_url_query(limit = "foo") |>
      send(),
    "When provided, 'offset' and 'limit' must be non-negative integers.")

  expect_error(
    get_all_resources("plot-observations", limit = "foo"),
    "limit must be a finite, non-negative integer")

  expect_error(
    request(get_vb_base_url()) |>
      req_url_path_append("plot-observations") |>
      req_url_query(limit = -1) |>
      send(),
    "No additional error details from server.")

  expect_error(
    get_all_resources("plot-observations", limit = -1),
    "limit must be a finite, non-negative integer")

  expect_error(
    get_all_resources("parties", detail = "minimal"),
    "When provided, 'detail' must be 'full'."
    )

})

test_that("Getting plot observations works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    obs_zero <- get_plot_observation("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(obs_zero), 0L)

  names_obs_one <- c("author_datum", "author_e", "author_location",
    "author_n", "author_obs_code", "author_plot_code", "author_zone",
    "auto_taxon_cover", "azimuth", "basal_area", "bryophyte_quality",
    "confidentiality_reason", "confidentiality_status", "country",
    "cover_dispersion", "cover_method_id", "date_accuracy",
    "dominant_stratum", "dsg_poly", "effort_level", "elevation",
    "elevation_accuracy", "elevation_range", "field_cover", "field_ht",
    "floating_cover", "floristic_quality", "growthform_1_cover",
    "growthform_1_type", "growthform_2_cover", "growthform_2_type",
    "growthform_3_cover", "growthform_3_type", "homogeneity",
    "hydrologic_regime", "landform", "landscape_narrative",
    "layout_narrative", "lichen_quality", "location_accuracy",
    "location_narrative", "max_slope_aspect", "max_slope_gradient",
    "method_narrative", "min_slope_aspect", "min_slope_gradient",
    "name_other", "nonvascular_cover", "nonvascular_ht", "obs_end_date",
    "observation_id", "observation_accession_code",
    "observation_narrative", "obs_start_date", "organic_depth",
    "original_data", "parent_id", "percent_bare_soil",
    "percent_bed_rock", "percent_litter", "percent_other",
    "percent_rock_gravel", "percent_water", "percent_wood",
    "permanence", "phenologic_aspect", "placement_method", "plot_id",
    "plot_accession_code", "previous_obs_id", "project_id", "rock_type",
    "shape", "shore_distance", "shrub_cover", "shrub_ht",
    "slope_aspect", "slope_gradient", "soil_depth", "soil_drainage",
    "soil_moisture_regime", "soil_taxon_id", "soil_taxon_src",
    "stand_maturity", "stand_size", "state_province",
    "stem_observation_area", "stem_sample_method", "stem_size_limit",
    "stratum_assignment", "stratum_method_accession_code",
    "stratum_method_description", "stratum_method_name",
    "submerged_cover", "submerged_ht", "successional_status",
    "surficial_deposits", "taxon_observation_area", "topo_position",
    "total_cover", "tree_cover", "tree_ht", "water_depth",
    "water_salinity")
  names_obs_all_full <- c("accession_code", "area", "author_datum",
    "author_e", "author_location", "author_n", "author_obs_code",
    "author_plot_code", "author_zone", "auto_taxon_cover", "azimuth",
    "basal_area", "bryophyte_quality", "confidentiality_reason",
    "confidentiality_status", "country", "cover_dispersion",
    "cover_method_id", "date_accuracy", "date_entered",
    "dominant_stratum", "dsg_poly", "effort_level", "elevation",
    "elevation_accuracy", "elevation_range", "emb_observation",
    "field_cover", "field_ht", "floating_cover", "floristic_quality",
    "growthform_1_cover", "growthform_1_type", "growthform_2_cover",
    "growthform_2_type", "growthform_3_cover", "growthform_3_type",
    "has_observation_synonym", "homogeneity", "hydrologic_regime",
    "interp_bestfit_cc_id", "interp_bestfit_ci_id",
    "interp_bestfit_code", "interp_bestfit_party_id",
    "interp_bestfit_partyname", "interp_bestfit_sciname",
    "interp_current_cc_id", "interp_current_ci_id",
    "interp_current_code", "interp_current_party_id",
    "interp_current_partyname", "interp_current_sciname",
    "interp_orig_cc_id", "interp_orig_ci_id", "interp_orig_code",
    "interp_orig_party_id", "interp_orig_party_name",
    "interp_orig_sci_name", "landform", "landscape_narrative",
    "latitude", "layout_narrative", "lichen_quality",
    "location_accuracy", "location_narrative", "longitude",
    "max_slope_aspect", "max_slope_gradient", "method_narrative",
    "min_slope_aspect", "min_slope_gradient", "name_other",
    "nonvascular_cover", "nonvascular_ht", "notes_mgt", "notes_public",
    "number_of_taxa", "obs_end_date", "observation_id",
    "observation_narrative", "obs_start_date", "organic_depth",
    "original_data", "parent_id", "percent_bare_soil",
    "percent_bed_rock", "percent_litter", "percent_other",
    "percent_rock_gravel", "percent_water", "percent_wood",
    "permanence", "phenologic_aspect", "placement_method", "plot_id",
    "plot_accession_code", "plot_notes_public", "plot_validation_level",
    "previous_obs_id", "project_id", "reference_id",
    "representativeness", "revisions", "rock_type", "shape",
    "shore_distance", "shrub_cover", "shrub_ht", "slope_aspect",
    "slope_gradient", "soil_depth", "soil_drainage",
    "soil_moisture_regime", "soil_taxon_id", "soil_taxon_src",
    "stand_maturity", "stand_size", "state_province",
    "stem_observation_area", "stem_sample_method", "stem_size_limit",
    "stratum_method_id", "submerged_cover", "submerged_ht",
    "successional_status", "surficial_deposits",
    "taxon_observation_area", "topo_position", "top_taxon_1_name",
    "top_taxon_2_name", "top_taxon_3_name", "top_taxon_4_name",
    "top_taxon_5_name", "total_cover", "tree_cover", "tree_ht",
    "water_depth", "water_salinity")
  names_obs_all_minimal <- c("author_obs_code", "author_plot_code",
    "country", "latitude", "longitude", "obs_accession_code",
    "observation_id", "plot_id", "plot_accession_code",
    "state_province")

  obs_one <- get_plot_observation("VB.Ob.41618.50D47AJX5G5U8WY")
  expect_identical(nrow(obs_one), 1L)
  expect_named(obs_one, names_obs_one, ignore.order = TRUE)

  obs_all_full <- get_all_plot_observations(detail = "full", limit = 5)
  expect_identical(nrow(obs_all_full), 5L)
  expect_named(obs_all_full, names_obs_all_full, ignore.order = TRUE)

  obs_all_minimal <- get_all_plot_observations(detail = "minimal", limit = 5)
  expect_identical(nrow(obs_all_minimal), 5L)
  expect_named(obs_all_minimal, names_obs_all_minimal, ignore.order = TRUE)

})

test_that("Getting plot observation details works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    det_obs_zero <- get_plot_observation_details("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_type(det_obs_zero, "list")
  expect_identical(length(det_obs_zero), 0L)

  names_det_obs <- c("area", "author_obs_code", "author_plot_code",
    "auto_taxon_cover", "bryophyte_quality", "confidentiality_text",
    "country", "cover_method_accession_code", "cover_type",
    "date_entered", "effort_level", "elevation", "floristic_quality",
    "interp_current_partyname", "latitude", "lichen_quality",
    "location_narrative", "longitude", "obs_accession_code",
    "obs_end_date", "obs_start_date", "permanence", "plot_accession_code",
    "plot_validation_level", "plot_validation_level_descr",
    "project_accession_code", "project_name", "slope_aspect",
    "slope_gradient", "state_province", "stratum_method_accession_code",
    "stratum_method_description", "stratum_method_name",
    "taxon_observation_area")
  names_det_taxa <- c("author_plant_name", "basal_area", "biomass",
    "cover", "cover_code", "inference_area", "int_curr_plant_code",
    "int_curr_plant_common", "int_curr_plant_sci_full",
    "int_curr_plant_sci_name_no_auth", "int_orig_plant_code",
    "int_orig_plant_common", "int_orig_plant_sci_full",
    "int_orig_plant_sci_name_no_auth", "stratum")
  names_det_comm <- c("accession_code", "comm_name")

  det_obs_one <- get_plot_observation_details("VB.Ob.41618.50D47AJX5G5U8WY")
  expect_type(det_obs_one, "list")
  expect_identical(length(det_obs_one), 3L)
  expect_named(
    det_obs_one,
    c("plot_observation", "taxa", "communities"),
    ignore.order = TRUE
  )

  df_plot_observation <- det_obs_one[["plot_observation"]]
  expect_s3_class(df_plot_observation, "data.frame")
  expect_identical(nrow(df_plot_observation), 1L)
  expect_named(df_plot_observation, names_det_obs, ignore.order = TRUE)

  df_taxa <- det_obs_one[["taxa"]]
  expect_s3_class(df_taxa, "data.frame")
  expect_identical(nrow(df_taxa), 8L)
  expect_named(df_taxa, names_det_taxa, ignore.order = TRUE)

  df_communities <- det_obs_one[["communities"]]
  expect_s3_class(df_communities, "data.frame")
  expect_identical(nrow(df_communities), 1L)
  expect_named(df_communities, names_det_comm, ignore.order = TRUE)

})

test_that("Getting taxon observations works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    txo_zero <- get_taxon_observation("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(txo_zero), 0L)

  names_txo_one <- c("author_plant_name", "emb_taxon_observation",
    "int_curr_plant_code", "int_curr_plant_common",
    "int_curr_plant_concept_id", "int_curr_plant_sci_full",
    "int_curr_plant_sci_name_no_auth", "int_orig_plant_code",
    "int_orig_plant_common", "int_orig_plant_concept_id",
    "int_orig_plant_sci_full", "int_orig_plant_sci_name_no_auth",
    "max_cover", "observation_id", "reference_id",
    "taxon_inference_area", "taxon_observation_id",
    "taxon_observation_accession_code")
  names_txo_all_full <- c("author_plant_name",
    "emb_taxon_observation", "int_curr_plant_code",
    "int_curr_plant_common", "int_curr_plant_concept_id",
    "int_curr_plant_sci_full", "int_curr_plant_sci_name_no_auth",
    "int_orig_plant_code", "int_orig_plant_common",
    "int_orig_plant_concept_id", "int_orig_plant_sci_full",
    "int_orig_plant_sci_name_no_auth", "max_cover", "observation_id",
    "reference_id", "taxon_inference_area", "taxon_observation_id",
    "taxon_observation_accession_code")
  names_txo_all_minimal <- c("author_plant_name",
    "emb_taxon_observation", "int_curr_plant_code",
    "int_curr_plant_common", "int_curr_plant_concept_id",
    "int_curr_plant_sci_full", "int_curr_plant_sci_name_no_auth",
    "int_orig_plant_code", "int_orig_plant_common",
    "int_orig_plant_concept_id", "int_orig_plant_sci_full",
    "int_orig_plant_sci_name_no_auth", "max_cover", "observation_id",
    "reference_id", "taxon_inference_area", "taxon_observation_id",
    "taxon_observation_accession_code")

  txo_one <- get_taxon_observation("VB.TO.693826.Q8MM3I559WIQP3D")
  expect_identical(nrow(txo_one), 1L)
  expect_named(txo_one, names_txo_one, ignore.order = TRUE)

  txo_all_full <- get_all_taxon_observations(detail = "full", limit = 5)
  expect_identical(nrow(txo_all_full), 5L)
  expect_named(txo_all_full, names_txo_all_full, ignore.order = TRUE)

  txo_all_minimal <- get_all_taxon_observations(detail = "minimal", limit = 5)
  expect_identical(nrow(txo_all_minimal), 5L)
  expect_named(txo_all_minimal, names_txo_all_minimal, ignore.order = TRUE)

  txo_all_max2 <- get_all_taxon_observations(max_taxa_per_plot = 2, limit = 500)
  expect_identical(max(table(txo_all_max2$observation_id)), 2L)

})

test_that("Getting community classifications works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    cc_zero <- get_community_classification("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(cc_zero), 0L)

  names_cc_one <- c("class_confidence", "class_fit", "class_notes",
    "class_start_date", "class_stop_date", "comm_authority_id",
    "comm_class_accession_code", "comm_code", "comm_concept_id",
    "comm_framework", "comm_level", "comm_name", "emb_comm_class",
    "emb_comm_interpretation", "expert_system", "inspection",
    "multivariate_analysis", "nomenclatural_type", "notes",
    "table_analysis", "type")
  names_cc_all_full <- c("class_confidence", "class_fit", "class_notes",
    "class_start_date", "class_stop_date", "comm_authority_id",
    "comm_class_accession_code", "comm_code", "comm_concept_id",
    "comm_framework", "comm_level", "comm_name", "emb_comm_class",
    "emb_comm_interpretation", "expert_system", "inspection",
    "multivariate_analysis", "nomenclatural_type", "notes",
    "table_analysis", "type")
  names_cc_all_minimal <- c("comm_class_accession_code",
    "comm_concept_accession_code", "comm_name", "obs_accession_code")

  cc_one <- get_community_classification("VB.Cl.34809.2HZMTQQVR2GWE9P")
  expect_identical(nrow(cc_one), 1L)
  expect_named(cc_one, names_cc_one, ignore.order = TRUE)

  cc_all_full <- get_all_community_classifications(detail = "full", limit = 5)
  expect_identical(nrow(cc_all_full), 5L)
  expect_named(cc_all_full, names_cc_all_full, ignore.order = TRUE)

  cc_all_minimal <- get_all_community_classifications(detail = "minimal", limit = 5)
  expect_identical(nrow(cc_all_minimal), 5L)
  expect_named(cc_all_minimal, names_cc_all_minimal, ignore.order = TRUE)

})

test_that("Getting community concepts works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    co_zero <- get_community_concept("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(co_zero), 0L)

  names_co_one <- c("accession_code", "class_system",
    "comm_description", "comm_name", "comm_name_date_entered",
    "comm_name_status", "current_accepted", "default_name", "obs_count",
    "party_id", "party_accession_code", "reference_id", "usage_start",
    "usage_stop")
  names_co_all <- c("accession_code", "comm_description",
    "comm_name_date_entered", "current_accepted", "default_name",
    "obs_count", "reference_id", "reference_accession_code")

  co_one <- get_community_concept("VB.cc.30617.ARTEMISIATRIDEN")
  expect_identical(nrow(co_one), 1L)
  expect_named(co_one, names_co_one, ignore.order = TRUE)

  co_all <- get_all_community_concepts(limit = 5)
  expect_identical(nrow(co_all), 5L)
  expect_named(co_all, names_co_all, ignore.order = TRUE)

})

test_that("Getting parties works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    party_zero <- get_party("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(party_zero), 0L)

  names_party_one <- c("contact_instructions", "given_name",
    "middle_name", "organization_name", "party_id",
    "party_accession_code", "salutation", "surname")
  names_party_all <- c("contact_instructions", "given_name",
    "middle_name", "organization_name", "party_id",
    "party_accession_code", "salutation", "surname")

  party_one <- get_party("VB.py.191378.VOLUNTEER")
  expect_identical(nrow(party_one), 1L)
  expect_named(party_one, names_party_one, ignore.order = TRUE)

  party_all <- get_all_parties(limit = 5)
  expect_identical(nrow(party_all), 5L)
  expect_named(party_all, names_party_all, ignore.order = TRUE)

})

test_that("Getting projects works", {
  skip_if_not(ENABLED && interactive())
  local_vb_debug(0)

  expect_message(
    proj_zero <- get_project("does_not_exist"),
    "No records returned",
    fixed = TRUE
  )
  expect_identical(nrow(proj_zero), 0L)

  names_proj_one <- c("last_plot_added_date", "obs_count", "project_id",
    "project_accession_code", "project_description", "project_name",
    "start_date", "stop_date")
  names_proj_all <- c("last_plot_added_date", "obs_count", "project_id",
    "project_accession_code", "project_description", "project_name",
    "start_date", "stop_date")

  proj_one <- get_project("VB.pj.10508.SOUTHWESTGAPCOL")
  expect_identical(nrow(proj_one), 1L)
  expect_named(proj_one, names_proj_one, ignore.order = TRUE)

  proj_all <- get_all_projects(limit = 5)
  expect_identical(nrow(proj_all), 5L)
  expect_named(proj_all, names_proj_all, ignore.order = TRUE)

})
