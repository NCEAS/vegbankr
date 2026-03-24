valid_project <- data.frame(
  user_pj_code = c("MOJA")
)

valid_parties <- data.frame(
  user_py_code = c("PY001", "PY002"),
  surname = c("Smith", "Jones")
)

valid_contributors <- data.frame(
  user_cr_code = c("CR001", "CR002", "CR003"),
  vb_py_code = c(NA, NA, "VB789"),
  user_py_code = c("PY001", "PY002", NA),
  vb_ar_code = c("ar.16", "ar.18", "ar.55"),
  record_identifier = c("MOJA", "MOJA", "MOJA"),
  contributor_type = c("Project", "Project", "Project")
)

valid_plots <- data.frame(
  user_ob_code = c("OB001", "OB002", "OB003"),
  user_pl_code = c("PL001", "PL002", NA),
  vb_pl_code = c(NA, NA, "VB001"),
  author_plot_code = c("AP001", "AP002", "AP003"),
  author_obs_code = c("AO001", "AO002", "AO003"),
  user_pj_code = c("MOJA", "MOJA", "MOJA"),
  user_parent_pl_code = c(NA, "PL001", NA),
  latitude = c(37.7749, 34.0522, 40.7128),
  longitude = c(-122.4194, -118.2437, -74.0060),
  obs_start_date = c("2024-01-15", "2024-02-20", "2024-03-10")
)

valid_community_classifications <- data.frame(
  user_ob_code = c("OB001", "OB002", "OB003"),
  user_cl_code = c("CL001", "CL002", "CL003"),
  vb_cc_code = c("CC123", "CC456", "CC789")
)

valid_strata_cover_data <- data.frame(
  user_ob_code = c("OB001", "OB001", "OB002"),
  user_tm_code = c("TM001", "TM002", "TM003"),
  user_to_code = c("TO001", "TO001", "TO002"),
  user_sr_code = c("SR001", "SR002", "SR003"), 
  author_plant_name = c("Quercus alba", "Quercus alba", "Pinus strobus"),
  cover = c(25, 15, 40)
)

# Strata data
valid_strata <- data.frame(
  user_ob_code = c("OB001", "OB001", "OB002"),
  user_sr_code = c("SR001", "SR002", "SR003"),  
  vb_sy_code = c("SY001", "SY002", "SY003")
)

valid_taxon <- data.frame(
  user_ti_code = c("TI001", "TI002", "TI003"),  
  user_to_code = c("TO001", "TO001", "TO002"),  
  vb_pc_code = c("PC123", "PC456", "PC789"),
  vb_ar_code = c("ar.55", "ar.34", "ar.16"),
  user_py_code = c("PY001", "PY002", NA),  
  vb_py_code = c(NA, NA, "VB999"),  
  original_interpretation = c(TRUE, TRUE, FALSE),
  current_interpretation = c(TRUE, FALSE, TRUE)
)

valid_disturbances <- data.frame(
  user_do_code = c("DO001", "DO002", "DO003"),  # unique values (no duplicates)
  user_ob_code = c("OB001", "OB002", "OB003"),  # must exist in plot_observations
  type = c("fire", "grazing", "logging"),
  intensity = c("high", "moderate", "low"),
  comment = c("Wildfire 2020", "Cattle grazing", "Selective harvest")
)

valid_soils <- data.frame(
  user_so_code = c("SO001", "SO002", "SO003"),  # unique values (no duplicates)
  user_ob_code = c("OB001", "OB002", "OB003"),  # must exist in plot_observations
  horizon = c("A", "A", "B"),
  texture = c("sandy loam", "clay", "silt loam"),
  depth = c(15, 20, 30)
)

valid_stem <- data.frame(
  user_sc_code = c("SC001", "SC002", "SC003"),
  user_tm_code = c("TM001", "TM001", "TM002"),
  stem_count = c(5, 12, 3),
  user_sl_code = c(1, 2, 3),
  stem_diameter = c(15.5, 22.3, 8.7)
)


test_that("vb_validate_plot_observations works for valid data", {
  # Valid parties table
  result <- vb_validate_plot_observations(
    projects = valid_project,
    parties = valid_parties,
    contributors = valid_contributors,
    plot_observations = valid_plots,
    community_classifications = valid_community_classifications, 
    strata_cover_data = valid_strata_cover_data,
    strata = valid_strata,
    taxon_interpretations = valid_taxon,
    disturbances = valid_disturbances,
    soils = valid_soils,
    stem_data = valid_stem
  )
  
  expect_true(all(unlist(result)))
  
})