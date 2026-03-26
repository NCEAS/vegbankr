# Upload sanity tests to be run against an actual instance of the API.
#
# This test suite is really intended to be run against a local instance
# of the API and database with access mode 'open', insofar as running
# this against a production DB would relevant credentials and
# authentication, and moreover would change the state of the database by
# incrementing sequential ID fields even in dry_run mode.

ENABLED <- FALSE

test_that("Uploading plot observations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_ob_code = "my_ob_code",
    author_obs_code = "my_author_obs_code",
    user_pl_code = "my_pl_code",
    author_plot_code = "my_author_plot_code",
    confidentiality_status = 0,
    user_pj_code = "my_pj_code"
  )
  projects = data.frame(
    user_pj_code = "my_pj_code",
    project_name = "my_project_name"
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_plot_observations(df, projects = projects, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading plant concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_pc_code = "my_pc_code",
    name = "my name",
    plant_concept_status = "my status",
    start_date = '2026-03-01',
    vb_rf_code = "rf.33",
    vb_status_py_code = "py.511"
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_plant_concepts(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading community concepts works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_cc_code = "my_cc_code",
    name = "my name",
    comm_concept_status = "my status",
    start_date = '2026-03-01',
    vb_rf_code = "rf.33",
    vb_status_py_code = "py.511"
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_community_concepts(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading taxon interpretations works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_ti_code = "my_ti_code",
    vb_to_code = "to.3527293",
    vb_pc_code = "pc.402424",  # Eubotrys racemosus
    vb_py_code = "py.410",  # michael lee
    vb_ar_code = "ar.46",  # unspecified
    original_interpretation = TRUE,
    current_interpretation = TRUE
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_taxon_interpretations(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading community classifications works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_cl_code = "my_ti_code",
    vb_ob_code = "ob.4229",
    vb_cc_code = "cc.141"
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_community_classifications(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading cover methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_cm_code = "my_cm_code",
    cover_type = "cvr_type",
    vb_rf_code = "rf.1",
    cover_code = "cvr_code",
    cover_percent = 0
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_cover_methods(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Uploading stratum methods works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  df <- data.frame(
    user_sm_code = "my_sm_code",
    stratum_method_name = "my_stratum_method_name"
  )
  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_upload_stratum_methods(df, dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})

test_that("Dataset creation works", {
  skip_if_not(ENABLED && interactive())
  skip_on_cran()
  local_vb_debug(0)

  printed_dry_run <- capture.output({
    msgs <- capture_messages(
      vb_create_dataset(
        name = "My dataset",
        description = "A collection of great plot observations",
        observations = c("ob.4229", "ob.4230", "ob.4231"),
        dry_run=TRUE)
    )
  })
  expect_true(any(grepl("dry run", msgs)))
})
