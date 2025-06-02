test_that("vb_debug sets the correct verbosity", {
  withr::local_options(vegbank.debug = NULL)

  expect_message(vb_debug(0), "Disabling VegBank debugging")
  expect_equal(getOption("vegbank.debug"), 0)

  expect_message(vb_debug(1), "Enabling VegBank debugging with verbosity 1")
  expect_equal(getOption("vegbank.debug"), 1)

  expect_message(vb_debug(2), "Enabling VegBank debugging with verbosity 2")
  expect_equal(getOption("vegbank.debug"), 2)

  expect_message(vb_debug(3), "Enabling VegBank debugging with verbosity 3")
  expect_equal(getOption("vegbank.debug"), 3)
})

test_that("vb_debug rejects invalid inputs", {
  withr::local_options(vegbank.debug = NULL)
  expect_error(vb_debug(NULL), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug(NA), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug(4), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug(-1), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug("a"), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug(c(1, 2)), "verbosity must be 0, 1, 2, or 3")
  expect_error(vb_debug(print), "verbosity must be 0, 1, 2, or 3")
})

test_that("vb_undebug resets verbosity to 0", {
  withr::local_options(vegbank.debug = 2)
  expect_message(vb_undebug(), "Disabling VegBank debugging")
  expect_equal(getOption("vegbank.debug"), 0)
})

test_that("vb_verbosity returns current valid verbosity", {

  withr::local_options(vegbank.debug = 0)
  expect_equal(vb_verbosity(), 0)

  withr::local_options(vegbank.debug = 1)
  expect_equal(vb_verbosity(), 1)

  withr::local_options(vegbank.debug = 2)
  expect_equal(vb_verbosity(), 2)

  withr::local_options(vegbank.debug = 3)
  expect_equal(vb_verbosity(), 3)
})

test_that("vb_verbosity resets invalid verbosity", {

  withr::local_options(vegbank.debug = 5)
  expect_warning(expect_equal(vb_verbosity(), 0),
                 "invalid verbosity level; disabling debug mode")
  expect_equal(getOption("vegbank.debug"), 0)

  withr::local_options(vegbank.debug = "a")
  expect_warning(expect_equal(vb_verbosity(), 0),
                 "invalid verbosity level; disabling debug mode")
  expect_equal(getOption("vegbank.debug"), 0)
})

test_that("vb_verbosity returns 0 when unset", {
  withr::local_options(vegbank.debug = NULL)
  expect_equal(vb_verbosity(), 0)
})
