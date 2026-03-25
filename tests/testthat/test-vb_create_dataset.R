# Helpers

valid_args <- list(
  name = "My Dataset",
  description = "A test dataset",
  observations = c("ob.1", "ob.2", "ob.3")
)

make_request <- function(...) {
  args <- modifyList(valid_args, list(...))
  do.call(build_dataset_request, args)
}

parse_body <- function(req) {
  jsonlite::fromJSON(req$body$data, simplifyVector = FALSE)
}

# Input validation: `name`

test_that("`name` must be a single non-NA character string", {
  expect_error(
    vb_create_dataset(123, valid_args$description, valid_args$observations),
    "`name` must be a single non-NA character string"
  )
  expect_error(
    vb_create_dataset(NA_character_, valid_args$description, valid_args$observations),
    "`name` must be a single non-NA character string"
  )
  expect_error(
    vb_create_dataset(c("a", "b"), valid_args$description, valid_args$observations),
    "`name` must be a single non-NA character string"
  )
})

with_mock_api({
  local_base_url(NULL)
  test_that("`name` must be 100 characters or fewer", {
    expect_error(
      vb_create_dataset(strrep("a", 101), valid_args$description, valid_args$observations),
      "`name` must be 100 characters or fewer"
    )
    # Boundary: exactly 100 characters should not error (tested via mocked response)
    expect_POST(
      vb_create_dataset(strrep("a", 100), valid_args$description, valid_args$observations)
    )
  })
})

# Input validation: `description`

test_that("`description` must be a single non-NA character string", {
  expect_error(
    vb_create_dataset(valid_args$name, 123, valid_args$observations),
    "`description` must be a single non-NA character string"
  )
  expect_error(
    vb_create_dataset(valid_args$name, NA_character_, valid_args$observations),
    "`description` must be a single non-NA character string"
  )
  expect_error(
    vb_create_dataset(valid_args$name, c("a", "b"), valid_args$observations),
    "`description` must be a single non-NA character string"
  )
})

# Input validation: `observations`

test_that("`observations` must be a non-empty character vector", {
  expect_error(
    vb_create_dataset(valid_args$name, valid_args$description, character(0)),
    "`observations` must be a non-empty character vector"
  )
  expect_error(
    vb_create_dataset(valid_args$name, valid_args$description, 1:3),
    "`observations` must be a non-empty character vector"
  )
})

test_that("`observations` rejects invalid observation codes", {
  expect_error(
    vb_create_dataset(valid_args$name, valid_args$description, c("ob.1", "ob.0", "bad")),
    "invalid codes: ob.0, bad"
  )
  # Boundary: ob.0 is invalid (pattern requires [1-9] first digit)
  expect_error(
    vb_create_dataset(valid_args$name, valid_args$description, "ob.0"),
    "invalid codes"
  )
})

# build_dataset_request: request structure

test_that("build_dataset_request returns an httr2 request object", {
  req <- make_request()
  expect_s3_class(req, "httr2_request")
})

test_that("build_dataset_request uses POST and correct URL path", {
  req <- make_request()
  expect_equal(req$method, "POST")
  expect_match(req$url, "user-datasets")
})

test_that("build_dataset_request sets dry_run query parameter correctly", {
  req_false <- make_request(dry_run = FALSE)
  req_true <- make_request(dry_run = TRUE)
  expect_match(req_false$url, "dry_run=FALSE")
  expect_match(req_true$url, "dry_run=TRUE")
})

test_that("build_dataset_request sets Content-Type header", {
  req <- make_request()
  expect_equal(req$headers[["Content-Type"]], "application/json")
})

test_that("build_dataset_request encodes name and description in body", {
  req <- make_request()
  body <- parse_body(req)
  expect_equal(body$name, valid_args$name)
  expect_equal(body$description, valid_args$description)
})

test_that("build_dataset_request encodes observations as a list in body", {
  req <- make_request()
  body <- parse_body(req)
  expect_equal(body$data$observation, as.list(valid_args$observations))
})

test_that("build_dataset_request handles a single observation", {
  req <- make_request(observations = "ob.99")
  body <- parse_body(req)
  expect_equal(body$data$observation, list("ob.99"))
})

# vb_create_dataset: response handling

with_mock_api({
  local_base_url(NULL)
  test_that("vb_create_dataset returns expected output on success", {
    printed_dry_run <- capture.output({
      msgs <- capture_messages(
        do.call(vb_create_dataset, valid_args)
      )
    })
    expect_true(any(grepl("Upload complete", msgs)))
    expect_equal(printed_dry_run,
        c("$di",
          "    action user_di_code vb_di_code",
          "1 inserted            1       di.1",
          "2 inserted            2       di.2",
          "3 inserted            3       di.3",
          "",
          "$ds",
          "    action user_ds_code vb_ds_code",
          "1 inserted   My Dataset       ds.1",
          ""))

  })
})
