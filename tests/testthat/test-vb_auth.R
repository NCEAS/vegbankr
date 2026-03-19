make_jwt <- function(claims = list()) {
  header  <- jsonlite::base64url_enc(charToRaw('{"alg":"none"}'))
  payload <- jsonlite::base64url_enc(charToRaw(jsonlite::toJSON(claims, auto_unbox = TRUE)))
  paste(header, payload, "sig", sep = ".")
}

make_expired_jwt <- function() {
  make_jwt(list(exp = as.numeric(Sys.time()) - 1800))
}

make_valid_jwt <- function() {
  make_jwt(list(exp = as.numeric(Sys.time()) + 1800))
}

test_that("vb_set_token stores tokens", {
  withr::local_options(vegbank.access_token = NULL, vegbank.refresh_token = NULL)

  expect_message(vb_set_token(access_token = "eyJhbGciOi"), "access token")
  expect_equal(getOption("vegbank.access_token"), "eyJhbGciOi")

  expect_message(vb_set_token(refresh_token = "eAJpc3MiOi"), "refresh token")
  expect_equal(getOption("vegbank.refresh_token"), "eAJpc3MiOi")

  expect_message(
    vb_set_token(access_token = "eyJhbGciOi", refresh_token = "eAJpc3MiOi"),
    "access token and refresh token"
  )
  expect_equal(getOption("vegbank.access_token"), "eyJhbGciOi")
  expect_equal(getOption("vegbank.refresh_token"), "eAJpc3MiOi")

  expect_message(vb_set_token(tokens = list(access_token = "eyJhdWQiOi", refresh_token = "eAJpc3MiOi")))
  expect_equal(getOption("vegbank.access_token"), "eyJhdWQiOi")
  expect_equal(getOption("vegbank.refresh_token"), "eAJpc3MiOi")
})

test_that("vb_set_token rejects invalid inputs", {
  expect_error(vb_set_token(), "at least one")
  expect_error(vb_set_token(access_token = 123), "non-empty string")
  expect_error(vb_set_token(access_token = ""), "non-empty string")
  expect_error(
    vb_set_token(access_token = "eyJhbGciOi", tokens = list(access_token = "eyJhdWQiOi")),
    "not both"
  )
})

test_that("vb_unset_token clears stored tokens", {
  withr::local_options(vegbank.access_token = "eyJhbGciOi", vegbank.refresh_token = "eyJhbGciOi")
  expect_message(vb_unset_token(), "cleared")
  expect_null(getOption("vegbank.access_token"))
  expect_null(getOption("vegbank.refresh_token"))
})

test_that("vb_access_token_is_valid checks stored access token", {
  withr::local_options(vegbank.access_token = make_valid_jwt())
  expect_true(vb_access_token_is_valid())

  withr::local_options(vegbank.access_token = make_expired_jwt())
  expect_false(vb_access_token_is_valid())

  withr::local_options(vegbank.access_token = NULL)
  expect_false(vb_access_token_is_valid())
})

test_that("vb_refresh_token_is_valid checks stored refresh token", {
  withr::local_options(vegbank.refresh_token = make_valid_jwt())
  expect_true(vb_refresh_token_is_valid())

  withr::local_options(vegbank.refresh_token = make_expired_jwt())
  expect_false(vb_refresh_token_is_valid())

  withr::local_options(vegbank.refresh_token = NULL)
  expect_false(vb_refresh_token_is_valid())
})
