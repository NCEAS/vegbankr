test_that("default vb_base_url is used", {
    old_base <- get_vb_base_url()
    set_vb_base_url("")
    expect_equal(get_vb_base_url(), "https://api.vegbank.org")
    set_vb_base_url(old_base)
})
test_that("vb_base_url option set", {
    old_base <- get_vb_base_url()
    set_vb_base_url("http://localhost", 8080)
    expect_equal(get_vb_base_url(), "http://localhost:8080")
    set_vb_base_url(old_base)
})
test_that("set_vb_base_url(NULL) reports default URL", {
    old_base <- get_vb_base_url()
    expect_message(set_vb_base_url(NULL),
                   "Using https://api.vegbank.org as base URL")
    set_vb_base_url(old_base)
})
