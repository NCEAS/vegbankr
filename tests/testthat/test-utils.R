test_that("default vb_base_url is used", {
    local_base_url(NULL)
    expect_equal(get_vb_base_url(), "https://api.vegbank.org")
    suppressMessages(set_vb_base_url(""))
    expect_equal(get_vb_base_url(), "https://api.vegbank.org")
})
test_that("vb_base_url option set", {
    local_base_url(NULL)
    suppressMessages(set_vb_base_url("http://localhost", 8080))
    expect_equal(get_vb_base_url(), "http://localhost:8080")
})
test_that("set_vb_base_url(NULL) reports default URL", {
    local_base_url(NULL)
    expect_message(set_vb_base_url(NULL),
                   "Using https://api.vegbank.org as base URL")
})
