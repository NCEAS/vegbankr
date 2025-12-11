# Helper to locally set the VegBank base URL inside a test function,
# then set it back to the original URL upon completion
local_base_url <- function(base_url, env = parent.frame()) {
  old_base <- vb_get_base_url()
  suppressMessages(vb_set_base_url(base_url))
  withr::defer(suppressMessages(vb_set_base_url(old_base)), env = env)
}
