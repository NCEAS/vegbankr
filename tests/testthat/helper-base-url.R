# Helper to locally set the VegBank base URL inside a test function,
# then set it back to the original URL upon completion
local_base_url <- function(base_url, env = parent.frame()) {
  old_base <- get_vb_base_url()
  suppressMessages(set_vb_base_url(base_url))
  withr::defer(suppressMessages(set_vb_base_url(old_base)), env = env)
}
