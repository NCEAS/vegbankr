# Helper to locally set the VegBank debug level inside a test function,
# then set it back to the original level upon completion
local_vb_debug <- function(verbosity, env = parent.frame()) {
  old_verbosity <- vb_verbosity()
  suppressMessages(vb_debug(verbosity))
  withr::defer(suppressMessages(vb_debug(old_verbosity)), env = env)
}
