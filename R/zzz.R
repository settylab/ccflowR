#' @title Package Initialization
#' @description Package load and unload hooks
#' @name zzz
NULL

.onLoad <- function(libname, pkgname) {
  # Initialize global settings
  .ccflow_env$settings <- CCFlowSettings$new()


  # Register repr_html method for Jupyter notebook display
  # This allows sankey objects to display properly in IRkernel
  if (requireNamespace("repr", quietly = TRUE)) {
    registerS3method("repr_html", "sankey", repr_html.sankey, envir = asNamespace("repr"))
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "ccflowR v", utils::packageVersion(pkgname),
    " - Cell-Cell Communication Flow Visualization"
  )
}

.onUnload <- function(libpath) {
  # Clean up
  rm(list = ls(envir = .ccflow_env), envir = .ccflow_env)
}
