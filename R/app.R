#' Launch the rasblocking explorer
#'
#' Opens a browser interface to the structural diagnostics, the official
#' feasibility checks, the cost model, the two verification gates and the
#' pooling candidate generator. Any instance in the GMNS node/link format can be
#' loaded, so the tool is not specific to the RAS 2026 data.
#'
#' @param ... Passed to [shiny::runApp()], e.g. `port` or `launch.browser`.
#' @return Invisibly, the value of [shiny::runApp()].
#' @export
run_app <- function(...) {
  for (p in c("shiny", "bslib", "DT", "ggplot2")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("run_app() needs the '", p, "' package; install it with ",
           "install.packages(\"", p, "\")", call. = FALSE)
    }
  }
  dir <- system.file("shiny", package = "rasblocking")
  if (!nzchar(dir)) stop("shiny app not found in the installed package")
  shiny::runApp(dir, ...)
}
