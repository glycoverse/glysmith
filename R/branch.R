#' Run a single side route
#'
#' The branch starts at the current output. The next outer step also receives
#' that output. Use [fork()] to end a route with multiple siblings.
#' @param name A label for the branch.
#' @param route One step, sequence, fork or reusable blueprint.
#' @returns A branch for use inside [blueprint()].
#' @examples
#' blueprint(br("scores", step_pca()) %=>% step_dea_limma())
#' @export
br <- function(name, route) {
  checkmate::assert_string(name, min.chars = 1)
  if (missing(route) || !.connected_route(route)) {
    cli::cli_abort("br() needs a single analytical route.")
  }
  structure(list(name = name, route = route), class = "glysmith_branch")
}
