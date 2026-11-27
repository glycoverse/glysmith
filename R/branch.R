#' Run a single side route
#'
#' The branch starts at the current output. The next outer step also receives
#' that output. Use [fork()] to end a route with multiple siblings.
#' @param name A label for the branch.
#' @param ... Steps, branches, forks or sub-blueprints.
#' @returns A branch for use inside [blueprint()].
#' @examples
#' blueprint(br("scores", step_pca()), step_dea_limma())
#' @export
br <- function(name, ...) {
  checkmate::assert_string(name, min.chars = 1)
  steps <- as.list(rlang::list2(...))
  structure(list(name = name, steps = steps), class = "glysmith_branch")
}
