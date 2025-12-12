#' Create a Blueprint
#'
#' A blueprint is a list of steps that are executed in order.
#'
#' @param steps A list of steps.
#'
#' @returns A blueprint object.
#' @noRd
new_blueprint <- function(steps) {
  structure(steps, class = "glysmith_blueprint")
}

#' Run a list of steps
#'
#' @param blueprint A `glysmith_blueprint` object.
#' @param ctx Context list.
#' @param quiet Whether to suppress progress output.
#'
#' @returns Updated context.
#' @noRd
run_blueprint <- function(blueprint, ctx, quiet = FALSE) {
  for (s in blueprint) {
    if (!inherits(s, "glysmith_step")) {
      cli::cli_abort("Invalid step object.")
    }
    if (!is.null(s$condition) && !isTRUE(s$condition(ctx))) {
      next
    }
    if (!quiet) cli::cli_progress_step(s$label)
    ctx <- s$run(ctx)
    ctx$meta$steps <- c(ctx$meta$steps, s$id)
  }
  ctx
}