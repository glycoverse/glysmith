#' Create a GlySmith step object
#'
#' A step is a function object with metadata that mutates and returns context.
#'
#' @param id Step id.
#' @param label Human-readable label for progress output.
#' @param run A function(ctx) that returns updated ctx.
#' @param report A function(x) that returns a markdown string for reporting, or NULL.
#'   The string can contain additional information surrounded by "<AI>" and "</AI>" tags,
#'   which will be fed to AI for polishing.
#'   If AI polishing is turned off, the content in "<AI>" and "</AI>" tags will be removed.
#' @param packages Character vector of R packages required to run the step.
#' @param condition Optional function(ctx) returning a list of `check` and `reason`.
#'   - `check` is TRUE/FALSE to decide execution.
#'   - `reason` is a human-readable string to explain why `check` is FALSE.
#' @param retry Number of retry attempts on failure. Default is 0.
#' @param signature The original call signature for display in messages.
#'
#' @returns A `glysmith_step` object.
#' @noRd
step <- function(
  id,
  label,
  run,
  report = NULL,
  packages = character(0),
  condition = NULL,
  retry = 0L,
  signature = NULL,
  connected = .exp_port()
) {
  checkmate::assert_character(packages, any.missing = FALSE)

  structure(
    list(
      id = id,
      label = label,
      run = run,
      report = report,
      connected = connected,
      packages = packages,
      condition = condition,
      retry = retry,
      signature = signature %||% paste0("step_", id, "()")
    ),
    class = "glysmith_step"
  )
}

#' @export
print.glysmith_step <- function(x, ...) {
  cli::cli_text("<step {.val {x$signature}}> {.emph {x$label}}")
}
