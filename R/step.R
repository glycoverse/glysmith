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
#' @param require Character vector of required `ctx$data` keys.
#' @param generate Character vector of generated `ctx$data` keys.
#' @param condition Optional function(ctx) returning a list of `check` and `reason`.
#'   - `check` is TRUE/FALSE to decide execution.
#'   - `reason` is a human-readable string to explain why `check` is FALSE.
#' @param repeatable Whether this step can appear multiple times in a blueprint.
#'   If TRUE, duplicate step IDs are allowed. Default is FALSE.
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
  outputs = list(),
  require = character(0),
  generate = character(0),
  condition = NULL,
  repeatable = FALSE,
  retry = 0L,
  signature = NULL
) {
  structure(
    list(
      id = id,
      label = label,
      run = run,
      report = report,
      require = require,
      generate = generate,
      condition = condition,
      repeatable = repeatable,
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
