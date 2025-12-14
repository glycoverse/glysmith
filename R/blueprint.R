#' Create a Blueprint
#'
#' A blueprint is a list of steps that are executed in order.
#'
#' @param steps A list of steps.
#'
#' @returns A blueprint object.
#' @noRd
blueprint <- function(steps) {
  bp <- new_blueprint(steps)
  validate_blueprint(bp)
}

#' @noRd
new_blueprint <- function(steps) {
  structure(steps, class = "glysmith_blueprint")
}

#' Validate a blueprint's ctx$data dependencies
#'
#' This is a static check based on each step's declared `require` and `generate`
#' fields (keys under `ctx$data`).
#'
#' @param blueprint A `glysmith_blueprint` object.
#'
#' @returns Invisibly returns TRUE if the blueprint is valid.
#' @noRd
validate_blueprint <- function(blueprint) {
  if (!inherits(blueprint, "glysmith_blueprint")) {
    cli::cli_abort("Invalid blueprint object.")
  }

  initial_data <- character(0)
  known <- initial_data

  for (s in blueprint) {
    if (!inherits(s, "glysmith_step")) {
      cli::cli_abort("Invalid step object.")
    }

    require <- s$require %||% character(0)
    generate <- s$generate %||% character(0)

    checkmate::assert_character(require, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_character(generate, any.missing = FALSE, null.ok = TRUE)

    if (length(require) > 0) {
      missing <- setdiff(require, known)
      if (length(missing) > 0) {
        cli::cli_abort(c(
          "Blueprint cannot run due to missing step dependencies.",
          "x" = "Step '{s$id}' requires missing ctx$data keys: {.field {missing}}.",
          "i" = "Add a prior step that generates these keys, or remove them from {.field require}."
        ))
      }
    }

    if (length(generate) > 0) {
      known <- unique(c(known, generate))
    }
  }

  invisible(blueprint)
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
    retries_left <- s$retry
    while (TRUE) {
      if (!quiet) cli::cli_progress_step(s$label)
      tryCatch(
        {
          ctx <- s$run(ctx)
          break
        },
        error = function(e) {
          if (retries_left > 0) {
            retries_left <<- retries_left - 1  # `retries_left` is defined outside the function scope
            if (!quiet) cli::cli_alert_warning("Step '{s$id}' failed. Retrying... ({retries_left + 1} attempts left)")
          } else {
            stop(e)
          }
        }
      )
    }
    ctx$meta$steps <- c(ctx$meta$steps, s$id)
  }
  ctx
}