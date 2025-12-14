#' Create a Blueprint
#'
#' A blueprint is a list of steps that are executed in order.
#'
#' @param ... One or more step objects.
#'
#' @returns A blueprint object.
#' @examples
#' blueprint(
#'   step_preprocess(),
#'   step_pca(),
#'   step_dea(),  # this comma is ok
#' )
#' @export
blueprint <- function(...) {
  bp <- new_blueprint(as.list(rlang::list2(...)))
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

#' @export
print.glysmith_blueprint <- function(x, ...) {
  cli::cli_h2("Blueprint ({.val {length(x)}} steps)")
  for (s in x) {
    cli::cli_ul(s$id)
  }
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
    if (!is.null(s$require)) {
      missing_deps <- setdiff(s$require, names(ctx$data))
      if (length(missing_deps) > 0) {
        if (!quiet) cli::cli_alert_warning("Skipping Step '{s$id}' due to missing ctx$data keys: {.field {missing_deps}}.")
        next
      }
    }

    retries_left <- s$retry
    while (TRUE) {
      if (!quiet) cli::cli_progress_step(s$label)

      step_res <- tryCatch(
        {
          # Success path
          new_ctx <- NULL
          logs_output <- utils::capture.output({
            logs_message <- utils::capture.output({
              new_ctx <- s$run(ctx)
            }, type = "message")
          }, type = "output")

          list(status = "success", new_ctx = new_ctx, logs = list(output = logs_output, message = logs_message))
        },
        error = function(e) {
          # Error path
          list(status = "error", error = e)
        }
      )

      if (step_res$status == "success") {
        ctx <- step_res$new_ctx
        ctx$meta$logs[[s$id]] <- step_res$logs
        ctx$meta$steps <- c(ctx$meta$steps, s$id)
        break
      } else {
        # Error
        e <- step_res$error
        if (retries_left > 0) {
          retries_left <- retries_left - 1
          if (!quiet) cli::cli_alert_warning("Step '{s$id}' failed. Retrying... ({retries_left + 1} attempts left)")
          # Loop continues
        } else {
          # Failed and skip
          if (!quiet) cli::cli_alert_warning("Step '{s$id}' failed. Skipping... Error: {e$message}")
          ctx$meta$logs[[s$id]] <- list(error = e$message)
          break
        }
      }
    }
  }
  ctx
}

# ---------- Blueprints ------------------------------------
#' Default blueprint
#'
#' This blueprint contains the following steps:
#' - step_preprocess(): Preprocess the data using `glyclean::auto_clean()`.
#' - step_ident_overview(): Summarize the experiment using `glyexp::summarize_experiment()`.
#' - step_pca(): Principal component analysis using `glystats::gly_pca()`,
#'   and plot the PCA using `glyvis::plot_pca()`.
#' - step_dea(): Differential analysis using `glystats::gly_dea()`.
#' - step_volcano(): Plot a volcano plot using `glyvis::plot_volcano()`.
#' - step_enrich_go(): Perform GO enrichment analysis using `glystats::gly_enrich_go()`.
#' - step_enrich_kegg(): Perform KEGG enrichment analysis using `glystats::gly_enrich_kegg()`.
#' - step_enrich_reactome(): Perform Reactome enrichment analysis using `glystats::gly_enrich_reactome()`.
#' - step_derive_traits(): Derive traits using `glydet::derive_traits()`.
#' - step_dta(): Differential trait analysis using `glystats::gly_limma()`.
#'
#' @returns A `glysmith_blueprint` object.
#' @examples
#' blueprint_default()
#' @export
blueprint_default <- function() {
  new_blueprint(list(
    step_preprocess(),
    step_ident_overview(),
    step_pca(),
    step_dea(),
    step_volcano(),
    step_enrich_go(),
    step_enrich_kegg(),
    step_enrich_reactome(),
    step_derive_traits(),
    step_dta()
  ))
}