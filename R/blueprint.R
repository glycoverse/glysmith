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
  steps <- as.list(rlang::list2(...))
  names(steps) <- purrr::map_chr(steps, "id")
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
  ctx <- .run_blueprint_ensure_ctx(ctx)

  if (!"preprocess" %in% names(blueprint) && "raw_exp" %in% names(ctx$data)) {
    ctx <- ctx_add_data(ctx, "clean_exp", ctx_get_data(ctx, "raw_exp"))
  }

  for (step in blueprint) {
    if (.run_blueprint_should_skip(step, ctx, quiet = quiet)) next

    step_run <- .run_blueprint_run_with_retry(step, ctx, quiet = quiet)
    ctx <- step_run$ctx
  }

  ctx
}

.run_blueprint_ensure_ctx <- function(ctx) {
  ctx$meta <- ctx$meta %||% list()
  ctx$meta$steps <- ctx$meta$steps %||% character(0)
  ctx$meta$logs <- ctx$meta$logs %||% list()

  ctx$data <- ctx$data %||% list()
  ctx
}

.run_blueprint_should_skip <- function(step, ctx, quiet = FALSE) {
  if (!is.null(step$condition) && !isTRUE(step$condition(ctx))) return(TRUE)

  required <- step$require %||% character(0)
  if (length(required) == 0) return(FALSE)

  missing_deps <- setdiff(required, names(ctx$data))
  if (length(missing_deps) == 0) return(FALSE)

  if (!quiet) {
    cli::cli_alert_warning(
      "Skipping Step '{step$id}' due to missing ctx$data keys: {.field {missing_deps}}."
    )
  }
  TRUE
}

.run_blueprint_run_with_retry <- function(step, ctx, quiet = FALSE) {
  retries_left <- as.integer(step$retry %||% 0L)

  while (TRUE) {
    if (!quiet) cli::cli_progress_step(step$label)

    attempt <- .run_blueprint_run_once(step, ctx)

    if (attempt$status == "success") {
      ctx <- .run_blueprint_ensure_ctx(attempt$new_ctx)
      ctx$meta$logs[[step$id]] <- attempt$logs
      ctx$meta$steps <- c(ctx$meta$steps, step$id)
      return(list(ctx = ctx))
    }

    e <- attempt$error
    if (retries_left > 0) {
      retries_left <- retries_left - 1
      if (!quiet) {
        cli::cli_alert_warning(
          "Step '{step$id}' failed. Retrying... ({retries_left + 1} attempts left)"
        )
      }
      next
    }

    if (!quiet) cli::cli_alert_warning("Step '{step$id}' failed. Skipping... Error: {e$message}")
    ctx$meta$logs[[step$id]] <- list(error = e$message)
    return(list(ctx = ctx))
  }
}

.run_blueprint_run_once <- function(step, ctx) {
  tryCatch(
    {
      new_ctx <- NULL
      logs_output <- utils::capture.output({
        logs_message <- utils::capture.output({
          new_ctx <- step$run(ctx)
        }, type = "message")
      }, type = "output")

      list(
        status = "success",
        new_ctx = new_ctx,
        logs = list(output = logs_output, message = logs_message)
      )
    },
    error = function(e) {
      list(status = "error", error = e)
    }
  )
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
#' @param preprocess Whether to include [step_preprocess()].
#' @param enrich Whether to include the enrichment steps,
#'   i.e. [step_enrich_go()], [step_enrich_kegg()], and [step_enrich_reactome()].
#' @param traits Whether to include the derived trait analysis steps,
#'   i.e. [step_derive_traits()] and [step_dta()].
#'
#' @returns A `glysmith_blueprint` object.
#' @examples
#' blueprint_default()
#' @export
blueprint_default <- function(preprocess = TRUE, enrich = TRUE, traits = TRUE) {
  steps <- list()
  if (preprocess) {
    steps <- append(steps, list(step_preprocess()))
  }
  steps <- append(steps, list(step_ident_overview(), step_pca(), step_dea(), step_volcano()))
  if (enrich) {
    steps <- append(steps, list(step_enrich_go(), step_enrich_kegg(), step_enrich_reactome()))
  }
  if (traits) {
    steps <- append(steps, list(step_derive_traits(), step_dta()))
  }
  names(steps) <- purrr::map_chr(steps, "id")
  new_blueprint(steps)
}
