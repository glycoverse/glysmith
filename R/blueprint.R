#' Create a Blueprint
#'
#' A blueprint is a list of steps that are executed in order.
#' Type `step_` and TAB in RStudio to see all available steps.
#'
#' @param ... One or more step objects.
#'
#' @returns A blueprint object.
#' @examples
#' blueprint(
#'   step_preprocess(),
#'   step_pca(),
#'   step_dea_limma(),  # this comma is ok
#' )
#' @export
blueprint <- function(...) {
  steps <- as.list(rlang::list2(...))
  steps <- .expand_branches(steps)
  names(steps) <- purrr::map_chr(steps, "id")
  bp <- new_blueprint(steps)
  bp <- validate_blueprint(bp)
  bp
}

#' Save or Load a Blueprint
#'
#' - `write_blueprint()` saves a blueprint to a RDS file.
#' - `read_blueprint()` loads a blueprint from a RDS file.
#'
#' @param bp A [blueprint()].
#' @param file A character string giving the name of the file to save to or load from.
#'
#' @returns Invisibly returns the blueprint object.
#' @examples
#' bp <- blueprint(
#'   step_preprocess(),
#'   step_pca(),
#'   step_dea_limma(),
#' )
#' write_blueprint(bp, tempfile(fileext = ".rds"))
#' @export
write_blueprint <- function(bp, file) {
  saveRDS(bp, file)
}

#' @rdname write_blueprint
#' @export
read_blueprint <- function(file) {
  x <- readRDS(file)
  if (!inherits(x, "glysmith_blueprint")) {
    cli::cli_abort(c(
      "Must be of type {.cls glysmith_blueprint}.",
      "x" = "Got {.cls {class(x)}}"
    ))
  }
  x
}

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

  .validate_blueprint_duplicates(blueprint)
  .validate_blueprint_dependencies(blueprint)
  .validate_blueprint_overwrite(blueprint)
  invisible(blueprint)
}

#' Check any duplicated steps in the blueprint.
#'
#' Repeatable steps (with `repeatable = TRUE`) are allowed to have duplicate IDs.
#'
#' @param blueprint A `glysmith_blueprint` object.
#'
#' @returns NULL. Raises an error if the blueprint is not valid.
#' @noRd
.validate_blueprint_duplicates <- function(blueprint) {
  step_ids <- names(blueprint)
  step_counts <- table(step_ids)

  # Find IDs that appear more than once
  duplicated_ids <- names(step_counts)[step_counts > 1]

  if (length(duplicated_ids) > 0) {
    # For each duplicated ID, check if all steps with that ID are repeatable
    for (id in duplicated_ids) {
      dup_steps <- blueprint[names(blueprint) == id]
      all_repeatable <- purrr::every(dup_steps, ~ isTRUE(.x$repeatable))

      if (!all_repeatable) {
        # Find signatures of non-repeatable duplicated steps
        non_repeatable_dups <- purrr::keep(dup_steps, ~ isFALSE(.x$repeatable))
        duplicated_sigs <- purrr::map_chr(non_repeatable_dups, ~ .x$signature)
        cli::cli_abort(c(
          "Blueprint cannot contain duplicated steps.",
          "x" = "{.code {duplicated_sigs}} {?is/are} duplicated."
        ))
      }
    }
  }
}

#' Check any missing dependencies in the blueprint.
#'
#' A missing dependency means that a step requires data that is not present in the context.
#'
#' @param blueprint A `glysmith_blueprint` object.
#'
#' @returns NULL. Raises an error if the blueprint is not valid.
#' @noRd
.validate_blueprint_dependencies <- function(blueprint) {
  initial_data <- character(0)
  known <- initial_data

  # `missing_keys[step_id]` is a character vector of missing keys for the step.
  missing_keys <- list()

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
      missing <- setdiff(missing, "exp")  # exp is always present

      # If the step is in a branch, check if the missing keys can be resolved from global data.
      # This allows branches to "inherit" dependencies from the main flow.
      if (!is.null(s$branch) && length(missing) > 0) {
        prefix <- paste0(s$branch, "__")
        # Identify missing keys that have the branch prefix
        prefixed_missing <- missing[startsWith(missing, prefix)]
        if (length(prefixed_missing) > 0) {
          # Strip prefix to get the "global" key
          unprefixed <- substr(prefixed_missing, nchar(prefix) + 1, nchar(prefixed_missing))
          # Check if the global key is known
          found_globally <- unprefixed %in% known
          # Remove resolved keys from missing list
          resolved <- prefixed_missing[found_globally]
          missing <- setdiff(missing, resolved)
        }
      }

      missing_keys[[s$id]] <- missing
    }

    if (length(generate) > 0) {
      known <- unique(c(known, generate))
    }
  }

  unique_missing_keys <- unique(unlist(missing_keys))
  if (length(unique_missing_keys) > 0) {
    msg <- "Blueprint is not valid due to missing step dependencies."

    for (i in seq_along(blueprint)) {
      s <- blueprint[[i]]
      if (length(missing_keys[[s$id]]) > 0) {
        step_sig <- s$signature
        new_part <- c("x" = paste0("Step {.code ", step_sig, "} requires missing data: {.field {missing_keys[['", s$id, "']]}}." ))
        msg <- c(msg, new_part)
      }
    }

    key_steps <- .get_key_steps(unique_missing_keys)
    extract_signatures <- function(steps) purrr::map_chr(steps, "signature")
    sigs <- purrr::map(key_steps, extract_signatures)
    for (k in names(key_steps)) {
      new_part <- c("i" = paste0("Data {.field ", k, "} can be generated by {.code {sigs[['", k, "']]}}." ))
      msg <- c(msg, new_part)
    }

    cli::cli_abort(msg)
  }
}

#' Check any overwrite of existing data in the blueprint.
#'
#' An overwrite means that a step generates data that already exists in the context.
#' It is not fatal, but when it happens, a warning should be raised.
#'
#' @param blueprint A `glysmith_blueprint` object.
#'
#' @returns NULL. Raises a warning if the blueprint is not valid.
#' @noRd
.validate_blueprint_overwrite <- function(blueprint) {
  generated_map <- list()
  overwrite_msgs <- character(0)

  for (s in blueprint) {
    generate <- s$generate %||% character(0)
    current_step_sig <- s$signature

    for (key in generate) {
      if (key %in% names(generated_map)) {
        previous_step_sig <- generated_map[[key]]
        msg <- paste0("{.code ", current_step_sig, "} overwrites data {.field ", key, "} generated by {.code ", previous_step_sig, "}.")
        overwrite_msgs <- c(overwrite_msgs, msg)
      }
      generated_map[[key]] <- current_step_sig
    }
  }

  if (length(overwrite_msgs) > 0) {
    msgs <- c("Blueprint contains data overwrites:", overwrite_msgs)
    names(msgs) <- c("", rep("*", length(overwrite_msgs)))
    msgs <- c(msgs, "i" = "You can ignore this if this is your intention.")
    cli::cli_warn(msgs)
  }
}

#' Get the steps that generate the missing keys
#'
#' @param missing_keys Character vector of missing keys.
#'
#' @returns
#'   A list of key steps. Names are the missing keys, and values are the steps that generate them.
#'   For example, if `missing_keys` is `c("x", "y")`, then `key_steps[["x"]]` is a list of steps that generate `x`,
#'   and `key_steps[["y"]]` is a list of steps that generate `y`.
#' @noRd
.get_key_steps <- function(missing_keys) {
  key_steps <- list()
  steps <- all_steps()
  for (k in missing_keys) {
    key_steps[[k]] <- steps[purrr::map_lgl(steps, ~ k %in% .x$generate)]
  }
  key_steps
}

#' @export
print.glysmith_blueprint <- function(x, ...) {
  cli::cli_h2("Blueprint ({.val {length(x)}} steps)")
  top_id <- cli::cli_ul(.close = FALSE)
  branch_id <- NULL
  current_branch <- NULL

  for (s in x) {
    if (!is.null(s$branch)) {
      if (!identical(current_branch, s$branch)) {
        if (!is.null(branch_id)) {
          cli::cli_end(branch_id)
        }
        current_branch <- s$branch
        cli::cli_li(paste0("br(\"", current_branch, "\")"))
        branch_id <- cli::cli_ul(.close = FALSE)
      }
      cli::cli_li(s$branch_signature %||% s$signature)
      next
    }

    if (!is.null(branch_id)) {
      cli::cli_end(branch_id)
      branch_id <- NULL
      current_branch <- NULL
    }
    cli::cli_li(s$signature)
  }

  if (!is.null(branch_id)) {
    cli::cli_end(branch_id)
  }
  cli::cli_end(top_id)
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
  if (!is.null(step$condition)) {
    condition_res <- step$condition(ctx)
    if (isFALSE(condition_res$check)) {
      if (!quiet) {
        cli::cli_alert_info(
          "Skipping `{step$signature}` because {condition_res$reason}."
        )
      }
      return(TRUE)
    }
  }

  required <- step$require %||% character(0)
  if (length(required) == 0) return(FALSE)

  missing_deps <- setdiff(required, names(ctx$data))

  # Allow branches to inherit dependencies from the main flow
  if (!is.null(step$branch) && length(missing_deps) > 0) {
    prefix <- paste0(step$branch, "__")
    prefixed_missing <- missing_deps[startsWith(missing_deps, prefix)]

    if (length(prefixed_missing) > 0) {
      unprefixed <- substr(prefixed_missing, nchar(prefix) + 1, nchar(prefixed_missing))
      found_globally <- unprefixed %in% names(ctx$data)
      resolved <- prefixed_missing[found_globally]
      missing_deps <- setdiff(missing_deps, resolved)
    }
  }

  if (length(missing_deps) == 0) return(FALSE)

  if (!quiet) {
    cli::cli_alert_warning(
      "Skipping `{step$signature}` due to missing ctx$data keys: {.field {missing_deps}}."
    )
  }
  TRUE
}

.run_blueprint_run_with_retry <- function(step, ctx, quiet = FALSE) {
  retries_left <- as.integer(step$retry %||% 0L)

  while (TRUE) {
    step_id <- NULL
    if (!quiet) step_id <- cli::cli_progress_step(step$label)

    attempt <- .run_blueprint_run_once(step, ctx)

    if (attempt$status == "success") {
      ctx <- .run_blueprint_ensure_ctx(attempt$new_ctx)
      ctx$meta$logs[[step$id]] <- attempt$logs
      ctx$meta$steps <- c(ctx$meta$steps, step$id)
      if (!quiet && !is.null(step_id)) {
        cli::cli_progress_done(id = step_id)
      }
      return(list(ctx = ctx))
    }

    e <- attempt$error
    if (retries_left > 0) {
      retries_left <- retries_left - 1
      if (!quiet) {
        cli::cli_progress_done(id = step_id, result = "clear")
        cli::cli_alert_warning(
          "`{step$signature}` failed. Retrying... ({retries_left + 1} attempts left)"
        )
      }
      next
    }

    if (!quiet) {
      cli::cli_progress_done(id = step_id, result = "failed")
      cli::cli_alert_warning("`{step$signature}` failed. Error: {e$message}")
    }
    ctx$meta$logs[[step$id]] <- list(error = e$message)
    return(list(ctx = ctx))
  }
}

.run_blueprint_run_once <- function(step, ctx) {
  tryCatch(
    {
      new_ctx <- NULL
      logs_warning <- list()
      logs_output <- utils::capture.output({
        logs_message <- utils::capture.output({
          new_ctx <- withCallingHandlers(
            step$run(ctx),
            warning = function(w) {
              logs_warning[[length(logs_warning) + 1]] <<- w
              invokeRestart("muffleWarning")
            }
          )
        }, type = "message")
      }, type = "output")

      list(
        status = "success",
        new_ctx = new_ctx,
        logs = list(output = logs_output, message = logs_message, warning = logs_warning)
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
#' - step_ident_overview(): Summarize the experiment using `glyexp::summarize_experiment()`.
#' - step_preprocess(): Preprocess the data using `glyclean::auto_clean()`.
#' - step_pca(): Principal component analysis using `glystats::gly_pca()`,
#'   and plot the PCA using `glyvis::plot_pca()`.
#' - step_dea_limma(): Differential analysis using `glystats::gly_limma()`.
#' - step_volcano(): Plot a volcano plot using `glyvis::plot_volcano()`.
#' - step_heatmap(on = "sig_exp"): Plot a heatmap using `glyvis::plot_heatmap()`.
#' - step_sig_enrich_go(): Perform GO enrichment analysis using `glystats::gly_enrich_go()`.
#' - step_sig_enrich_kegg(): Perform KEGG enrichment analysis using `glystats::gly_enrich_kegg()`.
#' - step_sig_enrich_reactome(): Perform Reactome enrichment analysis using `glystats::gly_enrich_reactome()`.
#' - step_derive_traits(): Derive traits using `glydet::derive_traits()`.
#' - step_dea_limma(on = "trait_exp"): Differential trait analysis using `glystats::gly_limma()`.
#' - step_heatmap(on = "sig_trait_exp"): Plot a heatmap using `glyvis::plot_heatmap()`.
#'
#' @param preprocess Whether to include [step_preprocess()].
#' @param enrich Whether to include the enrichment steps,
#'   i.e. [step_sig_enrich_go()], [step_sig_enrich_kegg()], and [step_sig_enrich_reactome()].
#' @param traits Whether to include the derived trait analysis steps,
#'   i.e. [step_derive_traits()] and `step_dea_limma(on = "trait_exp")`.
#'
#' @returns A `glysmith_blueprint` object.
#' @examples
#' blueprint_default()
#' @export
blueprint_default <- function(preprocess = TRUE, enrich = TRUE, traits = TRUE) {
  steps <- list(step_ident_overview())
  if (preprocess) {
    steps <- append(steps, list(step_preprocess()))
  }
  steps <- append(steps, list(
    step_pca(),
    step_dea_limma(),
    step_volcano(),
    step_heatmap("sig_exp")
  ))
  if (enrich) {
    steps <- append(steps, list(
      step_sig_enrich_go(),
      step_sig_enrich_kegg(),
      step_sig_enrich_reactome()
    ))
  }
  if (traits) {
    steps <- append(steps, list(
      step_derive_traits(),
      step_dea_limma(on = "trait_exp"),
      step_heatmap("sig_trait_exp")
    ))
  }
  names(steps) <- purrr::map_chr(steps, "id")
  new_blueprint(steps)
}
