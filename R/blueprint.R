#' Compose a tree of analytical steps
#'
#' A step receives its predecessor's experiment or fitted result. A terminal
#' step emits only reporting artifacts. Use [fork()] for siblings and [br()]
#' for a side route that leaves the current connection available.
#' See `vignette("blueprint")` for a complete analysis.
#' @param ... Steps, side branches, forks or reusable blueprints.
#' @returns A validated blueprint. Printing it shows each producer and consumer.
#' @examples
#' blueprint(step_preprocess(), fork(step_pca(), step_dea_limma()))
#' @export
blueprint <- function(...) {
  bp <- structure(
    rlang::list2(...),
    class = "glysmith_blueprint",
    format_version = 2L
  )
  if (!length(bp)) {
    cli::cli_abort("A blueprint needs at least one step.")
  }
  .compile_blueprint(bp)
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
#'   fork(step_pca(), step_dea_limma()),
#' )
#' write_blueprint(bp, tempfile(fileext = ".rds"))
#' @export
write_blueprint <- function(bp, file) {
  .assert_blueprint(bp)
  saveRDS(bp, file)
}

#' @rdname write_blueprint
#' @export
read_blueprint <- function(file) {
  x <- readRDS(file)
  .assert_blueprint(x)
  x
}

.assert_blueprint <- function(x) {
  if (
    !inherits(x, "glysmith_blueprint") ||
      !identical(attr(x, "format_version"), 2L)
  ) {
    cli::cli_abort(
      "Incompatible blueprint. Rebuild it with blueprint(), br() and fork()."
    )
  }
}

.run_blueprint_run_once <- function(step, ctx) {
  tryCatch(
    {
      new_ctx <- NULL
      logs_warning <- list()
      logs_output <- utils::capture.output(
        {
          logs_message <- utils::capture.output(
            {
              new_ctx <- withCallingHandlers(
                step$run(ctx),
                warning = function(w) {
                  logs_warning[[length(logs_warning) + 1]] <<- w
                  invokeRestart("muffleWarning")
                }
              )
            },
            type = "message"
          )
        },
        type = "output"
      )

      list(
        status = "success",
        new_ctx = new_ctx,
        logs = list(
          output = logs_output,
          message = logs_message,
          warning = logs_warning
        )
      )
    },
    error = function(e) {
      list(status = "error", error = e)
    }
  )
}

#' Fan out from the current blueprint output
#'
#' Each argument is a sibling route that receives the same input. Pass a step
#' for a one-step route or a blueprint for an ordered route.
#'
#' @param ... Two or more routes. Argument names can label routes.
#' @returns A fork specification to use in a blueprint.
#' @export
fork <- function(...) {
  routes <- rlang::list2(...)
  if (
    length(routes) < 2L ||
      !all(vapply(routes, .connected_route, logical(1)))
  ) {
    stop(
      "fork() needs at least two sibling routes.",
      call. = FALSE
    )
  }
  structure(routes, class = "glysmith_fork")
}

.connected_route <- function(x) {
  inherits(
    x,
    c("glysmith_step", "glysmith_blueprint", "glysmith_fork")
  )
}

.connected_branch <- function(x) {
  inherits(x, "glysmith_branch")
}

# An unbound sub-blueprint may start at any compatible outlet. All routes
# leaving its inlet must accept the same type. forge_analysis binds it to exp.
.compile_blueprint <- function(blueprint, input_type = NULL) {
  .assert_blueprint(blueprint)
  nodes <- list()
  inlet <- input_type %||% c("exp", "filtered_exp", "contrast", "omnibus")
  walk <- function(items, parent, type, labels, main_exp) {
    for (item in items) {
      if (!length(type)) {
        cli::cli_abort(
          "No output follows a terminal step or fork at this position."
        )
      }
      if (inherits(item, "glysmith_fork")) {
        for (j in seq_along(item)) {
          route <- item[[j]]
          label <- names(item)[j]
          walk(list(route), parent, type, c(labels, label), main_exp)
        }
        type <- character()
      } else if (.connected_branch(item)) {
        if (!length(item$steps)) {
          cli::cli_abort("A side branch cannot be empty.")
        }
        walk(item$steps, parent, type, c(labels, item$name), main_exp)
      } else if (inherits(item, "glysmith_blueprint")) {
        .assert_blueprint(item)
        tail <- walk(item, parent, type, labels, main_exp)
        parent <- tail$parent
        type <- tail$type
        main_exp <- tail$main_exp
      } else {
        if (!inherits(item, "glysmith_step") || is.null(item$connected)) {
          cli::cli_abort(
            "Each connected step must declare its input and output."
          )
        }
        contract <- item$connected
        available <- if (parent == "root") inlet else type
        accepted <- intersect(available, contract$accepts)
        if (!length(accepted)) {
          cli::cli_abort(
            "{item$signature} requires {contract$accepts}, received {available} from {parent}."
          )
        }
        if (parent == "root") {
          inlet <<- accepted
        }
        id <- paste0("node_", length(nodes) + 1L)
        nodes[[id]] <<- list(
          id = id,
          parent = parent,
          step = item,
          route = paste(labels[nzchar(labels)], collapse = "/")
        )
        parent <- id
        type <- contract$produces
        if (length(type) && type %in% c("exp", "filtered_exp")) main_exp <- id
      }
    }
    list(parent = parent, type = type, main_exp = main_exp)
  }

  tail <- walk(blueprint, "root", inlet, character(), "root")
  list(nodes = nodes, accepts = inlet, main_exp = tail$main_exp)
}

#' @export
print.glysmith_blueprint <- function(x, ...) {
  plan <- .compile_blueprint(x)
  cat(
    "Blueprint (root accepts ",
    paste(plan$accepts, collapse = " or "),
    ")\n",
    sep = ""
  )
  for (n in plan$nodes) {
    label <- if (nzchar(n$route)) paste0(" [", n$route, "]") else ""
    outlet <- n$step$connected$produces %||% "terminal"
    cat(sprintf(
      "  %s -> %s  %s%s (%s)\n",
      n$parent,
      n$id,
      n$step$signature,
      label,
      outlet
    ))
  }
  invisible(x)
}

# The key names below are local to a single invocation of a step. They adapt
# the existing analysis code to a single wire without sharing mutable context.
.exp_port <- function(output = NULL) {
  list(
    accepts = c("exp", "filtered_exp"),
    produces = if (!is.null(output)) "exp",
    key = output
  )
}

.fit_port <- function(
  produces = NULL,
  key = NULL,
  accepts = c("contrast", "omnibus")
) {
  list(accepts = accepts, produces = produces, key = key)
}

.connected_node <- function(node, wire, group_col) {
  step <- node$step
  port <- step$connected
  ctx <- new_ctx(wire$exp, group_col)
  if (!is.null(wire$fit)) {
    ctx$data$dea_res <- wire$fit
  }
  ctx$filter_args <- wire$filter_args
  specs <- .dependency_specs(step$packages)
  missing <- specs[!vapply(specs, rlang::is_installed, logical(1))]
  if (length(missing)) {
    cli::cli_abort("Missing step dependencies: {missing}.")
  }
  if (!nrow(wire$exp)) {
    return(list(
      status = "skipped",
      reason = "input experiment has zero variables"
    ))
  }
  if (!is.null(step$condition)) {
    cond <- step$condition(ctx)
    if (!cond$check) return(list(status = "skipped", reason = cond$reason))
  }
  for (i in seq_len(1L + step$retry)) {
    attempt <- .run_blueprint_run_once(step, ctx)
    if (attempt$status == "success") break
  }
  if (attempt$status != "success") {
    stop(attempt$error)
  }
  local <- attempt$new_ctx
  local$exp <- wire$exp
  local$meta$logs[[step$id]] <- attempt$logs
  output <- NULL
  if (!is.null(port$key)) {
    value <- ctx_get_data(local, port$key)
    output <- switch(
      port$produces,
      contrast = ,
      omnibus = list(
        exp = wire$exp,
        fit = value,
        filter_args = port$filter_args
      ),
      filtered_exp = list(exp = value, fit = wire$fit),
      exp = list(exp = value)
    )
  }
  content <- if (is.function(step$report)) {
    tryCatch(step$report(local), error = function(e) {
      paste("Report generation failed:", conditionMessage(e))
    })
  }
  list(
    status = "success",
    output = output,
    plots = local$plots,
    tables = local$tables,
    internal = local$data[setdiff(
      names(local$data),
      c(names(ctx$data), port$key)
    )],
    explanation = local$meta$explanation,
    logs = attempt$logs,
    report = content
  )
}

run_blueprint <- function(blueprint, ctx, quiet = FALSE) {
  plan <- .compile_blueprint(blueprint, "exp")
  ctx$data$root <- ctx$data$exp
  wires <- list(root = list(exp = ctx$data$root))
  for (node in plan$nodes) {
    id <- node$id
    upstream <- wires[[node$parent]]
    if (is.null(upstream)) {
      res <- list(
        status = "blocked",
        reason = ctx$meta$nodes[[node$parent]]$reason
      )
    } else {
      res <- tryCatch(
        .connected_node(node, upstream, ctx$group_col),
        error = function(e) {
          list(status = "failed", reason = conditionMessage(e))
        }
      )
    }
    ctx$meta$nodes[[id]] <- list(
      step = node$step$signature,
      label = node$step$label,
      parent = node$parent,
      route = node$route,
      status = res$status,
      reason = res$reason
    )
    if (res$status != "success") {
      if (!quiet) {
        cli::cli_alert_info(
          "{id} {node$step$signature} {res$status}: {res$reason}"
        )
      }
      next
    }
    wires[[id]] <- res$output
    if (!is.null(res$output)) {
      ctx$data[[id]] <- if (
        node$step$connected$produces %in% c("contrast", "omnibus")
      ) {
        res$output$fit
      } else {
        res$output$exp
      }
    }
    prefix <- paste0(id, "__")
    for (kind in c("plots", "tables", "internal")) {
      field <- if (kind == "internal") "data" else kind
      for (key in names(res[[kind]])) {
        name <- paste0(prefix, key)
        ctx[[field]][[name]] <- res[[kind]][[key]]
        ctx$meta$explanation[[paste0(
          field,
          "$",
          name
        )]] <- res$explanation[[paste0(field, "$", key)]]
      }
    }
    ctx$meta$steps <- c(ctx$meta$steps, id)
    ctx$meta$logs[[id]] <- res$logs
    ctx$meta$reports[[id]] <- res$report
  }
  ctx$data["exp"] <- list(wires[[plan$main_exp]]$exp)
  ctx
}
