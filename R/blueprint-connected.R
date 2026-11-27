#' Fan out from the current blueprint output
#'
#' Each argument is a sibling route that receives the same input. Pass a step
#' for a one-step route or a connected blueprint for an ordered route.
#'
#' @param ... Two or more routes. Argument names can label routes.
#' @returns A fork specification to use in a connected blueprint.
#' @export
fork <- function(...) {
  routes <- rlang::list2(...)
  if (
    length(routes) < 2L ||
      !all(vapply(routes, .connected_route, logical(1)))
  ) {
    stop(
      "fork() needs at least two step or connected blueprint routes.",
      call. = FALSE
    )
  }
  structure(routes, class = "glysmith_fork")
}

.connected_route <- function(x) {
  inherits(
    x,
    c("glysmith_step", "glysmith_connected_blueprint", "glysmith_fork")
  )
}

.connected_branch <- function(x) {
  inherits(x, "glysmith_branch")
}

.new_connected_blueprint <- function(items) {
  if (
    !length(items) ||
      !all(vapply(
        items,
        function(x) {
          .connected_route(x) || .connected_branch(x)
        },
        logical(1)
      ))
  ) {
    stop(
      "A connected blueprint needs at least one valid step, branch, or fork.",
      call. = FALSE
    )
  }
  bp <- structure(
    items,
    class = c("glysmith_connected_blueprint", "glysmith_blueprint")
  )
  .compile_connected_blueprint(bp)
  bp
}

# An unbound sub-blueprint may start at any compatible outlet. All routes
# leaving its inlet must accept the same type. forge_analysis binds it to exp.
.compile_connected_blueprint <- function(blueprint, input_type = NULL) {
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
          cli::cli_abort("A connected branch cannot be empty.")
        }
        walk(item$steps, parent, type, c(labels, item$name), main_exp)
      } else if (inherits(item, "glysmith_connected_blueprint")) {
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
        target <- intersect(item$require, .on_choices())
        default_target <- if (identical(contract$accepts, "filtered_exp")) {
          "sig_exp"
        } else {
          "exp"
        }
        if (length(setdiff(target, default_target))) {
          cli::cli_abort(
            "Omit the legacy on target in {item$signature}. Its input comes from {parent}."
          )
        }
        if (isTRUE(contract$legacy_filter)) {
          cli::cli_abort(
            "Set cutoffs and comparisons in step_filter_sig_vars() after {item$signature}."
          )
        }
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
print.glysmith_connected_blueprint <- function(x, ...) {
  plan <- .compile_connected_blueprint(x)
  cat(
    "Connected blueprint (root accepts ",
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
  accepts = c("contrast", "omnibus"),
  only = FALSE
) {
  list(accepts = accepts, produces = produces, key = key, only = only)
}

.connected_node <- function(node, wire, group_col) {
  step <- node$step
  port <- step$connected
  ctx <- new_ctx(wire$exp, group_col)
  ctx$connected <- TRUE
  if (!is.null(wire$fit)) {
    ctx$data$dea_res <- wire$fit
  }
  if (identical(port$accepts, "filtered_exp")) {
    ctx$data$sig_exp <- wire$exp
  }
  missing <- step$packages[
    !vapply(step$packages, rlang::is_installed, logical(1))
  ]
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
  if (!is.null(port$run)) {
    step$run <- port$run
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
      omnibus = list(exp = wire$exp, fit = value),
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

.run_connected_blueprint <- function(blueprint, ctx, quiet) {
  plan <- .compile_connected_blueprint(blueprint, "exp")
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
