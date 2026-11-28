#' Compose a tree of analytical steps
#'
#' Connect a producer to its consumer with `%=>%`. Routes separated by
#' commas in `blueprint()` start independently from the original experiment.
#' Use [fork()] to share an intermediate output and [br()] for a side route
#' that leaves the outer connection available. See `vignette("blueprint")`.
#'
#' @param ... One or more routes, optionally named. A trailing comma is allowed.
#' @param lhs,rhs Steps, routes, forks, side branches or reusable blueprints.
#' @returns `blueprint()` returns a validated blueprint. Printing it draws each
#'   step beneath its producer and shows its result ID. `%=>%` returns an
#'   ordered route for use in a blueprint. Neither constructor runs the analysis.
#' @examples
#' blueprint(
#'   step_preprocess() %=>% step_dea_limma() %=>% fork(
#'     step_volcano(),
#'     step_filter_sig_vars() %=>% step_heatmap()
#'   )
#' )
#' blueprint(scores = step_pca(), differential = step_dea_limma())
#' @export
blueprint <- function(...) {
  routes <- rlang::list2(...)
  if (!length(routes)) {
    cli::cli_abort("A blueprint needs at least one route.")
  }
  bp <- structure(routes, class = "glysmith_blueprint", format_version = 3L)
  .compile_blueprint(bp)
  bp
}

#' @rdname blueprint
#' @export
`%=>%` <- function(lhs, rhs) {
  items <- function(x) {
    if (!.connected_route(x)) {
      cli::cli_abort("Both operands of %=>% must be analytical routes.")
    }
    if (inherits(x, "glysmith_sequence")) unclass(x) else list(x)
  }
  route <- structure(c(items(lhs), items(rhs)), class = "glysmith_sequence")
  .compile_route(route, c("exp", "filtered_exp", "contrast", "omnibus"))
  route
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
#'   step_preprocess() %=>%
#'     fork(step_pca(), step_dea_limma()),
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
      !identical(attr(x, "format_version"), 3L)
  ) {
    cli::cli_abort(
      "Incompatible blueprint. Rebuild it with blueprint() and %=>%."
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
#' for a one-step route, a `%=>%` sequence or a reusable blueprint.
#'
#' @param ... One or more routes. Argument names can label routes.
#' @returns A fork specification to use in a blueprint.
#' @export
fork <- function(...) {
  routes <- rlang::list2(...)
  if (
    !length(routes) ||
      !all(vapply(routes, .connected_route, logical(1)))
  ) {
    stop(
      "fork() needs one or more analytical routes.",
      call. = FALSE
    )
  }
  structure(routes, class = "glysmith_fork")
}

.connected_route <- function(x) {
  inherits(
    x,
    c(
      "glysmith_step",
      "glysmith_sequence",
      "glysmith_blueprint",
      "glysmith_fork",
      "glysmith_branch"
    )
  )
}

.compile_blueprint <- function(blueprint) {
  .assert_blueprint(blueprint)
  .compile_route(blueprint, "exp")
}

# A reusable sequence can start at a fit. Its siblings must all accept the
# same inlet. A complete blueprint binds that inlet to the original experiment.
.compile_route <- function(route, inlet) {
  nodes <- list()
  visit <- function(item, wire, labels) {
    if (!length(wire$type)) {
      cli::cli_abort(
        "No output follows a terminal step or fork at this position."
      )
    }
    if (inherits(item, "glysmith_sequence")) {
      for (child in item) {
        wire <- visit(child, wire, labels)
      }
      return(wire)
    }
    if (inherits(item, "glysmith_branch")) {
      visit(item$route, wire, c(labels, item$name))
      return(wire)
    }
    if (inherits(item, c("glysmith_blueprint", "glysmith_fork"))) {
      blueprint <- inherits(item, "glysmith_blueprint")
      if (blueprint) {
        .assert_blueprint(item)
      }
      for (j in seq_along(item)) {
        tail <- visit(item[[j]], wire, c(labels, names(item)[j]))
      }
      if (blueprint && length(item) == 1L) {
        return(tail)
      }
      # Forks and multi-entry blueprints expose no single branch output.
      wire$type <- character()
      return(wire)
    }
    if (!inherits(item, "glysmith_step") || is.null(item$connected)) {
      cli::cli_abort(
        "Each route must contain steps with declared inputs and outputs."
      )
    }
    contract <- item$connected
    available <- if (wire$parent == "root") inlet else wire$type
    accepted <- intersect(available, contract$accepts)
    if (!length(accepted)) {
      cli::cli_abort(
        "{item$signature} requires {contract$accepts}, received {available} from {wire$parent}."
      )
    }
    if (wire$parent == "root") {
      inlet <<- accepted
    }
    id <- paste0("node_", length(nodes) + 1L)
    nodes[[id]] <<- list(
      id = id,
      parent = wire$parent,
      step = item,
      route = paste(labels[nzchar(labels)], collapse = "/")
    )
    wire$parent <- id
    wire$type <- contract$produces
    if (length(wire$type) && wire$type %in% c("exp", "filtered_exp")) {
      wire$main_exp <- id
    }
    wire
  }
  root <- list(parent = "root", type = inlet, main_exp = "root")
  tail <- visit(route, root, character())
  list(nodes = nodes, accepts = inlet, main_exp = tail$main_exp)
}

#' @export
print.glysmith_blueprint <- function(x, ...) {
  plan <- .compile_blueprint(x)
  ids <- c("root", names(plan$nodes))
  parents <- purrr::map_chr(plan$nodes, "parent")
  root_label <- sprintf(
    "%s (root accepts %s)",
    cli::style_bold("Blueprint"),
    paste(plan$accepts, collapse = " or ")
  )
  labels <- purrr::map_chr(plan$nodes, function(n) {
    label <- if (nzchar(n$route)) {
      cli::col_yellow(paste0(" [", n$route, "]"))
    } else {
      ""
    }
    type <- n$step$connected$produces %||% "terminal"
    outlet <- switch(
      type,
      contrast = ,
      omnibus = cli::col_magenta(type),
      terminal = cli::style_dim(type),
      cli::col_green(type)
    )
    sprintf(
      " %s  %s%s (%s)",
      cli::style_dim(n$id),
      cli::col_cyan(paste(n$step$signature, collapse = "\n")),
      label,
      outlet
    )
  })
  tree <- tibble::tibble(
    id = ids,
    children = split(names(plan$nodes), factor(parents, levels = ids)),
    label = c(root_label, labels)
  )
  print(cli::tree(tree, width = .Machine$integer.max))
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
  plan <- .compile_blueprint(blueprint)
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
