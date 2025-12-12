#' Get arguments from dots
#'
#' The function extracts arguments from `rlang::list2(...)` by prefix.
#' For example, `get_args("pkg", "f", list(pkg.f.a = 1, pkg.f.b = 2, c = 3))`
#' will return `list(a = 1, b = 2)`.
#'
#' @param pkg The package name.
#' @param func The function name.
#' @param dots The list of arguments.
#'
#' @returns A list of arguments.
#' @noRd
.get_args <- function(pkg, func, dots) {
  prefix <- paste0(pkg, ".", func, ".")
  if (is.null(names(dots))) return(list())
  idx <- startsWith(names(dots), prefix)
  if (!any(idx)) return(list())
  out <- dots[idx]
  names(out) <- substring(names(out), nchar(prefix) + 1)
  out
}

#' Run a glycoverse function with arguments from dots
#'
#' The function is a syntactic sugar for `rlang::exec(f, exp, !!!.get_args(pkg, func, dots))`,
#' with special handling for group column specification argument.
#' For example, `.run_function(pkg::f, exp, group_col, dots, "group_col")`
#' is the same as `rlang::exec(pkg::f, exp, group_col = group_col, !!!.get_args(pkg, func, dots))`.
#'
#' @param f The function to run.
#' @param exp The experiment object.
#' @param group_col The group column name.
#' @param dots The list of arguments.
#' @param group_arg The name of the group column specification argument.
#'   NULL if the function does not need group column specification.
#'
#' @returns The result of the function.
#' @noRd
.run_function <- function(f, exp, group_col, dots, group_arg = NULL) {
  f_str <- rlang::as_label(rlang::enexpr(f))
  pkg <- stringr::str_split_i(f_str, stringr::fixed("::"), 1)
  func <- stringr::str_split_i(f_str, stringr::fixed("::"), 2)
  args <- .get_args(pkg, func, dots)
  if (!is.null(group_arg)) {
    args[[group_arg]] <- group_col
  }
  suppressMessages(rlang::exec(f, exp, !!!args))
}

#' Create a GlySmith step object
#'
#' A step is a function object with metadata that mutates and returns context.
#'
#' @param id Step id.
#' @param label Human-readable label for progress output.
#' @param run A function(ctx) that returns updated ctx.
#' @param outputs A list describing declared outputs (tables/plots/meta).
#' @param condition Optional function(ctx) returning TRUE/FALSE to decide execution.
#'
#' @returns A `glysmith_step` object.
#' @noRd
step <- function(id, label, run, outputs = list(), condition = NULL) {
  structure(
    list(
      id = id,
      label = label,
      run = run,
      outputs = outputs,
      condition = condition
    ),
    class = "glysmith_step"
  )
}

#' Create analysis context
#'
#' @param exp A `glyexp_experiment`.
#' @param group_col Group column name.
#' @param dots `rlang::list2(...)`.
#'
#' @returns A context list.
#' @noRd
new_ctx <- function(exp, group_col, dots) {
  list(
    exp = exp,
    group_col = group_col,
    dots = dots,
    plots = list(),
    tables = list(),
    meta = list(explanation = list(), steps = character(0)),
    data = list(),
    cache = list()
  )
}

#' Add a plot to context
#'
#' @param ctx Context list.
#' @param id Plot id.
#' @param p A ggplot object.
#' @param explanation Optional explanation string.
#'
#' @returns Updated context.
#' @noRd
ctx_add_plot <- function(ctx, id, p, explanation = NULL) {
  ctx$plots[[id]] <- p
  if (!is.null(explanation)) {
    ctx$meta$explanation[[paste0("plots$", id)]] <- explanation
  }
  ctx
}

#' Add a table to context
#'
#' @param ctx Context list.
#' @param id Table id.
#' @param t A tibble/data.frame.
#' @param explanation Optional explanation string.
#'
#' @returns Updated context.
#' @noRd
ctx_add_table <- function(ctx, id, t, explanation = NULL) {
  ctx$tables[[id]] <- t
  if (!is.null(explanation)) {
    ctx$meta$explanation[[paste0("tables$", id)]] <- explanation
  }
  ctx
}

#' Run a list of steps
#'
#' @param ctx Context list.
#' @param steps A list of `glysmith_step`.
#' @param quiet Whether to suppress progress output.
#'
#' @returns Updated context.
#' @noRd
run_blueprint <- function(ctx, steps, quiet = FALSE) {
  for (s in steps) {
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
