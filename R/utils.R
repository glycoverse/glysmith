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

#' Collect dots for a step
#'
#' Combine step-level dots (from blueprint construction) and dots passed through
#' `forge_analysis()` using the `step_id.pkg.fun.arg` convention. The latter has
#' higher priority when both are supplied.
#'
#' @param step_id Step identifier.
#' @param ctx_dots A dots list captured by `forge_analysis()`.
#' @param step_dots A dots list captured by the step constructor.
#'
#' @returns A merged dots list for the step.
#' @noRd
.collect_step_dots <- function(step_id, ctx_dots, step_dots = list()) {
  ctx_dots <- ctx_dots %||% list()
  step_dots <- step_dots %||% list()

  step_prefix <- paste0(step_id, ".")
  from_forge <- ctx_dots[startsWith(names(ctx_dots), step_prefix)]
  names(from_forge) <- substring(names(from_forge), nchar(step_prefix) + 1)

  ctx_general <- ctx_dots[!startsWith(names(ctx_dots), step_prefix)]

  merged <- utils::modifyList(step_dots, ctx_general)
  utils::modifyList(merged, from_forge)
}

#' Run a glycoverse function with arguments from dots
#'
#' The function is a syntactic sugar for `rlang::exec(f, exp, !!!.get_args(pkg, func, dots))`,
#' with special handling for group column specification argument.
#' For example, `.run_function(pkg::f, exp, dots)`
#' is the same as `rlang::exec(pkg::f, exp, !!!.get_args(pkg, func, dots))`.
#'
#' @param f The function to run.
#' @param exp The experiment object.
#' @param dots The list of arguments.
#'
#' @returns The result of the function.
#' @noRd
.run_function <- function(f, exp, dots) {
  f_str <- rlang::as_label(rlang::enexpr(f))
  pkg <- stringr::str_split_i(f_str, stringr::fixed("::"), 1)
  func <- stringr::str_split_i(f_str, stringr::fixed("::"), 2)
  args <- .get_args(pkg, func, dots)
  rlang::exec(f, exp, !!!args)
}

#' Ask if user wants to overwrite an existing directory
#'
#' This helper exists to keep the prompt and input on the same line (via
#' `readline(prompt = ...)`) and to make the interactive behavior testable.
#'
#' @returns User input string.
#' @noRd
.ask_overwrite_dir <- function() {
  # Use readline prompt to keep user input on the same line.
  prompt <- paste0("\u2139 ", "Directory already exists. Overwrite? [y/N] ")
  readline(prompt = prompt)
}

#' Ask if user wants to overwrite an existing file
#'
#' This helper exists to keep the prompt and input on the same line (via
#' `readline(prompt = ...)`) and to make the interactive behavior testable.
#'
#' @returns User input string.
#' @noRd
.ask_overwrite_file <- function() {
  # Use readline prompt to keep user input on the same line.
  prompt <- paste0("\u2139 ", "File already exists. Overwrite? [y/N] ")
  readline(prompt = prompt)
}