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

#' Get step-scoped arguments from forge_analysis dots
#'
#' Extract arguments from `rlang::list2(...)` by step-scoped prefix.
#' For example, `preprocess.glyclean.auto_clean.batch_col = "x"` will be picked up
#' when `step_id = "preprocess"`, `pkg = "glyclean"`, and `func = "auto_clean"`.
#'
#' @param step_id Step id.
#' @param pkg The package name.
#' @param func The function name.
#' @param dots The list of arguments from `forge_analysis(...)`.
#'
#' @returns A list of arguments.
#' @noRd
.get_step_args <- function(step_id, pkg, func, dots) {
  prefix <- paste0(step_id, ".", pkg, ".", func, ".")
  if (is.null(names(dots))) return(list())
  idx <- startsWith(names(dots), prefix)
  if (!any(idx)) return(list())
  out <- dots[idx]
  names(out) <- substring(names(out), nchar(prefix) + 1)
  out
}

#' Collect arguments for a function call from global and step dots
#'
#' `global_dots` comes from `forge_analysis(...)` using keys like
#' `step_id.pkg.func.arg` (step-scoped). `step_dots` comes from step construction
#' using keys like `pkg.func.arg` (step-local). Global dots take precedence.
#'
#' @param pkg The package name.
#' @param func The function name.
#' @param step_id The step id.
#' @param global_dots Dots from `forge_analysis(...)`.
#' @param step_dots Dots from step construction.
#' @param holy_args A list of arguments that overwrite global and step dots.
#'
#' @returns A list of arguments to splice into `rlang::exec()`.
#' @noRd
.collect_step_dots <- function(pkg, func, step_id, global_dots, step_dots, holy_args) {
  args_step <- .get_args(pkg, func, step_dots)
  args_global <- .get_step_args(step_id, pkg, func, global_dots)
  args <- utils::modifyList(args_step, args_global)
  if (any(names(holy_args) %in% names(args))) {
    conflict_args <- names(holy_args)[names(holy_args) %in% names(args)]
    cli::cli_alert_warning(c(
      "Arguments {.arg {conflict_args}} of {.fn {func}} are ignored for step {.val {step_id}}.",
      "i" = "These arguments are controlled by glysmith and cannot be overwritten."
    ))
  }
  args <- utils::modifyList(args, holy_args)
  args
}

#' Run a glycoverse function with arguments from dots
#'
#' The function is a syntactic sugar for `rlang::exec(f, x, !!!.get_args(pkg, func, dots))`,
#' with special handling for group column specification argument.
#' For example, `.run_function(pkg::f, x, dots)`
#' is the same as `rlang::exec(pkg::f, x, !!!.get_args(pkg, func, dots))`.
#'
#' @param f The function to run.
#' @param x The first argument to the function.
#' @param step_id The step id.
#' @param global_dots The list of arguments from `forge_analysis(...)`.
#' @param step_dots The list of arguments from step construction.
#' @param holy_args A list of arguments that overwrite global and step dots.
#'   This argument is used by internal functions to force some arguments.
#'   Default is an empty list.
#'
#' @returns The result of the function.
#' @noRd
.run_function <- function(f, x, step_id, global_dots, step_dots, holy_args = list()) {
  f_str <- rlang::as_label(rlang::enexpr(f))
  pkg <- stringr::str_split_i(f_str, stringr::fixed("::"), 1)
  func <- stringr::str_split_i(f_str, stringr::fixed("::"), 2)
  args <- .collect_step_dots(
    pkg, func, step_id,
    global_dots = global_dots, step_dots = step_dots, holy_args = holy_args
  )
  rlang::exec(f, x, !!!args)
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