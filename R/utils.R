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