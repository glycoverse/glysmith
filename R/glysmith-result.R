glysmith_result <- function(exp, data, plots, tables, meta, blueprint) {
  structure(
    list(exp = exp, data = data, plots = plots, tables = tables, meta = meta, blueprint = blueprint),
    class = "glysmith_result"
  )
}

#' @export
print.glysmith_result <- function(x, ...) {
  cli::cli_h3("GlySmith Analysis Result")
  cli::cli_text("Plots: {.val {length(x$plots)}}, Tables: {.val {length(x$tables)}}, Data: {.val {length(x$data)}}")
  invisible(x)
}

#' Get Data from GlySmith Result
#'
#' Helper functions to get processed experiment, plots, tables or data from a glysmith result object.
#'
#' @param x A glysmith result object.
#' @param name The name of the plot or table to get. If not specified, return available names.
#'
#' @returns
#'   - `cast_exp()`: a [glyexp::experiment()].
#'   - `cast_plot()`: a [ggplot2::ggplot()].
#'   - `cast_table()`: a [tibble::tibble()].
#'   - `cast_data()`: can be any R object.
#' @examples
#' library(glyexp)
#' exp <- real_experiment2
#' result <- forge_analysis(exp)
#' cast_exp(result)
#' cast_table(result)
#' cast_table(result, "summary")
#'
#' @export
cast_exp <- function(x) {
  checkmate::assert_class(x, "glysmith_result")
  x$exp
}

#' @rdname cast_exp
#' @export
cast_plot <- function(x, name = NULL) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name, null.ok = TRUE)
  if (is.null(name)) {
    return(names(x$plots))
  }
  if (!name %in% names(x$plots)) {
    cli::cli_abort(c(
      "Plot '{name}' not found in the result.",
      "i" = "Available plots: {.val {names(x$plots)}}"
    ))
  }
  x$plots[[name]]
}

#' @rdname cast_exp
#' @export
cast_table <- function(x, name = NULL) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name, null.ok = TRUE)
  if (is.null(name)) {
    return(names(x$tables))
  }
  if (!name %in% names(x$tables)) {
    cli::cli_abort(c(
      "Table '{name}' not found in the result.",
      "i" = "Available tables: {.val {names(x$tables)}}"
    ))
  }
  x$tables[[name]]
}

#' @rdname cast_exp
#' @export
cast_data <- function(x, name = NULL) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name, null.ok = TRUE)
  if (is.null(name)) {
    return(names(x$data))
  }
  if (!name %in% names(x$data)) {
    cli::cli_abort(c(
      "Data '{name}' not found in the result.",
      "i" = "Available data: {.val {names(x$data)}}"
    ))
  }
  x$data[[name]]
}