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

#' Get Plots or Tables from GlySmith Result
#'
#' Helper functions to get processed experiment, plots or tables from a glysmith result object.
#' Just a syntax sugar for `$exp`, `$plots$plot_name` and `$tables$table_name` 
#' elements of a glysmith result object, respectively.
#'
#' @param x A glysmith result object.
#' @param name The name of the plot or table to get.
#'
#' @returns A glyexp_experiment object, a ggplot object or a tibble.
#' @examples
#' library(glyexp)
#' exp <- real_experiment2
#' result <- forge_analysis(exp)
#' cast_exp(result)
#' cast_table(result, "summary")
#'
#' @export
cast_exp <- function(x) {
  checkmate::assert_class(x, "glysmith_result")
  x$exp
}

#' @rdname cast_exp
#' @export
cast_plot <- function(x, name) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name)
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
cast_table <- function(x, name) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name)
  if (!name %in% names(x$tables)) {
    cli::cli_abort(c(
      "Table '{name}' not found in the result.",
      "i" = "Available tables: {.val {names(x$tables)}}"
    ))
  }
  x$tables[[name]]
}