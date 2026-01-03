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
  cast_component(x, "plots", name, "Plot")
}

#' @rdname cast_exp
#' @export
cast_table <- function(x, name = NULL) {
  cast_component(x, "tables", name, "Table")
}

#' @rdname cast_exp
#' @export
cast_data <- function(x, name = NULL) {
  cast_component(x, "data", name, "Data")
}

cast_component <- function(x, component, name, label, call = rlang::caller_env()) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(name, null.ok = TRUE)

  items <- x[[component]]

  if (is.null(name)) {
    return(names(items))
  }

  if (!name %in% names(items)) {
    cli::cli_abort(c(
      "{label} '{name}' not found in the result.",
      "i" = "Available {component}: {.val {names(items)}}"
    ), call = call)
  }

  items[[name]]
}