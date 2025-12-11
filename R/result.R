glysmith_result <- function(exp, plots, tables, meta) {
  structure(
    list(exp = exp, plots = plots, tables = tables, meta = meta),
    class = "glysmith_result"
  )
}

#' @export
print.glysmith_result <- function(x, ...) {
  cli::cli_h3("GlySmith Analysis Result")
  cli::cli_text("Plots: {.val {length(x$plots)}}, Tables: {.val {length(x$tables)}}")
  invisible(x)
}