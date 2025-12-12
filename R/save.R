#' Save GlySmith Result
#'
#' Save processed experiment, plots and tables of a glysmith result object to a directory.
#'
#' @param x A glysmith result object.
#' @param dir The directory to save the result.
#' @param plot_ext The extension of the plot files. Either "pdf", "png" or "svg". Default is "pdf".
#' @param table_ext The extension of the table files. Either "csv" or "tsv". Default is "csv".
#' @param plot_width The width of the plot in inches. Default is 5.
#' @param plot_height The height of the plot in inches. Default is 5.
#'
#' @returns A glysmith result object.
#' @examples
#' library(glyexp)
#' exp <- real_experiment
#' result <- forge_analysis(exp)
#' save_result(result, tempdir())
#'
#' @export
quench_result <- function(x, dir, plot_ext = "pdf", table_ext = "csv", plot_width = 5, plot_height = 5) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(dir)
  checkmate::assert_choice(plot_ext, c("pdf", "png", "svg"))
  checkmate::assert_choice(table_ext, c("csv", "tsv"))

  if (fs::dir_exists(dir)) {
    cli::cli_alert_info("Directory already exists. Overwrite? [y/N]")
    ans <- readline()
    ans <- tolower(ans)
    if (ans == "n") {
      cli::cli_abort("Operation cancelled. Result not saved.")
    } else if (ans == "y" || ans == "") {
      fs::dir_delete(dir)
    } else {
      cli::cli_abort("Invalid input. Operation cancelled. Result not saved.")
    }
  }

  fs::dir_create(dir, recurse = TRUE)
  fs::dir_create(fs::path(dir, "plots"))
  fs::dir_create(fs::path(dir, "tables"))

  for (plot in names(x$plots)) {
    file_path <- fs::path(dir, "plots", paste0(plot, ".", plot_ext))
    ggplot2::ggsave(file_path, cast_plot(x, plot), width = plot_width, height = plot_height)
  }

  for (table in names(x$tables)) {
    file_path <- fs::path(dir, "tables", paste0(table, ".", table_ext))
    writer <- switch(table_ext, "csv" = readr::write_csv, "tsv" = readr::write_tsv)
    writer(cast_table(x, table), file_path)
  }

  readr::write_rds(x$exp, fs::path(dir, "experiment.rds"))
  readr::write_rds(x$meta, fs::path(dir, "meta.rds"))

  cli::cli_alert_success("Result saved to {.path {dir}}")
}