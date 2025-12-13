#' Render a HTML Report for GlySmith Results
#'
#' Generate a self-contained HTML report for a `glysmith_result` object.
#' The report is rendered via `rmarkdown::render()` using an internal R Markdown template.
#'
#' @param x A `glysmith_result` object.
#' @param output_file Path to the output HTML file.
#' @param title Report title.
#' @param open Whether to open the report in a browser after rendering.
#'
#' @returns The normalized path to the generated HTML file.
#' @examples
#' library(glyexp)
#' exp <- real_experiment2
#' result <- forge_analysis(exp)
#' polish_report(result, tempfile(fileext = ".html"), open = FALSE)
#'
#' @export
polish_report <- function(
  x,
  output_file,
  title = "GlySmith report",
  open = interactive()
) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(output_file)
  checkmate::assert_string(title)
  checkmate::assert_flag(open)

  template <- system.file("templates", "polish_report.Rmd", package = "glysmith")
  out_dir <- fs::path_dir(output_file)
  if (!fs::dir_exists(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
  }
  if (fs::file_exists(output_file)) {
    ans <- .ask_overwrite_file()
    ans <- tolower(trimws(ans))
    if (ans == "n") {
      cli::cli_abort("Operation cancelled. Report not saved.")
    } else if (ans == "y" || ans == "") {
      fs::file_delete(output_file)
    } else {
      cli::cli_abort("Invalid input. Operation cancelled. Report not saved.")
    }
  }

  tmp_dir <- tempfile("glysmith-report-render-")
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  tmp_rmd <- fs::path(tmp_dir, "polish_report.Rmd")
  fs::file_copy(template, tmp_rmd, overwrite = TRUE)

  step_reports <- .build_step_reports(x)

  rendered <- rmarkdown::render(
    input = tmp_rmd,
    output_file = fs::path_file(output_file),
    output_dir = out_dir,
    params = list(
      x = x,
      title = title,
      step_reports = step_reports
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  rendered <- normalizePath(rendered, winslash = "/", mustWork = TRUE)
  if (isTRUE(open)) utils::browseURL(rendered)
  rendered
}

# Build an ordered list of step report entries for a glysmith_result.
# Each entry is a list(id, label, content), where content is a string (markdown) or NULL.
# @noRd
.build_step_reports <- function(x) {
  steps_executed <- character(0)
  if (!is.null(x$meta) && is.list(x$meta) && !is.null(x$meta$steps)) {
    steps_executed <- x$meta$steps
  }

  step_map <- rlang::set_names(x$blueprint, purrr::map_chr(x$blueprint, "id"))
  ids <- steps_executed

  purrr::map(ids, function(id) {
    s <- step_map[[id]]
    if (is.null(s)) {
      return(list(id = id, label = id, content = NULL))
    }
    content <- NULL
    if (is.function(s$report)) {
      content <- tryCatch(
        s$report(x),
        error = function(e) {
          paste0(
            "Report generation failed for step `", id, "`: ",
            conditionMessage(e)
          )
        }
      )
    }
    list(id = s$id, label = s$label, content = content)
  })
}
