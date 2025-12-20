#' Render a HTML Report for GlySmith Results
#'
#' Generate a self-contained HTML report for a `glysmith_result` object.
#' The report is rendered via `rmarkdown::render()` using an internal R Markdown template.
#'
#' @param x A `glysmith_result` object.
#' @param output_file Path to the output HTML file.
#' @param title Report title.
#' @param open Whether to open the report in a browser after rendering.
#' @param ai_polish Whether to polish the report text using AI (deepseek-chat). Default is FALSE.
#' @param ai_api_key API key for the AI model. If NULL, uses the environment
#'   variable `DEEPSEEK_API_KEY`.
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
  open = interactive(),
  ai_polish = FALSE,
  ai_api_key = NULL
) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(output_file)
  checkmate::assert_string(title)
  checkmate::assert_flag(open)
  checkmate::assert_flag(ai_polish)
  checkmate::assert_string(ai_api_key, null.ok = TRUE)

  template <- system.file("templates", "polish_report.Rmd", package = "glysmith")
  out_dir <- fs::path_dir(output_file)
  if (!fs::dir_exists(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
  }
  if (fs::file_exists(output_file)) {
    ans <- .ask_overwrite_file()
    ans <- tolower(trimws(ans))
    if (ans == "n" || ans == "") {
      cli::cli_abort("Operation cancelled. Report not saved.")
    } else if (ans == "y") {
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

  step_reports <- .build_step_reports(
    x,
    ai_polish = ai_polish,
    ai_api_key = ai_api_key
  )

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

#' Polish text using AI
#'
#' Internal helper function to polish text using an LLM.
#'
#' @param text The text to polish.
#' @param model The AI model to use.
#' @param api_key The API key. If NULL, uses environment variable.
#' @return The polished text, or original text if polishing fails.
#' @noRd
.polish_text <- function(text, model = "deepseek-chat", api_key = NULL) {
  if (is.null(text) || !nzchar(text)) {
    return(text)
  }

  rlang::check_installed("ellmer", reason = "for AI polishing")

  system_prompt <- paste(
    "You are a scientific writing assistant.",
    "Your task is to polish the following text to be more professional,",
    "fluent, and suitable for a scientific report.",
    "Keep the original meaning and content intact.",
    "Do not add new information or change the facts.",
    "Output only the polished text, without any explanation or preamble.",
    "The text is in markdown format.",
    "Remove meaningless logging messages like 'Normalization completed'.",
    "Format the text to be one paragraph.",
    "Content in <AI></AI> tags should also be polished and included in the result."
  )

  tryCatch(
    {
      credentials <- if (!is.null(api_key)) function() api_key else NULL
      chat <- ellmer::chat_deepseek(
        system_prompt = system_prompt,
        model = model,
        credentials = credentials,
        echo = "none"
      )
      polished <- chat$chat(text)
      polished
    },
    error = function(e) {
      cli::cli_warn(c(
        "AI polishing failed, using original text.",
        "i" = "Error: {conditionMessage(e)}"
      ))
      text
    }
  )
}

# Build an ordered list of step report entries for a glysmith_result.
# Each entry is a list(id, label, content), where content is a string (markdown) or NULL.
# @param x A glysmith_result object.
# @param ai_polish Whether to polish content using AI.
# @param ai_api_key The API key for the AI model.
# @noRd
.build_step_reports <- function(
  x,
  ai_polish = FALSE,
  ai_api_key = NULL
) {
  steps_executed <- character(0)
  if (!is.null(x$meta) && is.list(x$meta) && !is.null(x$meta$steps)) {
    steps_executed <- x$meta$steps
  }

  step_map <- x$blueprint
  ids <- steps_executed

  if (isTRUE(ai_polish)) {
    # Check for API key early to avoid repeated warnings
    api_key_available <- !is.null(ai_api_key) || nzchar(Sys.getenv("DEEPSEEK_API_KEY"))
    if (!api_key_available) {
      cli::cli_warn(c(
        "AI polishing is enabled but no API key is available.",
        "i" = "Set {.envvar DEEPSEEK_API_KEY} or provide {.arg ai_api_key}.",
        "i" = "Proceeding without AI polishing."
      ))
      ai_polish <- FALSE
    } else {
      cli::cli_alert_info("Polishing report with AI (deepseek-chat)...")
    }
  }

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

    # Apply AI polishing if enabled and content is not empty
    if (isTRUE(ai_polish)) {
      content <- .polish_text(content, model = "deepseek-chat", api_key = ai_api_key)
    } else {
      content <- stringr::str_remove_all(content, "<AI>[\\s\\S]*?</AI>")
    }

    list(id = s$id, label = s$label, content = content)
  })
}
