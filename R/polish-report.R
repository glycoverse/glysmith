#' Render a HTML Report for GlySmith Results
#'
#' Generate a self-contained HTML report for a `glysmith_result` object.
#' The report is rendered via `rmarkdown::render()` using an internal R Markdown template.
#' If `use_ai` is TRUE, the report text will be polished and organized into sections using LLM (deepseek-chat).
#' To use this feature, you have to provide an API key and set it in the environment variable `DEEPSEEK_API_KEY`
#' by running `Sys.setenv(DEEPSEEK_API_KEY = "your_api_key")`.
#' You can apply the API key on https://platform.deepseek.com.
#'
#' @param x A `glysmith_result` object.
#' @param output_file Path to the output HTML file.
#' @param title Report title.
#' @param open Whether to open the report in a browser after rendering.
#' @param use_ai Whether to polish the report text and organize sections using AI (deepseek-chat). Default is FALSE.
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
  use_ai = FALSE
) {
  checkmate::assert_class(x, "glysmith_result")
  checkmate::assert_string(output_file)
  checkmate::assert_string(title)
  checkmate::assert_flag(open)
  checkmate::assert_flag(use_ai)

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

  report_sections <- .build_report_sections(x, use_ai = use_ai)

  rendered <- rmarkdown::render(
    input = tmp_rmd,
    output_file = fs::path_file(output_file),
    output_dir = out_dir,
    params = list(
      x = x,
      title = title,
      report_sections = report_sections
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  rendered <- normalizePath(rendered, winslash = "/", mustWork = TRUE)
  if (isTRUE(open)) utils::browseURL(rendered)
  rendered
}

#' Build an ordered list of step report entries for a glysmith_result.
#'
#' Each entry is a list(id, label, content), where content is a string (markdown) or NULL.
#'
#' @param x A glysmith_result object.
#' @param use_ai Whether to polish content using AI.
#' @noRd
.build_step_reports <- function(x, use_ai = FALSE, api_key = NULL) {
  steps_executed <- character(0)
  if (!is.null(x$meta) && is.list(x$meta) && !is.null(x$meta$steps)) {
    steps_executed <- x$meta$steps
  }

  step_map <- x$blueprint
  ids <- steps_executed

  if (isTRUE(use_ai)) {
    if (is.null(api_key)) {
      api_key <- .get_api_key()
    }
    cli::cli_alert_info("Polishing report with AI (deepseek-chat)...")
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
    if (isTRUE(use_ai)) {
      content <- .polish_text(content, api_key)
    } else {
      content <- stringr::str_remove_all(content, "<AI>[\\s\\S]*?</AI>")
    }

    list(id = s$id, label = s$label, content = content)
  })
}

#' Build report sections with optional AI organization.
#'
#' Each section includes ordered entries (step reports and plots).
#'
#' @param x A glysmith_result object.
#' @param use_ai Whether to organize sections with AI.
#' @noRd
.build_report_sections <- function(x, use_ai = FALSE) {
  api_key <- NULL
  if (isTRUE(use_ai)) {
    api_key <- .get_api_key()
  }

  step_reports <- .build_step_reports(x, use_ai = use_ai, api_key = api_key)
  plot_entries <- .build_plot_entries(x)

  section_plan <- NULL
  if (isTRUE(use_ai)) {
    section_plan <- .organize_report_sections(step_reports, plot_entries, api_key)
  }

  if (!is.null(section_plan)) {
    return(.assemble_report_sections(step_reports, plot_entries, section_plan))
  }

  .default_report_sections(step_reports, plot_entries)
}

#' Build a list of plot entries for reporting.
#'
#' @param x A glysmith_result object.
#' @noRd
.build_plot_entries <- function(x) {
  plots <- x$plots
  if (is.null(plots)) {
    return(list())
  }

  explanation <- NULL
  if (!is.null(x$meta) && is.list(x$meta)) {
    explanation <- x$meta$explanation
  }

  purrr::imap(plots, function(p, id) {
    desc <- NULL
    key <- paste0("plots$", id)
    if (!is.null(explanation)) {
      if (is.list(explanation) && key %in% names(explanation)) {
        desc <- explanation[[key]]
      } else if (!is.list(explanation) && key %in% names(explanation)) {
        val <- explanation[key]
        if (length(val) == 1 && !is.na(val) && nzchar(val)) {
          desc <- unname(val)
        }
      }
    }
    if (!is.null(desc) && is.na(desc)) {
      desc <- NULL
    }
    if (!is.null(desc) && !nzchar(desc)) {
      desc <- NULL
    }
    list(id = id, label = id, description = desc, plot = p)
  })
}

#' Organize report sections with AI.
#'
#' @param step_reports Step report entries.
#' @param plot_entries Plot entries.
#' @param api_key API key for the LLM.
#' @param model AI model to use.
#' @noRd
.organize_report_sections <- function(step_reports, plot_entries, api_key, model = "deepseek-chat") {
  if (length(step_reports) == 0 && length(plot_entries) == 0) {
    return(NULL)
  }

  step_lines <- purrr::map_chr(step_reports, function(step) {
    label <- step$label %||% step$id
    paste0("- ", step$id, " | ", label)
  })
  plot_lines <- purrr::map_chr(plot_entries, function(plot) {
    desc <- plot$description
    if (is.null(desc) || !nzchar(desc)) {
      desc <- plot$label %||% plot$id
    }
    paste0("- ", plot$id, " | ", desc)
  })

  user_prompt <- paste(
    "Steps (in execution order):",
    if (length(step_lines) > 0) paste(step_lines, collapse = "\n") else "(none)",
    "",
    "Plots:",
    if (length(plot_lines) > 0) paste(plot_lines, collapse = "\n") else "(none)",
    "",
    "Return format (no code fences):",
    "## <Section title>",
    "items: step:<id>; plot:<id>; step:<id>",
    "",
    "Rules:",
    "- Use only the provided ids.",
    "- Keep order consistent with execution order.",
    "- Group plot-making steps with the analysis they support.",
    "- Avoid a separate 'Plots' section unless nothing else fits.",
    "- Include every step and plot exactly once.",
    sep = "\n"
  )

  system_prompt <- paste(
    "You are a scientific report section organizer.",
    "Your task is to group analysis steps into coherent sections",
    "and insert plots where they fit best.",
    "Keep section titles short and in Title Case.",
    "Output only the requested format.",
    sep = " "
  )

  output <- tryCatch(
    .ask_ai(system_prompt, user_prompt, api_key, model),
    error = function(e) {
      cli::cli_warn(c(
        "AI section organization failed, using default order.",
        "i" = "Error: {conditionMessage(e)}"
      ))
      NULL
    }
  )

  if (is.null(output)) {
    return(NULL)
  }

  plan <- .parse_section_plan(output)
  if (is.null(plan) || length(plan) == 0) {
    cli::cli_warn("AI section organization returned an invalid plan; using default order.")
    return(NULL)
  }
  plan
}

#' Parse AI output into a section plan.
#'
#' @param output AI output string.
#' @noRd
.parse_section_plan <- function(output) {
  if (is.null(output) || !nzchar(output)) {
    return(NULL)
  }

  output_clean <- stringr::str_remove_all(output, "```[a-zA-Z]*|```")
  lines <- stringr::str_split_1(output_clean, "\n")
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]
  if (length(lines) == 0) {
    return(NULL)
  }

  sections <- list()
  current <- NULL

  for (line in lines) {
    if (stringr::str_starts(line, "## ")) {
      if (!is.null(current)) {
        sections <- append(sections, list(current))
      }
      title <- stringr::str_remove(line, "^##\\s+")
      title <- stringr::str_trim(title)
      current <- list(title = title, items = list())
      next
    }

    if (is.null(current)) {
      next
    }

    if (stringr::str_starts(stringr::str_to_lower(line), "items:")) {
      raw_items <- stringr::str_remove(line, "^items:\\s*")
      items <- .parse_section_items(raw_items)
      current$items <- items
    }
  }

  if (!is.null(current)) {
    sections <- append(sections, list(current))
  }

  if (length(sections) == 0) {
    return(NULL)
  }
  sections
}

.parse_section_items <- function(raw_items) {
  raw_items <- stringr::str_trim(raw_items)
  if (!nzchar(raw_items)) {
    return(list())
  }
  if (stringr::str_to_lower(raw_items) %in% c("none", "n/a", "na")) {
    return(list())
  }

  parts <- stringr::str_split_1(raw_items, ";")
  parts <- stringr::str_trim(parts)
  parts <- parts[parts != ""]

  items <- purrr::map(parts, function(part) {
    if (!stringr::str_detect(part, ":")) {
      return(NULL)
    }
    type <- stringr::str_extract(part, "^[^:]+")
    id <- stringr::str_remove(part, "^[^:]+:")
    type <- stringr::str_to_lower(stringr::str_trim(type))
    id <- stringr::str_trim(id)
    if (!type %in% c("step", "plot") || !nzchar(id)) {
      return(NULL)
    }
    list(type = type, id = id)
  })

  purrr::compact(items)
}

#' Assemble report sections from a plan.
#'
#' @param step_reports Step report entries.
#' @param plot_entries Plot entries.
#' @param section_plan Parsed section plan.
#' @noRd
.assemble_report_sections <- function(step_reports, plot_entries, section_plan) {
  step_ids <- purrr::map_chr(step_reports, "id")
  plot_ids <- purrr::map_chr(plot_entries, "id")
  step_map <- rlang::set_names(step_reports, step_ids)
  plot_map <- rlang::set_names(plot_entries, plot_ids)

  used_steps <- character(0)
  used_plots <- character(0)
  sections <- list()

  for (section in section_plan) {
    title <- section$title %||% "Analysis"
    items <- section$items %||% list()
    entries <- list()

    for (item in items) {
      if (!is.list(item) || is.null(item$type) || is.null(item$id)) {
        next
      }
      if (item$type == "step" && item$id %in% names(step_map) && !item$id %in% used_steps) {
        step <- step_map[[item$id]]
        entries <- append(entries, list(.step_entry(step)))
        used_steps <- c(used_steps, item$id)
      }
      if (item$type == "plot" && item$id %in% names(plot_map) && !item$id %in% used_plots) {
        plot <- plot_map[[item$id]]
        entries <- append(entries, list(.plot_entry(plot)))
        used_plots <- c(used_plots, item$id)
      }
    }

    sections <- append(sections, list(list(title = title, entries = entries)))
  }

  remaining_steps <- step_ids[!step_ids %in% used_steps]
  remaining_plots <- plot_ids[!plot_ids %in% used_plots]
  if (length(remaining_steps) > 0 || length(remaining_plots) > 0) {
    entries <- list()
    for (step_id in remaining_steps) {
      entries <- append(entries, list(.step_entry(step_map[[step_id]])))
    }
    for (plot_id in remaining_plots) {
      entries <- append(entries, list(.plot_entry(plot_map[[plot_id]])))
    }
    sections <- append(sections, list(list(title = "Additional results", entries = entries)))
  }

  sections
}

#' Default report sections when AI is not used.
#'
#' @param step_reports Step report entries.
#' @param plot_entries Plot entries.
#' @noRd
.default_report_sections <- function(step_reports, plot_entries) {
  sections <- list()
  if (length(step_reports) > 0) {
    entries <- purrr::map(step_reports, .step_entry)
    sections <- append(sections, list(list(title = "Analysis narrative", entries = entries)))
  }
  if (length(plot_entries) > 0) {
    entries <- purrr::map(plot_entries, .plot_entry)
    sections <- append(sections, list(list(title = "Plots", entries = entries)))
  }
  sections
}

.step_entry <- function(step) {
  list(
    type = "step",
    id = step$id,
    label = step$label %||% step$id,
    content = step$content
  )
}

.plot_entry <- function(plot) {
  list(
    type = "plot",
    id = plot$id,
    label = plot$label %||% plot$id,
    description = plot$description,
    plot = plot$plot
  )
}

#' Polish text using AI
#'
#' Internal helper function to polish text using an LLM.
#'
#' @param text The text to polish.
#' @param api_key The API key for the LLM.
#' @param model The AI model to use.
#' @return The polished text, or original text if polishing fails.
#' @noRd
.polish_text <- function(text, api_key, model = "deepseek-chat") {
  if (is.null(text) || !nzchar(text)) {
    return(text)
  }

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
    .ask_ai(system_prompt, text, api_key, model),
    error = function(e) {
      cli::cli_warn(c(
        "AI polishing failed, using original text.",
        "i" = "Error: {conditionMessage(e)}"
      ))
      text
    }
  )
}
