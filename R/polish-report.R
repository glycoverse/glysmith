#' Render a HTML Report for GlySmith Results
#'
#' Generate a self-contained HTML report for a `glysmith_result` object.
#' The report is rendered via `rmarkdown::render()` using an internal R Markdown template.
#' If `use_ai` is TRUE, the report text will be polished and organized into sections using LLM (deepseek-chat),
#' and plots will be described with a multimodal model (deepseek-vl-chat).
#' To use this feature, you have to provide an API key and set it in the environment variable `DEEPSEEK_API_KEY`
#' by running `Sys.setenv(DEEPSEEK_API_KEY = "your_api_key")`.
#' You can apply the API key on https://platform.deepseek.com.
#'
#' @param x A `glysmith_result` object.
#' @param output_file Path to the output HTML file.
#' @param title Report title.
#' @param open Whether to open the report in a browser after rendering.
#' @param use_ai Whether to polish the report text, organize sections, and generate plot descriptions using AI
#'   (deepseek-chat and deepseek-vision). Default is FALSE.
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

  template <- system.file(
    "templates",
    "polish_report.Rmd",
    package = "glysmith"
  )
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
  if (isTRUE(open)) {
    utils::browseURL(rendered)
  }
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
    cli::cli_alert_info("Polishing report text with AI (deepseek-chat)...")
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
            "Report generation failed for step `",
            id,
            "`: ",
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
  plot_entries <- .build_plot_entries(x, use_ai = use_ai, api_key = api_key)

  section_plan <- NULL
  if (isTRUE(use_ai)) {
    section_plan <- .organize_report_sections(
      step_reports,
      plot_entries,
      api_key
    )
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
.build_plot_entries <- function(x, use_ai = FALSE, api_key = NULL) {
  plots <- x$plots
  if (is.null(plots)) {
    return(list())
  }

  explanation <- NULL
  if (!is.null(x$meta) && is.list(x$meta)) {
    explanation <- x$meta$explanation
  }

  if (isTRUE(use_ai) && is.null(api_key)) {
    api_key <- .get_api_key()
  }

  if (isTRUE(use_ai) && length(plots) > 0) {
    cli::cli_alert_info("Generating AI plot descriptions (deepseek-vl-chat)...")
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
    label <- .humanize_plot_label(id, desc)

    # Handle new format (list with plot, width, height) vs legacy format (just plot)
    plot_obj <- p
    plot <- NULL
    width <- NULL
    height <- NULL
    if (is.list(p) && !is.null(p$plot)) {
      plot <- p$plot
      width <- p$width
      height <- p$height
    } else {
      plot <- p
    }

    if (isTRUE(use_ai)) {
      desc <- .describe_plot_ai(
        plot,
        label,
        desc,
        api_key,
        width = width,
        height = height
      )
    }
    desc <- .humanize_plot_description(desc)
    label <- .humanize_plot_label(id, desc)
    list(id = id, label = label, description = desc, plot = plot)
  })
}

#' Human-friendly plot labels
#'
#' @param id Plot id.
#' @param description Optional plot description.
#' @noRd
.humanize_plot_label <- function(id, description = NULL) {
  if (is.null(id) || !nzchar(id)) {
    return(id)
  }

  id <- stringr::str_trim(id)
  on_labels <- .on_suffix_labels()

  if (id == "volcano") {
    return("Volcano plot")
  }
  if (stringr::str_starts(id, "volcano_")) {
    contrast <- stringr::str_remove(id, "^volcano_")
    contrast <- stringr::str_replace_all(contrast, "_vs_", " vs ")
    contrast <- stringr::str_replace_all(contrast, "_", " ")
    contrast <- stringr::str_squish(contrast)
    return(paste0("Volcano plot: ", contrast))
  }

  if (stringr::str_starts(id, "heatmap")) {
    parts <- stringr::str_split_1(id, "_")
    suffix <- .extract_on_suffix(parts[-1])$suffix
    if (!is.null(suffix)) {
      return(paste("Heatmap of", on_labels[[suffix]]))
    }
    return("Heatmap")
  }

  if (stringr::str_starts(id, "pca")) {
    parts <- stringr::str_split_1(id, "_")
    info <- .extract_on_suffix(parts[-1])
    suffix <- info$suffix
    rest <- info$rest
    label <- "PCA"
    if (length(rest) > 0) {
      label <- paste(label, stringr::str_to_title(paste(rest, collapse = " ")))
    }
    if (!is.null(suffix)) {
      label <- paste0(label, " (", on_labels[[suffix]], ")")
    }
    return(.fix_plot_abbrev(label))
  }

  if (stringr::str_starts(id, "tsne")) {
    parts <- stringr::str_split_1(id, "_")
    info <- .extract_on_suffix(parts[-1])
    suffix <- info$suffix
    label <- "t-SNE"
    if (!is.null(suffix)) {
      label <- paste0(label, " (", on_labels[[suffix]], ")")
    }
    return(label)
  }

  if (stringr::str_starts(id, "umap")) {
    parts <- stringr::str_split_1(id, "_")
    info <- .extract_on_suffix(parts[-1])
    suffix <- info$suffix
    label <- "UMAP"
    if (!is.null(suffix)) {
      label <- paste0(label, " (", on_labels[[suffix]], ")")
    }
    return(label)
  }

  label <- stringr::str_replace_all(id, "_", " ")
  label <- stringr::str_squish(label)
  label <- stringr::str_to_title(label)
  label <- .fix_plot_abbrev(label)
  label
}

.fix_plot_abbrev <- function(label) {
  replacements <- c(
    "Pca" = "PCA",
    "Umap" = "UMAP",
    "Tsne" = "t-SNE",
    "Dea" = "DEA",
    "Go" = "GO",
    "Kegg" = "KEGG",
    "Vs" = "vs",
    "Sig" = "Significant"
  )
  for (from in names(replacements)) {
    label <- stringr::str_replace_all(
      label,
      paste0("\\b", from, "\\b"),
      replacements[[from]]
    )
  }
  label
}

.humanize_plot_description <- function(description) {
  if (is.null(description) || !nzchar(description)) {
    return(description)
  }

  desc <- description
  desc <- stringr::str_replace_all(desc, "_vs_", " vs ")
  desc <- stringr::str_replace_all(
    desc,
    "\\b(sig_trait_exp|sig_dynamic_motif_exp|sig_branch_motif_exp|sig_exp|trait_exp|dynamic_motif_exp|branch_motif_exp|exp)\\b",
    function(x) .humanize_on_label(x)
  )
  desc <- stringr::str_replace_all(desc, "_", " ")
  desc <- stringr::str_squish(desc)
  desc <- .fix_plot_abbrev(desc)
  desc
}

.describe_plot_ai <- function(
  plot,
  label,
  description,
  api_key,
  width = NULL,
  height = NULL,
  model = "deepseek-chat"
) {
  if (is.null(plot)) {
    return(description)
  }

  system_prompt <- paste(
    "You are a scientific visualization assistant.",
    "Write a concise alt-text style description (1 paragraph, 2-4 sentences).",
    "Briefly mention the plot type and axes if they are visible.",
    "Summarize 2-5 major visual patterns or group differences.",
    "Do not speculate about causes or conclusions.",
    "Avoid mentioning colors or styling unless essential.",
    "Output plain text only."
  )

  user_prompt <- paste0("Plot title: ", label)
  if (!is.null(description) && nzchar(description)) {
    user_prompt <- paste0(user_prompt, "\nExisting context: ", description)
  }
  user_prompt <- paste0(
    user_prompt,
    "\nDescribe the plot as you would for scientific alt-text."
  )

  tryCatch(
    {
      content <- .plot_to_content(plot, width = width, height = height)
      if (is.null(content)) {
        return(description)
      }
      response <- .ask_ai_multimodal(
        system_prompt,
        user_prompt,
        content,
        api_key,
        model
      )
      response <- stringr::str_squish(response)
      if (!nzchar(response)) {
        description
      } else {
        response
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "AI plot description failed, using existing description.",
        "i" = "Error: {conditionMessage(e)}"
      ))
      description
    }
  )
}

.plot_to_content <- function(plot, width = NULL, height = NULL) {
  rlang::check_installed("ellmer")
  # Default dimensions for AI description if not specified
  width <- width %||% 768
  height <- height %||% 768
  tmp_file <- tempfile("glysmith-plot-", fileext = ".png")
  old_dev <- grDevices::dev.cur()
  grDevices::png(tmp_file, width = width, height = height)
  grDevices::dev.control("enable")
  print(plot)

  content <- tryCatch(
    ellmer::content_image_plot(width = width, height = height),
    error = function(e) NULL
  )

  grDevices::dev.off()
  if (old_dev > 1) {
    grDevices::dev.set(old_dev)
  }

  if (is.null(content) && file.exists(tmp_file)) {
    content <- ellmer::content_image_file(tmp_file, "image/png", resize = "low")
  }

  if (file.exists(tmp_file)) {
    fs::file_delete(tmp_file)
  }

  content
}

.humanize_on_label <- function(on) {
  on <- stringr::str_to_lower(on)
  purrr::map_chr(on, function(x) {
    switch(
      x,
      sig_exp = "significant variables",
      trait_exp = "traits",
      sig_trait_exp = "significant traits",
      dynamic_motif_exp = "dynamic motifs",
      sig_dynamic_motif_exp = "significant dynamic motifs",
      branch_motif_exp = "branch motifs",
      sig_branch_motif_exp = "significant branch motifs",
      exp = "variables",
      x
    )
  })
}

.on_suffix_labels <- function() {
  c(
    sig = "significant variables",
    trait = "traits",
    sig_trait = "significant traits",
    dynamic_motif = "dynamic motifs",
    sig_dynamic_motif = "significant dynamic motifs",
    branch_motif = "branch motifs",
    sig_branch_motif = "significant branch motifs"
  )
}

.extract_on_suffix <- function(parts) {
  if (length(parts) == 0) {
    return(list(suffix = NULL, rest = character(0)))
  }

  labels <- .on_suffix_labels()

  if (length(parts) >= 2) {
    candidate <- paste(parts[1:2], collapse = "_")
    if (candidate %in% names(labels)) {
      return(list(suffix = candidate, rest = parts[-c(1, 2)]))
    }
  }

  if (parts[1] %in% names(labels)) {
    return(list(suffix = parts[1], rest = parts[-1]))
  }

  list(suffix = NULL, rest = parts)
}

#' Organize report sections with AI.
#'
#' @param step_reports Step report entries.
#' @param plot_entries Plot entries.
#' @param api_key API key for the LLM.
#' @param model AI model to use.
#' @noRd
.organize_report_sections <- function(
  step_reports,
  plot_entries,
  api_key,
  model = "deepseek-chat"
) {
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
    if (length(step_lines) > 0) {
      paste(step_lines, collapse = "\n")
    } else {
      "(none)"
    },
    "",
    "Plots:",
    if (length(plot_lines) > 0) {
      paste(plot_lines, collapse = "\n")
    } else {
      "(none)"
    },
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
    cli::cli_warn(
      "AI section organization returned an invalid plan; using default order."
    )
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

#' Extract section titles in order.
#'
#' @param section_plan Parsed section plan.
#' @noRd
.section_titles <- function(section_plan) {
  titles <- purrr::map_chr(section_plan, function(section) {
    section$title %||% "Analysis"
  })
  titles[!duplicated(titles)]
}

.plan_step_sections <- function(section_plan, step_ids) {
  step_sections <- character(0)
  for (section in section_plan) {
    title <- section$title %||% "Analysis"
    items <- section$items %||% list()
    for (item in items) {
      if (!is.list(item) || item$type != "step") {
        next
      }
      if (!item$id %in% step_ids) {
        next
      }
      if (!item$id %in% names(step_sections)) {
        step_sections[[item$id]] <- title
      }
    }
  }
  step_sections
}

.plan_plot_sections <- function(section_plan, plot_ids) {
  plot_sections <- character(0)
  for (section in section_plan) {
    title <- section$title %||% "Analysis"
    items <- section$items %||% list()
    for (item in items) {
      if (!is.list(item) || item$type != "plot") {
        next
      }
      if (!item$id %in% plot_ids) {
        next
      }
      if (!item$id %in% names(plot_sections)) {
        plot_sections[[item$id]] <- title
      }
    }
  }
  plot_sections
}

.infer_plot_owner <- function(plot_ids, step_ids) {
  purrr::map_chr(plot_ids, function(plot_id) {
    candidates <- purrr::keep(step_ids, function(step_id) {
      if (plot_id == step_id) {
        return(TRUE)
      }
      if (stringr::str_starts(plot_id, paste0(step_id, "_"))) {
        return(TRUE)
      }
      if (stringr::str_starts(step_id, "sig_enrich_")) {
        suffix <- stringr::str_remove(step_id, "^sig_enrich_")
        if (plot_id == suffix) {
          return(TRUE)
        }
      }
      FALSE
    })

    if (length(candidates) == 0) {
      return(NA_character_)
    }
    candidates[which.max(nchar(candidates))]
  })
}

#' Assemble report sections from a plan.
#'
#' @param step_reports Step report entries.
#' @param plot_entries Plot entries.
#' @param section_plan Parsed section plan.
#' @noRd
.assemble_report_sections <- function(
  step_reports,
  plot_entries,
  section_plan
) {
  step_ids <- purrr::map_chr(step_reports, "id")
  plot_ids <- purrr::map_chr(plot_entries, "id")
  step_map <- rlang::set_names(step_reports, step_ids)
  plot_map <- rlang::set_names(plot_entries, plot_ids)

  section_titles <- .section_titles(section_plan)
  step_sections <- .plan_step_sections(section_plan, step_ids)
  plot_sections <- .plan_plot_sections(section_plan, plot_ids)

  plot_owner <- .infer_plot_owner(plot_ids, step_ids)
  names(plot_owner) <- plot_ids

  # Build plot section map using purrr for cleaner iteration
  plot_section_map <- purrr::map(plot_ids, function(plot_id) {
    owner <- plot_owner[[plot_id]]
    if (!is.na(owner) && owner %in% names(step_sections)) {
      step_sections[[owner]]
    } else if (plot_id %in% names(plot_sections)) {
      plot_sections[[plot_id]]
    } else {
      NA_character_
    }
  })
  names(plot_section_map) <- plot_ids

  # Build named sections
  used_plots <- character(0)
  sections <- list()

  for (title in section_titles) {
    result <- .build_section_entries(
      step_reports,
      step_sections,
      plot_map,
      plot_ids,
      plot_owner,
      plot_section_map,
      title,
      used_plots
    )
    entries <- result$entries
    used_plots <- c(used_plots, result$used_plots)

    if (length(entries) > 0) {
      sections <- c(sections, list(list(title = title, entries = entries)))
    }
  }

  # Build additional results section for unassigned steps/plots
  remaining_plots <- plot_ids[!plot_ids %in% used_plots]
  remaining_steps <- step_ids[!step_ids %in% names(step_sections)]
  if (length(remaining_steps) > 0 || length(remaining_plots) > 0) {
    remaining_section <- .build_remaining_section(
      step_reports,
      step_sections,
      step_ids,
      plot_map,
      plot_ids,
      plot_owner,
      step_map,
      remaining_plots,
      used_plots
    )
    if (!is.null(remaining_section)) {
      sections <- c(sections, remaining_section)
    }
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
    entries <- purrr::keep(entries, ~ isTRUE(.x$has_content))
    if (length(entries) > 0) {
      sections <- append(
        sections,
        list(list(title = "Analysis narrative", entries = entries))
      )
    }
  }
  if (length(plot_entries) > 0) {
    entries <- purrr::map(plot_entries, .plot_entry)
    sections <- append(sections, list(list(title = "Plots", entries = entries)))
  }
  sections
}

.has_report_content <- function(content) {
  is.character(content) && length(content) == 1 && nzchar(content)
}

#' Collect step entries for a given section title.
#' @noRd
.get_step_entries_for_section <- function(step_reports, step_sections, title) {
  steps_in_section <- purrr::keep(
    step_reports,
    ~ {
      .x$id %in% names(step_sections) && step_sections[[.x$id]] == title
    }
  )
  purrr::map(steps_in_section, .step_entry)
}

#' Collect owned plot IDs for a step in a specific section.
#' @noRd
.get_owned_plot_ids <- function(
  step_id,
  title,
  plot_ids,
  plot_owner,
  plot_section_map,
  used_plots
) {
  idx <- !is.na(plot_owner) &
    !is.na(plot_section_map) &
    plot_owner == step_id &
    plot_section_map == title &
    !plot_ids %in% used_plots
  plot_ids[idx]
}

#' Collect unowned plot IDs for a section (plots not assigned to any step).
#' @noRd
.get_unowned_plot_ids <- function(
  title,
  plot_ids,
  plot_owner,
  plot_section_map,
  used_plots
) {
  idx <- is.na(plot_owner) &
    !is.na(plot_section_map) &
    plot_section_map == title &
    !plot_ids %in% used_plots
  plot_ids[idx]
}

#' Build entries for a single section.
#' @noRd
.build_section_entries <- function(
  step_reports,
  step_sections,
  plot_map,
  plot_ids,
  plot_owner,
  plot_section_map,
  title,
  used_plots
) {
  entries <- .get_step_entries_for_section(step_reports, step_sections, title)

  # Add plots owned by steps in this section
  steps_in_section <- purrr::keep(
    step_reports,
    ~ {
      .x$id %in% names(step_sections) && step_sections[[.x$id]] == title
    }
  )
  for (step in steps_in_section) {
    owned_ids <- .get_owned_plot_ids(
      step$id,
      title,
      plot_ids,
      plot_owner,
      plot_section_map,
      used_plots
    )
    plot_entries <- purrr::map(owned_ids, ~ .plot_entry(plot_map[[.x]]))
    entries <- c(entries, plot_entries)
    used_plots <- c(used_plots, owned_ids)
  }

  # Add unowned plots assigned to this section
  unowned_ids <- .get_unowned_plot_ids(
    title,
    plot_ids,
    plot_owner,
    plot_section_map,
    used_plots
  )
  plot_entries <- purrr::map(unowned_ids, ~ .plot_entry(plot_map[[.x]]))
  entries <- c(entries, plot_entries)
  used_plots <- c(used_plots, unowned_ids)

  list(entries = entries, used_plots = used_plots)
}

#' Build additional results section from remaining steps and plots.
#' @noRd
.build_remaining_section <- function(
  step_reports,
  step_sections,
  step_ids,
  plot_map,
  plot_ids,
  plot_owner,
  step_map,
  remaining_plots,
  used_plots
) {
  entries <- list()

  remaining_steps <- step_ids[!step_ids %in% names(step_sections)]
  for (step_id in remaining_steps) {
    step <- step_map[[step_id]]
    if (.has_report_content(step$content)) {
      entries <- c(entries, list(.step_entry(step)))
    }
    owned_ids <- plot_ids[
      !is.na(plot_owner) &
        plot_owner == step_id &
        plot_ids %in% remaining_plots &
        !plot_ids %in% used_plots
    ]
    plot_entries <- purrr::map(owned_ids, ~ .plot_entry(plot_map[[.x]]))
    entries <- c(entries, plot_entries)
    used_plots <- c(used_plots, owned_ids)
  }

  # Add any remaining unassigned plots
  final_remaining <- plot_ids[!plot_ids %in% used_plots]
  plot_entries <- purrr::map(final_remaining, ~ .plot_entry(plot_map[[.x]]))
  entries <- c(entries, plot_entries)

  if (length(entries) > 0) {
    list(list(title = "Additional results", entries = entries))
  } else {
    NULL
  }
}

.step_entry <- function(step) {
  list(
    type = "step",
    id = step$id,
    label = step$label %||% step$id,
    content = step$content,
    has_content = .has_report_content(step$content)
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
