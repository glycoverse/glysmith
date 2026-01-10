#' Create a Blueprint using Natural Language
#'
#' `r lifecycle::badge("experimental")`
#' Ask a Large Language Model (LLM) to create a blueprint for glycomics or glycoproteomics data analysis.
#' To use this function, you need to have a DeepSeek API key.
#' You can get a DeepSeek API key from https://platform.deepseek.com.
#' Then set the environment variable `DEEPSEEK_API_KEY` to your API key with
#' `Sys.setenv(DEEPSEEK_API_KEY = "your-api-key")`.
#'
#' @details
#' LLMs can be unstable. If you get an error, try again with another description.
#' Make sure to examine the returned blueprint carefully to ensure it's what you want.
#' You can also create parallel analysis branches with `br("name", step_..., step_...)`,
#' which will namespace outputs with the branch prefix.
#' If the LLM needs required information to proceed, it may ask clarifying questions
#' interactively and then retry with your answers.
#' After a blueprint is generated, the description is printed and, in interactive
#' sessions, you can press ENTER to accept it or type new requirements to refine
#' the blueprint. This review step can repeat until you accept the plan.
#'
#' Here are some examples that works:
#'
#' - "I want to know what pathways are enriched for my differentially expressed glycoforms."
#' - "I want a heatmap and a pca plot. I have already performed preprocessing myself."
#' - "I have a glycomics dataset. I want to calculate derived traits and perform DEA on them."
#'
#' @param description A description of what you want to analysis.
#' @param exp Optional. A `glyexp::experiment()` object to provide more context to the LLM.
#' @param group_col The column name of the group variable in the experiment. Default to "group".
#' @param model Model to use. Default to "deepseek-reasoner".
#' @param max_retries Maximum number of retries when the AI output is invalid. Default to 3.
#'
#' @export
inquire_blueprint <- function(description, exp = NULL, group_col = "group", model = "deepseek-reasoner", max_retries = 3) {
  checkmate::assert_string(description)
  checkmate::assert_class(exp, "glyexp_experiment", null.ok = TRUE)
  checkmate::assert_string(group_col)
  checkmate::assert_choice(model, c("deepseek-reasoner", "deepseek-chat"))
  checkmate::assert_count(max_retries)
  rlang::check_installed("ellmer")

  api_key <- .get_api_key()
  system_prompt <- .inquire_blueprint_sys_prompt()

  chat <- ellmer::chat_deepseek(
    system_prompt = system_prompt,
    model = model,
    echo = "none",
    credentials = function() api_key
  )

  # Initial prompt with dataset info
  exp_info <- .generate_exp_info(exp, group_col)
  current_prompt <- paste0(exp_info, "\n", "Requirements: ", description)

  retry_count <- 0L
  question_count <- 0L
  max_questions <- max(1L, max_retries)

  repeat {
    if (retry_count > 0) {
      cat("\n")
      cli::cli_alert_info("Attempt {retry_count}/{max_retries}: Retrying with feedback...")
      cat("\n")
    }

    # Call AI
    .print_ai_thinking(api_key)
    output <- as.character(chat$chat(current_prompt))
    result <- .process_blueprint_response(output)

    if (!is.null(result$questions)) {
      question_count <- question_count + 1L
      if (question_count > max_questions) {
        cli::cli_abort(c(
          "Failed to generate a valid blueprint after {max_questions} clarification rounds.",
          "x" = "The LLM keeps requesting more information.",
          "i" = "Please provide more details in the description and try again."
        ))
      }
      inquiry <- .ask_inquiry_questions(result$questions)
      current_prompt <- paste0(
        current_prompt,
        "\nClarifications:\n",
        .format_inquiry_answers(inquiry$questions, inquiry$answers)
      )
      next
    }

    if (result$valid) {
      return(.review_blueprint(
        result$blueprint,
        explanation = result$explanation,
        exp = exp,
        group_col = group_col,
        model = model,
        max_retries = max_retries
      ))
    }

    # Handle failure
    error_msg <- result$error
    if (retry_count < max_retries) {
      retry_count <- retry_count + 1L
      current_prompt <- paste0(
        "The previous blueprint was invalid:\n",
        error_msg, "\n",
        "Please fix the output and return a JSON object with `steps` (or `questions`)."
      )
    } else {
      cli::cli_abort(c(
        "Failed to generate a valid blueprint after {max_retries} retries.",
        "x" = "Last error: {error_msg}",
        "i" = "Please try a different description or inspect the AI output."
      ))
    }
  }
}

.process_blueprint_response <- function(output) {
  output_clean <- stringr::str_trim(output)
  json_text <- .extract_json_output(output_clean)

  if (is.null(json_text)) {
    questions <- .extract_inquiry_questions(output_clean)
    if (!is.null(questions)) {
      if (length(questions) == 0) {
        return(list(valid = FALSE, error = "Clarification questions requested but none provided."))
      }
      return(list(valid = FALSE, questions = questions, error = "Clarification requested."))
    }
    return(list(
      valid = FALSE,
      error = "Invalid JSON output. Return a JSON object with `steps` (array of strings) or `questions` (array of strings)."
    ))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(json_text),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    normalized_json <- .normalize_windows_paths(json_text)
    if (!identical(normalized_json, json_text)) {
      parsed <- tryCatch(
        jsonlite::fromJSON(normalized_json),
        error = function(e) e
      )
    }
  }
  if (inherits(parsed, "error")) {
    return(list(valid = FALSE, error = paste("Invalid JSON output:", conditionMessage(parsed))))
  }
  if (!is.list(parsed)) {
    return(list(valid = FALSE, error = "Invalid JSON output. Expected a JSON object."))
  }

  questions <- parsed$questions %||% NULL
  if (!is.null(questions)) {
    if (!is.character(questions) || length(questions) == 0) {
      return(list(valid = FALSE, error = "Clarification questions requested but none provided."))
    }
    return(list(valid = FALSE, questions = questions, error = "Clarification requested."))
  }

  steps <- parsed$steps %||% character(0)
  if (!is.character(steps)) {
    return(list(valid = FALSE, error = "Invalid JSON output. `steps` must be an array of strings."))
  }
  steps <- stringr::str_remove_all(steps, "`")
  steps <- stringr::str_trim(steps)
  if (length(steps) == 1 && stringr::str_detect(steps, ";")) {
    steps <- stringr::str_split_1(steps, ";")
    steps <- stringr::str_trim(steps)
  }
  steps <- stringr::str_remove(steps, ";$")
  steps <- steps[steps != ""]

  if (length(steps) == 0) {
    return(list(valid = FALSE, error = "The output is empty. Please provide steps in the JSON `steps` array."))
  }

  is_step <- stringr::str_detect(steps, "^step_[a-z0-9_]+\\s*\\(.*\\)$")
  is_branch <- stringr::str_detect(steps, "^br\\s*\\(.*\\)$")
  if (!all(is_step | is_branch)) {
    return(list(valid = FALSE, error = "Invalid format. Every entry must be a `step_...()` call or a `br(...)` branch."))
  }

  explanation <- parsed$explanation %||% NULL
  if (!is.null(explanation)) {
    explanation <- as.character(explanation)
  }

  # Try parsing and validation
  tryCatch(
    {
      # Parse and evaluate
      step_objects <- purrr::map(steps, function(step_str) {
        expr <- .parse_step_expr(step_str)
        eval(expr)
      })

      bp <- do.call(blueprint, step_objects)

      # If we get here, everything is valid
      list(valid = TRUE, blueprint = bp, explanation = explanation)
    },
    error = function(e) {
      list(valid = FALSE, error = paste("Error:", conditionMessage(e)))
    }
  )
}

.extract_json_output <- function(output) {
  output <- stringr::str_trim(output)
  if (!nzchar(output)) return(NULL)

  fence_match <- stringr::str_match(
    output,
    "(?s)```(?:json)?\\s*(\\{.*\\})\\s*```"
  )
  if (!is.na(fence_match[1, 2])) {
    return(stringr::str_trim(fence_match[1, 2]))
  }

  start_loc <- stringr::str_locate(output, "\\{")[1]
  end_locs <- stringr::str_locate_all(output, "\\}")[[1]]
  if (is.na(start_loc) || nrow(end_locs) == 0) return(NULL)

  end_loc <- end_locs[nrow(end_locs), 2]
  json_text <- substr(output, start_loc, end_loc)
  json_text <- stringr::str_trim(json_text)
  if (!nzchar(json_text)) return(NULL)
  json_text
}

.parse_step_expr <- function(step_str) {
  tryCatch(
    rlang::parse_expr(step_str),
    error = function(e) {
      msg <- conditionMessage(e)
      if (!grepl("used without hex digits|unrecognized escape", msg, ignore.case = TRUE)) {
        stop(e)
      }
      fixed_step <- .normalize_windows_paths(step_str)
      rlang::parse_expr(fixed_step)
    }
  )
}

.normalize_windows_paths <- function(text) {
  pattern <- "(['\"])([A-Za-z]:\\\\[^'\"]*|\\\\\\\\[^'\"]*)(\\1)"
  stringr::str_replace_all(text, pattern, function(match) {
    quote <- stringr::str_sub(match, 1, 1)
    path <- stringr::str_sub(match, 2, -2)
    normalized <- .normalize_windows_path(path)
    paste0(quote, normalized, quote)
  })
}

.normalize_windows_path <- function(path) {
  if (!stringr::str_detect(path, "\\\\")) return(path)
  parts <- stringr::str_split(path, "\\\\")[[1]]
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(path)
  normalized <- as.character(do.call(fs::path, as.list(parts)))
  if (stringr::str_starts(path, "\\\\")) {
    normalized <- paste0("//", normalized)
  }
  normalized
}

.inquire_blueprint_sys_prompt <- function() {
  step_descriptions <- .generate_step_descriptions()

  prompt <- paste(
    "You are a professional omics data scientist and glycobiologist.",
    "Your job is to create a blueprint for glycomics or glycoproteomics data analysis.",
    "\n",
    "A blueprint is a list of analytical steps and parameters to be used in the analysis.",
    "A step is a function call with arguments.",
    "Use step arguments with caution: prefer default values unless they are necessary.",
    "The only exception is the `on` argument, which is stable and controls data flow; set it when needed.",
    "Each step includes a USAGE section that has the highest priority. Follow it even if other text differs.",
    "\n",
    "Available analytical steps include:",
    step_descriptions,
    "\n",
    "You can use `br()` to create branches in a blueprint.",
    "For example, `step_preprocess();br('limma', step_dea_limma(), step_volcano());br('ttest', step_dea_ttest(), step_volcano())`.",
    "Use branches for creating parallel analysis branches that represent alternative approaches (e.g., comparing two methods).",
    "`br()` can also be used for grouping sequential steps or organizing the workflow.",
    "For example, steps about motif analysis can be grouped into a branch called 'motif',",
    "and steps about derived traits can be grouped into 'trait'.",
    "\n",
    "When in doubt, ASK! Clarifying questions lead to better analysis plans.",
    "If the user's request is vague, unclear, or appears to be a greeting, ask what they want to accomplish.",
    "NEEDLESS steps like PCA, heatmap, correlation, derived traits, or motif analysis are NOT necessary by default.",
    "Only include a step if the user explicitly requests it or if it's clearly required for their stated goal.",
    "If unclear, ask: 'What analysis do you want to perform?' rather than assuming.",
    "Less is more: do not add steps that are not explicitly requested or necessary.",
    "Keep the blueprint focused and avoid adding extra steps just because they seem useful.",
    "A minimal 5 step blueprint is better than a complex 15+ step one.",
    "If you need clarification, return ONLY a JSON object with a `questions` array.",
    "Do NOT include `steps` when asking questions.",
    "Do NOT ask for the column name of groups.",
    "\n",
    "If you think all information is provided, return a JSON object with:",
    "- `explanation`: a BRIEF description (1-3 sentences).",
    "- `steps`: an array of strings, each being a `step_...()` call or `br(...)` branch.",
    "Do not include any extra text outside JSON.",
    "\n",
    "Example output 1:",
    "{\"explanation\":\"Preprocess, then run PCA and DEA.\",\"steps\":[\"step_preprocess()\",\"step_pca()\",\"step_dea_limma()\",\"step_heatmap(on = 'sig_exp')\"]}",
    "",
    "Example output 2 (Branching):",
    "{\"explanation\":\"Compare two DEA methods after preprocessing.\",\"steps\":[\"step_preprocess()\",\"br('limma', step_dea_limma(), step_volcano())\",\"br('ttest', step_dea_ttest(), step_volcano())\"]}",
    "",
    "Example output 3 (Questions):",
    "{\"questions\":[\"What is your group column?\",\"Do you want pathway enrichment?\"]}",
    sep = "\n"
  )
  prompt
}

.generate_exp_info <- function(exp, group_col) {
  if (is.null(exp)) return("")

  n_samples <- ncol(exp)
  n_variables <- nrow(exp)
  glycan_type <- glyexp::get_glycan_type(exp)
  exp_type <- glyexp::get_exp_type(exp)
  has_structure <- "glycan_structure" %in% colnames(exp$var_info)
  n_groups <- length(unique(exp$sample_info[[group_col]]))

  # Generate summaries for sample_info and var_info
  sample_info_summary <- .summarize_tibble(exp$sample_info, "sample_info", group_col)
  var_info_summary <- .summarize_tibble(exp$var_info, "var_info")

  paste0(
    "\nDataset information:\n",
    "- Number of samples: ", n_samples, "\n",
    "- Number of variables (glycans/glycopeptides): ", n_variables, "\n",
    "- Glycan type: ", glycan_type, "\n",
    "- Experiment type: ", exp_type, "\n",
    "- Glycan structure available: ", if (has_structure) "Yes" else "No", "\n",
    "- Number of groups: ", n_groups, "\n",
    sample_info_summary, "\n",
    var_info_summary, "\n"
  )
}

#' Summarize a tibble/data.frame for LLM context
#' @noRd
.summarize_tibble <- function(tbl, name, highlight_col = NULL) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(paste0("- ", name, ": (empty)"))
  }

  # Use dplyr::glimpse for compact summary
  # Capture output and add highlighting for group column
  glimpse_output <- utils::capture.output(dplyr::glimpse(tbl))

  # Remove header line from glimpse (e.g., "Rows: 100")
  glimpse_output <- glimpse_output[-1]

  # Add group column highlight if specified
  if (!is.null(highlight_col)) {
    # Match pattern like "$ group <chr>" or "$group <chr>" from glimpse output
    pattern <- paste0("\\$ ", highlight_col, " <")
    replacement <- paste0("$ ", highlight_col, " [GROUP COLUMN] <")
    glimpse_output <- stringr::str_replace(glimpse_output, pattern, replacement)
  }

  # Prepend tibble name and indent all lines
  header <- paste0("- ", name, " (", nrow(tbl), " rows, ", ncol(tbl), " columns)")
  indented <- paste0("    ", glimpse_output, collapse = "\n")
  paste0(header, "\n", indented)
}

.generate_step_descriptions <- function() {
  steps <- all_steps()
  rd_db <- .get_rd_database()

  desc_list <- purrr::map_chr(steps, function(step_obj) {
    # Extract function name from signature: step_xxx(...) -> step_xxx
    func_name <- stringr::str_extract(step_obj$signature, "^[a-z0-9_]+")

    # Defaults if documentation not found
    title <- step_obj$label
    desc_text <- "No description available."
    params_text <- "  - PARAMETER: Not available."
    ai_text <- ""

    if (!is.null(rd_db)) {
      # Try to find the Rd file for this function
      # First try direct match
      rd <- rd_db[[paste0(func_name, ".Rd")]]

      # If not found, look through aliases
      if (is.null(rd)) {
        for (rd_name in names(rd_db)) {
          aliases <- .get_rd_tag_values(rd_db[[rd_name]], "\\alias")
          if (func_name %in% aliases) {
            rd <- rd_db[[rd_name]]
            break
          }
        }
      }

      if (!is.null(rd)) {
        title <- .get_rd_tag_text(rd, "\\title")
        description <- .get_rd_tag_text(rd, "\\description")
        details <- .get_rd_tag_text(rd, "\\details")
        ai_prompt <- .get_rd_section_text(rd, "AI Prompt")
        desc_text <- paste(description, details)
        # Clean up newlines and excessive spaces
        desc_text <- stringr::str_squish(desc_text)
        ai_text <- stringr::str_squish(ai_prompt)
        ai_text <- stringr::str_remove(ai_text, stringr::fixed("This section is for AI in inquire_blueprint() only."))

        # Extract arguments
        args <- .get_rd_arguments(rd)
        # Filter out "..." and arguments that shouldn't be touched by LLM usually
        args <- args[!names(args) %in% c("...", "signature")]
        args <- .filter_inquire_blueprint_args(func_name, args)

        if (length(args) > 0) {
          # Format parameters
          params_lines <- purrr::imap_chr(args, function(desc, name) {
            paste0("    - `", name, "`: ", stringr::str_squish(desc))
          })
          params_text <- paste0("  - PARAMETER:\n", paste(params_lines, collapse = "\n"))
        } else {
          params_text <- "  - PARAMETER: None."
        }
      }
    }

    # Construct the block
    # - `step_name`
    #   - USAGE: ...
    #   - FUNCTION: title. description.
    #   - PARAMETER: ...

    block <- paste0(
      "- `", func_name, "`\n",
      if (nzchar(ai_text)) paste0("  - USAGE: ", ai_text, "\n") else "",
      "  - FUNCTION: ", title, ". ", desc_text, "\n",
      if (nzchar(params_text)) paste0(params_text, "\n") else ""
    )
    block
  })

  paste(desc_list, collapse = "\n")
}

.filter_inquire_blueprint_args <- function(func_name, args) {
  if (length(args) == 0) return(args)
  # Placeholder for future filtering rules; keep all args for now.
  args
}

.extract_inquiry_questions <- function(output_clean) {
  lines <- stringr::str_split(output_clean, "\n")[[1]]
  if (length(lines) == 0) return(NULL)
  first <- stringr::str_trim(lines[1])
  if (!stringr::str_detect(first, stringr::regex("^QUESTIONS?:", ignore_case = TRUE))) {
    return(NULL)
  }

  rest <- stringr::str_trim(lines[-1])
  rest <- rest[rest != ""]
  rest <- stringr::str_remove(rest, "^[-*]\\s*")
  rest <- rest[rest != ""]

  if (length(rest) == 0) {
    inline <- stringr::str_trim(stringr::str_remove(first, stringr::regex("^QUESTIONS?:", ignore_case = TRUE)))
    if (nzchar(inline)) return(inline)
  }

  rest
}

.format_inquiry_answers <- function(questions, answers) {
  entries <- purrr::map2_chr(questions, answers, function(question, answer) {
    paste0("- Q: ", question, "\n  A: ", answer)
  })
  paste(entries, collapse = "\n")
}

.ask_inquiry_questions <- function(questions) {
  checkmate::assert_character(questions, min.len = 1)
  if (!.is_interactive()) {
    cli::cli_abort(c(
      "LLM requires more information to continue.",
      "x" = "This prompt needs interactive input for clarification.",
      "i" = "Re-run with the missing details included in `description`."
    ))
  }
  cli::cli_h3(cli::style_bold(cli::col_blue("Need more information")))
  cli::cli_text(cli::style_italic(cli::col_silver("Please answer the following questions:")))
  list_id <- cli::cli_ol(.close = FALSE)
  answers <- purrr::map_chr(questions, function(question) {
    cli::cli_li(cli::style_bold(cli::col_cyan(question)))
    prompt <- cli::style_bold(cli::col_green("  Answer: "))
    readline(prompt = prompt)
  })
  cli::cli_end(list_id)
  cat("\n")
  list(questions = questions, answers = answers)
}

#' Review a blueprint and optionally refine it with new requirements
#'
#' Prints the blueprint description, prompts for new requirements, and uses the
#' LLM to update the blueprint until the user accepts it.
#'
#' @param bp A `glysmith_blueprint` object.
#' @param explanation Optional description returned by the LLM.
#' @param exp Optional experiment metadata passed to [modify_blueprint()].
#' @param group_col Group column name passed to [modify_blueprint()].
#' @param model Model name passed to [modify_blueprint()].
#' @param max_retries Maximum retries passed to [modify_blueprint()].
#'
#' @returns A confirmed `glysmith_blueprint` object.
#' @noRd
.review_blueprint <- function(bp, explanation, exp, group_col, model, max_retries) {
  checkmate::assert_class(bp, "glysmith_blueprint")
  if (!is.null(explanation) && nzchar(explanation)) {
    .print_blueprint_explanation(explanation)
  }
  if (!.is_interactive()) {
    return(bp)
  }

  repeat {
    response <- .ask_blueprint_review()
    response <- stringr::str_trim(response)
    if (!nzchar(response)) {
      return(bp)
    }
    bp <- modify_blueprint(
      bp = bp,
      description = response,
      exp = exp,
      group_col = group_col,
      model = model,
      max_retries = max_retries
    )
  }
}

#' Ask the user to confirm or refine a blueprint
#'
#' This helper exists to keep the prompt and input on the same line (via
#' `readline(prompt = ...)`) and to make the interactive behavior testable.
#'
#' @returns User input string. Press ENTER (empty input) to accept.
#' @noRd
.ask_blueprint_review <- function() {
  cli::cli_h3(cli::style_bold(cli::col_blue("Review Blueprint")))
  prompt <- cli::style_bold(cli::col_green("Looks good? Press ENTER to accept, or type new requirements: "))
  response <- readline(prompt = prompt)
  cat("\n")
  response
}

.print_blueprint_explanation <- function(explanation) {
  cli::cli_h3(cli::style_bold(cli::col_blue("Blueprint Description")))
  desc <- explanation
  desc_id <- cli::cli_ul(.close = FALSE)
  cli::cli_li(desc)
  cli::cli_end(desc_id)
}

.get_rd_database <- function() {
  # Try to get Rd database from installed package
  rd_db <- tryCatch(tools::Rd_db("glysmith"), error = function(e) NULL)

  if (length(rd_db) == 0) {
    # Fallback for devtools::load_all() environment
    # Try to find man directory in package path
    # We assume we are in the package root or standard structure
    man_path <- system.file("man", package = "glysmith")
    if (!nzchar(man_path)) {
      # If system.file fails (e.g. not installed), try generic relative path if working dir is root
      if (dir.exists("man")) man_path <- "man"
    }

    if (nzchar(man_path) && dir.exists(man_path)) {
      rd_files <- list.files(man_path, pattern = "\\.Rd$", full.names = TRUE)
      if (length(rd_files) > 0) {
        rd_db <- lapply(rd_files, tools::parse_Rd)
        names(rd_db) <- basename(rd_files)
      }
    }
  }

  rd_db
}

.get_rd_tag <- function(rd, tag) {
  # Find all elements with matching Rd_tag
  tags <- purrr::map_chr(rd, function(x) attr(x, "Rd_tag") %||% "")
  ## Handling nested tags? Usually toplevel is enough for title/desc/alias
  # If tag not found at top level, maybe we don't search deep for now
  rd[tags == tag]
}

.get_rd_tag_values <- function(rd, tag) {
  # Get the values (as character) of a tag
  # e.g. for \alias{step_pca} -> "step_pca"
  elements <- .get_rd_tag(rd, tag)
  purrr::map_chr(elements, function(x) as.character(x))
}

.parse_rd_content <- function(x) {
  if (is.list(x)) {
    paste(purrr::map_chr(x, .parse_rd_content), collapse = "")
  } else {
    as.character(x)
  }
}

.clean_rd_text <- function(text) {
  text <- stringr::str_replace_all(text, "\\\\link\\{(.*?)\\}", "\\1")
  text <- stringr::str_replace_all(text, "\\\\code\\{(.*?)\\}", "`\\1`")
  text <- stringr::str_replace_all(text, "\\\\emph\\{(.*?)\\}", "*\\1*")
  text <- stringr::str_replace_all(text, "\\\\[a-zA-Z]+\\{(.*?)\\}", "\\1")
  text
}

.get_rd_tag_text <- function(rd, tag) {
  elements <- .get_rd_tag(rd, tag)
  if (length(elements) == 0) return("")
  # Concatenate all matching elements (usually just one for title/desc)
  text <- paste(purrr::map_chr(elements, .parse_rd_content), collapse = " ")
  .clean_rd_text(text)
}

.get_rd_section_text <- function(rd, section_title) {
  sections <- rd[purrr::map_lgl(rd, function(x) attr(x, "Rd_tag") == "\\section")]
  if (length(sections) == 0) return("")
  for (section in sections) {
    if (length(section) < 2) next
    title <- stringr::str_squish(.parse_rd_content(section[[1]]))
    if (!identical(stringr::str_to_lower(title), stringr::str_to_lower(section_title))) next
    text <- .parse_rd_content(section[[2]])
    return(.clean_rd_text(text))
  }
  ""
}

.get_rd_arguments <- function(rd) {
  args_tag <- .get_rd_tag(rd, "\\arguments")
  if (length(args_tag) == 0) return(character(0))

  # \arguments is a list of \item{name}{desc}
  # Actually it's a list where some elements are \item and some are newlines
  args_list <- args_tag[[1]] # The content of \arguments

  items <- list()
  for (i in seq_along(args_list)) {
    el <- args_list[[i]]
    tag_attr <- attr(el, "Rd_tag")
    if (!is.null(tag_attr) && tag_attr == "\\item") {
      # \item{name}{desc}
      arg_name <- .parse_rd_content(el[[1]])
      arg_desc <- .parse_rd_content(el[[2]])
      # Remove newlines for cleaner text
      arg_name <- stringr::str_trim(arg_name)
      arg_name <- stringr::str_replace_all(arg_name, "\\\\[a-zA-Z]+\\{(.*?)\\}", "\\1")
      arg_desc <- stringr::str_replace_all(arg_desc, "\\n", " ")
      arg_desc <- stringr::str_replace_all(arg_desc, "\\\\link\\{(.*?)\\}", "\\1")
      arg_desc <- stringr::str_replace_all(arg_desc, "\\\\code\\{(.*?)\\}", "`\\1`")
      arg_desc <- stringr::str_replace_all(arg_desc, "\\\\[a-zA-Z]+\\{(.*?)\\}", "\\1")
      items[[arg_name]] <- arg_desc
    }
  }
  items
}
