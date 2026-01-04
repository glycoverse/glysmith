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

  for (i in 0:max_retries) {
    if (i > 0) {
      cli::cli_alert_info("Attempt {i}/{max_retries}: Retrying with feedback...")
    }

    # Call AI
    output <- as.character(chat$chat(current_prompt))
    result <- .process_blueprint_response(output)

    if (result$valid) {
      if (!is.null(result$explanation) && nzchar(result$explanation)) {
        cli::cli_h3("Blueprint Description")
        cli::cli_text(result$explanation)
      }
      return(result$blueprint)
    }

    # Handle failure
    error_msg <- result$error
    if (i < max_retries) {
      current_prompt <- paste0(
        "The previous blueprint was invalid:\n",
        error_msg, "\n",
        "Please fix the blueprint and return the corrected list of steps."
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
  # Clean up the output - remove backticks and trim whitespace
  output_clean <- stringr::str_remove_all(output, "`")
  output_clean <- stringr::str_trim(output_clean)

  # Split explanation and steps by "---" delimiter
  explanation <- NULL
  if (stringr::str_detect(output_clean, "---")) {
    parts <- stringr::str_split_1(output_clean, "---")
    if (length(parts) >= 2) {
      explanation <- stringr::str_trim(parts[1])
      output_clean <- stringr::str_trim(parts[2])
    }
  }

  steps <- stringr::str_split_1(output_clean, ";")
  steps <- stringr::str_trim(steps)
  steps <- steps[steps != ""]

  if (length(steps) == 0) {
    return(list(valid = FALSE, error = "The output is empty. Please provide a list of steps separated by ';'."))
  }

  is_step <- stringr::str_detect(steps, "^step_[a-z0-9_]+\\s*\\(.*\\)$")
  is_branch <- stringr::str_detect(steps, "^br\\s*\\(.*\\)$")
  if (!all(is_step | is_branch)) {
    return(list(valid = FALSE, error = "Invalid format. Every entry must be a `step_...()` call or a `br(...)` branch. Split entries with ';'."))
  }

  # Try parsing and validation
  tryCatch(
    {
      # Parse and evaluate
      step_objects <- purrr::map(steps, function(step_str) {
        expr <- rlang::parse_expr(step_str)
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

.inquire_blueprint_sys_prompt <- function() {
  step_descriptions <- .generate_step_descriptions()

  prompt <- paste(
    "You are a professional omics data scientist and glycobiologist.",
    "Your job is to create a blueprint for glycomics or glycoproteomics data analysis.",
    "A blueprint is a list of analytical steps and parameters to be used in the analysis.",
    "Use `br(\"name\", step_..., step_...)` ONLY for creating parallel analysis branches that represent alternative approaches (e.g., comparing two methods).",
    "`br()` can also be used for grouping sequential steps or organizing the workflow.",
    "For example, steps about motif analysis can be grouped into a branch called 'motif analysis',",
    "and steps about derived traits can be grouped into 'trait analysis'.",
    "Use step arguments with caution: prefer default values unless they are necessary.",
    "The only exception is the `on` argument, which is stable and controls data flow; set it when needed.",
    "Available analytical steps include:\n",
    step_descriptions,
    "\n",
    "Return format:",
    "1. First, provide a BRIEF description (1-3 sentences) of the blueprint.",
    "2. Then write `---` on a new line.",
    "3. Finally, list analytical steps (or branches) as function calls separated by `;`.",
    "",
    "Example output 1:",
    "Your data needs preprocessing to handle missing values, followed by statistical analysis to find significant changes.",
    "---",
    "step_preprocess();step_pca();step_dea_limma();step_heatmap(on = 'sig_exp')",
    "",
    "Example output 2 (Branching):",
    "The blueprint compares two DEA methods after preprocessing.",
    "---",
    "step_preprocess();br('limma', step_dea_limma(), step_volcano());br('ttest', step_dea_ttest(), step_volcano())",
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

  paste0(
    "\nDataset information:\n",
    "- Number of samples: ", n_samples, "\n",
    "- Number of variables (glycans/glycopeptides): ", n_variables, "\n",
    "- Glycan type: ", glycan_type, "\n",
    "- Experiment type: ", exp_type, "\n",
    "- Glycan structure available: ", if (has_structure) "Yes" else "No", "\n",
    "- Number of groups: ", n_groups, "\n"
  )
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
    params_text <- ""

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
        desc_text <- paste(description, details)
        # Clean up newlines and excessive spaces
        desc_text <- stringr::str_squish(desc_text)

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
        }
      }
    }

    # Construct the block
    # - `step_name`
    #   - FUNCTION: title. description.
    #   - PARAMETER: ...

    block <- paste0(
      "- `", func_name, "`\n",
      "  - FUNCTION: ", title, ". ", desc_text, "\n",
      if (nzchar(params_text)) paste0(params_text, "\n") else ""
    )
    block
  })

  paste(desc_list, collapse = "\n")
}

.filter_inquire_blueprint_args <- function(func_name, args) {
  if (length(args) == 0) return(args)
  allowed <- args  # Currently all args are allowed.
  args[names(args) %in% allowed]
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

.get_rd_tag_text <- function(rd, tag) {
  elements <- .get_rd_tag(rd, tag)
  if (length(elements) == 0) return("")
  # Concatenate all matching elements (usually just one for title/desc)
  text <- paste(purrr::map_chr(elements, .parse_rd_content), collapse = " ")
  # Simple cleanup of LaTeX-like macros if not handled by recursion
  # Remove \link{...} keeping content
  text <- stringr::str_replace_all(text, "\\\\link\\{(.*?)\\}", "\\1")
  text <- stringr::str_replace_all(text, "\\\\code\\{(.*?)\\}", "`\\1`")
  text <- stringr::str_replace_all(text, "\\\\emph\\{(.*?)\\}", "*\\1*")
  # General catch-all for other macros
  text <- stringr::str_replace_all(text, "\\\\[a-zA-Z]+\\{(.*?)\\}", "\\1")
  text
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
