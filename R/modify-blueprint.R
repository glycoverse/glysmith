#' Modify a Blueprint using Natural Language
#'
#' `r lifecycle::badge("experimental")`
#' Ask a Large Language Model (LLM) to modify an existing blueprint for glycomics
#' or glycoproteomics data analysis. To use this function, you need to have a
#' DeepSeek API key. You can get a DeepSeek API key from https://platform.deepseek.com.
#' Then set the environment variable `DEEPSEEK_API_KEY` to your API key with
#' `Sys.setenv(DEEPSEEK_API_KEY = "your-api-key")`.
#'
#' @details
#' LLMs can be unstable. If you get an error, try again with another description.
#' Make sure to examine the returned blueprint carefully to ensure it's what you want.
#' This function is a companion of [inquire_blueprint()].
#'
#' @param bp A `glysmith_blueprint` object.
#' @param description A description of how you want to modify the blueprint.
#' @param exp Optional. A `glyexp::experiment()` object to provide more context to the LLM.
#' @param group_col The column name of the group variable in the experiment. Default to "group".
#' @param model Model to use. Default to "deepseek-reasoner".
#' @param max_retries Maximum number of retries when the AI output is invalid. Default to 3.
#'
#' @export
modify_blueprint <- function(
  bp,
  description,
  exp = NULL,
  group_col = "group",
  model = "deepseek-reasoner",
  max_retries = 3
) {
  checkmate::assert_class(bp, "glysmith_blueprint")
  checkmate::assert_string(description)
  checkmate::assert_class(exp, "glyexp_experiment", null.ok = TRUE)
  checkmate::assert_string(group_col)
  checkmate::assert_choice(model, c("deepseek-reasoner", "deepseek-chat"))
  checkmate::assert_count(max_retries)
  rlang::check_installed("ellmer")

  api_key <- .get_api_key()
  system_prompt <- .modify_blueprint_sys_prompt()

  chat <- ellmer::chat_deepseek(
    system_prompt = system_prompt,
    model = model,
    echo = "none",
    credentials = function() api_key
  )

  exp_info <- .generate_exp_info(exp, group_col)
  bp_info <- .format_blueprint_for_prompt(bp)
  current_prompt <- paste0(
    exp_info, "\n",
    "Current blueprint:\n",
    bp_info, "\n",
    "Modification request: ", description
  )

  for (i in 0:max_retries) {
    if (i > 0) {
      cli::cli_alert_info("Attempt {i}/{max_retries}: Retrying with feedback...")
    }

    output <- as.character(chat$chat(current_prompt))
    result <- .process_blueprint_response(output)

    if (result$valid) {
      if (!is.null(result$explanation) && nzchar(result$explanation)) {
        cli::cli_h3("Blueprint Description")
        cli::cli_text(result$explanation)
      }
      return(result$blueprint)
    }

    error_msg <- result$error
    if (i < max_retries) {
      current_prompt <- paste0(
        "The previous blueprint was invalid:\n",
        error_msg, "\n",
        "Please fix the output and return a JSON object with `steps`."
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

.modify_blueprint_sys_prompt <- function() {
  step_descriptions <- .generate_step_descriptions()

  prompt <- paste(
    "You are a professional omics data scientist and glycobiologist.",
    "Your job is to modify an existing blueprint for glycomics or glycoproteomics data analysis.",
    "A blueprint is a list of analytical steps and parameters to be used in the analysis.",
    "You will receive a current blueprint and a modification request.",
    "Return a full updated blueprint, not a diff or patch.",
    "If no changes are needed, return the original blueprint.",
    "Use `br(\"name\", step_..., step_...)` ONLY for creating parallel analysis branches that represent alternative approaches (e.g., comparing two methods).",
    "Do NOT use `br()` for grouping sequential steps or organizing the workflow. Prefer a single linear sequence of steps.",
    "Use step arguments with caution: prefer default values unless they are necessary.",
    "The only exception is the `on` argument, which is stable and controls data flow; set it when needed.",
    "Available analytical steps include:\n",
    step_descriptions,
    "\n",
    "Return format:",
    "Return ONLY a JSON object with:",
    "- `explanation`: a BRIEF description (1-3 sentences) of the updated blueprint.",
    "- `steps`: an array of strings, each being a `step_...()` call or `br(...)` branch.",
    "Do not include any extra text outside JSON.",
    "Example output:",
    "{\"explanation\":\"Add a heatmap after PCA.\",\"steps\":[\"step_preprocess()\",\"step_pca()\",\"step_heatmap()\"]}",
    sep = "\n"
  )
  prompt
}

.format_blueprint_for_prompt <- function(bp) {
  checkmate::assert_class(bp, "glysmith_blueprint")
  if (length(bp) == 0) {
    return("(empty)")
  }

  steps <- purrr::map_chr(bp, function(step) {
    signature <- step$signature %||% paste0("step_", step$id, "()")
    stringr::str_trim(signature)
  })

  paste(steps, collapse = ";")
}
