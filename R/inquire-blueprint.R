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
#'
#' Here are some examples that works:
#'
#' - "I want to know what pathways are enriched for my differentially expressed glycoforms."
#' - "I want a heatmap and a pca plot. I have already performed preprocessing myself."
#' - "I have a glycomics dataset. I want to calculate derived traits and perform DEA on them."
#'
#' @param description A description of what you want to analysis.
#' @param model Model to use. Default to "deepseek-reasoner".
#'
#' @export
inquire_blueprint <- function(description, model = "deepseek-reasoner") {
  checkmate::assert_string(description)
  checkmate::assert_choice(model, c("deepseek-reasoner", "deepseek-chat"))

  api_key <- .get_api_key()
  system_prompt <- .inquire_blueprint_sys_prompt()
  output <- .ask_ai(system_prompt, description, api_key, model)

  .raise_invalid_ai_output <- function(output) {
    cli::cli_abort(c(
      "Invalid AI output.",
      "x" = "AI output: {output}",
      "i" = "AI can be wrong. Please try another description or create a blueprint manually."
    ))
  }

  # Clean up the output - remove backticks and trim whitespace
  output <- stringr::str_remove_all(output, "`")
  output <- stringr::str_trim(output)

  steps <- stringr::str_split_1(output, ";")
  steps <- stringr::str_trim(steps)
  steps <- steps[steps != ""]

  if (length(steps) == 0) {
    .raise_invalid_ai_output(output)
  }
  if (!all(stringr::str_detect(steps, "^step_.*?\\(.*?\\)$"))) {
    .raise_invalid_ai_output(output)
  }

  # Parse and evaluate each step
  step_objects <- tryCatch(
    {
      purrr::map(steps, function(step_str) {
        expr <- rlang::parse_expr(step_str)
        eval(expr)
      })
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to parse AI output as step functions.",
        "x" = "AI output: {output}",
        "x" = "Error: {e$message}",
        "i" = "AI can be wrong. Please try another description or create a blueprint manually."
      ))
    }
  )

  # Create and return the blueprint
  names(step_objects) <- purrr::map_chr(step_objects, "id")
  bp <- new_blueprint(step_objects)
  validate_blueprint(bp)
  bp
}

.inquire_blueprint_sys_prompt <- function() {
  prompt <- paste(
    "You are a professional omics data scientist and glycobiologist.",
    "Your job is to create a blueprint for glycomics or glycoproteomics data analysis.",
    "A blueprint is a list of analytical steps and parameters to be used in the analysis.",
    "Available analytical steps include:\n",
    "- `step_preprocess`",
    "  - FUNCTION: Perform normalization, imputation, and other preprocessing operations.",
    "  - USAGE: Usually the first step, but can be skipped if user says the data has been preprocessed.",
    "- `step_ident_overview`",
    "  - FUNCTION: Generate identification summary including number of glycans or glycoforms.",
    "  - USAGE: Usually after `step_preprocess`. Always use.",
    "- `step_pca`",
    "  - FUNCTION: Perform principal component analysis (tables and plots).",
    "  - USAGE: When user asks for PCA analysis.",
    "- `step_dea_limma`, `step_dea_ttest`, `step_dea_anova`, `step_dea_wilcox`, `step_dea_kruskal`",
    "  - FUNCTION: Perform differential analysis.",
    "  - USAGE: When user asks for DEA analysis or DEA analysis is needed. If the user doesn't explicitly specify DEA method, use step_dea_limma. Otherwise, use the method specified.",
    "  - PARAMETER:",
    "    - `on`: Dataset to perform DEA. One of 'exp' or 'trait_exp'. Default is 'exp'.",
    "- `step_heatmap`",
    "  - FUNCTION: Create a heatmap of the data.",
    "  - USAGE: When user asks for a heatmap or DEA is performed. If DEA is performed, set `on` to 'sig_exp' or 'sig_trait_exp'.",
    "  - PARAMETER:",
    "    - `on`: Dataset to draw heatmap. One of 'exp', 'sig_exp', 'trait_exp', or 'sig_trait_exp'. Default is 'exp'.",
    "  - DEPENDENCY:",
    "    - If `on` is `sig_exp`, one of `step_dea_xxx` steps must be used before.",
    "    - If `on` is `trait_exp`, `step_derive_traits` must be used before.",
    "    - If `on` is `sig_trait_exp`, both `step_derive_traits` and one of `step_dea_xxx(on = 'trait_exp')` steps must be used before.",
    "- `step_volcano`",
    "  - FUNCTION: Create a volcano plot of the data.",
    "  - USAGE: Always use when DEA is performed on 'exp'.",
    "  - DEPENDENCY: One of `step_dea_xxx` steps must be used before.",
    "- `step_sig_enrich_go`, `step_sig_enrich_kegg`, `step_sig_enrich_reactome`",
    "  - FUNCTION: Perform gene ontology, KEGG, and Reactome enrichment analysis.",
    "  - USAGE: If the user asks for enrichment or biological functions.",
    "  - DEPENDENCY: One of `step_dea_xxx(on = 'sig_exp')` steps must be used before.",
    "- `step_derive_traits`",
    "  - FUNCTION: Calculate derived traits from the experiment.",
    "  - USAGE: If the user asks for traits or trait analysis, or want some advaned glycan structure analysis.",
    "\n",
    "Return: ONLY RETURN A list of analytical steps and parameters in function calls, separated by `;`.",
    "For example:",
    "`step_preprocess();step_ident_overview();step_pca();step_dea_limma();step_heatmap(on = 'sig_exp')`",
    "Do NOT add any explanation.",
    sep = "\n"
  )
  prompt
}