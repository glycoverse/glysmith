#' Ask if user wants to overwrite an existing directory
#'
#' This helper exists to keep the prompt and input on the same line (via
#' `readline(prompt = ...)`) and to make the interactive behavior testable.
#'
#' @returns User input string.
#' @noRd
.ask_overwrite_dir <- function() {
  # Use readline prompt to keep user input on the same line.
  prompt <- paste0("\u2139 ", "Directory already exists. Overwrite? [y/N] ")
  readline(prompt = prompt)
}

#' Ask if user wants to overwrite an existing file
#'
#' This helper exists to keep the prompt and input on the same line (via
#' `readline(prompt = ...)`) and to make the interactive behavior testable.
#'
#' @returns User input string.
#' @noRd
.ask_overwrite_file <- function() {
  # Use readline prompt to keep user input on the same line.
  prompt <- paste0("\u2139 ", "File already exists. Overwrite? [y/N] ")
  readline(prompt = prompt)
}

#' Print an AI progress message and glycan fact
#'
#' @param api_key API key for the selected AI provider.
#' @param provider AI provider name.
#' @param model Optional model name.
#' @param base_url Optional provider base URL.
#' @noRd
.print_ai_thinking <- function(
  api_key,
  provider = "deepseek",
  model = NULL,
  base_url = NULL
) {
  messages <- c(
    "Forging the blueprint...",
    "Hammering out the details...",
    "Heating up the forge...",
    "Shaping the workflow...",
    "Tempering the analysis...",
    "Burning in the blueprint...",
    "Melting ideas into form...",
    "Casting the analysis...",
    "Analyzing the data...",
    "Polishing the plan..."
  )
  cli::cli_text(cli::style_bold(cli::col_blue(sample(messages, 1))))
  fact <- tryCatch(
    .generate_glycan_fact(
      api_key,
      provider = provider,
      model = model,
      base_url = base_url
    ),
    error = function(e) .glycan_fun_fact()
  )
  styled_fact <- cli::style_italic(cli::col_silver(fact))
  cli::cli_text("{fact}", .envir = rlang::env(fact = styled_fact))
}

#' Generate a short glycan fact with the configured AI provider
#'
#' @param api_key API key for the selected AI provider.
#' @param model Optional model name.
#' @param provider AI provider name.
#' @param base_url Optional provider base URL.
#' @returns A normalized one-sentence glycan fact.
#' @noRd
.generate_glycan_fact <- function(
  api_key,
  model = NULL,
  provider = "deepseek",
  base_url = NULL
) {
  focus <- sample(
    c(
      "structural diversity",
      "biosynthesis",
      "glycosylation sites",
      "glycan functions",
      "analytical methods",
      "glycoforms",
      "immune recognition",
      "glycan degradation",
      "glycan nomenclature",
      "lectin-glycan interactions",
      "glycan microarrays",
      "O-glycosylation",
      "enzymatic glycan synthesis",
      "glycan analysis by mass spectrometry",
      "glycan-mediated cell signaling",
      "evolution of glycan structures",
      "sialic acid diversity",
      "glycan roles in disease",
      "glycoengineering",
      "glycan-protein interactions",
      "therapeutic glycoproteins"
    ),
    1
  )
  nonce <- sample.int(1000000L, 1)
  system_prompt <- paste(
    "You are a glycobiology tutor.",
    "Provide one accurate, concise fact about glycans.",
    "Use ASCII characters only.",
    "Return a single sentence that starts with 'Do you know that' and ends with a question mark.",
    "No bullets, no quotes, no extra text."
  )
  user_prompt <- paste(
    "Return one sentence as instructed.",
    "Topic:",
    focus,
    "Avoid generic or repeated facts; prefer a less common but accurate detail.",
    "Random seed:",
    nonce
  )
  fact <- .ask_ai(
    system_prompt,
    user_prompt,
    api_key,
    model = model,
    provider = provider,
    base_url = base_url
  )
  .normalize_glycan_fact(fact)
}

#' Normalize AI-generated glycan fact text
#'
#' @param fact Raw fact text returned by the AI provider.
#' @returns A single question beginning with "Do you know that".
#' @noRd
.normalize_glycan_fact <- function(fact) {
  fact <- stringr::str_squish(as.character(fact))
  fact <- stringr::str_remove(fact, "^[-*]\\s*")
  fact <- stringr::str_remove(fact, "^[\"'`]+")
  if (
    !stringr::str_detect(
      fact,
      stringr::regex("^Do you know that", ignore_case = TRUE)
    )
  ) {
    fact <- paste0("Do you know that ", fact)
  }
  if (!stringr::str_detect(fact, "[.!?]$")) {
    fact <- paste0(fact, "?")
  }
  fact
}

#' Return a local fallback glycan fact
#'
#' @returns A one-sentence glycan fact.
#' @noRd
.glycan_fun_fact <- function() {
  facts <- c(
    "Do you know that glycans can be branched, creating huge diversity from a small set of monosaccharides?",
    "Do you know that N-glycosylation commonly occurs at the sequon Asn-X-Ser/Thr in proteins?",
    "Do you know that sialic acids often cap glycan chains and influence protein half-life in circulation?",
    "Do you know that glycan structures are not directly encoded by the genome but assembled by enzymes?",
    "Do you know that glycosylation can modulate cell-cell recognition and immune responses?"
  )
  sample(facts, 1)
}

#' Supported AI provider names
#'
#' @returns Character vector of provider choices accepted by glysmith.
#' @noRd
.ai_provider_choices <- function() {
  c(
    "deepseek",
    "openai",
    "anthropic",
    "gemini",
    "google_gemini",
    "openrouter",
    "openai_compatible"
  )
}

#' Normalize AI provider aliases
#'
#' @param provider Provider name or supported alias.
#' @returns Canonical provider name.
#' @noRd
.normalize_ai_provider <- function(
  provider = getOption("glysmith.ai_provider", "deepseek")
) {
  provider <- rlang::arg_match(provider, .ai_provider_choices())
  if (identical(provider, "google_gemini")) {
    return("gemini")
  }
  provider
}

#' Human-readable AI provider label
#'
#' @param provider Provider name.
#' @returns Provider label for messages.
#' @noRd
.ai_provider_label <- function(provider) {
  switch(
    .normalize_ai_provider(provider),
    deepseek = "DeepSeek",
    openai = "OpenAI",
    anthropic = "Anthropic",
    gemini = "Google Gemini",
    openrouter = "OpenRouter",
    openai_compatible = "OpenAI-compatible"
  )
}

#' Environment variable for an AI provider API key
#'
#' @param provider Provider name.
#' @returns Environment variable name.
#' @noRd
.ai_provider_envvar <- function(provider) {
  switch(
    .normalize_ai_provider(provider),
    deepseek = "DEEPSEEK_API_KEY",
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    gemini = "GEMINI_API_KEY",
    openrouter = "OPENROUTER_API_KEY",
    openai_compatible = "OPENAI_API_KEY"
  )
}

#' Resolve the default AI model for a provider
#'
#' @param provider Provider name.
#' @param model Optional model name supplied by the caller.
#' @returns Model name, or `NULL` to use the provider default.
#' @noRd
.resolve_ai_model <- function(
  provider = getOption("glysmith.ai_provider", "deepseek"),
  model = getOption("glysmith.ai_model", NULL)
) {
  provider <- .normalize_ai_provider(provider)
  if (!is.null(model)) {
    return(model)
  }
  if (identical(provider, "deepseek")) {
    return("deepseek-chat")
  }
  NULL
}

#' Resolve an AI API key
#'
#' @param provider Provider name.
#' @param api_key Optional explicit API key.
#' @returns API key string.
#' @noRd
.get_api_key <- function(
  provider = getOption("glysmith.ai_provider", "deepseek"),
  api_key = getOption("glysmith.ai_api_key", NULL)
) {
  if (!is.null(api_key) && nzchar(api_key)) {
    return(api_key)
  }
  provider <- .normalize_ai_provider(provider)
  envvar <- .ai_provider_envvar(provider)
  api_key <- Sys.getenv(envvar)
  if (api_key == "") {
    label <- .ai_provider_label(provider)
    cli::cli_abort(c(
      "API key for {label} chat model is not set.",
      "i" = "Please set the environment variable `{envvar}` to your API key, or pass `api_key` directly.",
      "i" = "For OpenAI-compatible endpoints, set `OPENAI_API_KEY` and pass `base_url`."
    ))
  }
  api_key
}

#' Create an ellmer chat object for a configured provider
#'
#' @param system_prompt System prompt for the chat object.
#' @param api_key API key for the selected provider.
#' @param provider Provider name.
#' @param model Optional model name.
#' @param base_url Optional provider base URL.
#' @returns An `ellmer` chat object.
#' @noRd
.create_ai_chat <- function(
  system_prompt,
  api_key,
  provider = getOption("glysmith.ai_provider", "deepseek"),
  model = getOption("glysmith.ai_model", NULL),
  base_url = getOption("glysmith.ai_base_url", NULL)
) {
  rlang::check_installed("ellmer")
  provider <- .normalize_ai_provider(provider)
  model <- .resolve_ai_model(provider, model)

  args <- list(
    system_prompt = system_prompt,
    model = model,
    echo = "none",
    credentials = function() api_key
  )
  if (is.null(model)) {
    args$model <- NULL
  }

  chat_fun <- switch(
    provider,
    deepseek = ellmer::chat_deepseek,
    openai = ellmer::chat_openai,
    anthropic = ellmer::chat_anthropic,
    gemini = ellmer::chat_google_gemini,
    openrouter = ellmer::chat_openrouter,
    openai_compatible = ellmer::chat_openai_compatible
  )

  if (identical(provider, "openai_compatible")) {
    if (is.null(base_url)) {
      cli::cli_abort(
        "`base_url` is required when `provider = \"openai_compatible\"`."
      )
    }
    args <- c(list(base_url = base_url, name = "OpenAI-compatible"), args)
  } else if (!is.null(base_url)) {
    args$base_url <- base_url
  }

  do.call(chat_fun, args)
}

#' Send a text-only AI request
#'
#' @param system_prompt System prompt for the AI request.
#' @param user_prompt User prompt for the AI request.
#' @param api_key API key for the selected provider.
#' @param model Optional model name.
#' @param provider Provider name.
#' @param base_url Optional provider base URL.
#' @returns Character AI response.
#' @noRd
.ask_ai <- function(
  system_prompt,
  user_prompt,
  api_key,
  model = getOption("glysmith.ai_model", NULL),
  provider = getOption("glysmith.ai_provider", "deepseek"),
  base_url = getOption("glysmith.ai_base_url", NULL)
) {
  chat <- .create_ai_chat(
    system_prompt = system_prompt,
    api_key = api_key,
    provider = provider,
    model = model,
    base_url = base_url
  )
  as.character(chat$chat(user_prompt))
}

#' Send a multimodal AI request
#'
#' @param system_prompt System prompt for the AI request.
#' @param user_prompt User prompt for the AI request.
#' @param content Multimodal content passed to `ellmer`.
#' @param api_key API key for the selected provider.
#' @param model Optional model name.
#' @param provider Provider name.
#' @param base_url Optional provider base URL.
#' @returns Character AI response.
#' @noRd
.ask_ai_multimodal <- function(
  system_prompt,
  user_prompt,
  content,
  api_key,
  model = getOption("glysmith.ai_model", NULL),
  provider = getOption("glysmith.ai_provider", "deepseek"),
  base_url = getOption("glysmith.ai_base_url", NULL)
) {
  chat <- .create_ai_chat(
    system_prompt = system_prompt,
    api_key = api_key,
    provider = provider,
    model = model,
    base_url = base_url
  )
  as.character(chat$chat(content, user_prompt))
}

#' Check interactive session for testable behavior
#'
#' @returns `TRUE` when running interactively.
#' @noRd
.is_interactive <- function() {
  interactive() && !nzchar(Sys.getenv("TESTTHAT"))
}

# Data-container helpers -------------------------------------------------------

#' Check a glysmith data container
#'
#' @param exp A `glyexp::experiment()`, `GlycomicSE`, or `GlycoproteomicSE`
#'   object.
#'
#' @returns `NULL` invisibly, or an error if `exp` is unsupported.
#' @noRd
.assert_data_container <- function(exp) {
  if (
    glyexp::is_experiment(exp) ||
      glyexp::is_glycomic_se(exp) ||
      glyexp::is_glycoproteomic_se(exp)
  ) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    paste0(
      "{.arg exp} must be a {.cls glyexp_experiment}, ",
      "{.cls GlycomicSE}, or {.cls GlycoproteomicSE} object."
    )
  )
}

#' Convert a supported data container to a glyco SummarizedExperiment
#'
#' @inheritParams .assert_data_container
#'
#' @returns A `GlycomicSE` or `GlycoproteomicSE` object.
#' @noRd
.as_glyco_se <- function(exp) {
  if (glyexp::is_glycomic_se(exp) || glyexp::is_glycoproteomic_se(exp)) {
    return(exp)
  }

  switch(
    glyexp::get_exp_type(exp),
    glycomics = glyexp::as_glycomic_se(exp),
    glycoproteomics = glyexp::as_glycoproteomic_se(exp),
    cli::cli_abort(
      "{.arg exp} must contain glycomics or glycoproteomics data."
    )
  )
}

#' Restore a legacy experiment container
#'
#' @param exp A data object produced while running a blueprint.
#' @param legacy Whether the original input was a `glyexp::experiment()`.
#'
#' @returns `exp`, converted to `glyexp::experiment()` when appropriate.
#' @noRd
.restore_data_container <- function(exp, legacy) {
  if (legacy && methods::is(exp, "SummarizedExperiment")) {
    return(glyexp::from_se(exp))
  }
  exp
}

#' Adapt a SummarizedExperiment for a legacy-only dependency
#'
#' @param exp A `SummarizedExperiment` object.
#'
#' @returns A `glyexp::experiment()` with the same assay and annotations.
#' @noRd
.as_legacy_experiment <- function(exp) {
  glyexp::from_se(exp)
}

#' Extract sample information
#'
#' @param exp A glyco `SummarizedExperiment` object.
#'
#' @returns A tibble with a `sample` identifier column.
#' @noRd
.get_sample_info <- function(exp) {
  if (glyexp::is_experiment(exp)) {
    return(glyexp::get_sample_info(exp))
  }
  info <- SummarizedExperiment::colData(exp)
  if ("sample" %in% colnames(info)) {
    return(tibble::as_tibble(info))
  }
  tibble::as_tibble(info, rownames = "sample")
}

#' Replace sample information
#'
#' @param exp A glyco `SummarizedExperiment` object.
#' @param sample_info A data frame containing replacement sample information.
#'
#' @returns `exp` with updated `colData`.
#' @noRd
.set_sample_info <- function(exp, sample_info) {
  if (glyexp::is_experiment(exp)) {
    exp$sample_info <- sample_info
    return(exp)
  }
  samples <- sample_info[["sample"]] %||% colnames(exp)
  sample_info <- dplyr::select(sample_info, -dplyr::any_of("sample"))
  SummarizedExperiment::colData(exp) <- S4Vectors::DataFrame(
    sample_info,
    row.names = samples,
    check.names = FALSE
  )
  exp
}

#' Extract variable information
#'
#' @inheritParams .get_sample_info
#'
#' @returns A tibble with a `variable` identifier column.
#' @noRd
.get_var_info <- function(exp) {
  if (glyexp::is_experiment(exp)) {
    return(glyexp::get_var_info(exp))
  }
  info <- SummarizedExperiment::rowData(exp)
  if ("variable" %in% colnames(info)) {
    return(tibble::as_tibble(info))
  }
  tibble::as_tibble(info, rownames = "variable")
}

#' Replace variable information
#'
#' @param exp A glyco `SummarizedExperiment` object.
#' @param var_info A data frame containing replacement variable information.
#'
#' @returns `exp` with updated `rowData`.
#' @noRd
.set_var_info <- function(exp, var_info) {
  if (glyexp::is_experiment(exp)) {
    exp$var_info <- var_info
    return(exp)
  }
  variables <- var_info[["variable"]] %||% rownames(exp)
  var_info <- dplyr::select(var_info, -dplyr::any_of("variable"))
  SummarizedExperiment::rowData(exp) <- S4Vectors::DataFrame(
    var_info,
    row.names = variables,
    check.names = FALSE
  )
  exp
}

#' Extract the experiment type
#'
#' @inheritParams .get_sample_info
#'
#' @returns Either `"glycomics"` or `"glycoproteomics"`.
#' @noRd
.get_exp_type <- function(exp) {
  if (glyexp::is_experiment(exp)) {
    return(glyexp::get_exp_type(exp))
  }
  if (glyexp::is_glycomic_se(exp)) {
    return("glycomics")
  }
  "glycoproteomics"
}

#' Extract the glycan type
#'
#' @inheritParams .get_sample_info
#'
#' @returns The glycan type stored in `metadata(exp)`.
#' @noRd
.get_glycan_type <- function(exp) {
  if (glyexp::is_experiment(exp)) {
    return(glyexp::get_glycan_type(exp))
  }
  S4Vectors::metadata(exp)[["glycan_type"]]
}

#' Subset samples by group
#'
#' @inheritParams .get_sample_info
#' @param groups Group values to retain, in the desired factor-level order.
#'
#' @returns A class-preserving subset of `exp`.
#' @noRd
.filter_groups <- function(exp, groups) {
  sample_info <- .get_sample_info(exp)
  keep <- sample_info[["group"]] %in% groups
  exp <- exp[, keep]
  sample_info <- .get_sample_info(exp)
  sample_info[["group"]] <- factor(sample_info[["group"]], levels = groups)
  .set_sample_info(exp, sample_info)
}

#' Subset variables by identifier
#'
#' @inheritParams .get_sample_info
#' @param variables Variable identifiers to retain.
#'
#' @returns A class-preserving subset of `exp`.
#' @noRd
.filter_variables <- function(exp, variables) {
  exp[rownames(exp) %in% variables, ]
}

#' Convert a glyco SummarizedExperiment to long-form data
#'
#' @inheritParams .get_sample_info
#'
#' @returns A tibble containing sample information, variable information, and
#'   an abundance `value` column.
#' @noRd
.as_data_tibble <- function(exp) {
  expr_mat <- as.matrix(SummarizedExperiment::assay(exp, 1))
  values <- tibble::tibble(
    sample = rep(colnames(exp), times = nrow(exp)),
    variable = rep(rownames(exp), each = ncol(exp)),
    value = as.vector(t(expr_mat))
  )
  values |>
    dplyr::left_join(.get_sample_info(exp), by = "sample") |>
    dplyr::left_join(.get_var_info(exp), by = "variable") |>
    dplyr::select(
      .data$sample,
      dplyr::any_of(setdiff(colnames(.get_sample_info(exp)), "sample")),
      .data$variable,
      dplyr::any_of(setdiff(colnames(.get_var_info(exp)), "variable")),
      .data$value
    )
}
