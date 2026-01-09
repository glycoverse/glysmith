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

.print_ai_thinking <- function(api_key) {
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
    .generate_glycan_fact(api_key),
    error = function(e) .glycan_fun_fact()
  )
  styled_fact <- cli::style_italic(cli::col_silver(fact))
  cli::cli_text("{fact}", .envir = rlang::env(fact = styled_fact))
}

.generate_glycan_fact <- function(api_key, model = "deepseek-chat") {
  focus <- sample(c(
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
  ), 1)
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
    "Topic:", focus,
    "Avoid generic or repeated facts; prefer a less common but accurate detail.",
    "Random seed:", nonce
  )
  fact <- .ask_ai(system_prompt, user_prompt, api_key, model = model)
  .normalize_glycan_fact(fact)
}

.normalize_glycan_fact <- function(fact) {
  fact <- stringr::str_squish(as.character(fact))
  fact <- stringr::str_remove(fact, "^[-*]\\s*")
  fact <- stringr::str_remove(fact, "^[\"'`]+")
  if (!stringr::str_detect(fact, stringr::regex("^Do you know that", ignore_case = TRUE))) {
    fact <- paste0("Do you know that ", fact)
  }
  if (!stringr::str_detect(fact, "[.!?]$")) {
    fact <- paste0(fact, "?")
  }
  fact
}

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

.get_api_key <- function() {
  api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (api_key == "") {
    cli::cli_abort(c(
      "API key for DeepSeek chat model is not set.",
      "i" = "Please set the environment variable `DEEPSEEK_API_KEY` to your API key.",
      "i" = "You can obtain an API key from https://platform.deepseek.com."
    ))
  }
  api_key
}

.ask_ai <- function(system_prompt, user_prompt, api_key, model = "deepseek-chat") {
  rlang::check_installed("ellmer")
  chat <- ellmer::chat_deepseek(
    system_prompt = system_prompt,
    model = model,
    echo = "none",
    credentials = function() api_key
  )
  as.character(chat$chat(user_prompt))
}

.ask_ai_multimodal <- function(system_prompt, user_prompt, content, api_key, model = "deepseek-chat") {
  rlang::check_installed("ellmer")
  chat <- ellmer::chat_deepseek(
    system_prompt = system_prompt,
    model = model,
    echo = "none",
    credentials = function() api_key
  )
  as.character(chat$chat(content, user_prompt))
}
