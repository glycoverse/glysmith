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
