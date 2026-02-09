test_that("ask_overwrite_dir and ask_overwrite_file use readline prompts", {
  dir_prompt <- NULL
  file_prompt <- NULL

  local_mocked_bindings(
    readline = function(prompt) {
      dir_prompt <<- prompt
      "y"
    },
    .package = "base"
  )
  expect_equal(glysmith:::.ask_overwrite_dir(), "y")
  expect_true(grepl("Directory already exists", dir_prompt, fixed = TRUE))

  local_mocked_bindings(
    readline = function(prompt) {
      file_prompt <<- prompt
      "n"
    },
    .package = "base"
  )
  expect_equal(glysmith:::.ask_overwrite_file(), "n")
  expect_true(grepl("File already exists", file_prompt, fixed = TRUE))
})

test_that("get_api_key reads from environment", {
  withr::local_envvar(c(DEEPSEEK_API_KEY = "test-key"))
  expect_equal(glysmith:::.get_api_key(), "test-key")
})

test_that("get_api_key errors when missing", {
  withr::local_envvar(c(DEEPSEEK_API_KEY = ""))
  expect_error(
    glysmith:::.get_api_key(),
    "API key for DeepSeek chat model is not set"
  )
})

test_that("ask_ai uses ellmer chat", {
  skip_if_not_installed("ellmer")

  captured <- list()
  local_mocked_bindings(
    chat_deepseek = function(system_prompt, model, echo, credentials) {
      captured$system_prompt <<- system_prompt
      captured$model <<- model
      captured$echo <<- echo
      captured$key <<- credentials()
      list(chat = function(prompt) {
        captured$user_prompt <<- prompt
        "mock-response"
      })
    },
    .package = "ellmer"
  )

  response <- glysmith:::.ask_ai("sys", "user", "key", model = "deepseek-chat")
  expect_equal(response, "mock-response")
  expect_equal(captured$system_prompt, "sys")
  expect_equal(captured$user_prompt, "user")
  expect_equal(captured$key, "key")
})

test_that("ask_ai_multimodal passes content to chat", {
  skip_if_not_installed("ellmer")

  captured <- list()
  local_mocked_bindings(
    chat_deepseek = function(system_prompt, model, echo, credentials) {
      captured$system_prompt <<- system_prompt
      captured$model <<- model
      list(chat = function(content, prompt) {
        captured$content <<- content
        captured$user_prompt <<- prompt
        "mock-response"
      })
    },
    .package = "ellmer"
  )

  response <- glysmith:::.ask_ai_multimodal(
    "sys",
    "user",
    content = list(type = "image"),
    api_key = "key",
    model = "deepseek-chat"
  )
  expect_equal(response, "mock-response")
  expect_equal(captured$content$type, "image")
  expect_equal(captured$user_prompt, "user")
})

# Tests for .print_ai_thinking
test_that(".print_ai_thinking calls glycan_fact functions", {
  skip_if_not_installed("cli")

  # Capture output from cli functions
  withr::local_options(list(cli.num_colors = 1))

  # The function uses cli::cli_text which doesn't print to stdout
  # We just verify it doesn't error and produces some output via messages
  expect_error(
    suppressMessages(glysmith:::.print_ai_thinking("fake-api-key")),
    NA
  )
})

test_that(".print_ai_thinking uses fallback fact on API error", {
  local_mocked_bindings(
    .generate_glycan_fact = function(...) stop("API error"),
    .package = "glysmith"
  )

  # Should use fallback without erroring
  expect_error(
    suppressMessages(glysmith:::.print_ai_thinking("fake-api-key")),
    NA
  )
})

# Tests for .generate_glycan_fact
test_that(".generate_glycan_fact returns a question starting with 'Do you know that'", {
  skip_if_not_installed("ellmer")

  local_mocked_bindings(
    .ask_ai = function(...) {
      "Do you know that glycans are important for cell recognition?"
    },
    .package = "glysmith"
  )

  fact <- glysmith:::.generate_glycan_fact("fake-api-key")
  expect_true(grepl("^Do you know that", fact))
  expect_true(grepl("\\?$", fact))
})

test_that(".generate_glycan_fact normalizes fact without question mark", {
  skip_if_not_installed("ellmer")

  local_mocked_bindings(
    .ask_ai = function(...) "Glycans are important",
    .package = "glysmith"
  )

  fact <- glysmith:::.generate_glycan_fact("fake-api-key")
  expect_true(grepl("\\?$", fact))
})

# Tests for .normalize_glycan_fact
test_that(".normalize_glycan_fact adds prefix if missing", {
  result <- glysmith:::.normalize_glycan_fact("glycans are diverse")
  expect_true(grepl("^Do you know that", result))
  expect_true(grepl("\\?$", result))
})

test_that(".normalize_glycan_fact removes leading bullets and quotes", {
  result <- glysmith:::.normalize_glycan_fact('* "glycans are diverse"')
  expect_false(grepl("^[*-]", result))
  expect_false(grepl('^"', result))
})

test_that(".normalize_glycan_fact squishes whitespace", {
  result <- glysmith:::.normalize_glycan_fact("  glycans   are  diverse  ")
  expect_false(grepl("  ", result))
})

test_that(".normalize_glycan_fact preserves valid format", {
  result <- glysmith:::.normalize_glycan_fact(
    "Do you know that glycans are diverse?"
  )
  expect_equal(result, "Do you know that glycans are diverse?")
})

# Tests for .glycan_fun_fact
test_that(".glycan_fun_fact returns a valid fact", {
  fact <- glysmith:::.glycan_fun_fact()
  expect_true(grepl("^Do you know that", fact))
  expect_true(grepl("\\?$", fact))
  expect_true(any(
    c(
      "glycans can be branched" = fact,
      "N-glycosylation" = fact,
      "sialic acids" = fact,
      "genome" = fact,
      "glycosylation" = fact
    ) %in%
      fact
  ))
})

# Tests for .is_interactive
test_that(".is_interactive returns FALSE in test environment", {
  # TESTTHAT is set during test runs
  result <- glysmith:::.is_interactive()
  expect_false(result)
})

test_that(".is_interactive checks interactive status", {
  # In a non-interactive R session, this should be FALSE
  # When TESTTHAT is set, it should always return FALSE
  if (nzchar(Sys.getenv("TESTTHAT"))) {
    expect_false(glysmith:::.is_interactive())
  }
})

# Tests for .ask_overwrite_dir and .ask_overwrite_file edge cases
test_that("ask_overwrite_dir handles different yes variations", {
  for (ans in c("Y", "y ", " Y")) {
    local_mocked_bindings(
      readline = function(prompt) ans,
      .package = "base"
    )
    result <- glysmith:::.ask_overwrite_dir()
    expect_equal(result, ans)
  }
})

test_that("ask_overwrite_file handles different yes variations", {
  for (ans in c("Y", "y ", " Y")) {
    local_mocked_bindings(
      readline = function(prompt) ans,
      .package = "base"
    )
    result <- glysmith:::.ask_overwrite_file()
    expect_equal(result, ans)
  }
})
