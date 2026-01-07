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
  expect_error(glysmith:::.get_api_key(), "API key for DeepSeek chat model is not set")
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
